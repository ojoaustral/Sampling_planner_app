# Sampling_zone_mapper_app Shiny app ####
# Author: Cristian Correa
# Last modified: 2024-10-07 

# Load necessary packages
library(shiny)
library(leaflet)
library("leaflet.extras")
library(DT)
library(clipr)
library(dplyr)

# Create a Shiny app to handle the leaflet map and coordinate capture
ui <- fluidPage(
  titlePanel("Sampling  Zone Editor"),
  mainPanel(
    p("Select a marker and click on the map to add new sampling zones. Drag markers to edit locations. Enable 'Delete mode' and click to delete zones."),
    actionButton("delete_mode", "Enable Delete Mode"),  # Remove class argument
    leafletOutput("map"),
    br(),
    h4("Zone Coordinates Editor"),
    DTOutput("table"), # Table to display and edit zone coordinates
    actionButton("save", "Save Data"),
    actionButton("copy_clipboard", "Copy to Clipboard"),
    hr(),
    textOutput("status")
  )
)

server <- function(input, output, session) {
  
  # Load zone coordinates from CSV
  zone_coords <- read.csv("zone_coords.csv", stringsAsFactors = FALSE)
  
  # Convert Latitude and Longitude to numeric if needed
  zone_coords$Latitude <- as.numeric(zone_coords$Latitude)
  zone_coords$Longitude <- as.numeric(zone_coords$Longitude)
  
  # Convert all columns starting with "zone_selection" to logical
  zone_coords <- zone_coords %>%
    mutate(across(starts_with("zone_selection"), as.logical))
  
  # Reactive value to store current data and delete mode status
  zone_data <- reactiveValues(coords = zone_coords, delete_mode = FALSE)
  
  # Reactive value to store current map view settings
  map_view <- reactiveValues(lat = NULL, lng = NULL, zoom = NULL)
  
  # Toggle delete mode when the button is clicked
  observeEvent(input$delete_mode, {
    zone_data$delete_mode <- !zone_data$delete_mode
    if (zone_data$delete_mode) {
      updateActionButton(session, "delete_mode", label = "Disable Delete Mode")  # Update only label
    } else {
      updateActionButton(session, "delete_mode", label = "Enable Delete Mode")   # Update only label
    }
  })
  
  # Render the leaflet map with markers
  output$map <- renderLeaflet({
    leaflet(data = zone_data$coords) %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      addMarkers(~Longitude, ~Latitude, 
                 label = ~zone,
                 layerId = ~zone, 
                 labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE, style = list(color = "orange"))) %>%
      addDrawToolbar(
        targetGroup = 'clicks',
        polylineOptions = FALSE,
        polygonOptions = FALSE,
        circleOptions = FALSE,
        rectangleOptions = FALSE,
        markerOptions = drawMarkerOptions(),
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
      ) %>%
      setView(lng = mean(zone_data$coords$Longitude, na.rm = TRUE), lat = mean(zone_data$coords$Latitude, na.rm = TRUE), zoom = 10) %>%
      htmlwidgets::onRender("
        function(el, x) {
          var map = this;
          map.eachLayer(function(layer) {
            if (layer.options.layerId) {
              layer.dragging.enable(); // Make the marker draggable
              layer.on('dragend', function(e) {
                var marker = e.target;
                var position = marker.getLatLng();
                Shiny.setInputValue('dragged_marker', {
                  id: marker.options.layerId,
                  lat: position.lat,
                  lng: position.lng
                });
              });
            }
          });
          map.on('moveend', function() {
            Shiny.setInputValue('map_bounds', {
              lat: map.getCenter().lat,
              lng: map.getCenter().lng,
              zoom: map.getZoom()
            });
          });
        }
      ")
  })
  
  # Observe when the map view changes and store the lat/lng/zoom
  observeEvent(input$map_bounds, {
    map_view$lat <- input$map_bounds$lat
    map_view$lng <- input$map_bounds$lng
    map_view$zoom <- input$map_bounds$zoom
  })
  
  # Observe when a new point is drawn and add it without resetting the map view
  observeEvent(input$map_draw_new_feature, {
    feature <- input$map_draw_new_feature
    if (feature$geometry$type == "Point") {
      new_coord <- data.frame(
        zone = paste0("New_", nrow(zone_data$coords) + 1),
        Latitude = feature$geometry$coordinates[[2]],
        Longitude = feature$geometry$coordinates[[1]],
        stringsAsFactors = FALSE
      )
      
      # Create a row with default FALSE for all zone_selection_* columns
      default_values <- as.data.frame(lapply(
        zone_data$coords %>% select(starts_with("zone_selection")),
        function(x) FALSE
      ))
      
      # Combine the new coordinates with the default FALSE values for zone_selection columns
      new_row <- cbind(new_coord, default_values)
      
      # Glue the new points to the existing table of points
      zone_data$coords <- bind_rows(zone_data$coords, new_row)
      
      # Update the map without resetting the view
      leafletProxy("map") %>%
        addMarkers(lng = new_coord$Longitude, lat = new_coord$Latitude,
                   label = new_coord$zone,
                   layerId = new_coord$zone,
                   labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE, style = list(color = "orange"))) %>%
        setView(lng = map_view$lng, lat = map_view$lat, zoom = map_view$zoom)
    }
  })
  
  # Update coordinates when marker is dragged (using input from JS)
  observeEvent(input$dragged_marker, {
    marker_id <- input$dragged_marker$id
    new_lat <- input$dragged_marker$lat
    new_lng <- input$dragged_marker$lng
    
    # Update the data frame with new coordinates
    zone_data$coords[zone_data$coords$zone == marker_id, c("Latitude", "Longitude")] <- c(new_lat, new_lng)
  })
  
  # Observe click event on markers and delete only if delete mode is active
  observeEvent(input$map_marker_click, {
    marker_id <- input$map_marker_click$id
    
    # If delete mode is active, delete the marker
    if (zone_data$delete_mode) {
      proxy <- leafletProxy("map")
      proxy %>% removeMarker(layerId = marker_id)
      
      # Remove the point from the data frame
      zone_data$coords <- zone_data$coords[zone_data$coords$zone != marker_id, ]
    }
  })
  
  # Render table to edit zone names and coordinates, with logical columns as checkboxes
  output$table <- renderDT({
    datatable(
      zone_data$coords,
      editable = TRUE,  # Make the table editable
      rownames = FALSE, # Disable rownames
      selection = 'none', # Disable row selection
      options = list(
        pageLength = 50,  # Set default number of rows per page
        stateSave = TRUE,  # Preserve table state (pagination and search)
        columnDefs = list(
          list(
            targets = grep("zone_selection", names(zone_data$coords)) - 1, # Identify the logical columns
            render = JS(
              "function(data, type, row, meta) {",
              "return '<input type=\"checkbox\"' + (data == true ? ' checked' : '') + ' onclick=\"Shiny.setInputValue(\\'checkbox_change\\', {row: ' + meta.row + ', col: ' + meta.col + ', checked: this.checked})\" />';",
              "}"
            )
          )
        )
      )
    )
  })
  
  # Update the reactive data when checkboxes are clicked
  observeEvent(input$checkbox_change, {
    row <- input$checkbox_change$row + 1  # Adjust for zero-indexing
    col <- input$checkbox_change$col + 1  # Adjust for zero-indexing
    checked <- input$checkbox_change$checked
    
    # Update the reactive data frame
    zone_data$coords[row, col] <- as.logical(checked)
  })
  
  # Update the coordinates or zone name if edited in the table
  observeEvent(input$table_cell_edit, {
    info <- input$table_cell_edit
    
    # No need to adjust indices because we have disabled row names
    edited_row <- info$row
    edited_col <- info$col + 1
    
    # Update the reactive data frame with the new value
    zone_data$coords[edited_row, edited_col] <- info$value
  })
  
  # Save the updated data frame to a CSV file
  observeEvent(input$save, {
    write.csv(zone_data$coords, "updated_zone_coords.csv", row.names = FALSE)
    output$status <- renderText({"Data saved to updated_zone_coords.csv"})
  })
  
  # Copy the updated data frame to clipboard
  observeEvent(input$copy_clipboard, {
    write_clip(zone_data$coords)
    output$status <- renderText({"Data copied to clipboard"})
  })
}

# Run the shiny app
shinyApp(ui, server)


