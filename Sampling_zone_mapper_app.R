# Load necessary packages
library(shiny)
library(leaflet)
library("leaflet.extras")
library(DT)
library(clipr)

setwd("C:/Users/crist/Dropbox/NewAtlantis/Bermuda/Sampling_planer_app/")


# Create the data frame as provided
all_zone_coords <- as.data.frame(
  matrix( c("Z1", 32.3414751, -64.6781091,
            "Z2", 32.3334034, -64.6488207,
            "Z3", 32.4675049, -64.5808043,
            "Z4.1", 32.3733700, -64.5480724,
            "Z4.2", 32.3733700, -64.5480724,
            "Z4.3", 32.3733700, -64.5480724,
            "Z5", 32.3744589, -64.7730149),
          ncol = 3, byrow = TRUE), 
  stringsAsFactors = FALSE
)
colnames(all_zone_coords) <- c("zone", "Latitude", "Longitude")
all_zone_coords$Latitude <- as.numeric(all_zone_coords$Latitude)
all_zone_coords$Longitude <- as.numeric(all_zone_coords$Longitude)

# Calculate the center of all_zone_coords
center_lat <- mean(all_zone_coords$Latitude)
center_lng <- mean(all_zone_coords$Longitude)

# Create a Shiny app to handle the leaflet map and coordinate capture
ui <- fluidPage(
  titlePanel("Geographical Zone Editor"),
  sidebarLayout(
    sidebarPanel(
      h4("Zone Coordinates Editor"),
      DTOutput("table"), # Table to display and edit zone coordinates
      actionButton("delete_mode", "Enable Delete Mode"),  # Remove class argument
      actionButton("save", "Save Data"),
      actionButton("copy_clipboard", "Copy to Clipboard"),
      hr(),
      textOutput("status"),
      p("Note: Delete mode must be activated to remove points from the map.")
    ),
    mainPanel(
      leafletOutput("map"),
      p("Click on the map to add new points. Drag markers to edit locations. Activate delete mode to remove points.")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive value to store current data and delete mode status
  zone_data <- reactiveValues(coords = all_zone_coords, delete_mode = FALSE)
  
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
      setView(lng = center_lng, lat = center_lat, zoom = 10) %>%
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
        }
      ")
  })
  
  # Observe when a new point is drawn
  observeEvent(input$map_draw_new_feature, {
    feature <- input$map_draw_new_feature
    if(feature$geometry$type == "Point") {
      new_coord <- data.frame(
        zone = paste0("New_", nrow(zone_data$coords) + 1),
        Latitude = feature$geometry$coordinates[[2]],
        Longitude = feature$geometry$coordinates[[1]],
        stringsAsFactors = FALSE
      )
      zone_data$coords <- rbind(zone_data$coords, new_coord)
      
      # Update the map with new points
      leafletProxy("map") %>%
        addMarkers(lng = new_coord$Longitude, lat = new_coord$Latitude, 
                   label = new_coord$zone,
                   layerId = new_coord$zone, 
                   labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE, style = list(color = "orange")))
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
  
  # Render table to edit zone names and coordinates
  output$table <- renderDT({
    datatable(zone_data$coords, editable = TRUE, rownames = FALSE)  # Disable rownames
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
