# Load necessary packages

library(shiny)
library(leaflet)
library(leaflet.extras)
library(DT)
library(clipr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(lunar)

# Set working directory 
setwd("C:/Users/crist/Dropbox/NewAtlantis/Bermuda/Sampling_planer_app/")


# Define the UI with two tabs: Zone Mapper and Sampling Planner ####
ui <- navbarPage("Sampling Planner",
                 
                 # Tab 1: Zone Mapper
                 tabPanel("Zone Mapper",
                          fluidPage(
                            titlePanel("Sampling Zone Editor"),
                            mainPanel(
                              p("Select a marker and click on the map to add new sampling zones. Drag markers to edit locations. Enable 'Delete mode' and click to delete zones."),
                              actionButton("delete_mode", "Enable Delete Mode"),  
                              leafletOutput("map"),
                              br(),
                              h4("Zone Coordinates Editor"),
                              DTOutput("table"),  # Table to display and edit zone coordinates
                              actionButton("save", "Save Data"),
                              actionButton("copy_clipboard", "Copy to Clipboard"),
                              hr(),
                              textOutput("status")
                            )
                          )
                 ),
                 
                 # Tab 2: Sampling scheme
                 tabPanel("Sampling scheme",
                          fluidPage(
                            titlePanel("Sampling Calendar Visualization"),
                            sidebarLayout(
                              sidebarPanel(
                                div(class = "scrollable-sidebar",  # Apply scrolling class to the sidebar panel
                                    style = "max-height: 600px; overflow-y: auto;",  # Added for scrolling
                                    br(),
                                    br(),
                                    dateInput("start_date", "Start Date:", value = "2025-01-01"),
                                    dateInput("end_date", "End Date:", value = "2025-12-20"),
                                    # Choose backbone week type for calendar
                                    radioButtons("calendar_type", "Select Calendar Type:",
                                                 choices = list("Gregorian" = "gregorian", "Lunar" = "lunar"),
                                                 selected = "gregorian"),
                                    # Number of zones in Other zones 
                                    numericInput("n_otherzones", "Number of zones under the Other_zones umbrella:", value = 20, min = 1, max = 1000),
                                    
                                    # Number of replicas input
                                    numericInput("nrep_plankton", "Number of Replicas for Plankton:", value = 3, min = 1, max = 10),
                                    numericInput("nrep_eDNA", "Number of Replicas for eDNA:", value = 3, min = 1, max = 10),
                                    numericInput("nrep_eDNA2", "Number of Replicas for eDNA2:", value = 1, min = 1, max = 10),
                                    numericInput("nrep_sediment", "Number of Replicas for Sediment:", value = 3, min = 1, max = 10),
                                    numericInput("nrep_sediment2", "Number of Replicas for Sediment2:", value = 1, min = 1, max = 10),
                                    
                                    # Sampling frequencies input
                                    # textInput("freq_plankton", "Frequency for Plankton:", value = "2 weeks"),
                                    # textInput("freq_eDNA", "Frequency for eDNA:", value = "13 weeks"),
                                    # textInput("freq_eDNA2", "Frequency for eDNA2:", value = "52 weeks"),
                                    # textInput("freq_sediment", "Frequency for Sediment:", value = "26 weeks"),
                                    # textInput("freq_sediment2", "Frequency for Sediment2:", value = "52 weeks"),
                                    
                                    # Delays input
                                    numericInput("freq_plankton", "Frequency for Plankton (weeks):", value = 2, min = 0),
                                    numericInput("freq_eDNA", "Frequency for eDNA (weeks):", value = 13, min = 0),
                                    numericInput("freq_eDNA2", "Frequency for eDNA2 (weeks):", value = 52, min = 0),
                                    numericInput("freq_sediment", "Frequency for Sediment (weeks):", value = 26, min = 0),
                                    numericInput("freq_sediment2", "Frequency for Sediment2 (weeks):", value = 52, min = 0),
                                    
                                    # Delays input
                                    numericInput("delay_plankton", "Delay for Plankton (weeks):", value = 0, min = 0),
                                    numericInput("delay_eDNA", "Delay for eDNA (weeks):", value = 4, min = 0),
                                    numericInput("delay_eDNA2", "Delay for eDNA2 (weeks):", value = 17, min = 0),
                                    numericInput("delay_sediment", "Delay for Sediment (weeks):", value = 17, min = 0),
                                    numericInput("delay_sediment2", "Delay for Sediment2 (weeks):", value = 17, min = 0),
                                    
                                    # Add Save and Copy buttons to the sidebar
                                    actionButton("save_calendar", "Save Calendar Data"),
                                    actionButton("copy_calendar", "Copy Calendar to Clipboard")
                                )
                              ),
                              
                              mainPanel(
                                plotOutput("samplingPlot"),
                                tableOutput("samplingTable"),
                                hr(),
                                textOutput("status_calendar")  # Added for status messages
                              )
                            )
                          )
                 )
)

# Define the server logic ####
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
  
  # Reactive value to store current map view settings (HERE, set some value based on mean(zone_coords$Latitude) and mean(zone_coords$Longitude)
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
                 labelOptions = labelOptions(noHide = TRUE, direction = 'right', textOnly = TRUE, style = list(color = "orange"))) %>%
      addDrawToolbar(
        targetGroup = 'clicks',
        polylineOptions = FALSE,
        polygonOptions = FALSE,
        circleOptions = FALSE,
        rectangleOptions = FALSE,
        markerOptions = drawMarkerOptions(),
        edittoolbar = FALSE,
        ) %>%
      setView(lng = mean(zone_data$coords$Longitude, na.rm = TRUE), 
              lat = mean(zone_data$coords$Latitude, na.rm = TRUE), 
              zoom = 10) %>%
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
        zone = paste0("New_Z", nrow(zone_data$coords) + 1),
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
      colnames= c("Zone" = 1,
                  "Latitude" = 2,	
                  "Longitude" = 3,
                  "Plankton" = 4,
                  "Water eDNA (schedule_1)" = 5,
                  "Water eDNA (schedule_2)" = 6,
                  "Sediment  (schedule_1)" = 7,
                  "Sediment  (schedule_2)" = 8,
                  "Comments" = 9),
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
    ) %>%
      # Reduce the decimal places of Latitude and Longitude columns to 4
      formatRound(columns = c('Latitude', 'Longitude'), digits = 4)
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
    write.csv(zone_data$coords, "zone_coords.csv", row.names = FALSE)
    output$status <- renderText({"Data saved to zone_coords.csv"})
  })
  
  # Copy the updated data frame to clipboard
  observeEvent(input$copy_clipboard, {
    write_clip(zone_data$coords)
    output$status <- renderText({"Data copied to clipboard"})
  })  
  
  # Sampling calendar: based on the zone data from the zone_mapper_app ####
  zones_data <- reactive(zone_data$coords)
  
  
  
  
  
  # Reactive function to create the sampling calendar ####
  sampling_calendar_reactive <- reactive({
    
    # Convert checkbox selections to vectors for zones
    sampling_zones_plankton <- zones_data()$zone[zones_data()$zone_selection_plankton]
    sampling_zones_eDNA     <- zones_data()$zone[zones_data()$zone_selection_eDNA]
    sampling_zones_eDNA2    <- zones_data()$zone[zones_data()$zone_selection_eDNA2]
    sampling_zones_sediment <- zones_data()$zone[zones_data()$zone_selection_sediment]
    sampling_zones_sediment2<- zones_data()$zone[zones_data()$zone_selection_sediment2]
    
    # Determine the calendar type
    calendar_type <- input$calendar_type  # "gregorian" or "lunar"
    
    # Generate calendar backbone based on the selected calendar type
    if (calendar_type == "gregorian") {
      period_dates <- seq.Date(from = input$start_date, to = input$end_date, by = "week")
      moon_phase <- lunar::lunar.phase(period_dates, name = TRUE)
      
      calendar_backbone <- data.frame(
        period_ID = 1:length(period_dates),
        period_start_date = period_dates,
        moon_phase = moon_phase
      )
      
    } else if (calendar_type == "lunar") {
      all_dates <- seq.Date(from = input$start_date, to = input$end_date, by = "day")
      moon_phases <- lunar::lunar.phase(all_dates, name = TRUE)
      
      phase_transitions <- which(diff(as.numeric(factor(moon_phases))) != 0) + 1
      phase_dates <- all_dates[c(1, phase_transitions)]
      phase_names <- moon_phases[c(1, phase_transitions)]
      
      
      calendar_backbone <- data.frame(
        period_ID = seq_along(phase_dates),
        period_start_date = phase_dates,
        moon_phase = phase_names
      )
    }
    
    # calendar_backbone
    
    # Helper function to sample dates from the backbone
    sample_dates_from_backbone <- function(backbone, delay, frequency) {
      start_period <- backbone$period_start_date[1+delay]
      available_dates <- backbone$period_start_date[backbone$period_start_date >= start_period]
      sampled_periods <- available_dates[seq(1, length(available_dates), by = frequency)]
      return(backbone %>% filter(period_start_date %in% sampled_periods))
    }
    
    
    # Calculate sampling dates based on the calendar backbone
    sampled_plankton <- sample_dates_from_backbone(
      calendar_backbone, delay = (input$delay_plankton), frequency = input$freq_plankton
    ) 
    if (nrow(sampled_plankton) == 0) warning("No plankton samples found.")
    
    sampled_eDNA <- sample_dates_from_backbone(
      calendar_backbone, delay = (input$delay_eDNA), frequency = input$freq_eDNA
    )
    if (nrow(sampled_eDNA) == 0) warning("No eDNA samples found.")
    
    sampled_eDNA2 <- sample_dates_from_backbone(
      calendar_backbone, delay = (input$delay_eDNA2), frequency = input$freq_eDNA2
    )
    if (nrow(sampled_eDNA2) == 0) warning("No eDNA2 samples found.")
    
    sampled_sediment <- sample_dates_from_backbone(
      calendar_backbone, delay = (input$delay_sediment), frequency = input$freq_sediment
    )
    if (nrow(sampled_sediment) == 0) warning("No sediment samples found.")
    
    sampled_sediment2 <- sample_dates_from_backbone(
      calendar_backbone, delay = (input$delay_sediment2), frequency = input$freq_sediment2
    )
    if (nrow(sampled_sediment2) == 0) warning("No sediment2 samples found.")
    
    
   
    # print(sampled_eDNA2)
    # print(sampled_sediment)
    
    
    # Add sampling flags to the calendar backbone
    calendar_backbone <- calendar_backbone %>%
      mutate(
        sampling_flag_plankton   = period_start_date %in% sampled_plankton$period_start_date,
        sampling_flag_eDNA       = period_start_date %in% sampled_eDNA$period_start_date,
        sampling_flag_eDNA2      = period_start_date %in% sampled_eDNA2$period_start_date,
        sampling_flag_sediment   = period_start_date %in% sampled_sediment$period_start_date,
        sampling_flag_sediment2  = period_start_date %in% sampled_sediment2$period_start_date
      )
     # print(calendar_backbone)
    
    
    
    # Create a list to store each sampling calendar
    sampling_calendars <- list()
    
    # Expand data frames for each sample type
    if (length(sampling_zones_plankton) > 0) {
      sampling_calendar_plankton <- expand.grid(period_ID = calendar_backbone$period_ID,
                                                zone = sampling_zones_plankton) %>%
        left_join(calendar_backbone, by = "period_ID") %>%
        mutate(sampling = ifelse(sampling_flag_plankton, "Plankton", "No sampling"),
               n_replicas = ifelse(sampling_flag_plankton, input$nrep_plankton, 0))
      sampling_calendars[[length(sampling_calendars) + 1]] <- sampling_calendar_plankton %>%
        mutate(sampling_type = "Plankton")
    }
    
    if (length(sampling_zones_eDNA) > 0) {
      sampling_calendar_eDNA <- expand.grid(period_ID = calendar_backbone$period_ID,
                                            zone = sampling_zones_eDNA) %>%
        left_join(calendar_backbone, by = "period_ID") %>%
        mutate(sampling = ifelse(sampling_flag_eDNA, "eDNA", "No sampling"),
               n_replicas = ifelse(sampling_flag_eDNA, input$nrep_eDNA, 0))
      sampling_calendars[[length(sampling_calendars) + 1]] <- sampling_calendar_eDNA %>%
        mutate(sampling_type = "eDNA")
    }
    
    if (length(sampling_zones_eDNA2) > 0) {
      sampling_calendar_eDNA2 <- expand.grid(period_ID = calendar_backbone$period_ID,
                                            zone = sampling_zones_eDNA2) %>%
        left_join(calendar_backbone, by = "period_ID") %>%
        mutate(sampling = ifelse(sampling_flag_eDNA2, "eDNA", "No sampling"),
               n_replicas = ifelse(sampling_flag_eDNA2, input$nrep_eDNA, 0))
      sampling_calendars[[length(sampling_calendars) + 1]] <- sampling_calendar_eDNA2 %>%
        mutate(sampling_type = "eDNA")
    }
    if (length(sampling_zones_sediment) > 0) {
      sampling_calendar_sediment <- expand.grid(period_ID = calendar_backbone$period_ID,
                                            zone = sampling_zones_sediment) %>%
        left_join(calendar_backbone, by = "period_ID") %>%
        mutate(sampling = ifelse(sampling_flag_sediment, "Sediment", "No sampling"),
               n_replicas = ifelse(sampling_flag_sediment, input$nrep_sediment, 0))
      sampling_calendars[[length(sampling_calendars) + 1]] <- sampling_calendar_sediment %>%
        mutate(sampling_type = "Sediment")
    }
    if (length(sampling_zones_sediment2) > 0) {
      sampling_calendar_sediment2 <- expand.grid(period_ID = calendar_backbone$period_ID,
                                            zone = sampling_zones_sediment2) %>%
        left_join(calendar_backbone, by = "period_ID") %>%
        mutate(sampling = ifelse(sampling_flag_sediment2, "Sediment", "No sampling"),
               n_replicas = ifelse(sampling_flag_sediment2, input$nrep_sediment2, 0))
      sampling_calendars[[length(sampling_calendars) + 1]] <- sampling_calendar_sediment2 %>%
        mutate(sampling_type = "Sediment")
    }
    
    # Add other sample types similarly...
    sampling_calendars
    
    
    # Combine into one comprehensive calendar, only bind rows if data frames are non-NULL
    if (length(sampling_calendars) > 0) {
      sampling_calendar <- bind_rows(sampling_calendars) %>%
        mutate(zone = factor(zone, levels = rev(unique(zone))),
               sampling_type = factor(sampling_type, levels = c("Plankton", "eDNA", "Sediment")),
               sampling = factor(sampling, levels = c("No sampling", "Plankton", "eDNA", "Sediment")))
    } else {
      sampling_calendar <- NULL  # Return NULL if no sampling data is available
    }
    
    return(sampling_calendar)
    
  })
  
  
  
  # Render the calendar plot ####
  output$samplingPlot <- renderPlot({
    sampling_calendar <- sampling_calendar_reactive()
    
    # Make sure sampling_calendar is ready before moving on to avoid errors.
    if (is.null(sampling_calendar) || nrow(sampling_calendar) == 0) {
      return(NULL)  # Return nothing if no sampling calendar is available
    } 
    
    date_labels <- sampling_calendar %>%
      group_by(date_labels = format(period_start_date, "%b-%Y")) %>%
      summarize(period_ID = min(period_ID), .groups = "drop")
    
    moon_phase_labels <- sampling_calendar %>%
      select(period_ID, moon_phase) %>%
      mutate(moon_phase_symbol = case_when(
        moon_phase == "New" ~ "🌑",
        moon_phase == "Waxing" ~ "🌓",
        moon_phase == "Full" ~ "🌕",
        moon_phase == "Waning" ~ "🌗",
        TRUE ~ ""))
    
    ggplot(sampling_calendar, aes(x = period_ID, y = zone, fill = sampling)) + # , group = sampling_calendar
      geom_tile(aes(width = 0.8), color = "white", position = position_dodge(width = 0.9), alpha = 0.5) +
      scale_fill_manual(name = "Sample type", values = c(
        "No sampling" = "gray90",
        "Plankton" = "darkgreen",
        "eDNA" = "blue",
        "Sediment" = "brown"
      )) +
      annotate("text", x = moon_phase_labels$period_ID, y = -0.5, label = moon_phase_labels$moon_phase_symbol, size = 3, vjust = -0.5) +
      labs(
        title = "Sampling Calendar", 
        x = if (input$calendar_type == "gregorian") "Week Number" else "Lunar Phase Period", 
        y = "Sampling Zones"
      ) +
      scale_x_continuous(
        breaks = seq(min(sampling_calendar$period_ID)-1, max(sampling_calendar$period_ID)-1, by = 4),
        sec.axis = sec_axis(~., breaks = date_labels$period_ID, labels = date_labels$date_labels, name = "")
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  # Render table with sample totals ####
  output$samplingTable <- renderTable({
    sampling_calendar <- sampling_calendar_reactive()
    
    if (is.null(sampling_calendar) || nrow(sampling_calendar) == 0) {
      return(NULL)  # Return nothing if no sampling calendar is available
    } 
    
    df <- sampling_calendar %>%
      select(!starts_with("sampling_flag")) %>%
      filter(sampling != "No sampling") %>%
      group_by(sampling) %>%
      tally(wt = as.integer(ifelse(zone == "Other zones", input$n_otherzones, 1) * n_replicas))
    
    return(df)
  })
  
  
  
  # Save the sampling calendar to a CSV file with timestamp and calendar type
  observeEvent(input$save_calendar, {
    sampling_calendar <- sampling_calendar_reactive()
    
    if (!is.null(sampling_calendar)) {
      # Get current date-time and calendar type
      timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M")
      calendar_type <- ifelse(input$calendar_type == "gregorian", "Gregorian", "Lunar")
      
      # Construct the filename
      filename <- paste0("Sampling_calendar_", timestamp, "_", calendar_type, ".csv")
      
      # Save the calendar to a CSV file
      write.csv(sampling_calendar, filename, row.names = FALSE)
      
      # Update status message
      output$status_calendar <- renderText({paste("Data saved to", filename)})
    } else {
      output$status_calendar <- renderText({"No data to save."})
    }
  })
  
  # Copy the sampling calendar to the clipboard
  observeEvent(input$copy_calendar, {
    sampling_calendar <- sampling_calendar_reactive()
    
    if (!is.null(sampling_calendar)) {
      write_clip(sampling_calendar)
      output$status_calendar <- renderText({"Data copied to clipboard"})
    } else {
      output$status_calendar <- renderText({"No data to copy."})
    }
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
