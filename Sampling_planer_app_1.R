library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(lunar)
library(rlang)


# Define the UI with scrolling sidebar for inputs
ui <- fluidPage(
  tags$head(
    # CSS for making the sidebar scrollable
    tags$style(HTML("
      .scrollable-sidebar {
        max-height: 600px;  /* Set the maximum height for the sidebar */
        overflow-y: auto;   /* Enable vertical scrolling */
      }
    "))
  ),
  titlePanel("Sampling Calendar Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      div(class = "scrollable-sidebar",  # Apply scrolling class to the sidebar panel
          dateInput("start_date", "Start Date:", value = "2025-01-01"),
          dateInput("end_date", "End Date:", value = "2025-12-20"),
          
          # Step 1: Complete list of sampling zones input
          textInput(
            "all_zones", 
            "Complete List of Sampling Zones (comma-separated):", 
            value = ""
          ),
          
          # Step 2: Checkbox groups for assigning zones to sampling types
          uiOutput("zone_selection_plankton"),
          uiOutput("zone_selection_eDNA"),
          uiOutput("zone_selection_eDNA2"),
          uiOutput("zone_selection_sediment"),
          uiOutput("zone_selection_sediment2"),
          
          # Number of replicas input
          numericInput("nrep_plankton", "Number of Replicas for Plankton:", value = 3, min = 1, max = 10),
          numericInput("nrep_eDNA", "Number of Replicas for eDNA:", value = 3, min = 1, max = 10),
          numericInput("nrep_eDNA2", "Number of Replicas for eDNA2:", value = 1, min = 1, max = 10),
          numericInput("nrep_sediment", "Number of Replicas for Sediment:", value = 3, min = 1, max = 10),
          numericInput("nrep_sediment2", "Number of Replicas for Sediment2:", value = 1, min = 1, max = 10),
          
          # Sampling frequencies input
          textInput("freq_plankton", "Frequency for Plankton:", value = "2 weeks"),
          textInput("freq_eDNA", "Frequency for eDNA:", value = "13 weeks"),
          textInput("freq_eDNA2", "Frequency for eDNA2:", value = "52 weeks"),
          textInput("freq_sediment", "Frequency for Sediment:", value = "26 weeks"),
          textInput("freq_sediment2", "Frequency for Sediment2:", value = "52 weeks"),
          
          # Delays input
          numericInput("delay_plankton", "Delay for Plankton (weeks):", value = 0, min = 0),
          numericInput("delay_eDNA", "Delay for eDNA (weeks):", value = 4, min = 0),
          numericInput("delay_eDNA2", "Delay for eDNA2 (weeks):", value = 17, min = 0),
          numericInput("delay_sediment", "Delay for Sediment (weeks):", value = 17, min = 0),
          numericInput("delay_sediment2", "Delay for Sediment2 (weeks):", value = 17, min = 0)
      )
    ),
    
    mainPanel(
      plotOutput("samplingPlot"),
      tableOutput("samplingTable")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Load the zones from the CSV file
  zones_data <- reactive({
    # Ensure the file exists and load the data correctly
    req(file.exists("zone_coords.csv"))  # Ensure the file exists
    read.csv("zone_coords.csv")
  })
  
  
  # Update the all_zones input dynamically based on the loaded file
  observe({
    zones <- zones_data()$zone
    updateTextInput(session, "all_zones", value = paste(zones, collapse = ","))
  })
  
  # Create reactive list of zones based on user input
  all_zones_reactive <- reactive({
    req(input$all_zones)  # Ensure input is available
    unlist(strsplit(input$all_zones, ","))
  })
  
  # Create generalized pre-selected sampling type zone selection
  preselected_reactive <- function(zone_selection = "zone_selection_plankton") {
    reactive({
      zones_data() %>%
        filter(!!sym(zone_selection)) %>%  # Dynamically use the column name for filtering
        pull(zone) %>%  # Extract the 'zone' column
        unlist()        # Convert to a character vector
    })
  }
  
  # Generate UI for each sampling type zone selection
  output$zone_selection_plankton <- renderUI({
    checkboxGroupInput(
      "zones_plankton", 
      "Sampling Zones for Plankton:", 
      choices = all_zones_reactive(), 
      selected = preselected_reactive("zone_selection_plankton")()  # Call the reactive function and evaluate it
    )
  })
  
  ########################
  
  
  output$zone_selection_eDNA <- renderUI({
    checkboxGroupInput(
      "zones_eDNA", 
      "Sampling Zones for eDNA:", 
      choices = all_zones_reactive(), 
      selected = preselected_reactive("zone_selection_eDNA")()
    )
  })
  
  output$zone_selection_eDNA2 <- renderUI({
    checkboxGroupInput(
      "zones_eDNA2", 
      "Sampling Zones for eDNA2:", 
      choices = all_zones_reactive(), 
      selected = preselected_reactive("zone_selection_eDNA2")()
    )
  })
  
  output$zone_selection_sediment <- renderUI({
    checkboxGroupInput(
      "zones_sediment", 
      "Sampling Zones for Sediment:", 
      choices = all_zones_reactive(), 
      selected = preselected_reactive("zone_selection_sediment")()
    )
  })
  
  output$zone_selection_sediment2 <- renderUI({
    checkboxGroupInput(
      "zones_sediment2", 
      "Sampling Zones for Sediment2:", 
      choices = all_zones_reactive(), 
      selected = preselected_reactive("zone_selection_sediment2")()
    )
  })
  
  
  ###################################################################
  
  sampling_calendar_reactive <- reactive({
    
    # Convert checkbox selections to vectors for zones
    sampling_zones_plankton <- input$zones_plankton
    sampling_zones_eDNA <- input$zones_eDNA
    sampling_zones_eDNA2 <- input$zones_eDNA2
    sampling_zones_sediment <- input$zones_sediment
    sampling_zones_sediment2 <- input$zones_sediment2
    
    start_date <- input$start_date
    end_date <- input$end_date
    
    # Define delays using lubridate weeks function
    delay_plankton <- weeks(input$delay_plankton)
    delay_eDNA <- weeks(input$delay_eDNA)
    delay_eDNA2 <- weeks(input$delay_eDNA2)
    delay_sediment <- weeks(input$delay_sediment)
    delay_sediment2 <- weeks(input$delay_sediment2)
    
    # Create a backbone weekly calendar
    week_ID <- seq.Date(from = start_date, to = end_date, by = "week")
    moon_phase <- lunar.phase(week_ID, name = TRUE)
    
    weekly_schema <- data.frame(
      week_ID = 1:length(week_ID),
      week_start_date = week_ID,
      moon_phase = moon_phase
    )
    
    # Calculate sampling dates for each sample type
    sampling_dates_plankton <- seq.Date(from = start_date + delay_plankton, to = end_date, by = input$freq_plankton)
    sampling_dates_eDNA <- seq.Date(from = start_date + delay_eDNA, to = end_date, by = input$freq_eDNA)
    sampling_dates_eDNA2 <- seq.Date(from = start_date + delay_eDNA2, to = end_date, by = input$freq_eDNA2)
    sampling_dates_sediment <- seq.Date(from = start_date + delay_sediment, to = end_date, by = input$freq_sediment)
    sampling_dates_sediment2 <- seq.Date(from = start_date + delay_sediment2, to = end_date, by = input$freq_sediment2)
    
    # Add sampling flags to 'weekly_schema'
    weekly_schema <- weekly_schema %>%
      mutate(sampling_flag_plankton = ifelse(week_start_date %in% sampling_dates_plankton, TRUE, FALSE),
             sampling_flag_eDNA = ifelse(week_start_date %in% sampling_dates_eDNA, TRUE, FALSE),
             sampling_flag_eDNA2 = ifelse(week_start_date %in% sampling_dates_eDNA2, TRUE, FALSE),
             sampling_flag_sediment = ifelse(week_start_date %in% sampling_dates_sediment, TRUE, FALSE),
             sampling_flag_sediment2 = ifelse(week_start_date %in% sampling_dates_sediment2, TRUE, FALSE))
    
    # Create a list to store each sampling calendar
    sampling_calendars <- list()
    
    # Expand data frames for each sample type
    if (!is.null(sampling_zones_plankton) && length(sampling_zones_plankton) > 0) {
      sampling_calendar_plankton <- expand.grid(week_ID = weekly_schema$week_ID, zone = sampling_zones_plankton) %>%
        left_join(weekly_schema, by = "week_ID") %>%
        mutate(sampling = ifelse(sampling_flag_plankton, "Plankton", "No sampling"),
               n_replicas = ifelse(sampling_flag_plankton, input$nrep_plankton, 0))
      sampling_calendars[[length(sampling_calendars) + 1]] <- sampling_calendar_plankton %>% mutate(sampling_calendar = "Plankton")
    }
    
    if (!is.null(sampling_zones_eDNA) && length(sampling_zones_eDNA) > 0) {
      sampling_calendar_eDNA <- expand.grid(week_ID = weekly_schema$week_ID, zone = sampling_zones_eDNA) %>%
        left_join(weekly_schema, by = "week_ID") %>%
        mutate(sampling = ifelse(sampling_flag_eDNA, "eDNA", "No sampling"),
               n_replicas = ifelse(sampling_flag_eDNA, input$nrep_eDNA, 0))
      sampling_calendars[[length(sampling_calendars) + 1]] <- sampling_calendar_eDNA %>% mutate(sampling_calendar = "eDNA")
    }
    
    if (!is.null(sampling_zones_eDNA2) && length(sampling_zones_eDNA2) > 0) {
      sampling_calendar_eDNA2 <- expand.grid(week_ID = weekly_schema$week_ID, zone = sampling_zones_eDNA2) %>%
        left_join(weekly_schema, by = "week_ID") %>%
        mutate(sampling = ifelse(sampling_flag_eDNA2, "eDNA", "No sampling"),
               n_replicas = ifelse(sampling_flag_eDNA2, input$nrep_eDNA2, 0))
      sampling_calendars[[length(sampling_calendars) + 1]] <- sampling_calendar_eDNA2 %>% mutate(sampling_calendar = "eDNA")
    }
    
    if (!is.null(sampling_zones_sediment) && length(sampling_zones_sediment) > 0) {
      sampling_calendar_sediment <- expand.grid(week_ID = weekly_schema$week_ID, zone = sampling_zones_sediment) %>%
        left_join(weekly_schema, by = "week_ID") %>%
        mutate(sampling = ifelse(sampling_flag_sediment, "Sediment", "No sampling"),
               n_replicas = ifelse(sampling_flag_sediment, input$nrep_sediment, 0))
      sampling_calendars[[length(sampling_calendars) + 1]] <- sampling_calendar_sediment %>% mutate(sampling_calendar = "Sediment")
    }
    
    if (!is.null(sampling_zones_sediment2) && length(sampling_zones_sediment2) > 0) {
      sampling_calendar_sediment2 <- expand.grid(week_ID = weekly_schema$week_ID, zone = sampling_zones_sediment2) %>%
        left_join(weekly_schema, by = "week_ID") %>%
        mutate(sampling = ifelse(sampling_flag_sediment2, "Sediment", "No sampling"),
               n_replicas = ifelse(sampling_flag_sediment2, input$nrep_sediment2, 0))
      sampling_calendars[[length(sampling_calendars) + 1]] <- sampling_calendar_sediment2 %>% mutate(sampling_calendar = "Sediment")
    }
    
    # Combine into one comprehensive calendar, only bind rows if data frames are non-NULL
    if (length(sampling_calendars) > 0) {
      sampling_calendar <- bind_rows(sampling_calendars) %>%
        mutate(zone = factor(zone, levels = rev(unique(zone))),
               sampling_calendar = factor(sampling_calendar, levels = c("Plankton", "eDNA", "Sediment")),
               sampling = factor(sampling, levels = c("No sampling", "Plankton", "eDNA", "Sediment")))
    } else {
      sampling_calendar <- NULL  # Return NULL if no sampling data is available
    }
    
    return(sampling_calendar)
  })
  
  
  output$samplingPlot <- renderPlot({
    sampling_calendar <- sampling_calendar_reactive()
    
    if (is.null(sampling_calendar) || nrow(sampling_calendar) == 0) {
      return(NULL)  # Return nothing if no sampling calendar is available
    } 
    
    date_labels <- sampling_calendar %>%
      group_by(date_labels = format(week_start_date, "%b-%Y")) %>%
      summarize(week_ID = min(week_ID))
    
    moon_phase_labels <- sampling_calendar %>%
      select(week_ID, moon_phase) %>%
      mutate(moon_phase_symbol = case_when(
        moon_phase == "New" ~ "🌑",
        moon_phase == "Waxing" ~ "🌓",
        moon_phase == "Full" ~ "🌕",
        moon_phase == "Waning" ~ "🌗",
        TRUE ~ ""))
    
    ggplot(sampling_calendar, aes(x = week_ID, y = zone, fill = sampling, group = sampling_calendar)) +
      geom_tile(aes(width = 0.8), color = "white", position = position_dodge(width = 0.9), alpha = 0.5) +
      scale_fill_manual(name = "Sample type", values = c(
        "No sampling" = "gray90",
        "Plankton" = "darkgreen",
        "eDNA" = "blue",
        "Sediment" = "brown"
      )) +
      annotate("text", x = moon_phase_labels$week_ID, y = -0.5, label = moon_phase_labels$moon_phase_symbol, size = 3, vjust = -0.5) +
      labs(title = "Sampling Calendar", x = "Week Number", y = "Sampling Zones") +
      scale_x_continuous(
        breaks = seq(min(sampling_calendar$week_ID)-1, max(sampling_calendar$week_ID)-1, by = 4),
        sec.axis = sec_axis(~., breaks = date_labels$week_ID, labels = date_labels$date_labels, name = "")
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$samplingTable <- renderTable({
    sampling_calendar <- sampling_calendar_reactive()
    
    if (is.null(sampling_calendar) || nrow(sampling_calendar) == 0) {
      return(NULL)  # Return nothing if no sampling calendar is available
    } 
    
    df <- sampling_calendar %>%
      select(!starts_with("sampling_flag")) %>%
      filter(sampling != "No sampling") %>%
      group_by(sampling) %>%
      tally(wt = ifelse(zone == "Z6...Z26", 20, 1) * n_replicas)
    
    df
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



