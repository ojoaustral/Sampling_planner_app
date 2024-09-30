#
# The objective of this script is to help plan and visualize the temporal sampling schema or sampling calendar
#

rm(list = ls())

# Libraries ####
library(dplyr)
library(ggplot2)
library(lubridate)
library(lunar)  # To calculate lunar phases

# Set working directory 

setwd("C:/Users/crist/Dropbox/NewAtlantis/Bermuda/Sampling_planer_app/")

# (Input) Enter study period (start and end dates) ####
start_date <- as.Date("2025-01-01")
end_date   <- as.Date("2025-12-20")

# (Input) Enter sampling zones for each sample type ####
sampling_zones_plankton  <- c("Z1", "Z2", "Z3", "Z4.1")
sampling_zones_eDNA      <- c("Z1", "Z2", "Z3", "Z4.1", "Z4.2", "Z4.3", "Z5")
sampling_zones_eDNA2     <- c("Z6...Z26")
if(sum(sampling_zones_eDNA2 %in% sampling_zones_eDNA)>0) print("Warning! Avoid zone overlaps between eDNA sample types")
sampling_zones_sediment  <- c("Z1", "Z2", "Z3", "Z5")
sampling_zones_sediment2 <- c("Z6...Z26")
if(sum(sampling_zones_sediment2 %in% sampling_zones_sediment)>0) print("Warning! Avoid zone overlaps between sediment sample types")

# (Input) Enter number of replicas for each sample type ####
nrep_plankton  <- 3
nrep_eDNA      <- 3
nrep_eDNA2     <- 1   
nrep_sediment  <- 3
nrep_sediment2 <- 1

# (Input) Enter sampling frequencies (expressed in weeks) ####
freq_plankton  <- "2 weeks"   # Plankton samples taken every second week
freq_eDNA      <- "13 weeks"  # eDNA samples taken every quarter or 13 weeks
freq_eDNA2     <- "52 weeks"  # eDNA samples taken once a year in complementary sites
freq_sediment  <- "26 weeks"  # Sediment samples taken twice a year
freq_sediment2 <- "52 weeks"  # Sediment samples taken once a year in complementary sites

# (Input) Enter delays before launching every sampling type (expressed in weeks) ####
delay_plankton  <- weeks(0)     # 2 weeks delay for plankton
delay_eDNA      <- weeks(4)     # 4 weeks delay for eDNA
delay_eDNA2     <- weeks(17)    # 17 weeks delay for eDNA2
delay_sediment  <- weeks(17)    # 17 weeks delay for sediment
delay_sediment2 <- weeks(17)    # 17 weeks delay for sediment2

# Set up the backbone weekly calendar #### 

# Create a data frame with a column (integer) identifying every week within the whole study period,
# and corresponding lunar phases
week_ID <- seq.Date(from = start_date, to = end_date, by = "week")
moon_phase <- lunar.phase(week_ID, name = T)  # Get lunar phase as "New", "First Quarter", "Full", etc.
weekly_schema <- data.frame(
  week_ID = 1:length(week_ID),  # Assign a week number
  week_start_date = week_ID,    # The start date of each week (depending on start_date, weeks may begin on Mondays or any day of the week)
  moon_phase = moon_phase       # Add the moon phase column
            )
# Calculate sampling dates for each sample type ####
# Calculate sampling dates based on start_date, end_date, and frequencies, applying delays
sampling_dates_plankton  <- seq.Date(from = start_date + delay_plankton, to = end_date, by = freq_plankton)
sampling_dates_eDNA      <- seq.Date(from = start_date + delay_eDNA, to = end_date, by = freq_eDNA)
sampling_dates_eDNA2     <- seq.Date(from = start_date + delay_eDNA2, to = end_date, by = freq_eDNA2)
sampling_dates_sediment  <- seq.Date(from = start_date + delay_sediment, to = end_date, by = freq_sediment)
sampling_dates_sediment2 <- seq.Date(from = start_date + delay_sediment2, to = end_date, by = freq_sediment2)


# Add sampling flags to 'weekly_schema' ####
# Note this part is matching specific sampling dates so these must be coherent with the backbone weekly sequence  
weekly_schema <- weekly_schema %>%
  mutate(sampling_flag_plankton = ifelse(week_start_date %in% sampling_dates_plankton, TRUE, FALSE),
         sampling_flag_eDNA     = ifelse(week_start_date %in% sampling_dates_eDNA, TRUE, FALSE),
         sampling_flag_eDNA2    = ifelse(week_start_date %in% sampling_dates_eDNA2, TRUE, FALSE),
         sampling_flag_sediment = ifelse(week_start_date %in% sampling_dates_sediment, TRUE, FALSE),
         sampling_flag_sediment2 = ifelse(week_start_date %in% sampling_dates_sediment2, TRUE, FALSE))

# Expand data frames that include all zones and weeks for each sampling type ####
# This is necessary for plotting
sampling_calendar_plankton <- expand.grid(
  week_ID = weekly_schema$week_ID,
  zone = sampling_zones_plankton
)

sampling_calendar_eDNA <- expand.grid(
  week_ID = weekly_schema$week_ID,
  zone = sampling_zones_eDNA
)

sampling_calendar_eDNA2 <- expand.grid(
  week_ID = weekly_schema$week_ID,
  zone = sampling_zones_eDNA2
)

sampling_calendar_sediment <- expand.grid(
  week_ID = weekly_schema$week_ID,
  zone = sampling_zones_sediment
)

sampling_calendar_sediment2 <- expand.grid(
  week_ID = weekly_schema$week_ID,
  zone = sampling_zones_sediment2
)

# Add the sampling flags to these calendars by joining with weekly_schema ####
sampling_calendar_plankton <- sampling_calendar_plankton %>%
  left_join(weekly_schema, by = "week_ID") %>%
  mutate(sampling = ifelse(sampling_flag_plankton, "Plankton", "No sampling"),
         n_replicas = ifelse(sampling_flag_plankton, nrep_plankton, 0))

sampling_calendar_eDNA <- sampling_calendar_eDNA %>%
  left_join(weekly_schema, by = "week_ID") %>%
  mutate(sampling = ifelse(sampling_flag_eDNA, "eDNA", "No sampling"),
         n_replicas = ifelse(sampling_flag_eDNA, nrep_eDNA, 0))

sampling_calendar_eDNA2 <- sampling_calendar_eDNA2 %>%
  left_join(weekly_schema, by = "week_ID") %>%
  mutate(sampling = ifelse(sampling_flag_eDNA2, "eDNA", "No sampling"),
         n_replicas = ifelse(sampling_flag_eDNA2, nrep_eDNA2, 0))

sampling_calendar_sediment <- sampling_calendar_sediment %>%
  left_join(weekly_schema, by = "week_ID") %>%
  mutate(sampling = ifelse(sampling_flag_sediment, "Sediment", "No sampling"),
         n_replicas = ifelse(sampling_flag_sediment, nrep_sediment, 0))

sampling_calendar_sediment2 <- sampling_calendar_sediment2 %>%
  left_join(weekly_schema, by = "week_ID") %>%
  mutate(sampling = ifelse(sampling_flag_sediment2, "Sediment", "No sampling"),
         n_replicas = ifelse(sampling_flag_sediment2, nrep_sediment2, 0))

# Combine sample-type wise calendars into one comprehensive calendar ####
sampling_calendar <- bind_rows(
  sampling_calendar_plankton %>% mutate(sampling_calendar = "Plankton"),
  sampling_calendar_eDNA %>% mutate(sampling_calendar = "eDNA"),
  sampling_calendar_eDNA2 %>% mutate(sampling_calendar = "eDNA"),
  sampling_calendar_sediment %>% mutate(sampling_calendar = "Sediment"),
  sampling_calendar_sediment2 %>% mutate(sampling_calendar = "Sediment")
) %>% 
  mutate(zone = factor(zone, levels = rev(unique(zone))),
         sampling_calendar = factor(sampling_calendar, levels = c("Plankton", "eDNA", "Sediment")),
         sampling = factor(sampling, levels = c("No sampling", "Plankton", "eDNA", "Sediment")))  # Factor levels in desired order

# Creating labels for the secondary axes with date labels and moon phases ####
date_labels <- weekly_schema %>%
  filter(format(week_start_date, "%m") %in% c(paste0("0", 1:9), "10", "11", "12")) %>%
  group_by(date_labels = format(week_start_date, "%b-%Y")) %>%
  summarize(week_ID = min(week_ID))

moon_phase_labels <- weekly_schema %>%  select(week_ID, moon_phase) %>% 
  mutate(moon_phase_symbol = case_when(
      moon_phase == "New" ~ "🌑",
      moon_phase == "Waxing" ~ "🌓",
      moon_phase == "Full" ~ "🌕",
      moon_phase == "Waning" ~ "🌗", TRUE ~ " "))
    
# Plotting the calendar ####
ggplot(sampling_calendar, aes(x = week_ID, y = zone, fill = sampling, group = sampling_calendar))+
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
    breaks = seq(min(sampling_calendar$week_ID)-1, max(sampling_calendar$week_ID)-1, by = 4),  # Add tick marks every 4 weeks
    #labels = function(x) format(as.Date(x, origin = "1970-01-01"), "%Y-%m-%d"),  # Format labels as dates
    sec.axis = sec_axis(~., breaks = date_labels$week_ID, labels = date_labels$date_labels, name = "")
  ) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


df <- sampling_calendar %>% 
  select(!starts_with("sampling_flag")) %>% 
  filter(sampling != "No sampling") %>% 
  group_by(sampling) %>%
  tally(wt = ifelse(zone == "Z6...Z26", 20,1)*n_replicas)

df

# Make interactive leaflet map ####

# Load the leaflet package
library(leaflet)

all_zone_coords <- as.data.frame(
  matrix( c("Z1", 32.3414751, -64.6781091,
            "Z2", 32.3334034, -64.6488207,
            "Z3", 32.4675049, -64.5808043,
            "Z4.1", 32.3733700, -64.5480724,
            "Z4.2", 32.3733700, -64.5480724,
            "Z4.3", 32.3733700, -64.5480724,
            "Z5", 32.3744589, -64.7730149),
            ncol = 3, byrow = T), 
               stringsAsFactors = F
            )
colnames(all_zone_coords) <- c("zone", "Latitude", "Longitude")
all_zone_coords$Latitude <- as.numeric(all_zone_coords$Latitude)
all_zone_coords$Longitude <- as.numeric(all_zone_coords$Longitude)

# Calculate the center of all_zone_coords
center_lat <- mean(all_zone_coords$Latitude)
center_lng <- mean(all_zone_coords$Longitude)

# Create a leaflet map
map2 <- leaflet(data = all_zone_coords) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addCircleMarkers(~Longitude, ~Latitude, 
    #label = ~as.character(zone),
    radius = 2, color = "red", fill = T, fillOpacity = 1) %>%
  addLabelOnlyMarkers( ~Longitude, ~Latitude,
    label = ~zone, #paste(Sample_CODE,Sample_Name),
    labelOptions = labelOptions(noHide = T,
      direction = 'top', textOnly = TRUE, style = list(color = "orange") ) ) %>%
  setView(lng = center_lng, lat = center_lat, zoom = 10)

# Print the map
map2



