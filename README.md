# Sampling_planer_app: Sampling Calendar Visualization App

## Overview

This Shiny application allows users to plan and visualize complex temporal sampling schemas or sampling calendars. It supports various sample types, such as plankton, eDNA, and sediment. Additional sample types can be created to accomodate different sampling schemes for the same sample type, for example for high-spatial resolution and low frequency water eDNA and sediment samples. Each sample type has customizable parameters such as sampling zones, frequencies, sampling launching delays, and the number of replicas.

The app provides an interactive interface for users to:

1. **Input Sampling Parameters**: Specify a complete list of sampling zones and dynamically assign them to different sample types using intuitive checkbox groups.
2. **Customize Sampling Schedules**: Adjust sampling frequencies and delays for each sample type, enabling tailored planning across different study periods.
3. **Visualize Sampling Calendar**: Generate a visual representation of the sampling schedule across different zones and weeks, annotated with lunar phases for additional context.
4. **Summarize Sampling Effort**: Display a summary table of the number of samples planned for each type, helping to track and optimize resource allocation.

## Features

- **Dynamic Zone Assignment**: Define sampling zones for each sample type using checkbox inputs, based on a user-defined complete list of zones.
- **Customizable Parameters**: Modify sampling frequency, delay, and the number of replicas for each sample type.
- **Interactive Plot**: Visualize the temporal distribution of sampling events with an easy-to-read calendar format.
- **Lunar Phase Annotations**: See how sampling aligns with lunar phases, which can be relevant for certain ecological studies.

## How to Run the App

1. Ensure you have the required R packages installed:
    ```r
    install.packages(c("shiny", "dplyr", "ggplot2", "lubridate", "lunar"))
    ```
2. Clone this repository:
    ```bash
    git clone https://github.com/yourusername/sampling-calendar-app.git
    ```
3. Open R or RStudio, and set the working directory to the app folder:
    ```r
    setwd("path_to_your_app_directory")
    ```
4. Run the Shiny app:
    ```r
    shiny::runApp()
    ```

## Additional Information

- This app is designed for planning and visualizing ecological sampling schedules but can be adapted for other use cases.
- Feel free to modify the code to suit your specific needs or contribute to the repository.

## Author

[Your Name]
