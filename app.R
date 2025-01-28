library(shiny)
library(tidyverse)
library(rmarkdown)
library(xfun)
library(knitr)

# Define the UI
ui <- fluidPage(
  titlePanel("Hyrox Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      tags$hr(),
      h4("Instructions"),
      p("Upload a CSV with the required format. Times should be in 'mm:ss.d'.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary",
                 h3("Front Page Summary"),
                 tableOutput("summaryTable")),
        tabPanel("Stations",
                 h3("Station Times"),
                 tableOutput("stationTable"),
                 plotOutput("stationPlot")),
        tabPanel("Runs",
                 h3("Run Times"),
                 tableOutput("runTable"),
                 plotOutput("pacePlot")),
        tabPanel("Delta Analysis",
                 fluidRow(
                   column(
                     width = 12,
                     h3("Time Delta Analysis"),
                     plotOutput("deltaPlot", height = "500px"),
                     tableOutput("deltaTable")
                   )
                 )
        )
        
      )
    )
  )
)

# Define the Server
server <- function(input, output, session) {
  process_data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    
    # Convert mm:ss.d to total seconds and back to mm:ss.d for table display
    time_cols <- c("run1_time", "skiErg_time", "run2_time", "sledPush_time", 
                   "run3_time", "sledPull_time", "run4_time", "burpeeBroadJumps_time", 
                   "run5_time", "rowing_time", "run6_time", "farmersCarry_time", 
                   "run7_time", "sandbagLunges_time", "run8_time", "wallBalls_time", 
                   "roxZone_time")
    
    for (col in time_cols) {
      # Convert mm:ss.d to total seconds
      df[[col]] <- sapply(df[[col]], function(x) {
        min_sec <- strsplit(as.character(x), ":")[[1]]
        mins <- as.numeric(min_sec[1])
        sec <- as.numeric(min_sec[2])
        total_seconds <- mins * 60 + sec
        return(total_seconds)
      })
    }
    
    # Calculate cumulative times and reformat as mm:ss.d
    df <- df %>%
      mutate(
        total_run_time = run1_time + run2_time + run3_time + run4_time + run5_time + 
          run6_time + run7_time + run8_time,
        total_workout_time = rowSums(select(., all_of(time_cols))),
        total_run_time_fmt = sprintf("%02d:%02d:%04.1f", total_run_time %/% 3600, (total_run_time %% 3600) %/% 60, total_run_time %% 60),
        total_workout_time_fmt = sprintf("%02d:%02d:%04.1f", total_workout_time %/% 3600, (total_workout_time %% 3600) %/% 60, total_workout_time %% 60)
      )
    df
  })
  
  # Helper function to format times as mm:ss.d
  format_time <- function(seconds) {
    sprintf("%02d:%02d:%04.1f", seconds %/% 3600, (seconds %% 3600) %/% 60, seconds %% 60)
  }
  
  output$summaryTable <- renderTable({
    df <- process_data()
    df %>%
      select(name, total_run_time_fmt, total_workout_time_fmt) %>%
      rename(
        "Name" = name,
        "Total Run Time (hh:mm:ss.d)" = total_run_time_fmt,
        "Total Workout Time (hh:mm:ss.d)" = total_workout_time_fmt
      )
  })
  
  output$stationTable <- renderTable({
    df <- process_data()
    
    df %>%
      select(name, skiErg_time, sledPush_time, sledPull_time, burpeeBroadJumps_time, 
             rowing_time, farmersCarry_time, sandbagLunges_time, wallBalls_time) %>%
      mutate(across(starts_with("skiErg") | starts_with("sledPush") | starts_with("sledPull") | 
                      starts_with("burpeeBroadJumps") | starts_with("rowing") | starts_with("farmersCarry") |
                      starts_with("sandbagLunges") | starts_with("wallBalls"),
                    ~ format_time(.)))  # Apply format_time to all time columns
  })
  
  
  
  
  
  
  output$runTable <- renderTable({
    df <- process_data()
    df %>%
      select(name, run1_time, run2_time, run3_time, run4_time, run5_time, 
             run6_time, run7_time, run8_time) %>%
      rename_with(~ gsub("_time", "", .), everything()) %>%
      rename_with(~ gsub("([A-Z])", " \\1", ., perl = TRUE)) %>%
      rename("Name" = name) %>%
      mutate(across(starts_with("run"), ~ format_time(.)))  # Apply time format to each run time column
  })

    output$stationPlot <- renderPlot({
    df <- process_data()
    df_long <- df %>%
      pivot_longer(cols = c("skiErg_time", "sledPush_time", "sledPull_time", 
                            "burpeeBroadJumps_time", "rowing_time", 
                            "farmersCarry_time", "sandbagLunges_time", "wallBalls_time"), 
                   names_to = "Station", values_to = "Time") %>%
      mutate(Station = factor(Station, levels = c("skiErg_time", "sledPush_time", "sledPull_time", 
                                                  "burpeeBroadJumps_time", "rowing_time", 
                                                  "farmersCarry_time", "sandbagLunges_time", "wallBalls_time")))
    
    ggplot(df_long, aes(x = Station, y = Time, group = name, color = name)) +
      geom_line() +
      geom_point() +
      scale_y_continuous(labels = function(x) sprintf("%02d:%02.1f", x %/% 60, x %% 60)) +
      labs(title = "Station Times", x = "Station", y = "Time (mm:ss.d)") +
      theme_minimal()
  })
  
  output$pacePlot <- renderPlot({
    df <- process_data()
    df_long <- df %>%
      pivot_longer(cols = starts_with("run"), names_to = "Run", values_to = "Time") %>%
      mutate(Pace = Time)
    
    ggplot(df_long, aes(x = Run, y = Pace, group = name, color = name)) +
      geom_line() +
      geom_point() +
      scale_y_continuous(labels = function(x) sprintf("%02d:%02d", x %/% 60, x %% 60)) +
      labs(title = "Running Pace", x = "Run", y = "Pace (mm:ss)") +
      theme_minimal()
  })
  
  # Render Delta Table
  output$deltaTable <- renderTable({
    df <- process_data()
    
    # Define the correct order for the segments and their corresponding column names
    segment_order <- c("Run 1", "SkiErg", "Run 2", "Sled Push", "Run 3", "Sled Pull", "Run 4", 
                       "Burpee Broad Jumps", "Run 5", "Rowing", "Run 6", "Farmers Carry", 
                       "Run 7", "Sandbag Lunges", "Run 8", "Wall Balls", "RoxZone")
    column_names <- c("run1_time", "skiErg_time", "run2_time", "sledPush_time", "run3_time", "sledPull_time",
                      "run4_time", "burpeeBroadJumps_time", "run5_time", "rowing_time", "run6_time", 
                      "farmersCarry_time", "run7_time", "sandbagLunges_time", "run8_time", "wallBalls_time", "roxZone_time")
    
    # Exclude the total_run_time and total_workout_time columns
    df_long <- df %>%
      select(-contains("total_run_time"), -contains("total_workout_time"), -contains("_fmt")) %>%
      pivot_longer(cols = all_of(column_names), names_to = "Segment", values_to = "Time") %>%
      mutate(Segment = recode(Segment,
                              run1_time = "Run 1", skiErg_time = "SkiErg", run2_time = "Run 2", sledPush_time = "Sled Push",
                              run3_time = "Run 3", sledPull_time = "Sled Pull", run4_time = "Run 4", burpeeBroadJumps_time = "Burpee Broad Jumps",
                              run5_time = "Run 5", rowing_time = "Rowing", run6_time = "Run 6", farmersCarry_time = "Farmers Carry",
                              run7_time = "Run 7", sandbagLunges_time = "Sandbag Lunges", run8_time = "Run 8", wallBalls_time = "Wall Balls", 
                              roxZone_time = "RoxZone")) %>%
      group_by(Segment) %>%
      mutate(Baseline = min(Time)) %>%  # Find the baseline for each segment (the fastest time for each segment)
      ungroup() %>%
      group_by(name) %>%
      mutate(SegmentDelta = Time - Baseline) %>%  # Calculate the delta for each segment individually
      ungroup() %>%
      mutate(Segment = factor(Segment, levels = segment_order)) %>%
      # Format SegmentDelta in hh:mm:ss.d format
      mutate(SegmentDeltaFormatted = sprintf("%02d:%02d:%05.2f", 
                                             SegmentDelta %/% 3600, 
                                             (SegmentDelta %% 3600) %/% 60, 
                                             SegmentDelta %% 60)) %>%
      select(Segment, name, SegmentDeltaFormatted) %>%  # Select relevant columns for the table
      pivot_wider(names_from = name, values_from = SegmentDeltaFormatted)  # Create wide format table
    
    df_long
  })
  
  # Render Cumulative Delta Plot
  output$deltaPlot <- renderPlot({
    df <- process_data()
    
    # Define the correct order for the segments and their corresponding column names
    segment_order <- c("Run 1", "SkiErg", "Run 2", "Sled Push", "Run 3", "Sled Pull", "Run 4", 
                       "Burpee Broad Jumps", "Run 5", "Rowing", "Run 6", "Farmers Carry", 
                       "Run 7", "Sandbag Lunges", "Run 8", "Wall Balls", "RoxZone")
    column_names <- c("run1_time", "skiErg_time", "run2_time", "sledPush_time", "run3_time", "sledPull_time",
                      "run4_time", "burpeeBroadJumps_time", "run5_time", "rowing_time", "run6_time", 
                      "farmersCarry_time", "run7_time", "sandbagLunges_time", "run8_time", "wallBalls_time", "roxZone_time")
    
    # Exclude the total_run_time and total_workout_time columns
    df_long <- df %>%
      select(-contains("total_run_time"), -contains("total_workout_time"), -contains("_fmt")) %>%
      pivot_longer(cols = all_of(column_names), names_to = "Segment", values_to = "Time") %>%
      # Replace column names with human-readable names
      mutate(Segment = recode(Segment,
                              run1_time = "Run 1", skiErg_time = "SkiErg", run2_time = "Run 2", sledPush_time = "Sled Push",
                              run3_time = "Run 3", sledPull_time = "Sled Pull", run4_time = "Run 4", burpeeBroadJumps_time = "Burpee Broad Jumps",
                              run5_time = "Run 5", rowing_time = "Rowing", run6_time = "Run 6", farmersCarry_time = "Farmers Carry",
                              run7_time = "Run 7", sandbagLunges_time = "Sandbag Lunges", run8_time = "Run 8", wallBalls_time = "Wall Balls", 
                              roxZone_time = "RoxZone")) %>%
      # Group by name to calculate cumulative times correctly for each competitor
      group_by(name) %>%
      mutate(CumulativeTime = cumsum(Time)) %>%  # Compute cumulative time for each competitor
      ungroup() %>%
      # Initialize baseline as the first segment's time for each competitor
      group_by(Segment) %>%
      mutate(Baseline = min(CumulativeTime)) %>%
      ungroup() %>%
      # Compute the cumulative delta: progressively track how much worse (or better) the time is
      group_by(name) %>%
      mutate(CumulativeDelta = CumulativeTime - Baseline) %>%
      ungroup() %>%
      # Ensure the 'Segment' is treated as a factor with proper order
      mutate(Segment = factor(Segment, levels = segment_order))
    
    ggplot(df_long, aes(x = Segment, y = CumulativeDelta, color = name, group = name)) +
      geom_line(size = 1) +  # Line for cumulative delta
      geom_point(size = 2) +  # Points at each segment
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +  # Reference line at 0 delta
      scale_y_continuous(labels = function(x) sprintf("%+d:%02.1f", x %/% 60, x %% 60)) +  # Formatting time as mm:ss.d
      labs(title = "Cumulative Time Delta from Baseline", x = "Segment", y = "Cumulative Delta (mm:ss.d)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
  })
  
}
# Run the Shiny App
shinyApp(ui, server)

