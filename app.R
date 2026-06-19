library(shiny)
library(tidyverse)
library(rmarkdown)
library(xfun)
library(knitr)
library(DT)

# ---------------------------------------------------------------------------
# Design tokens
# ---------------------------------------------------------------------------
COL_BG       <- "#14171C"
COL_SURFACE  <- "#1B2027"
COL_SURFACE2 <- "#20262E"
COL_BORDER   <- "#2A2F38"
COL_TEXT     <- "#F2F3F0"
COL_MUTED    <- "#8B919C"
COL_ACCENT   <- "#C8FF3D"   # volt - baseline / leader / primary accent
COL_ALERT    <- "#FF5A3C"

# Distinguishable line colours for competitors, dark-background safe
COMPETITOR_PALETTE <- c("#C8FF3D", "#FF5A3C", "#4FB6FF", "#FF6FB5",
                        "#FFB23D", "#3DDC84", "#B98EFF", "#F2F3F0")

theme_hyrox <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.background   = element_rect(fill = COL_BG, color = NA),
      panel.background  = element_rect(fill = COL_BG, color = NA),
      panel.grid.major  = element_line(color = COL_BORDER, linewidth = 0.3),
      panel.grid.minor  = element_blank(),
      axis.text         = element_text(color = COL_MUTED, size = 10),
      axis.title        = element_text(color = COL_TEXT, size = 11, face = "bold"),
      axis.text.x       = element_text(angle = 45, hjust = 1),
      legend.background = element_rect(fill = COL_BG, color = NA),
      legend.key        = element_rect(fill = COL_BG, color = NA),
      legend.text       = element_text(color = COL_TEXT, size = 10),
      legend.title      = element_blank(),
      legend.position   = "top",
      plot.margin       = margin(10, 16, 10, 10)
    )
}

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Bebas+Neue&family=Oswald:wght@400;500;600&family=Inter:wght@400;500;600&family=JetBrains+Mono:wght@400;500;600&display=swap"),
    tags$style(HTML("
      body, .container-fluid { background-color: #14171C; color: #F2F3F0; font-family: 'Inter', sans-serif; }

      .app-header { padding: 26px 4px 20px 4px; border-bottom: 1px solid #2A2F38; margin-bottom: 22px; }
      .app-header h1 { font-family: 'Bebas Neue', sans-serif; font-size: 40px; letter-spacing: 2px; margin: 0; line-height: 1; color: #F2F3F0; }
      .app-header h1 span { color: #C8FF3D; }
      .app-header p { font-family: 'Inter', sans-serif; text-transform: uppercase; letter-spacing: 2px; font-size: 11px; color: #8B919C; margin: 6px 0 0 0; }

      .well { background-color: #1B2027 !important; border: 1px solid #2A2F38 !important; border-radius: 6px !important; box-shadow: none !important; padding: 20px !important; }
      .well h4 { font-family: 'Oswald', sans-serif; text-transform: uppercase; letter-spacing: 1.5px; font-size: 13px; color: #8B919C; border-top: 1px solid #2A2F38; padding-top: 14px; margin-top: 14px; }
      .well p { color: #8B919C; font-size: 13px; line-height: 1.5; }
      hr { border-color: #2A2F38; }

      .btn-file { background-color: #C8FF3D !important; color: #14171C !important; border: none !important; font-family: 'Oswald', sans-serif; text-transform: uppercase; letter-spacing: 1px; font-weight: 500; }
      .form-control { background-color: #20262E !important; color: #F2F3F0 !important; border: 1px solid #2A2F38 !important; }

      .nav-tabs { border-bottom: 1px solid #2A2F38; }
      .nav-tabs > li > a { font-family: 'Oswald', sans-serif; text-transform: uppercase; letter-spacing: 1.5px; font-size: 13px; color: #8B919C; background: transparent; border: none !important; padding: 12px 18px; }
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {
        color: #F2F3F0 !important; background: transparent !important; border: none !important; border-bottom: 2px solid #C8FF3D !important;
      }
      .nav-tabs > li > a:hover { background: transparent !important; color: #F2F3F0; }

      .tab-content h3 { font-family: 'Oswald', sans-serif; font-size: 15px; text-transform: uppercase; letter-spacing: 1.5px; color: #F2F3F0; border-left: 4px solid #C8FF3D; padding-left: 12px; margin: 4px 0 16px 0; }

      .panel-block { background-color: #1B2027; border: 1px solid #2A2F38; border-radius: 6px; padding: 20px; margin-bottom: 22px; }

      table.dataTable { background-color: #1B2027 !important; color: #F2F3F0 !important; font-family: 'JetBrains Mono', monospace; font-size: 13px; }
      table.dataTable thead th { font-family: 'Oswald', sans-serif; text-transform: uppercase; letter-spacing: 1px; font-size: 11px; color: #8B919C !important; border-bottom: 1px solid #2A2F38 !important; background-color: #1B2027 !important; }
      table.dataTable tbody td { border-color: #2A2F38 !important; }
      table.dataTable.stripe tbody tr.odd { background-color: #20262E !important; }
      table.dataTable.stripe tbody tr.even { background-color: #1B2027 !important; }
      .dataTables_wrapper .dataTables_filter input, .dataTables_wrapper .dataTables_length select {
        background-color: #20262E; color: #F2F3F0; border: 1px solid #2A2F38;
      }
      .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_paginate { color: #8B919C !important; }
      .paginate_button { color: #8B919C !important; }
      .paginate_button.current { background: #C8FF3D !important; color: #14171C !important; border-radius: 4px !important; border: none !important; }
    "))
  ),
  
  div(class = "app-header",
      h1(HTML("HYROX <span>SPLIT</span> TRACKER")),
      p("Segment-by-segment race comparison")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      fileInput("file", "Upload race data", accept = ".csv"),
      tags$hr(),
      h4("Format"),
      p("CSV with one row per competitor. Times should be in 'mm:ss.d'.")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Summary",
                 div(class = "panel-block",
                     h3("Front Page Summary"),
                     DTOutput("summaryTable"))
        ),
        tabPanel("Stations",
                 div(class = "panel-block",
                     h3("Station Times"),
                     DTOutput("stationTable")),
                 div(class = "panel-block",
                     plotOutput("stationPlot", height = "420px"))
        ),
        tabPanel("Runs",
                 div(class = "panel-block",
                     h3("Run Times"),
                     DTOutput("runTable")),
                 div(class = "panel-block",
                     plotOutput("pacePlot", height = "420px"))
        ),
        tabPanel("Delta Analysis",
                 div(class = "panel-block",
                     h3("Cumulative Time Delta"),
                     plotOutput("deltaPlot", height = "500px")),
                 div(class = "panel-block",
                     h3("Segment Delta Table"),
                     DTOutput("deltaTable"))
        )
      )
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {
  
  process_data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    
    time_cols <- c("run1_time", "skiErg_time", "run2_time", "sledPush_time",
                   "run3_time", "sledPull_time", "run4_time", "burpeeBroadJumps_time",
                   "run5_time", "rowing_time", "run6_time", "farmersCarry_time",
                   "run7_time", "sandbagLunges_time", "run8_time", "wallBalls_time",
                   "roxZone_time")
    
    for (col in time_cols) {
      df[[col]] <- sapply(df[[col]], function(x) {
        min_sec <- strsplit(as.character(x), ":")[[1]]
        mins <- as.numeric(min_sec[1])
        sec <- as.numeric(min_sec[2])
        total_seconds <- mins * 60 + sec
        return(total_seconds)
      })
    }
    
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
  
  format_time <- function(seconds) {
    sprintf("%02d:%02d:%04.1f", seconds %/% 3600, (seconds %% 3600) %/% 60, seconds %% 60)
  }
  
  # Shared styling for the simpler reference tables
  style_table <- function(df) {
    datatable(
      df,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = TRUE,
                     columnDefs = list(list(className = "dt-right", targets = "_all")))
    )
  }
  
  output$summaryTable <- renderDT({
    df <- process_data()
    tbl <- df %>%
      select(name, total_run_time_fmt, total_workout_time_fmt) %>%
      rename(
        "Name" = name,
        "Total Run Time (hh:mm:ss.d)" = total_run_time_fmt,
        "Total Workout Time (hh:mm:ss.d)" = total_workout_time_fmt
      )
    style_table(tbl)
  })
  
  output$stationTable <- renderDT({
    df <- process_data()
    tbl <- df %>%
      select(name, skiErg_time, sledPush_time, sledPull_time, burpeeBroadJumps_time,
             rowing_time, farmersCarry_time, sandbagLunges_time, wallBalls_time) %>%
      mutate(across(starts_with("skiErg") | starts_with("sledPush") | starts_with("sledPull") |
                      starts_with("burpeeBroadJumps") | starts_with("rowing") | starts_with("farmersCarry") |
                      starts_with("sandbagLunges") | starts_with("wallBalls"),
                    ~ format_time(.))) %>%
      rename_with(~ gsub("_time", "", .), -name) %>%
      rename_with(~ gsub("([A-Z])", " \\1", .), -name) %>%
      rename("Name" = name)
    style_table(tbl)
  })
  
  output$runTable <- renderDT({
    df <- process_data()
    tbl <- df %>%
      select(name, run1_time, run2_time, run3_time, run4_time, run5_time,
             run6_time, run7_time, run8_time) %>%
      rename_with(~ gsub("_time", "", .), everything()) %>%
      rename_with(~ gsub("([A-Z])", " \\1", .), everything()) %>%
      rename("Name" = name) %>%
      mutate(across(starts_with("run"), ~ format_time(.)))
    style_table(tbl)
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
      geom_line(linewidth = 1) +
      geom_point(size = 2.3) +
      scale_color_manual(values = COMPETITOR_PALETTE) +
      scale_y_continuous(labels = function(x) sprintf("%02d:%02.1f", x %/% 60, x %% 60)) +
      labs(x = "Station", y = "Time (mm:ss.d)") +
      theme_hyrox()
  }, bg = "transparent")
  
  output$pacePlot <- renderPlot({
    df <- process_data()
    df_long <- df %>%
      pivot_longer(cols = starts_with("run"), names_to = "Run", values_to = "Time") %>%
      mutate(Pace = Time)
    
    ggplot(df_long, aes(x = Run, y = Pace, group = name, color = name)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2.3) +
      scale_color_manual(values = COMPETITOR_PALETTE) +
      scale_y_continuous(labels = function(x) sprintf("%02d:%02d", x %/% 60, x %% 60)) +
      labs(x = "Run", y = "Pace (mm:ss)") +
      theme_hyrox()
  }, bg = "transparent")
  
  # Shared helper: build the long-format delta data once, reused by table + plot
  build_delta_long <- function(df) {
    segment_order <- c("Run 1", "SkiErg", "Run 2", "Sled Push", "Run 3", "Sled Pull", "Run 4",
                       "Burpee Broad Jumps", "Run 5", "Rowing", "Run 6", "Farmers Carry",
                       "Run 7", "Sandbag Lunges", "Run 8", "Wall Balls", "RoxZone")
    column_names <- c("run1_time", "skiErg_time", "run2_time", "sledPush_time", "run3_time", "sledPull_time",
                      "run4_time", "burpeeBroadJumps_time", "run5_time", "rowing_time", "run6_time",
                      "farmersCarry_time", "run7_time", "sandbagLunges_time", "run8_time", "wallBalls_time", "roxZone_time")
    
    df %>%
      select(-contains("total_run_time"), -contains("total_workout_time"), -contains("_fmt")) %>%
      pivot_longer(cols = all_of(column_names), names_to = "Segment", values_to = "Time") %>%
      mutate(Segment = recode(Segment,
                              run1_time = "Run 1", skiErg_time = "SkiErg", run2_time = "Run 2", sledPush_time = "Sled Push",
                              run3_time = "Run 3", sledPull_time = "Sled Pull", run4_time = "Run 4", burpeeBroadJumps_time = "Burpee Broad Jumps",
                              run5_time = "Run 5", rowing_time = "Rowing", run6_time = "Run 6", farmersCarry_time = "Farmers Carry",
                              run7_time = "Run 7", sandbagLunges_time = "Sandbag Lunges", run8_time = "Run 8", wallBalls_time = "Wall Balls",
                              roxZone_time = "RoxZone")) %>%
      mutate(Segment = factor(Segment, levels = segment_order))
  }
  
  output$deltaTable <- renderDT({
    df <- process_data()
    df_long <- build_delta_long(df) %>%
      group_by(Segment) %>%
      mutate(Baseline = min(Time)) %>%
      ungroup() %>%
      group_by(name) %>%
      mutate(SegmentDelta = Time - Baseline) %>%
      ungroup() %>%
      mutate(SegmentDeltaFormatted = sprintf("%02d:%02d:%05.2f",
                                             SegmentDelta %/% 3600,
                                             (SegmentDelta %% 3600) %/% 60,
                                             SegmentDelta %% 60)) %>%
      select(Segment, name, SegmentDeltaFormatted) %>%
      pivot_wider(names_from = name, values_from = SegmentDeltaFormatted)
    
    competitor_cols <- setdiff(colnames(df_long), "Segment")
    
    dt <- datatable(
      df_long,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE,
                     columnDefs = list(list(className = "dt-center", targets = "_all")))
    )
    
    # Highlight the segment leader (zero delta) in each row
    for (col in competitor_cols) {
      dt <- dt %>% formatStyle(
        col,
        backgroundColor = styleEqual("00:00:00.00", "#243321"),
        color = styleEqual("00:00:00.00", "#C8FF3D"),
        fontWeight = styleEqual("00:00:00.00", "bold")
      )
    }
    dt
  })
  
  output$deltaPlot <- renderPlot({
    df <- process_data()
    df_long <- build_delta_long(df) %>%
      group_by(name) %>%
      mutate(CumulativeTime = cumsum(Time)) %>%
      ungroup() %>%
      group_by(Segment) %>%
      mutate(Baseline = min(CumulativeTime)) %>%
      ungroup() %>%
      group_by(name) %>%
      mutate(CumulativeDelta = CumulativeTime - Baseline) %>%
      ungroup()
    
    ggplot(df_long, aes(x = Segment, y = CumulativeDelta, color = name, group = name)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#C8FF3D", linewidth = 0.6) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2.6) +
      scale_color_manual(values = COMPETITOR_PALETTE) +
      scale_y_continuous(labels = function(x) sprintf("%+d:%02.1f", x %/% 60, x %% 60)) +
      labs(x = "Segment", y = "Cumulative Delta (mm:ss.d)") +
      theme_hyrox()
  }, bg = "transparent")
  
}

# Run the Shiny App
shinyApp(ui, server)