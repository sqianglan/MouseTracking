# Modules/tab_calendar_events.R
# Modern Calendar UI for plugging events with enhanced UX

suppressPackageStartupMessages({
  library(shiny)
  library(ggplot2)
  library(DBI)
  library(RSQLite)
  library(ggsci)
  library(dplyr)
  library(lubridate)
})

# Modern UI for the plugging calendar modal
plugging_calendar_modal_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Custom CSS for Apple-style design with semitransparent sky blue
    tags$head(
      tags$style(HTML("
        /* Apple-style design with semitransparent sky blue color scheme */
        .calendar-container {
          background: rgba(255, 255, 255, 0.95);
          backdrop-filter: blur(20px);
          -webkit-backdrop-filter: blur(20px);
          border-radius: 16px;
          padding: 24px;
          box-shadow: 0 8px 32px rgba(135, 206, 235, 0.15);
          border: 1px solid rgba(135, 206, 235, 0.2);
          margin-bottom: 24px;
          font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
        }
        
        .calendar-header {
          background: linear-gradient(135deg, rgba(135, 206, 235, 0.1) 0%, rgba(173, 216, 230, 0.1) 100%);
          backdrop-filter: blur(10px);
          -webkit-backdrop-filter: blur(10px);
          border: 1px solid rgba(135, 206, 235, 0.3);
          border-radius: 12px;
          padding: 12px;
          margin-bottom: 12px;
          box-shadow: 0 4px 16px rgba(135, 206, 235, 0.1);
        }
        
        .calendar-controls {
          display: flex;
          justify-content: space-between;
          align-items: center;
          flex-wrap: wrap;
          gap: 20px;
          margin-bottom: 0px;
          width: 100%;
        }
        
        .nav-btn {
          background: linear-gradient(135deg, rgba(135, 206, 235, 0.9) 0%, rgba(173, 216, 230, 0.9) 100%);
          color: #1e3a5f;
          border: 1px solid rgba(135, 206, 235, 0.4);
          border-radius: 10px;
          padding: 8px 16px;
          font-weight: 600;
          cursor: pointer;
          transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
          box-shadow: 0 2px 8px rgba(135, 206, 235, 0.2);
          font-size: 13px;
          backdrop-filter: blur(10px);
          -webkit-backdrop-filter: blur(10px);
        }
        
        .nav-btn:hover {
          background: linear-gradient(135deg, rgba(135, 206, 235, 1) 0%, rgba(173, 216, 230, 1) 100%);
          transform: translateY(-2px);
          box-shadow: 0 6px 20px rgba(135, 206, 235, 0.3);
          border-color: rgba(135, 206, 235, 0.6);
        }
        
        .nav-btn:active {
          transform: translateY(0);
          box-shadow: 0 2px 8px rgba(135, 206, 235, 0.2);
        }
        
        .nav-btn:disabled {
          background: rgba(200, 200, 200, 0.5);
          color: rgba(100, 100, 100, 0.6);
          cursor: not-allowed;
          transform: none;
          box-shadow: none;
        }
        
        .current-month {
          font-size: 20px;
          min-width: 200px;
          letter-spacing: -0.5px;
          font-weight: 700;
          color: #1e3a5f;
          text-align: center;
          text-shadow: 0 1px 2px rgba(0,0,0,0.1);
        }
        
        .current-year {
          font-size: 18px;
          min-width: 90px;
          letter-spacing: -0.3px;
        }
        
        .navigation-container {
          display: flex;
          flex-direction: column;
          align-items: center;
          gap: 12px;
        }
        
        .year-navigation {
          display: flex;
          align-items: center;
          gap: 12px;
        }
        
        .month-navigation {
          display: flex;
          align-items: center;
          gap: 12px;
        }
        
        .stats-panel {
          display: flex;
          flex-direction: row;
          gap: 8px;
          min-width: 300px;
          flex-wrap: wrap;
        }
        
        .export-section {
          display: flex;
          justify-content: center;
          margin-top: 24px;
        }
        
        .export-btn {
          background: linear-gradient(135deg, rgba(135, 206, 235, 0.9) 0%, rgba(173, 216, 230, 0.9) 100%);
          color: #1e3a5f;
          border: 1px solid rgba(135, 206, 235, 0.4);
          border-radius: 12px;
          padding: 12px 24px;
          font-weight: 600;
          cursor: pointer;
          transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
          box-shadow: 0 4px 16px rgba(135, 206, 235, 0.2);
          display: flex;
          align-items: center;
          gap: 10px;
          font-size: 14px;
          backdrop-filter: blur(10px);
          -webkit-backdrop-filter: blur(10px);
        }
        
        .export-btn:hover {
          background: linear-gradient(135deg, rgba(135, 206, 235, 1) 0%, rgba(173, 216, 230, 1) 100%);
          transform: translateY(-2px);
          box-shadow: 0 8px 24px rgba(135, 206, 235, 0.3);
          border-color: rgba(135, 206, 235, 0.6);
        }
        
        .export-btn:active {
          transform: translateY(0);
          box-shadow: 0 4px 16px rgba(135, 206, 235, 0.2);
        }
        
        .calendar-body {
          background: rgba(255, 255, 255, 0.95);
          backdrop-filter: blur(20px);
          -webkit-backdrop-filter: blur(20px);
          border: 1px solid rgba(135, 206, 235, 0.2);
          border-radius: 12px;
          padding: 8px;
          box-shadow: 0 4px 16px rgba(135, 206, 235, 0.1);
        }
        
        .calendar-plot {
          border-radius: 12px;
          overflow: hidden;
          box-shadow: 0 4px 16px rgba(135, 206, 235, 0.1);
          height: 520px;
          background: rgba(255, 255, 255, 0.8);
        }
        
        .calendar-legend-container {
          display: flex;
          gap: 24px;
          align-items: flex-start;
          width: 100%;
        }
        
        .calendar-plot-container {
          flex: 0 0 75%;
        }
        
        .legend-container {
          background: rgba(255, 255, 255, 0.95);
          backdrop-filter: blur(20px);
          -webkit-backdrop-filter: blur(20px);
          border: 1px solid rgba(135, 206, 235, 0.2);
          border-radius: 12px;
          padding: 20px;
          box-shadow: 0 4px 16px rgba(135, 206, 235, 0.1);
          min-width: 200px;
          max-width: 240px;
          flex: 0 0 25%;
          max-height: 520px;
          overflow-y: auto;
        }
        
        .legend-title {
          font-size: 18px;
          font-weight: 700;
          color: #1e3a5f;
          margin-bottom: 16px;
          text-align: center;
          letter-spacing: -0.3px;
        }
        
        .legend-grid {
          display: flex;
          flex-direction: column;
          gap: 12px;
        }
        
        .legend-section {
          background: rgba(135, 206, 235, 0.08);
          border-radius: 10px;
          padding: 12px;
          border-left: 4px solid rgba(135, 206, 235, 0.6);
          backdrop-filter: blur(10px);
          -webkit-backdrop-filter: blur(10px);
        }
        
        .legend-section.confirmed {
          border-left-color: rgba(76, 175, 80, 0.8);
          background: rgba(76, 175, 80, 0.08);
        }
        
        .legend-section.estimated {
          border-left-color: rgba(255, 152, 0, 0.8);
          background: rgba(255, 152, 0, 0.08);
        }
        
        .legend-section.waiting {
          border-left-color: rgba(158, 158, 158, 0.8);
          background: rgba(158, 158, 158, 0.08);
        }
        
        .legend-section-title {
          font-weight: 700;
          margin-bottom: 10px;
          color: #1e3a5f;
          font-size: 13px;
          letter-spacing: -0.2px;
        }
        
        .legend-item {
          display: flex;
          align-items: center;
          gap: 10px;
          margin: 6px 0;
          padding: 8px;
          border-radius: 8px;
          background: rgba(255, 255, 255, 0.8);
          box-shadow: 0 2px 8px rgba(135, 206, 235, 0.1);
          backdrop-filter: blur(10px);
          -webkit-backdrop-filter: blur(10px);
          border: 1px solid rgba(135, 206, 235, 0.1);
        }
        
        .color-indicator {
          width: 18px;
          height: 18px;
          border-radius: 50%;
          border: 2px solid rgba(255, 255, 255, 0.9);
          box-shadow: 0 2px 6px rgba(0,0,0,0.15);
        }
        
        .mouse-info {
          flex: 1;
          font-size: 12px;
        }
        
        .mouse-id {
          font-weight: 700;
          color: #1e3a5f;
          letter-spacing: -0.2px;
        }
        
        .mouse-details {
          font-size: 11px;
          color: #5a6c7d;
          margin-top: 2px;
        }
        
        .stat-card {
          background: linear-gradient(135deg, rgba(135, 206, 235, 0.9) 0%, rgba(173, 216, 230, 0.9) 100%);
          color: #1e3a5f;
          padding: 8px;
          border-radius: 8px;
          text-align: center;
          box-shadow: 0 2px 8px rgba(135, 206, 235, 0.2);
          backdrop-filter: blur(10px);
          -webkit-backdrop-filter: blur(10px);
          border: 1px solid rgba(135, 206, 235, 0.3);
        }
        
        .stat-number {
          font-size: 20px;
          font-weight: 700;
          margin-bottom: 2px;
          letter-spacing: -0.3px;
        }
        
        .stat-label {
          font-size: 10px;
          opacity: 0.9;
          font-weight: 600;
          letter-spacing: 0.2px;
        }
        
        /* Custom scrollbar for legend */
        .legend-container::-webkit-scrollbar {
          width: 6px;
        }
        
        .legend-container::-webkit-scrollbar-track {
          background: rgba(135, 206, 235, 0.1);
          border-radius: 3px;
        }
        
        .legend-container::-webkit-scrollbar-thumb {
          background: rgba(135, 206, 235, 0.4);
          border-radius: 3px;
        }
        
        .legend-container::-webkit-scrollbar-thumb:hover {
          background: rgba(135, 206, 235, 0.6);
        }
        
        /* Responsive design */
        @media (max-width: 768px) {
          .calendar-controls {
            flex-direction: column;
            align-items: stretch;
          }
          
          .month-navigation {
            justify-content: center;
          }
          
          .export-section {
            justify-content: center;
          }
          
          .calendar-legend-container {
            flex-direction: column;
          }
          
          .calendar-plot-container {
            flex: 0 0 100%;
          }
          
          .legend-container {
            flex: 0 0 100%;
            max-width: none;
          }
          
          .calendar-container {
            padding: 16px;
          }
          
          .calendar-header {
            padding: 16px;
          }
        }
        
        /* Animation for smooth transitions */
        .calendar-container * {
          transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        }
      "))
    ),
    
    # Main calendar container
    div(class = "calendar-container",
      # Header with controls
      div(class = "calendar-header",
        div(class = "calendar-controls",
          # Navigation container (Combined Year and Month navigation)
          div(class = "navigation-container",
            # Combined month/year navigation
            div(class = "month-navigation",
              actionButton(ns("prev_month"), "◀", class = "nav-btn"),
              div(class = "current-month", 
                  textOutput(ns("current_month_year_display"))),
              actionButton(ns("next_month"), "▶", class = "nav-btn")
            )
          ),
          
          # Event summary on the right
          div(class = "stats-panel",
            div(class = "stat-card",
              div(class = "stat-number", textOutput(ns("total_events"))),
              div(class = "stat-label", "Total Events")
            ),
            div(class = "stat-card",
              div(class = "stat-number", textOutput(ns("confirmed_events"))),
              div(class = "stat-label", "Confirmed")
            ),
            div(class = "stat-card",
              div(class = "stat-number", textOutput(ns("estimated_events"))),
              div(class = "stat-label", "Estimated")
            ),
            div(class = "stat-card",
              div(class = "stat-number", textOutput(ns("waiting_events"))),
              div(class = "stat-label", "Waiting")
            )
          )
        )
      ),
      
      # Calendar and legend side by side
      div(class = "calendar-legend-container",
        # Calendar on the left
        div(class = "calendar-plot-container",
          div(class = "calendar-body",
            div(class = "calendar-plot",
              plotOutput(ns("calendar_plot"), height = "520px")
            )
          )
        ),
        
        # Legend on the right
        div(class = "legend-container",
          uiOutput(ns("calendar_legend")),
          # Export section below legends
          div(class = "export-section",
            downloadButton(ns("export_ical_btn"), 
                          "📅 Export iCal", 
                          class = "export-btn")
          )
        )
      )
    )
  )
}

##Debug
#DB_PATH <- "mice_colony_test.db"

# Server logic for the plugging calendar modal
plugging_calendar_modal_server <- function(id, db_path = DB_PATH) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values for current month/year
    current_month <- reactiveVal(as.integer(format(Sys.Date(), "%m")))
    current_year <- reactiveVal(as.integer(format(Sys.Date(), "%Y")))

    # Update current month display
    output$current_month_year_display <- renderText({
      paste(month.name[current_month()], as.character(current_year()))
    })

    # Month navigation handlers
    observeEvent(input$prev_month, {
      new_month <- current_month() - 1
      if (new_month < 1) {
        new_month <- 12
        current_year(current_year() - 1)
      }
      current_month(new_month)
    })

    observeEvent(input$next_month, {
      new_month <- current_month() + 1
      if (new_month > 12) {
        new_month <- 1
        current_year(current_year() + 1)
      }
      current_month(new_month)
    })

    # Function to generate consistent colors for different asu_id values
    get_color_for_asu <- function(asu_id) {
      colors <- c("#5F9EA0", "#4682B4", "#4caf50", "#388e3c", "#ff9800", 
                  "#f57c00", "#2196f3", "#1976d2", "#9c27b0", "#673ab7")
      hash_val <- sum(utf8ToInt(asu_id)) %% length(colors)
      return(colors[hash_val + 1])
    }

    # Helper function to parse expected ages
    parse_expected_ages <- function(exp_ages) {
      # Default case: Return E13.5 if input is invalid
      if (is.null(exp_ages) || length(exp_ages) == 0 || is.na(exp_ages) || exp_ages == "") {
        return(13.5)
      }
      
      # Step 1: Split into parts, keeping hyphenated ranges together
      # Split on commas/semicolons/spaces but NOT hyphens
      ages_split <- unlist(strsplit(exp_ages, "[,;]|(?<=[0-9]) +", perl = TRUE))
      ages_split <- trimws(ages_split)
      ages_split <- ages_split[ages_split != "" & grepl("[0-9]", ages_split)]
      
      # If no valid parts, return default
      if (length(ages_split) == 0) {
        return(13.5)
      }
      
      # Step 2: Process each part (expand ranges if hyphen exists)
      process_part <- function(x) {
        if (grepl("-", x)) {  # Handle ranges (e.g., "E13-E16" → 13:16)
          nums <- as.numeric(gsub("[^0-9.]", "", unlist(strsplit(x, "-"))))
          if (length(nums) == 2 && !any(is.na(nums))) {
            return(seq(nums[1], nums[2]))
          } else {
            return(numeric(0))  # Invalid range → ignore
          }
        } else {  # Single value (e.g., "E13" → 13)
          num <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", x)))
          if (!is.na(num) && num > 0) {
            return(num)
          } else {
            return(numeric(0))  # Invalid → ignore
          }
        }
      }
      
      # Apply processing and flatten results
      ages_numeric <- unlist(lapply(ages_split, process_part))
      
      # If no valid numbers, return default
      if (length(ages_numeric) == 0) {
        return(13.5)
      }
      
      # Return unique, sorted, unnamed values
      return(sort(unique(unname(ages_numeric))))
    }

    # Helper function to safely parse dates
    safe_parse_date <- function(date_string) {
      if (is.null(date_string) || is.na(date_string) || date_string == "" || date_string == "Unknown") {
        return(as.Date(NA))
      }
      
      # Try different date formats
      date_formats <- c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d", "%m-%d-%Y", "%d-%m-%Y")
      
      for (format in date_formats) {
        tryCatch({
          parsed_date <- as.Date(date_string, format = format)
          if (!is.na(parsed_date)) {
            return(unname(parsed_date))
          }
        }, error = function(e) {
          # Continue to next format
        })
      }
      
      # If all formats fail, return NA
      warning("Could not parse date: ", date_string)
      return(as.Date(NA))
    }

    # Helper function to calculate base date
    calculate_base_date <- function(status, plug_date, pairing_start_date, pairing_end_date) {
      # For Plugged or Plug Confirmed status
      if (status %in% c("Plugged", "Plug Confirmed")) {
        # First try to use plug_observed_date if available
        parsed_plug_date <- safe_parse_date(plug_date)
        if (!is.na(parsed_plug_date)) {
          return(list(date = parsed_plug_date, is_estimated = FALSE))
        } else {
          # If no plug observed date, use pairing_start_date for estimation
          parsed_pairing_start_date <- safe_parse_date(pairing_start_date)
          if (!is.na(parsed_pairing_start_date)) {
            return(list(date = parsed_pairing_start_date, is_estimated = TRUE))
          }
        }
      } else if (status == "Not Observed (Waiting for confirmation)") {
        # Use pairing_start_date for estimation
        parsed_pairing_date <- safe_parse_date(pairing_start_date)
        if (!is.na(parsed_pairing_date)) {
          return(list(date = parsed_pairing_date + 1, is_estimated = TRUE))
        }
      } else if (status == "Surprising Plug!!") {
        # Use pairing_start_date + 1 for estimation (since plug_observed_date is "Unknown")
        # Add 1 day to account for the day of pairing, then add embryonic age
        parsed_pairing_date <- safe_parse_date(pairing_start_date)
        if (!is.na(parsed_pairing_date)) {
          return(list(date = parsed_pairing_date + 1, is_estimated = TRUE))
        }
      }
      return(NULL)
    }

    # Helper function to create calendar events
    create_calendar_events <- function(pluggings) {
      if (is.null(pluggings) || nrow(pluggings) == 0) {
        return(data.frame(
          asu_id = character(0),
          label = character(0),
          day = integer(0),
          date = as.Date(character(0)),
          target_month = integer(0),
          target_year = integer(0),
          color = character(0),
          status = character(0),
          status_category = character(0),
          is_estimated = logical(0),
          expected_age = numeric(0),
          stringsAsFactors = FALSE
        ))
      }
      
      events_list <- list()
      
      for (i in seq_len(nrow(pluggings))) {
        row <- pluggings[i, ]
        
        # Skip if required fields are missing
        if (is.null(row$asu_id) || is.na(row$asu_id) || 
            is.null(row$plugging_status) || is.na(row$plugging_status)) {
          next
        }
        
        # Calculate base date
        base_date_result <- calculate_base_date(
          row$plugging_status, 
          row$plug_observed_date, 
          row$pairing_start_date,
          row$pairing_end_date
        )
        
        if (is.null(base_date_result)) {
          next
        }
        
        base_date <- base_date_result$date
        is_estimated <- base_date_result$is_estimated
        
        # Determine color and status category
        if (row$plugging_status == "Not Observed (Waiting for confirmation)") {
          color <- "#9e9e9e"  # Gray
          status_category <- "waiting"
        } else if (is_estimated) {
          color <- "#ff9800"  # Orange for estimated
          status_category <- "estimated"
        } else {
          color <- get_color_for_asu(row$asu_id)
          status_category <- "confirmed"
        }
        
        # Parse expected ages
        expected_ages <- parse_expected_ages(row$expected_age_for_harvesting)
        
        # Create events for each expected age
        for (age in expected_ages) {
          harvest_date <- base_date + floor(age)
          
          if (!is.na(harvest_date)) {
            event <- data.frame(
              asu_id = as.character(row$asu_id),
              label = paste0(row$asu_id, " @E", floor(age) + 0.5),
              day = as.integer(format(harvest_date, "%d")),
              date = harvest_date,
              target_month = as.integer(format(harvest_date, "%m")),
              target_year = as.integer(format(harvest_date, "%Y")),
              color = color,
              status = as.character(row$plugging_status),
              status_category = status_category,
              is_estimated = is_estimated,
              expected_age = age,
              stringsAsFactors = FALSE
            )
            events_list[[length(events_list) + 1]] <- event
          }
        }
      }
      
      if (length(events_list) == 0) {
        return(data.frame(
          asu_id = character(0),
          label = character(0),
          day = integer(0),
          date = as.Date(character(0)),
          target_month = integer(0),
          target_year = integer(0),
          color = character(0),
          status = character(0),
          status_category = character(0),
          is_estimated = logical(0),
          expected_age = numeric(0),
          stringsAsFactors = FALSE
        ))
      }
      
      # Combine all events and ensure proper data types
      result <- do.call(rbind, events_list)
      
      # Ensure all required columns exist and have proper types
      if (nrow(result) > 0) {
        result$asu_id <- as.character(result$asu_id)
        result$label <- as.character(result$label)
        result$day <- as.integer(result$day)
        result$target_month <- as.integer(result$target_month)
        result$target_year <- as.integer(result$target_year)
        result$color <- as.character(result$color)
        result$status <- as.character(result$status)
        result$status_category <- as.character(result$status_category)
        result$is_estimated <- as.logical(result$is_estimated)
        result$expected_age <- as.numeric(result$expected_age)
      }
      
      return(result)
    }

    # Reactive: Get plugging events with observed dates
    plugging_data <- reactive({
      con <- NULL
      tryCatch({
        con <- dbConnect(SQLite(), db_path)
        query <- "
          SELECT 
            ph.female_id, 
            ph.plug_observed_date, 
            ph.expected_age_for_harvesting, 
            ph.plugging_status, 
            ph.pairing_start_date, 
            ph.pairing_end_date,
            ms.asu_id 
          FROM plugging_history ph 
          LEFT JOIN mice_stock ms ON ph.female_id = ms.asu_id 
          WHERE 
            ms.status = 'Alive'
            AND (
              (ph.plug_observed_date IS NOT NULL AND ph.plug_observed_date != '') 
              OR 
              (ph.plugging_status IN ('Plugged', 'Plug Confirmed') 
               AND ph.pairing_end_date IS NOT NULL AND ph.pairing_end_date != '')
              OR 
              (ph.plugging_status = 'Not Observed (Waiting for confirmation)' 
               AND ph.pairing_start_date IS NOT NULL AND ph.pairing_start_date != '')
              OR 
              (ph.plugging_status = 'Surprising Plug!!' 
               AND ph.pairing_start_date IS NOT NULL AND ph.pairing_start_date != '')
            )
        "
        result <- dbGetQuery(con, query)
        
        if (nrow(result) > 0) {
          # Use safe date parsing for all date columns
          result$plug_observed_date <- unname(sapply(result$plug_observed_date, safe_parse_date))
          result$pairing_start_date <- unname(sapply(result$pairing_start_date, safe_parse_date))
          result$pairing_end_date <- unname(sapply(result$pairing_end_date, safe_parse_date))
        }
        
        return(result)
      }, error = function(e) {
        warning("Error fetching plugging data: ", e$message)
        return(data.frame(
          female_id = character(0),
          plug_observed_date = as.Date(character(0)),
          expected_age_for_harvesting = character(0),
          plugging_status = character(0),
          pairing_start_date = as.Date(character(0)),
          pairing_end_date = as.Date(character(0)),
          asu_id = character(0),
          stringsAsFactors = FALSE
        ))
      }, finally = {
        if (!is.null(con)) dbDisconnect(con)
      })
    })

    # Reactive: Calculate all calendar events (cached for performance)
    all_calendar_events <- reactive({
      pluggings <- plugging_data()
      create_calendar_events(pluggings)
    })

    # Reactive: Filter events for current month/year
    current_month_events <- reactive({
      req(current_month(), current_year())
      
      month_num <- current_month()
      year_num <- current_year()
      
      events <- all_calendar_events()
      
      if (is.null(events) || nrow(events) == 0) {
        # Return empty data frame with proper structure
        return(data.frame(
          asu_id = character(0),
          label = character(0),
          day = integer(0),
          date = as.Date(character(0)),
          target_month = integer(0),
          target_year = integer(0),
          color = character(0),
          status = character(0),
          status_category = character(0),
          is_estimated = logical(0),
          expected_age = numeric(0),
          stringsAsFactors = FALSE
        ))
      }
      
      # Ensure events has the required columns
      required_cols <- c("target_month", "target_year", "day")
      if (!all(required_cols %in% names(events))) {
        warning("Events data missing required columns")
        return(data.frame(
          asu_id = character(0),
          label = character(0),
          day = integer(0),
          date = as.Date(character(0)),
          target_month = integer(0),
          target_year = integer(0),
          color = character(0),
          status = character(0),
          status_category = character(0),
          is_estimated = logical(0),
          expected_age = numeric(0),
          stringsAsFactors = FALSE
        ))
      }
      
      # Filter for current month/year and validate days
      filtered <- events[
        events$target_month == month_num & 
        events$target_year == year_num & 
        !is.na(events$day) & 
        events$day > 0 & 
        events$day <= 31, 
      ]
      
      # Ensure filtered result is a data frame
      if (nrow(filtered) == 0) {
        return(data.frame(
          asu_id = character(0),
          label = character(0),
          day = integer(0),
          date = as.Date(character(0)),
          target_month = integer(0),
          target_year = integer(0),
          color = character(0),
          status = character(0),
          status_category = character(0),
          is_estimated = logical(0),
          expected_age = numeric(0),
          stringsAsFactors = FALSE
        ))
      }
      
      return(filtered)
    })

    # Statistics outputs
    output$total_events <- renderText({
      events <- current_month_events()
      if (is.null(events) || nrow(events) == 0) {
        return("0")
      }
      as.character(nrow(events))
    })

    output$confirmed_events <- renderText({
      events <- current_month_events()
      if (is.null(events) || nrow(events) == 0) {
        return("0")
      }
      as.character(sum(events$status_category == "confirmed", na.rm = TRUE))
    })

    output$estimated_events <- renderText({
      events <- current_month_events()
      if (is.null(events) || nrow(events) == 0) {
        return("0")
      }
      as.character(sum(events$status_category == "estimated", na.rm = TRUE))
    })

    output$waiting_events <- renderText({
      events <- current_month_events()
      if (is.null(events) || nrow(events) == 0) {
        return("0")
      }
      as.character(sum(events$status_category == "waiting", na.rm = TRUE))
    })

    # Render the calendar plot
    output$calendar_plot <- renderPlot({
      req(current_month(), current_year())
      
      tryCatch({
        month_num <- current_month()
        year_num <- current_year()
        events <- current_month_events()
        
        # Ensure events is a proper data frame
        if (is.null(events) || nrow(events) == 0) {
          events <- data.frame(
            asu_id = character(0),
            label = character(0),
            day = integer(0),
            date = as.Date(character(0)),
            target_month = integer(0),
            target_year = integer(0),
            color = character(0),
            status = character(0),
            status_category = character(0),
            is_estimated = logical(0),
            expected_age = numeric(0),
            stringsAsFactors = FALSE
          )
        }
        
        # Create modern ggplot2 calendar
        create_modern_calendar(month_num, year_num, events)
      }, error = function(e) {
        # Create a simple error plot if something goes wrong
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                   label = paste("Calendar Error:", e$message), 
                   size = 6, color = "#1E3A5F") +
          theme_minimal() +
          theme(
            axis.text = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            plot.background = element_rect(fill = "transparent", color = NA),
            panel.background = element_rect(fill = "transparent", color = NA)
          ) +
          labs(title = "Calendar Display Error") +
          xlim(0, 1) + ylim(0, 1)
      })
    })

    # Function to create modern ggplot2 calendar
    create_modern_calendar <- function(month_num, year_num, events) {
      library(ggplot2)
      library(dplyr)
      
      # Get the first day of the month and number of days
      first_day <- as.Date(paste(year_num, month_num, "01", sep = "-"))
      
      # Calculate last day more robustly
      if (month_num == 12) {
        # For December, use January 1st of next year minus 1 day
        last_day <- as.Date(paste(year_num + 1, "01", "01", sep = "-")) - 1
      } else {
        # For other months, use first day of next month minus 1 day
        last_day <- as.Date(paste(year_num, month_num + 1, "01", sep = "-")) - 1
      }
      
      num_days <- as.numeric(format(last_day, "%d"))
      
      # Get the day of week for the first day (1 = Monday, 7 = Sunday)
      first_day_of_week <- as.numeric(format(first_day, "%u"))
      
      # Create calendar grid
      calendar_data <- data.frame()
      
      # Calculate starting position - no conversion needed since %u already gives 1-7
      start_pos <- first_day_of_week - 1  # Convert to 0-based indexing
      
      # Calculate how many weeks are needed and find the last Sunday
      total_positions <- start_pos + num_days - 1
      num_weeks <- ceiling((total_positions + 1) / 7)
      
      # Find the last Sunday in the month
      last_sunday_week <- 0
      for (day in 1:num_days) {
        grid_pos <- start_pos + day - 1
        week <- floor(grid_pos / 7) + 1
        day_of_week <- (grid_pos %% 7) + 1
        if (day_of_week == 7) {  # Sunday
          last_sunday_week <- week
        }
      }
      
      # Create grid positions
      for (day in 1:num_days) {
        # Calculate grid position
        grid_pos <- start_pos + day - 1
        week <- floor(grid_pos / 7) + 1
        day_of_week <- (grid_pos %% 7) + 1
        
        # Convert to our coordinate system (1-7 for days, 1-6 for weeks)
        x <- day_of_week
        y <- 7 - week  # Invert Y axis so week 1 is at the top
        
        # Determine if it's weekend (Saturday = 6, Sunday = 7)
        is_weekend <- day_of_week >= 6
        
        calendar_data <- rbind(calendar_data, data.frame(
          day = day,
          x = x,
          y = y,
          date = as.Date(paste(year_num, month_num, day, sep = "-")),
          is_today = as.Date(paste(year_num, month_num, day, sep = "-")) == Sys.Date(),
          is_weekend = is_weekend
        ))
      }
      
      # Process events for strips
      event_strips <- data.frame()
      if (nrow(events) > 0) {
        # Group events by day
        events_by_day <- events %>%
          group_by(day) %>%
          mutate(
            event_count = n(),
            strip_position = row_number(),
            strip_height = 0.15,  # Height of each strip
            strip_spacing = 0.05,  # Space between strips
            total_height = event_count * strip_height + (event_count - 1) * strip_spacing,
            y_offset = (total_height - strip_height) / 2 - (strip_position - 1) * (strip_height + strip_spacing)
          ) %>%
          ungroup()
        
        # Create strip data
        event_strips <- events_by_day %>%
          left_join(calendar_data, by = "day") %>%
          mutate(
            strip_y = y + y_offset,
            strip_width = 0.9,
            strip_height_actual = strip_height
          )
      }
      
      # Create the plot with Apple-style design
      p <- ggplot(calendar_data, aes(x = x, y = y)) +
        # Calendar grid - Apple-style with semitransparent sky blue
        geom_tile(fill = "#FFFFFF", 
                  color = "#87CEEB", 
                  linewidth = 1, 
                  width = 0.95, height = 0.85, 
                  alpha = 0.9) +
        # Today highlighting with sky blue
        geom_tile(data = calendar_data[calendar_data$is_today, ], 
                  fill = "#E6F3FF", 
                  color = "#87CEEB", 
                  linewidth = 1, 
                  width = 0.95, height = 0.85) +
    
        # Day numbers with Apple-style typography - moved above cells
        geom_text(aes(label = day, y = y + 0.25), 
                  size = 5.5, fontface = "bold", 
                  color = ifelse(calendar_data$is_today, "#87CEEB", "#1E3A5F")) +
        # Apple-style theme
        theme_minimal() +
        theme(
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.margin = margin(0, 0, 0, 0, "pt"),  # Reduce padding
          legend.position = "none"
        ) +
        # Coordinate system
        scale_x_continuous(limits = c(0.5, 7.5), breaks = 1:7, 
                          labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
        # Weekend background - light gray box from SAT/SUN text to last Sunday row
        annotate("rect", xmin = 5.5, xmax = 7.5, ymin = 6.6 - last_sunday_week, ymax = 7.0, 
                fill = "lightgray", alpha = 0.4, color = NA) +
        # Day labels with Apple-style styling - moved above cells
        annotate("text", x = 1:7, y = 6.8, 
                label = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), 
                size = 5.5, fontface = "bold", 
                color = "#1E3A5F")
      
      # Add event strips and labels only if there are events
      if (nrow(event_strips) > 0) {
        p <- p +
          # Event strips
          geom_tile(data = event_strips, 
                    aes(x = x, y = strip_y, fill = color, width = strip_width, height = strip_height_actual), 
                    color = "#1E3A5F", 
                    linewidth = 0.5, 
                    alpha = 0.85) +
          # Event labels on strips
          geom_text(data = event_strips, 
                    aes(label = label, x = x, y = strip_y), 
                    size = 2.8, color = "white", fontface = "bold") +
          # Custom fill scale for events
          scale_fill_identity()
      }
      
      return(p)
    }

    # Render modern calendar legend
    output$calendar_legend <- renderUI({
      req(current_month(), current_year())
      
      events <- current_month_events()
      
      # Handle empty events case
      if (is.null(events) || nrow(events) == 0) {
        return(
          div(class = "legend-container",
            tags$p("No events for this month.", 
                   style = "text-align: center; color: #6c757d; font-style: italic; padding: 20px;")
          )
        )
      }
      
      # Group events by status category
      confirmed_events <- events[events$status_category == "confirmed", ]
      estimated_events <- events[events$status_category == "estimated", ]
      waiting_events <- events[events$status_category == "waiting", ]
      
      legend_items <- list()
      
      # Create legend grid
      legend_grid <- list()
      
      # Confirmed events section
      if (nrow(confirmed_events) > 0) {
        unique_confirmed <- confirmed_events[!duplicated(confirmed_events$asu_id), ]
        confirmed_items <- lapply(seq_len(nrow(unique_confirmed)), function(i) {
          div(class = "legend-item",
            div(class = "color-indicator", 
                style = paste0("background-color: ", unique_confirmed$color[i])),
            div(class = "mouse-info",
              div(class = "mouse-id", unique_confirmed$asu_id[i]),
              div(class = "mouse-details", 
                  paste("Status:", unique_confirmed$status[i]))
            )
          )
        })
        
        legend_grid[[length(legend_grid) + 1]] <- div(class = "legend-section confirmed",
          confirmed_items
        )
      }
      
      # Estimated events section
      if (nrow(estimated_events) > 0) {
        unique_estimated <- estimated_events[!duplicated(estimated_events$asu_id), ]
        estimated_items <- lapply(seq_len(nrow(unique_estimated)), function(i) {
          div(class = "legend-item",
            div(class = "color-indicator", 
                style = paste0("background-color: ", unique_estimated$color[i])),
            div(class = "mouse-info",
              div(class = "mouse-id", unique_estimated$asu_id[i]),
              div(class = "mouse-details", 
                  paste("Status:", unique_estimated$status[i], "(Estimated)"))
            )
          )
        })
        
        legend_grid[[length(legend_grid) + 1]] <- div(class = "legend-section estimated",
          estimated_items
        )
      }
      
      # Waiting events section
      if (nrow(waiting_events) > 0) {
        unique_waiting <- waiting_events[!duplicated(waiting_events$asu_id), ]
        waiting_items <- lapply(seq_len(nrow(unique_waiting)), function(i) {
          div(class = "legend-item",
            div(class = "color-indicator", 
                style = paste0("background-color: ", unique_waiting$color[i])),
            div(class = "mouse-info",
              div(class = "mouse-id", unique_waiting$asu_id[i]),
              div(class = "mouse-details", 
                  paste("Status:", unique_waiting$status[i]))
            )
          )
        })
        
        legend_grid[[length(legend_grid) + 1]] <- div(class = "legend-section waiting",
          waiting_items
        )
      }
      
      # Add legend grid to main items
      legend_items[[length(legend_items) + 1]] <- div(class = "legend-grid", legend_grid)
      
      do.call(tagList, legend_items)
    })
    
    # Function to generate iCal content
    generate_ical_content <- function(events, month_name, year) {
      if (is.null(events) || nrow(events) == 0) {
        return("")
      }
      
      # Ensure events is a proper data frame
      if (!is.data.frame(events)) {
        return("")
      }
      
      # iCal header
      ical_content <- c(
        "BEGIN:VCALENDAR",
        "VERSION:2.0",
        "PRODID:-//Mouse Management System//Calendar Export//EN",
        "CALSCALE:GREGORIAN",
        "METHOD:PUBLISH"
      )
      
      # Generate unique ID for each event
      uid_counter <- 1
      
      for (i in seq_len(nrow(events))) {
        event <- events[i, ]
        
        # Format date for iCal (YYYYMMDD format)
        event_date <- format(event$date, "%Y%m%d")
        
        # Create unique identifier
        uid <- paste0("mouse-harvest-", uid_counter, "@mousemanagement.local")
        uid_counter <- uid_counter + 1
        
        # Determine event description based on status and estimation
        description <- paste0(
          "Mouse ID: ", event$asu_id, "\n",
          "Status: ", event$status, "\n",
          "Harvest Age: ", gsub("@E", "E", event$label), "\n"
        )
        
        if (event$is_estimated) {
          description <- paste0(description, "Note: Date estimated (no plug observed date)\n")
        }
        
        # Create iCal event
        ical_event <- c(
          "BEGIN:VEVENT",
          paste0("UID:", uid),
          paste0("DTSTART;VALUE=DATE:", event_date),
          paste0("DTEND;VALUE=DATE:", event_date),
          paste0("SUMMARY:Mice Experiment - ", event$label),
          paste0("DESCRIPTION:", gsub("\n", "\\n", description)),
          paste0("CATEGORIES:Mouse Harvest"),
          "STATUS:CONFIRMED",
          "SEQUENCE:0",
          paste0("DTSTAMP:", format(Sys.time(), "%Y%m%dT%H%M%SZ")),
          "END:VEVENT"
        )
        
        ical_content <- c(ical_content, ical_event)
      }
      
      # iCal footer
      ical_content <- c(ical_content, "END:VCALENDAR")
      
      return(paste(ical_content, collapse = "\r\n"))
    }
    
    # Download handler for iCal export
    output$export_ical_btn <- downloadHandler(
      filename = function() {
        month_num <- current_month()
        year_num <- current_year()
        paste0("mouse_harvest_calendar_", sprintf("%02d", month_num), "_", year_num, ".ics")
      },
      content = function(file) {
        tryCatch({
          events <- current_month_events()
          month_names <- c("January", "February", "March", "April", "May", "June",
                          "July", "August", "September", "October", "November", "December")
          ical_content <- generate_ical_content(events, month_names[current_month()], current_year())
          writeLines(ical_content, file)
        }, error = function(e) {
          # Write empty iCal file if there's an error
          writeLines("", file)
        })
      }
    )
  })
}

# Function to show the calendar modal (reusable)
show_plugging_calendar_modal <- function(id = "plugging_calendar_modal", db_path = DB_PATH) {
  showModal(modalDialog(
    title = div(
      style = "text-align: center; font-size: 20px; font-weight: 700; color: #1e3a5f; margin-bottom: 1px;",
      "📅 Mouse Harvest Calendar"
    ),
    size = "l",
    plugging_calendar_modal_ui(id),
    easyClose = TRUE,
    footer = NULL
  ))
  plugging_calendar_modal_server(id, db_path)
}
