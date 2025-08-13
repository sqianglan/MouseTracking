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
          padding: 8px;
          box-shadow: 0 8px 32px rgba(135, 206, 235, 0.15);
          border: 1px solid rgba(135, 206, 235, 0.2);
          margin-bottom: 8px;
          font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
        }
        
        .calendar-header {
          background: linear-gradient(135deg, rgba(135, 206, 235, 0.1) 0%, rgba(173, 216, 230, 0.1) 100%);
          backdrop-filter: blur(10px);
          -webkit-backdrop-filter: blur(10px);
          border: 1px solid rgba(135, 206, 235, 0.3);
          border-radius: 12px;
          padding: 6px;
          margin-bottom: 6px;
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
          flex-wrap: nowrap;
          
        }
        
        .stage-filters-panel {
          display: flex;
          flex-direction: row;
          gap: 8px;
          flex-wrap: nowrap;
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
          height: 600px;
          background: rgba(255, 255, 255, 0.8);
        }
        
        .legend-container {
          background: rgba(255, 255, 255, 0.95);
          backdrop-filter: blur(20px);
          -webkit-backdrop-filter: blur(20px);
          border: 1px solid rgba(135, 206, 235, 0.2);
          border-radius: 12px;
          padding: 16px;
          box-shadow: 0 4px 16px rgba(135, 206, 235, 0.1);
          margin-left: 16px;
          max-height: 600px;
          overflow-y: auto;
          width: 300px;
          flex-shrink: 0;
        }
        
        .calendar-plot-container {
          flex: 1;
          min-width: 0;
        }
        
        .calendar-main-content {
          display: flex;
          gap: 0;
          align-items: stretch;
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
          flex-direction: row;
          gap: 20px;
          flex-wrap: wrap;
        }
        
        .legend-section {
          background: rgba(135, 206, 235, 0.08);
          border-radius: 10px;
          padding: 12px;
          border-left: 4px solid rgba(135, 206, 235, 0.6);
          backdrop-filter: blur(10px);
          -webkit-backdrop-filter: blur(10px);
          flex: 1;
          min-width: 200px;
        }
        
        .legend-items-column {
          display: flex;
          flex-direction: column;
          gap: 8px;
          margin-top: 8px;
        }
        
        .legend-section.confirmed {
          border-left-color: rgba(46, 139, 87, 0.9);
          background: rgba(46, 139, 87, 0.08);
          box-shadow: 0 2px 8px rgba(46, 139, 87, 0.15);
        }
        
        .legend-section.observed {
          border-left-color: rgba(65, 105, 225, 0.9);
          background: rgba(65, 105, 225, 0.08);
          box-shadow: 0 2px 8px rgba(65, 105, 225, 0.15);
        }
        
        .legend-section.estimated {
          border-left-color: rgba(255, 140, 0, 0.9);
          background: rgba(255, 140, 0, 0.08);
          box-shadow: 0 2px 8px rgba(255, 140, 0, 0.15);
        }
        
        .legend-section.waiting {
          border-left-color: rgba(112, 128, 144, 0.9);
          background: rgba(112, 128, 144, 0.08);
          box-shadow: 0 2px 8px rgba(112, 128, 144, 0.15);
        }
        
        .legend-section-title {
          font-weight: 700;
          margin-bottom: 10px;
          color: #1e3a5f;
          font-size: 13px;
          letter-spacing: -0.2px;
          display: flex;
          align-items: center;
          gap: 8px;
        }
        
        .legend-checkbox {
          width: 16px;
          height: 16px;
          accent-color: #87CEEB;
          cursor: pointer;
        }
        
        .legend-item {
          display: flex;
          align-items: center;
          gap: 8px;
          padding: 6px 10px;
          border-radius: 6px;
          background: rgba(255, 255, 255, 0.9);
          box-shadow: 0 2px 6px rgba(135, 206, 235, 0.1);
          backdrop-filter: blur(10px);
          -webkit-backdrop-filter: blur(10px);
          border: 1px solid rgba(135, 206, 235, 0.15);
          flex: 0 0 auto;
        }
        
        .color-indicator {
          width: 14px;
          height: 14px;
          border-radius: 50%;
          border: 2px solid rgba(255, 255, 255, 0.9);
          box-shadow: 0 2px 4px rgba(0,0,0,0.15);
          flex-shrink: 0;
        }
        
        .mouse-info {
          font-size: 12px;
        }
        
        .mouse-id {
          font-weight: 700;
          color: #1e3a5f;
          letter-spacing: -0.2px;
          cursor: pointer;
          transition: all 0.2s ease;
        }
        
        .mouse-id:hover {
          color: #87CEEB;
          text-decoration: underline;
          transform: translateY(-1px);
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
          position: relative;
          width: 80px;
          height: 80px;
        }
        
        .stat-card.total-events {
          width: 100px;
          height: 80px;
        }
        
        .stat-button {
          border: none !important;
          background: linear-gradient(135deg, rgba(135, 206, 235, 0.9) 0%, rgba(173, 216, 230, 0.9) 100%) !important;
          color: #1e3a5f !important;
          padding: 2px !important;
          border-radius: 8px !important;
          text-align: center !important;
          box-shadow: 0 2px 2px rgba(135, 206, 235, 0.2) !important;
          backdrop-filter: blur(10px) !important;
          -webkit-backdrop-filter: blur(10px) !important;
          border: 1px solid rgba(135, 206, 235, 0.3) !important;
          position: relative !important;
          width: 80px !important;
          height: 80px !important;
          cursor: pointer !important;
          transition: all 0.2s ease !important;
        }
        
        .stat-button:hover {
          transform: translateY(-2px) !important;
          box-shadow: 0 4px 12px rgba(135, 206, 235, 0.3) !important;
        }
        
        .stat-button.selected {
          background: linear-gradient(135deg, rgba(135, 206, 235, 0.9) 0%, rgba(173, 216, 230, 0.9) 100%) !important;
          border: 2px solid rgba(135, 206, 235, 0.8) !important;
        }
        
        .stat-button.deselected {
          background: rgba(200, 200, 200, 0.5) !important;
          color: #666 !important;
          border: 1px solid rgba(200, 200, 200, 0.5) !important;
          opacity: 0.6 !important;
        }
        
        .stage-filter-card {
          border: none !important;
          background: linear-gradient(135deg, rgba(135, 206, 235, 0.9) 0%, rgba(173, 216, 230, 0.9) 100%) !important;
          color: #1e3a5f !important;
          padding: 8px !important;
          border-radius: 8px !important;
          text-align: center !important;
          box-shadow: 0 2px 8px rgba(135, 206, 235, 0.2) !important;
          backdrop-filter: blur(10px) !important;
          -webkit-backdrop-filter: blur(10px) !important;
          border: 1px solid rgba(135, 206, 235, 0.3) !important;
          position: relative !important;
          width: 80px !important;
          height: 80px !important;
          cursor: pointer !important;
          transition: all 0.2s ease !important;
        }
        
        .stage-filter-card:hover {
          transform: translateY(-2px) !important;
          box-shadow: 0 4px 12px rgba(135, 206, 235, 0.3) !important;
        }
        
        .stage-filter-card.selected {
          background: linear-gradient(135deg, rgba(135, 206, 235, 0.9) 0%, rgba(173, 216, 230, 0.9) 100%) !important;
          border: 2px solid rgba(135, 206, 235, 0.8) !important;
        }
        
        .stage-filter-card.deselected {
          background: rgba(200, 200, 200, 0.5) !important;
          color: #666 !important;
          border: 1px solid rgba(200, 200, 200, 0.5) !important;
          opacity: 0.6 !important;
        }
        
        .stat-content {
          display: flex;
          flex-direction: column;
          align-items: center;
          justify-content: center;
          width: 100%;
          height: 100%;
        }
        
        .stage-filter-title {
          font-size: 14px;
          font-weight: 600;
          color: #1e3a5f;
          margin-bottom: 4px;
          letter-spacing: 0.2px;
        }
        
        .stage-filter-card .stat-label {
          font-size: 14px;
          font-weight: 600;
        }
        </style>
        <script>
        Shiny.addCustomMessageHandler('updateButtonClass', function(data) {
          var element = document.getElementById(data.id);
          if (element) {
            element.classList.remove(data.removeClass);
            element.classList.add(data.addClass);
          }
        });
        </script>
        
        .stage-checkbox-group {
          display: flex;
          flex-direction: row;
          gap: 4px;
          flex-wrap: nowrap;
          justify-content: center;
          align-items: center;
        }
        
        .stage-checkbox-item {
          display: flex;
          align-items: center;
          gap: 2px;
          font-size: 8px;
          color: #1e3a5f;
          font-weight: 600;
        }
        
        .stage-checkbox-input {
          width: 10px;
          height: 10px;
          accent-color: #1e3a5f;
          cursor: pointer;
          margin: 0;
          padding: 0;
        }
        
        .stat-number {
          font-size: 24px;
          font-weight: 700;
          margin-bottom: 2px;
          letter-spacing: -0.3px;
        }
        
        .stat-label {
          font-size: 8px;
          opacity: 0.9;
          font-weight: normal;
          letter-spacing: 0.2px;
          text-align: center;
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
          
          .legend-grid {
            flex-direction: column;
          }
          
          .legend-section {
            min-width: auto;
          }
          
          .calendar-container {
            padding: 16px;
          }
          
          .calendar-header {
            padding: 16px;
          }
        }
        
        /* Custom modal size - larger than default 'l' */
        .modal-lg,
        .modal-dialog.modal-lg,
        .modal.show .modal-dialog,
        .modal-dialog {
          max-width: 85% !important;
          width: 85% !important;
        }
        
        /* Force modal container to use full width */
        .modal.fade.show {
          display: flex !important;
          align-items: center !important;
          justify-content: center !important;
        }
        
        /* Animation for smooth transitions */
        .calendar-container * {
          transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
        }
      "))
    ),
    
    # Main calendar container
    div(class = "calendar-container",
      # Dynamic content based on view mode
      uiOutput(ns("modal_content"))
    )
  )
}

##Debug
#DB_PATH <- "mice_colony_test.db"

# Server logic for the plugging calendar modal
plugging_calendar_modal_server <- function(id, db_path = DB_PATH, shared_plugging_state = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values for current month/year
    current_month <- reactiveVal(as.integer(format(Sys.Date(), "%m")))
    current_year <- reactiveVal(as.integer(format(Sys.Date(), "%Y")))
    
    # Reactive values for category visibility (default all checked)
    show_confirmed <- reactiveVal(TRUE)
    show_observed <- reactiveVal(TRUE)
    show_estimated <- reactiveVal(TRUE)
    show_waiting <- reactiveVal(TRUE)
    
    # Reactive values for stage filtering
    selected_stages <- reactiveVal(NULL)  # Will hold list of selected stages

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

    # Category visibility observers
    observeEvent(input$show_confirmed_btn, {
      show_confirmed(!show_confirmed())
    })
    
    observeEvent(input$show_observed_btn, {
      show_observed(!show_observed())
    })
    
    observeEvent(input$show_estimated_btn, {
      show_estimated(!show_estimated())
    })
    
    observeEvent(input$show_waiting_btn, {
      show_waiting(!show_waiting())
    })
    
    # Dynamic observers for stage checkboxes
    observeEvent(available_stages(), {
      stages <- available_stages()
      if (length(stages) > 0) {
        # Initialize selected_stages to all available stages if null
        if (is.null(selected_stages())) {
          selected_stages(stages)
        }
        
        # Create observers for each stage button
        lapply(stages, function(stage) {
          button_id <- paste0("stage_", gsub("\\.", "_", stage))
          
          observeEvent(input[[button_id]], {
            current_selected <- selected_stages()
            if (is.null(current_selected)) {
              current_selected <- character(0)
            }
            
            if (stage %in% current_selected) {
              # Remove stage if currently selected
              selected_stages(setdiff(current_selected, stage))
            } else {
              # Add stage if not currently selected
              selected_stages(c(current_selected, stage))
            }
          }, ignoreInit = TRUE)
        })
      }
    })

    # Use ggsci NPG color palette for clear distinction
    get_category_colors <- function() {
      # NPG color palette from ggsci - highly distinguishable colors
      npg_colors <- c("#E64B35", "#4DBBD5", "#00A087", "#3C5488", "#F39B7F", 
                      "#8491B4", "#91D1C2", "#DC0000", "#7E6148", "#B09C85")
      
      list(
        # Confirmed: NPG colors (high confidence with clear dates)
        confirmed = npg_colors,
        # Observed: NPG colors (medium confidence with observed dates)
        observed = npg_colors,
        # Estimated: Orange (low confidence, estimated dates)
        estimated = "#ff9800",
        # Waiting: Gray (pending confirmation)
        waiting = "#9e9e9e"
      )
    }
    
    # Function to assign colors based on category and ASU ID
    get_color_for_event <- function(status_category, asu_id) {
      colors <- get_category_colors()
      
      if (status_category == "confirmed") {
        # Use hash for variety within green palette
        hash_val <- sum(utf8ToInt(asu_id)) %% length(colors$confirmed)
        return(colors$confirmed[hash_val + 1])
      } else if (status_category == "observed") {
        # Use hash for variety within blue palette
        hash_val <- sum(utf8ToInt(asu_id)) %% length(colors$observed)
        return(colors$observed[hash_val + 1])
      } else if (status_category == "estimated") {
        return(colors$estimated)
      } else if (status_category == "waiting") {
        return(colors$waiting)
      }
      
      # Fallback
      return("#9e9e9e")
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
          is_surprising_plug = logical(0),
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
        
        # Determine status category and color based on new categorization
        is_surprising_plug <- row$plugging_status == "Surprising Plug!!"
        
        if (row$plugging_status == "Not Observed (Waiting for confirmation)") {
          status_category <- "waiting"
        } else if (row$plugging_status == "Plug Confirmed") {
          status_category <- "confirmed"  # High confidence events
        } else if (is_surprising_plug) {
          status_category <- "confirmed"  # For statistics, but special visual treatment
        } else if (row$plugging_status == "Plugged") {
          status_category <- "observed"   # Plug observed but not yet confirmed
        } else if (is_estimated) {
          status_category <- "estimated"  # Date estimated from pairing
        } else {
          status_category <- "observed"   # Default fallback
        }
        
        # Get color based on category, with special handling for surprising plug
        if (is_surprising_plug) {
          # Use NPG color for border but gray background will be handled in plotting
          color <- get_color_for_event("confirmed", row$asu_id)
        } else {
          color <- get_color_for_event(status_category, row$asu_id)
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
              is_surprising_plug = is_surprising_plug,
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
          is_surprising_plug = logical(0),
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
          is_surprising_plug = logical(0),
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
    
    # Reactive: Get unique stages available in current month
    available_stages <- reactive({
      events <- current_month_events_unfiltered()
      if (is.null(events) || nrow(events) == 0) {
        return(character(0))
      }
      
      # Extract unique stages and sort them
      stages <- unique(events$expected_age)
      stages <- stages[!is.na(stages)]
      stages <- sort(stages)
      
      # Format as E13.5, E14.5, etc.
      paste0("E", stages + 0.5)
    })

    # Reactive: Get unfiltered events for current month/year (for legend)
    current_month_events_unfiltered <- reactive({
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
          is_surprising_plug = logical(0),
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
          is_surprising_plug = logical(0),
          stringsAsFactors = FALSE
        ))
      }
      
      # Filter for current month/year and validate days (NO category filtering)
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
          is_surprising_plug = logical(0),
          stringsAsFactors = FALSE
        ))
      }
      
      return(filtered)
    })

    # Reactive: Filter events for current month/year (for calendar plot)
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
          is_surprising_plug = logical(0),
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
          is_surprising_plug = logical(0),
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
      
      # Apply category visibility filters
      if (nrow(filtered) > 0) {
        category_filter <- rep(FALSE, nrow(filtered))
        
        if (show_confirmed()) {
          category_filter <- category_filter | filtered$status_category == "confirmed"
        }
        if (show_observed()) {
          category_filter <- category_filter | filtered$status_category == "observed"
        }
        if (show_estimated()) {
          category_filter <- category_filter | filtered$status_category == "estimated"
        }
        if (show_waiting()) {
          category_filter <- category_filter | filtered$status_category == "waiting"
        }
        
        filtered <- filtered[category_filter, ]
        
        # Apply stage filters
        stages <- selected_stages()
        if (!is.null(stages) && length(stages) > 0 && nrow(filtered) > 0) {
          stage_filter <- paste0("E", filtered$expected_age + 0.5) %in% stages
          filtered <- filtered[stage_filter, ]
        }
      }
      
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
          is_surprising_plug = logical(0),
          stringsAsFactors = FALSE
        ))
      }
      
      return(filtered)
    })

    # Statistics outputs
    output$total_events <- renderText({
      events <- current_month_events_unfiltered()
      if (is.null(events) || nrow(events) == 0) {
        return("0")
      }
      as.character(length(unique(events$asu_id)))
    })

    output$confirmed_events <- renderText({
      events <- current_month_events_unfiltered()
      if (is.null(events) || nrow(events) == 0) {
        return("0")
      }
      as.character(length(unique(events$asu_id[events$status_category == "confirmed"])))
    })

    output$observed_events <- renderText({
      events <- current_month_events_unfiltered()
      if (is.null(events) || nrow(events) == 0) {
        return("0")
      }
      as.character(length(unique(events$asu_id[events$status_category == "observed"])))
    })

    output$estimated_events <- renderText({
      events <- current_month_events_unfiltered()
      if (is.null(events) || nrow(events) == 0) {
        return("0")
      }
      as.character(length(unique(events$asu_id[events$status_category == "estimated"])))
    })

    output$waiting_events <- renderText({
      events <- current_month_events_unfiltered()
      if (is.null(events) || nrow(events) == 0) {
        return("0")
      }
      as.character(length(unique(events$asu_id[events$status_category == "waiting"])))
    })
    
    # Render stage filters
    output$stage_filters <- renderUI({
      stages <- available_stages()
      
      if (length(stages) == 0) {
        return(NULL)
      }
      
      # Initialize selected_stages if NULL
      if (is.null(selected_stages())) {
        selected_stages(stages)  # Default all stages selected
      }
      
      stage_buttons <- lapply(stages, function(stage) {
        button_id <- paste0("stage_", gsub("\\.", "_", stage))
        button_class <- if(stage %in% selected_stages()) "stage-filter-card stage-button selected" else "stage-filter-card stage-button deselected"
        actionButton(ns(button_id), 
          div(class = "stat-label", stage),
          class = button_class
        )
      })
      
      div(style = "display: flex; flex-direction: row; gap: 8px; flex-wrap: nowrap;",
        do.call(tagList, stage_buttons)
      )
    })

    # Update statistics button visual states
    observe({
      session$sendCustomMessage(type = "updateButtonClass", 
        list(id = paste0(session$ns("show_confirmed_btn")), 
             addClass = if(show_confirmed()) "selected" else "deselected",
             removeClass = if(show_confirmed()) "deselected" else "selected"))
    })
    
    observe({
      session$sendCustomMessage(type = "updateButtonClass", 
        list(id = paste0(session$ns("show_observed_btn")), 
             addClass = if(show_observed()) "selected" else "deselected",
             removeClass = if(show_observed()) "deselected" else "selected"))
    })
    
    observe({
      session$sendCustomMessage(type = "updateButtonClass", 
        list(id = paste0(session$ns("show_estimated_btn")), 
             addClass = if(show_estimated()) "selected" else "deselected",
             removeClass = if(show_estimated()) "deselected" else "selected"))
    })
    
    observe({
      session$sendCustomMessage(type = "updateButtonClass", 
        list(id = paste0(session$ns("show_waiting_btn")), 
             addClass = if(show_waiting()) "selected" else "deselected",
             removeClass = if(show_waiting()) "deselected" else "selected"))
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
                  size = 7, fontface = "bold", 
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
        # Separate surprising plugs from regular events
        surprising_strips <- event_strips[event_strips$is_surprising_plug == TRUE, ]
        regular_strips <- event_strips[event_strips$is_surprising_plug == FALSE, ]
        
        p <- p +
          # Regular event strips
          geom_tile(data = regular_strips, 
                    aes(x = x, y = strip_y, fill = color, width = strip_width, height = strip_height_actual), 
                    color = "#1E3A5F", 
                    linewidth = 0.5, 
                    alpha = 0.85) +
          # Custom fill scale for events
          scale_fill_identity()
        
        # Add surprising plug strips with gray background, same border
        if (nrow(surprising_strips) > 0) {
          p <- p +
            geom_tile(data = surprising_strips, 
                      aes(x = x, y = strip_y, width = strip_width, height = strip_height_actual), 
                      fill = "#9e9e9e",  # Gray background
                      color = "#1E3A5F",  # Same border as regular events
                      linewidth = 0.5,   # Same border thickness
                      alpha = 0.85)
        }
        
        # Add labels - white for regular events, colored for surprising plugs
        if (nrow(regular_strips) > 0) {
          p <- p +
            geom_text(data = regular_strips, 
                      aes(label = label, x = x, y = strip_y), 
                      size = 4, color = "white", fontface = "bold")
        }
        
        if (nrow(surprising_strips) > 0) {
          p <- p +
            geom_text(data = surprising_strips, 
                      aes(label = label, x = x, y = strip_y, color = color), 
                      size = 4, fontface = "bold") +
            scale_color_identity()
        }
      }
      
      return(p)
    }

    # Render modern calendar legend
    output$calendar_legend <- renderUI({
      req(current_month(), current_year())
      
      events <- current_month_events_unfiltered()
      
      # Handle empty events case - show "No events" in legend area instead of empty categories
      if (is.null(events) || nrow(events) == 0) {
        return(
          tags$p("No events for this month.", 
                 style = "text-align: center; color: #6c757d; font-style: italic; padding: 40px; font-size: 16px;")
        )
      }
      
      # Group events by status category with priority order
      confirmed_events <- events[events$status_category == "confirmed", ]
      observed_events <- events[events$status_category == "observed", ]
      estimated_events <- events[events$status_category == "estimated", ]
      waiting_events <- events[events$status_category == "waiting", ]
      
      legend_items <- list()
      
      # Create legend grid with hierarchical sections
      legend_grid <- list()
      
      # Confirmed events section (highest priority - green)
      if (nrow(confirmed_events) > 0) {
        unique_confirmed <- confirmed_events[!duplicated(confirmed_events$asu_id), ]
        confirmed_items <- lapply(seq_len(nrow(unique_confirmed)), function(i) {
          div(class = "legend-item",
            div(class = "color-indicator", 
                style = paste0("background-color: ", unique_confirmed$color[i])),
            div(class = "mouse-info",
              div(class = "mouse-id", 
                  unique_confirmed$asu_id[i],
                  onclick = paste0('Shiny.setInputValue("', ns('mouse_id_clicked'), '", "', unique_confirmed$asu_id[i], '", {priority: "event"});'))
            )
          )
        })
        
        legend_grid[[length(legend_grid) + 1]] <- div(class = "legend-section confirmed",
          div(class = "legend-section-title", "🎯 CONFIRMED PLUGS"),
          div(class = "legend-items-column", confirmed_items)
        )
      }
      
      # Observed events section (medium-high priority - blue)
      if (nrow(observed_events) > 0) {
        unique_observed <- observed_events[!duplicated(observed_events$asu_id), ]
        observed_items <- lapply(seq_len(nrow(unique_observed)), function(i) {
          div(class = "legend-item",
            div(class = "color-indicator", 
                style = paste0("background-color: ", unique_observed$color[i])),
            div(class = "mouse-info",
              div(class = "mouse-id", 
                  unique_observed$asu_id[i],
                  onclick = paste0('Shiny.setInputValue("', ns('mouse_id_clicked'), '", "', unique_observed$asu_id[i], '", {priority: "event"});'))
            )
          )
        })
        
        legend_grid[[length(legend_grid) + 1]] <- div(class = "legend-section observed",
          div(class = "legend-section-title", "👁 OBSERVED PLUGS"),
          div(class = "legend-items-column", observed_items)
        )
      }
      
      # Estimated events section (medium-low priority - orange)
      if (nrow(estimated_events) > 0) {
        unique_estimated <- estimated_events[!duplicated(estimated_events$asu_id), ]
        estimated_items <- lapply(seq_len(nrow(unique_estimated)), function(i) {
          div(class = "legend-item",
            div(class = "color-indicator", 
                style = paste0("background-color: ", unique_estimated$color[i])),
            div(class = "mouse-info",
              div(class = "mouse-id", 
                  unique_estimated$asu_id[i],
                  onclick = paste0('Shiny.setInputValue("', ns('mouse_id_clicked'), '", "', unique_estimated$asu_id[i], '", {priority: "event"});'))
            )
          )
        })
        
        legend_grid[[length(legend_grid) + 1]] <- div(class = "legend-section estimated",
          div(class = "legend-section-title", "⚠ ESTIMATED DATES"),
          div(class = "legend-items-column", estimated_items)
        )
      }
      
      # Waiting events section (lowest priority - gray)
      if (nrow(waiting_events) > 0) {
        unique_waiting <- waiting_events[!duplicated(waiting_events$asu_id), ]
        waiting_items <- lapply(seq_len(nrow(unique_waiting)), function(i) {
          div(class = "legend-item",
            div(class = "color-indicator", 
                style = paste0("background-color: ", unique_waiting$color[i])),
            div(class = "mouse-info",
              div(class = "mouse-id", 
                  unique_waiting$asu_id[i],
                  onclick = paste0('Shiny.setInputValue("', ns('mouse_id_clicked'), '", "', unique_waiting$asu_id[i], '", {priority: "event"});'))
            )
          )
        })
        
        legend_grid[[length(legend_grid) + 1]] <- div(class = "legend-section waiting",
          div(class = "legend-section-title", "⏳ AWAITING CONFIRMATION"),
          div(class = "legend-items-column", waiting_items)
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
    
    # Reactive value to track current view mode
    view_mode <- reactiveVal("calendar")  # "calendar" or "details"
    selected_mouse_data <- reactiveVal(NULL)
    
    # Handle mouse ID clicks in legend
    observeEvent(input$mouse_id_clicked, {
      asu_id <- input$mouse_id_clicked
      if (is.null(asu_id)) return()
      
      con <- NULL
      tryCatch({
        con <- dbConnect(SQLite(), db_path)
        
        # Get detailed plugging information
        plugging <- dbGetQuery(con, 
          "SELECT ph.*, 
                  m.dob as male_dob, m.breeding_line as male_breeding_line, 
                  m.genotype as male_genotype, m.status as male_status,
                  f.dob as female_dob, f.breeding_line as female_breeding_line, 
                  f.genotype as female_genotype, f.status as female_status
           FROM plugging_history ph
           LEFT JOIN mice_stock m ON ph.male_id = m.asu_id
           LEFT JOIN mice_stock f ON ph.female_id = f.asu_id
           WHERE ph.female_id = ?
           ORDER BY ph.pairing_start_date DESC
           LIMIT 1",
          params = list(asu_id))
        
        if (nrow(plugging) == 0) {
          showNotification(paste("No plugging details found for", asu_id), type = "warning")
          return()
        }
        
        # Store the data and switch to details view
        selected_mouse_data(plugging[1, ])
        view_mode("details")
        
      }, error = function(e) {
        showNotification(paste("Error fetching details:", e$message), type = "error")
      }, finally = {
        if (!is.null(con)) dbDisconnect(con)
      })
      
      # Reset the input so the same mouse ID can be clicked again
      session$sendInputMessage("mouse_id_clicked", NULL)
    })
    
    # Handle back to calendar button
    observeEvent(input$back_to_calendar, {
      view_mode("calendar")
      selected_mouse_data(NULL)
    })
    
    # Dynamic modal content based on view mode
    output$modal_content <- renderUI({
      if (view_mode() == "calendar") {
        # Original calendar view
        tagList(
          # Statistics and legend
          div(class = "calendar-header",
            div(class = "calendar-controls",
              div(class = "navigation-container",
                div(class = "month-navigation",
                  actionButton(ns("prev_month"), "◀", class = "nav-btn"),
                  div(class = "current-month", 
                      textOutput(ns("current_month_year_display"))),
                  actionButton(ns("next_month"), "▶", class = "nav-btn")
                )
              ),
              div(class = "stats-panel",
                div(class = "stat-card total-events",
                  div(class = "stat-content",
                    div(class = "stat-number", textOutput(ns("total_events"))),
                    div(class = "stat-label", "Total Events")
                  )
                ),
                actionButton(ns("show_confirmed_btn"), 
                  div(
                    div(class = "stat-number", textOutput(ns("confirmed_events"))),
                    div(class = "stat-label", "Confirmed")
                  ),
                  class = "stat-card stat-button selected"
                ),
                actionButton(ns("show_observed_btn"), 
                  div(
                    div(class = "stat-number", textOutput(ns("observed_events"))),
                    div(class = "stat-label", "Observed")
                  ),
                  class = "stat-card stat-button selected"
                ),
                actionButton(ns("show_estimated_btn"), 
                  div(
                    div(class = "stat-number", textOutput(ns("estimated_events"))),
                    div(class = "stat-label", "Estimated")
                  ),
                  class = "stat-card stat-button selected"
                ),
                actionButton(ns("show_waiting_btn"), 
                  div(
                    div(class = "stat-number", textOutput(ns("waiting_events"))),
                    div(class = "stat-label", "Waiting")
                  ),
                  class = "stat-card stat-button selected"
                )
              ),
              div(class = "stage-filters-panel",
                uiOutput(ns("stage_filters"))
              )
            )
          ),
          div(class = "calendar-main-content",
            div(class = "calendar-plot-container",
              div(class = "calendar-body",
                div(class = "calendar-plot",
                  plotOutput(ns("calendar_plot"), height = "600px")
                )
              )
            ),
            div(class = "legend-container",
              uiOutput(ns("calendar_legend")),
              div(style = "margin-top: 16px; text-align: center;",
                downloadButton(paste0(id, "-export_ical_btn"), 
                              "📅 Export iCal", 
                              class = "export-btn")
              )
            )
          )
        )
      } else {
        # Details view
        req(selected_mouse_data())
        row <- selected_mouse_data()
        
        # Calculate ages
        male_age <- if(!is.na(row$male_dob)) round(as.numeric(Sys.Date() - as.Date(row$male_dob)) / 7, 1) else NA
        female_age <- if(!is.na(row$female_dob)) round(as.numeric(Sys.Date() - as.Date(row$female_dob)) / 7, 1) else NA
        
        tagList(
          # Back button
          div(
            style = "margin-bottom: 20px; text-align: left;",
            actionButton(ns("back_to_calendar"), "← Back to Calendar", 
                        class = "btn-primary",
                        style = "background: #007bff; border-color: #007bff; color: white;")
          ),
          
          # Details content
          div(
            style = "font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;",
            
            h4(paste("🐭 Plugging Details:", row$female_id), 
               style = "text-align: center; color: #1e3a5f; margin-bottom: 20px;"),
            
            # All information boxes in one row
            div(
              style = "display: flex; gap: 16px; flex-wrap: wrap;",
              
              # Breeding Pair Information
              div(
                style = "background: linear-gradient(135deg, #e8f5e8 0%, #d4edda 100%); border-radius: 8px; padding: 16px; border-left: 4px solid #28a745; flex: 1; min-width: 250px;",
                h5("👫 Breeding Pair", style = "margin: 0 0 8px 0; color: #2c3e50;"),
                div(
                  div(
                    strong("Male: "), paste0(row$male_id, " (", ifelse(is.na(male_age), "Unknown age", paste0(male_age, " wks")), ")"),
                    br(),
                    "Line: ", ifelse(is.na(row$male_breeding_line), "Unknown", row$male_breeding_line),
                    br(),
                    "Genotype: ", ifelse(is.na(row$male_genotype), "Unknown", row$male_genotype)
                  ),
                  br(),
                  div(
                    strong("Female: "), paste0(row$female_id, " (", ifelse(is.na(female_age), "Unknown age", paste0(female_age, " wks")), ")"),
                    br(),
                    "Line: ", ifelse(is.na(row$female_breeding_line), "Unknown", row$female_breeding_line),
                    br(),
                    "Genotype: ", ifelse(is.na(row$female_genotype), "Unknown", row$female_genotype)
                  )
                )
              ),
              
              # Timeline Information
              div(
                style = "background: rgba(135, 206, 235, 0.1); border-radius: 8px; padding: 16px; border-left: 4px solid #87CEEB; flex: 1; min-width: 200px;",
                h5("Timeline", style = "margin: 0 0 8px 0; color: #2c3e50;"),
                div(
                  "Pairing Start: ", strong(ifelse(is.na(row$pairing_start_date) || row$pairing_start_date == "", "Unknown", row$pairing_start_date)),
                  br(),
                  "Pairing End: ", strong(ifelse(is.na(row$pairing_end_date) || row$pairing_end_date == "", "Unknown", row$pairing_end_date)),
                  br(),
                  "Plug Observed: ", strong(ifelse(is.na(row$plug_observed_date) || row$plug_observed_date == "", "Unknown", row$plug_observed_date))
                )
              ),
              
              # Status Information
              div(
                style = "background: rgba(255, 193, 7, 0.1); border-radius: 8px; padding: 16px; border-left: 4px solid #ffc107; flex: 1; min-width: 200px;",
                h5("Status", style = "margin: 0 0 8px 0; color: #2c3e50;"),
                div(
                  "Current Status: ", strong(row$plugging_status),
                  br(),
                  "Expected Harvest Age: ", strong(ifelse(is.na(row$expected_age_for_harvesting) || row$expected_age_for_harvesting == "", "Not specified", row$expected_age_for_harvesting))
                )
              ),
              
              # Notes (only if exists)
              if (!is.na(row$notes) && row$notes != "") {
                div(
                  style = "background: rgba(108, 117, 125, 0.1); border-radius: 8px; padding: 16px; border-left: 4px solid #6c757d; flex: 1; min-width: 200px;",
                  h5("📝 Notes", style = "margin: 0 0 8px 0; color: #2c3e50;"),
                  div(style = "font-style: italic;", row$notes)
                )
              }
            )
          )
        )
      }
    })
  })
}

# Function to show the calendar modal (reusable)
show_plugging_calendar_modal <- function(id = "plugging_calendar_modal", db_path = DB_PATH, shared_plugging_state = NULL) {
  showModal(modalDialog(
    title = div(
      style = "text-align: center; font-size: 20px; font-weight: 700; color: #1e3a5f; margin-bottom: 1px;",
      "📅 Mouse Harvest Calendar"
    ),
    size = "l",
    plugging_calendar_modal_ui(id),
    easyClose = TRUE,
    footer = tagList(
      div(style = "text-align: center; width: 100%;",
        modalButton("Close")
      ),
      tags$script(HTML("
        $(document).ready(function() {
          setTimeout(function() {
            $('.modal-dialog').css({
              'max-width': '85%',
              'width': '85%'
            });
          }, 50);
        });
      "))
    )
  ))
  plugging_calendar_modal_server(id, db_path, shared_plugging_state)
}
