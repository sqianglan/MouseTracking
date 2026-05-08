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
  library(plotly)
})

# Source body weight modal functions
source("Modules/modal_body_weight.R")

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
          width: 250px;
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
          max-width: 70% !important;
          width: 70% !important;
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
    reset_calendar_modal_state <- function() {
      view_mode("calendar")
      selected_mouse_data(NULL)
    }

    reopen_calendar_modal <- function() {
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
            actionButton(ns("close_or_back_btn"), "Close", class = "btn btn-default")
          ),
          tags$script(HTML("
            $(document).ready(function() {
              setTimeout(function() {
                var $modal = $('.modal').last();
                $modal.off('hidden.bs.modal.calendarReset');
                $modal.on('hidden.bs.modal.calendarReset', function() {
                  Shiny.setInputValue('", ns("calendar_modal_closed"), "', Date.now(), {priority: 'event'});
                });
                $modal.find('.modal-dialog').css({
                  'max-width': '70%',
                  'width': '70%'
                });
              }, 50);
            });
          "))
        )
      ))
    }

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

    get_breeding_line_palette <- function() {
      c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e",
        "#e6ab02", "#a6761d", "#1f78b4", "#b2df8a", "#fb9a99")
    }

    blend_hex_color <- function(color, target = "#FFFFFF", amount = 0.2) {
      amount <- max(0, min(1, amount))
      color_rgb <- grDevices::col2rgb(color)
      target_rgb <- grDevices::col2rgb(target)
      blended_rgb <- round(color_rgb * (1 - amount) + target_rgb * amount)
      grDevices::rgb(blended_rgb[1], blended_rgb[2], blended_rgb[3], maxColorValue = 255)
    }

    get_color_for_event <- function(status_category, breeding_line, asu_id) {
      if (identical(status_category, "waiting")) {
        return("#9e9e9e")
      }

      palette <- get_breeding_line_palette()
      color_key <- if (!is.null(breeding_line) && !is.na(breeding_line) && trimws(breeding_line) != "") {
        trimws(breeding_line)
      } else {
        asu_id
      }

      hash_val <- sum(utf8ToInt(color_key)) %% length(palette)
      base_color <- palette[hash_val + 1]

      if (identical(status_category, "confirmed")) {
        return(base_color)
      }

      if (identical(status_category, "observed")) {
        return(blend_hex_color(base_color, "#FFFFFF", amount = 0.18))
      }

      if (identical(status_category, "estimated")) {
        return(blend_hex_color(base_color, "#FFFFFF", amount = 0.32))
      }

      "#9e9e9e"
    }

    get_event_priority <- function(status_category, status) {
      if (identical(status, "Plugged")) {
        return(1L)
      }

      if (identical(status_category, "confirmed")) {
        return(2L)
      }

      if (identical(status_category, "estimated")) {
        return(3L)
      }

      4L
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
        # First try to use plug_observed_date if available and not "Unknown"
        parsed_plug_date <- safe_parse_date(plug_date)
        if (!is.na(parsed_plug_date)) {
          return(list(date = parsed_plug_date, is_estimated = FALSE))
        } else {
          # Use pairing_start_date + 1 for estimation if plug_observed_date is "Unknown"
          # Add 1 day to account for the day of pairing, then add embryonic age
          parsed_pairing_date <- safe_parse_date(pairing_start_date)
          if (!is.na(parsed_pairing_date)) {
            return(list(date = parsed_pairing_date + 1, is_estimated = TRUE))
          }
        }
      }
      return(NULL)
    }

    # Helper function to create calendar events
    create_calendar_events <- function(pluggings) {
      if (is.null(pluggings) || nrow(pluggings) == 0) {
        return(data.frame(
          asu_id = character(0),
          breeding_line = character(0),
          label = character(0),
          day = integer(0),
          date = as.Date(character(0)),
          target_month = integer(0),
          target_year = integer(0),
          color = character(0),
          status = character(0),
          status_category = character(0),
          event_priority = integer(0),
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
        
        color <- get_color_for_event(status_category, row$breeding_line, row$asu_id)
        event_priority <- get_event_priority(status_category, row$plugging_status)
        
        # Parse expected ages
        expected_ages <- parse_expected_ages(row$expected_age_for_harvesting)
        
        # Create events for each expected age
        for (age in expected_ages) {
          harvest_date <- base_date + floor(age)
          
          if (!is.na(harvest_date)) {
            # Add surprising emoji to label if it's a surprising plug
            label_text <- if (is_surprising_plug) {
              paste0(row$asu_id, " @E", floor(age) + 0.5, " 😱")
            } else {
              paste0(row$asu_id, " @E", floor(age) + 0.5)
            }
            
            event <- data.frame(
              asu_id = as.character(row$asu_id),
              breeding_line = as.character(row$breeding_line),
              label = label_text,
              day = as.integer(format(harvest_date, "%d")),
              date = harvest_date,
              target_month = as.integer(format(harvest_date, "%m")),
              target_year = as.integer(format(harvest_date, "%Y")),
              color = color,
              status = as.character(row$plugging_status),
              status_category = status_category,
              event_priority = event_priority,
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
          breeding_line = character(0),
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
        result$breeding_line <- as.character(result$breeding_line)
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
            ms.breeding_line,
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
          breeding_line = character(0),
          asu_id = character(0),
          is_surprising_plug = logical(0),
          stringsAsFactors = FALSE
        ))
      }, finally = {
        if (!is.null(con)) dbDisconnect(con)
      })
    })

    open_calendar_details_for_mouse <- function(asu_id) {
      if (is.null(asu_id) || asu_id == "") {
        return()
      }

      con <- NULL
      tryCatch({
        con <- dbConnect(SQLite(), db_path)

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

        female_body_weight_history <- tryCatch({
          dbGetQuery(con, paste0("SELECT * FROM body_weight_history WHERE asu_id = '", asu_id, "' ORDER BY measurement_date DESC"))
        }, error = function(e) {
          data.frame()
        })

        female_plugging_history <- tryCatch({
          dbGetQuery(con, paste0("SELECT * FROM plugging_history WHERE female_id = '", asu_id, "' AND plugging_status != 'Deleted'"))
        }, error = function(e) {
          data.frame()
        })

        if (nrow(plugging) == 0) {
          showNotification(paste("No plugging details found for", asu_id), type = "warning")
          return()
        }

        selected_mouse_data(list(
          plugging = plugging[1, ],
          body_weight_history = build_event_weight_window(female_body_weight_history, plugging[1, , drop = FALSE], female_plugging_history),
          plugging_history = female_plugging_history
        ))
        view_mode("details")
      }, error = function(e) {
        showNotification(paste("Error fetching details:", e$message), type = "error")
      }, finally = {
        if (!is.null(con)) dbDisconnect(con)
      })
    }

    compute_event_strip_positions <- function(month_num, year_num, events) {
      if (is.null(events) || nrow(events) == 0) {
        return(data.frame())
      }

      first_day <- as.Date(paste(year_num, month_num, "01", sep = "-"))
      if (month_num == 12) {
        last_day <- as.Date(paste(year_num + 1, "01", "01", sep = "-")) - 1
      } else {
        last_day <- as.Date(paste(year_num, month_num + 1, "01", sep = "-")) - 1
      }

      num_days <- as.numeric(format(last_day, "%d"))
      first_day_of_week <- as.numeric(format(first_day, "%u"))
      start_pos <- first_day_of_week - 1

      calendar_data <- data.frame()
      for (day in 1:num_days) {
        grid_pos <- start_pos + day - 1
        week <- floor(grid_pos / 7) + 1
        day_of_week <- (grid_pos %% 7) + 1

        calendar_data <- rbind(calendar_data, data.frame(
          day = day,
          x = day_of_week,
          y = 7 - week,
          stringsAsFactors = FALSE
        ))
      }

      events_by_day <- events %>%
        dplyr::arrange(day, event_priority, status_category, breeding_line, asu_id, expected_age) %>%
        dplyr::group_by(day) %>%
        dplyr::mutate(
          event_count = dplyr::n(),
          strip_position = dplyr::row_number(),
          strip_height = 0.15,
          strip_spacing = 0.05,
          total_height = event_count * strip_height + (event_count - 1) * strip_spacing,
          y_offset = (total_height - strip_height) / 2 - (strip_position - 1) * (strip_height + strip_spacing)
        ) %>%
        dplyr::ungroup()

      events_by_day %>%
        dplyr::left_join(calendar_data, by = "day") %>%
        dplyr::mutate(
          strip_y = y + y_offset,
          strip_width = 0.9,
          strip_height_actual = strip_height
        )
    }

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
      
      # Use floor() + 0.5: group E14 and E14.5 as E14.5
      stages <- unique(floor(events$expected_age) + 0.5)
      stages <- stages[!is.na(stages)]
      stages <- sort(stages)
      
      # Format as E13.5, E14.5, etc.
      paste0("E", stages)
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
          # Use floor() + 0.5: both E14 and E14.5 treated as E14.5
          processed_ages <- paste0("E", floor(filtered$expected_age) + 0.5)
          stage_filter <- processed_ages %in% stages
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

    current_month_event_strips <- reactive({
      req(current_month(), current_year())
      compute_event_strip_positions(current_month(), current_year(), current_month_events())
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

    observeEvent(input$calendar_plot_click, {
      click_info <- input$calendar_plot_click
      req(click_info$x, click_info$y)

      event_strips <- current_month_event_strips()
      if (is.null(event_strips) || nrow(event_strips) == 0) {
        return()
      }

      matching_strips <- event_strips[
        abs(event_strips$x - click_info$x) <= (event_strips$strip_width / 2) &
          abs(event_strips$strip_y - click_info$y) <= (event_strips$strip_height_actual / 2),
        , drop = FALSE
      ]

      if (nrow(matching_strips) == 0) {
        return()
      }

      matching_strips$distance <- abs(matching_strips$x - click_info$x) + abs(matching_strips$strip_y - click_info$y)
      matching_strips <- matching_strips[order(matching_strips$distance, matching_strips$event_priority, matching_strips$asu_id), , drop = FALSE]

      open_calendar_details_for_mouse(matching_strips$asu_id[1])
    }, ignoreInit = TRUE)

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
      
      event_strips <- compute_event_strip_positions(month_num, year_num, events)
      
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
        p <- p +
          geom_tile(data = event_strips, 
                    aes(x = x, y = strip_y, fill = color, width = strip_width, height = strip_height_actual), 
                    color = "#1E3A5F", 
                    linewidth = 0.5, 
                    alpha = 0.85) +
          scale_fill_identity()

        p <- p +
          geom_text(data = event_strips, 
                    aes(label = label, x = x, y = strip_y), 
                    size = 4, color = "white", fontface = "bold")
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
          # Add emoji to mouse ID if it's a surprising plug
          mouse_display <- if (unique_confirmed$is_surprising_plug[i]) {
            paste0(unique_confirmed$asu_id[i], " 😱")
          } else {
            unique_confirmed$asu_id[i]
          }
          
          div(class = "legend-item",
            div(class = "color-indicator", 
                style = paste0("background-color: ", unique_confirmed$color[i])),
            div(class = "mouse-info",
              div(class = "mouse-id", 
                  mouse_display,
                  onclick = paste0('Shiny.setInputValue("', ns('mouse_id_clicked'), '", "', unique_confirmed$asu_id[i], '", {priority: "event"});'))
            )
          )
        })
        
        # Check if any confirmed events are surprising plugs
        has_surprising_plugs <- any(confirmed_events$is_surprising_plug)
        section_title <- if (has_surprising_plugs) {
          "🎯 CONFIRMED PLUGS (😱 = Surprising)"
        } else {
          "🎯 CONFIRMED PLUGS"
        }
        
        legend_grid[[length(legend_grid) + 1]] <- div(class = "legend-section confirmed",
          div(class = "legend-section-title", section_title),
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
      open_calendar_details_for_mouse(asu_id)
      
      # Reset the input so the same mouse ID can be clicked again
      session$sendInputMessage("mouse_id_clicked", NULL)
    })

    observeEvent(input$close_or_back_btn, {
      if (identical(view_mode(), "details")) {
        reset_calendar_modal_state()
      } else {
        removeModal()
      }
    }, ignoreInit = TRUE)

    observeEvent(input$calendar_modal_closed, {
      reset_calendar_modal_state()
    }, ignoreInit = TRUE)
    
    # Handle back to calendar button
    observeEvent(input$back_to_calendar, {
      reset_calendar_modal_state()
    })

    observeEvent(input$calendar_add_body_weight_clicked, {
      asu_id <- input$calendar_add_body_weight_clicked

      if (!is.null(asu_id) && asu_id != "") {
        show_body_weight_input(
          input,
          output,
          session,
          asu_id,
          back_input_id = session$ns("calendar_body_weight_back_clicked"),
          close_input_id = session$ns("calendar_body_weight_close_clicked")
        )
      }
    }, ignoreInit = TRUE)

    reopen_calendar_details_modal <- function(asu_id) {
      if (is.null(asu_id) || asu_id == "") {
        return()
      }

      removeModal()
      reopen_calendar_modal()
      open_calendar_details_for_mouse(asu_id)
    }

    build_calendar_current_prediction <- function(data) {
      if (is.null(data) || is.null(data$plugging)) {
        return(list(
          likelihood = "Unavailable",
          confidence = "Low",
          estimated_age_range = "Unknown",
          conclusion = "Prediction is unavailable for this event.",
          anchor = list(date = as.Date(NA), type = "Unknown Anchor"),
          fitted_anchor = list(offset_days = 0, fitted_curve = data.frame(), anchor_label = "Unknown"),
          fitted_curve = data.frame()
        ))
      }

      training_dataset <- tryCatch(build_plugging_prediction_dataset(db_path), error = function(e) data.frame())
      current_prediction_mode <- if (!is.null(shared_plugging_state) && !is.null(shared_plugging_state$prediction_breeding_line_mode)) {
        normalize_prediction_breeding_line_mode(shared_plugging_state$prediction_breeding_line_mode)
      } else {
        "feature"
      }
      tryCatch(
        predict_plugging_event_outcome(
          data$plugging,
          data$body_weight_history,
          training_dataset,
          breeding_line_mode = current_prediction_mode
        ),
        error = function(e) list(
          likelihood = "Unavailable",
          confidence = "Low",
          estimated_age_range = "Unknown",
          conclusion = "Prediction is unavailable for this event.",
          anchor = list(date = as.Date(NA), type = "Unknown Anchor"),
          fitted_anchor = list(offset_days = 0, fitted_curve = data.frame(), anchor_label = "Unknown"),
          fitted_curve = data.frame()
        )
      )
    }

    observeEvent(input$calendar_body_weight_back_clicked, {
      asu_id <- input$calendar_body_weight_back_clicked
      req(asu_id)

      reopen_calendar_details_modal(asu_id)
    }, ignoreInit = TRUE)

    observeEvent(input$calendar_body_weight_close_clicked, {
      asu_id <- input$calendar_body_weight_close_clicked
      req(asu_id)

      reopen_calendar_details_modal(asu_id)
    }, ignoreInit = TRUE)

    observeEvent(input$calendar_edit_expected_age_btn, {
      current_data <- selected_mouse_data()
      req(current_data, current_data$plugging)

      row <- current_data$plugging
      current_expected_age <- if (!is.null(row$expected_age_for_harvesting) && !is.na(row$expected_age_for_harvesting)) {
        row$expected_age_for_harvesting
      } else {
        ""
      }

      showModal(modalDialog(
        title = paste("Edit Expected Harvest Age (", row$female_id, ")"),
        size = "s",
        textInput(
          ns("calendar_expected_age_edit_input"),
          "Expected Age for Harvesting (Embryonic Days, e.g. 14)",
          value = current_expected_age,
          width = "100%"
        ),
        footer = tagList(
          actionButton(ns("cancel_calendar_expected_age_btn"), "Cancel", class = "btn btn-default"),
          actionButton(ns("save_calendar_expected_age_btn"), "Save", class = "btn-primary")
        )
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$cancel_calendar_expected_age_btn, {
      current_data <- selected_mouse_data()
      req(current_data, current_data$plugging)

      reopen_calendar_details_modal(current_data$plugging$female_id[1])
    }, ignoreInit = TRUE)

    observeEvent(input$save_calendar_expected_age_btn, {
      current_data <- selected_mouse_data()
      req(current_data, current_data$plugging)

      row <- current_data$plugging
      plugging_id <- row$id[1]
      female_id <- row$female_id[1]
      new_expected_age <- input$calendar_expected_age_edit_input

      con <- NULL
      tryCatch({
        con <- dbConnect(SQLite(), db_path)
        old_values <- dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))

        if (nrow(old_values) == 0) {
          showNotification("Plugging event not found", type = "error")
          return()
        }

        result <- dbExecute(
          con,
          "UPDATE plugging_history SET expected_age_for_harvesting = ?, updated_at = DATETIME('now') WHERE id = ?",
          params = list(new_expected_age, plugging_id)
        )

        if (result > 0) {
          log_audit_trail(
            "plugging_history",
            plugging_id,
            "UPDATE",
            old_values[1, , drop = FALSE],
            list(expected_age_for_harvesting = new_expected_age)
          )
          showNotification("Expected harvest age updated", type = "message")
          reopen_calendar_details_modal(female_id)
        } else {
          showNotification("Failed to update expected harvest age", type = "error")
        }
      }, error = function(e) {
        showNotification(paste("Error updating expected harvest age:", e$message), type = "error")
      }, finally = {
        if (!is.null(con)) dbDisconnect(con)
      })
    }, ignoreInit = TRUE)
    
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
                  plotOutput(ns("calendar_plot"), height = "600px", click = ns("calendar_plot_click"))
                )
              )
            ),
            div(class = "legend-container",
              uiOutput(ns("calendar_legend")),
              div(style = "margin-top: 16px; text-align: center;",
                downloadButton(paste0(id, "-export_ical_btn"), 
                              "📅 Export iCal", 
                              class = "export-btn"),
                div(style = "margin-top: 8px; font-size: 11px; color: #6c757d; font-style: italic;",
                    "💡 Tip: Click the events summary buttons above to filter events")
              )
            )
          )
        )
      } else {
        # Details view
        req(selected_mouse_data())
        data <- selected_mouse_data()
        row <- data$plugging
        female_body_weight_history <- data$body_weight_history
        female_plugging_history <- data$plugging_history
        current_prediction <- build_calendar_current_prediction(data)
        
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
            style = "font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; max-width: 860px; margin: 0 auto;",
            
            h4(paste("🐭 Plugging Details:", row$female_id), 
               style = "text-align: center; color: #1e3a5f; margin-bottom: 20px;"),
            div(
              style = "display: grid; grid-template-columns: minmax(250px, 1.15fr) minmax(410px, 2.85fr); gap: 16px; align-items: stretch;",
              div(
                style = "display: flex; flex-direction: column; height: 100%;",
                div(
                  style = "background: linear-gradient(135deg, #e8f5e8 0%, #d4edda 100%); border-radius: 8px; padding: 12px; border-left: 4px solid #28a745; margin-bottom: 12px;",
                  h5("👫 Breeding Pair", style = "margin: 0 0 10px 0; color: #2c3e50;"),
                  div(
                    div(
                      strong("Female: "), paste0(row$female_id, " (", ifelse(is.na(female_age), "Unknown age", paste0(female_age, " wks")), ")"),
                      br(),
                      "Line: ", ifelse(is.na(row$female_breeding_line), "Unknown", row$female_breeding_line),
                      br(),
                      "Genotype: ", ifelse(is.na(row$female_genotype), "Unknown", row$female_genotype)
                    ),
                    br(),
                    div(
                      strong("Male: "), paste0(row$male_id, " (", ifelse(is.na(male_age), "Unknown age", paste0(male_age, " wks")), ")"),
                      br(),
                      "Line: ", ifelse(is.na(row$male_breeding_line), "Unknown", row$male_breeding_line),
                      br(),
                      "Genotype: ", ifelse(is.na(row$male_genotype), "Unknown", row$male_genotype)
                    )
                  )
                ),
                div(
                  style = "background: rgba(135, 206, 235, 0.1); border-radius: 8px; padding: 12px; border-left: 4px solid #87CEEB; margin-bottom: 12px;",
                  h5("⏰ Timeline", style = "margin: 0 0 8px 0; color: #2c3e50;"),
                  div(
                    "Pairing Start: ", strong(ifelse(is.na(row$pairing_start_date) || row$pairing_start_date == "", "Unknown", row$pairing_start_date)),
                    br(),
                    "Pairing End: ", strong(ifelse(is.na(row$pairing_end_date) || row$pairing_end_date == "", "Unknown", row$pairing_end_date)),
                    br(),
                    "Plug Observed: ", strong(ifelse(is.na(row$plug_observed_date) || row$plug_observed_date == "", "Unknown", row$plug_observed_date))
                  )
                ),
                div(
                  style = "background: rgba(255, 193, 7, 0.1); border-radius: 8px; padding: 12px; border-left: 4px solid #ffc107; flex: 1;",
                  div(
                    style = "display: flex; justify-content: space-between; align-items: center; gap: 8px; margin-bottom: 8px;",
                    h5("📊 Status", style = "margin: 0; color: #2c3e50;"),
                    actionButton(
                      ns("calendar_edit_expected_age_btn"),
                      "Edit Harvest",
                      class = "btn btn-default btn-xs",
                      style = "padding: 2px 8px; line-height: 1.2;"
                    )
                  ),
                  div(
                    "Current Status: ", strong(row$plugging_status),
                    br(),
                    "Expected Harvest Age: ", strong(ifelse(is.na(row$expected_age_for_harvesting) || row$expected_age_for_harvesting == "", "Not specified", row$expected_age_for_harvesting)),
                    if (!is.na(row$notes) && row$notes != "") {
                      tagList(br(), "Notes: ", span(style = "font-style: italic;", row$notes))
                    }
                  )
                ),
                tagList({
                  summary_lines <- build_prediction_summary_lines(current_prediction)
                  metadata_lines <- build_prediction_metadata_lines(current_prediction)

                  if (length(c(summary_lines, metadata_lines)) > 0) {
                    div(
                      style = "background: rgba(245, 158, 11, 0.1); border-radius: 8px; padding: 12px; border-left: 4px solid #f59e0b; margin-top: 12px;",
                      h5("🧪 Prediction Details", style = "margin: 0 0 8px 0; color: #2c3e50;"),
                      if (length(summary_lines) > 0) {
                        div(
                          style = "color: #92400e; font-size: 0.92em; line-height: 1.4;",
                          tagList(lapply(summary_lines, function(line_text) div(line_text)))
                        )
                      },
                      if (length(metadata_lines) > 0) {
                        div(
                          style = "margin-top: 8px; color: #a16207; font-size: 0.82em; line-height: 1.35;",
                          tagList(lapply(metadata_lines, function(line_text) div(line_text)))
                        )
                      }
                    )
                  }
                })
              ),
              if (nrow(female_body_weight_history) > 0) {
                div(
                  style = "border-radius: 8px; padding: 12px; display: flex; flex-direction: column; height: 100%;",
                  div(
                    style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 12px;",
                    h5("📈 Female Body Weight Trend", style = "margin: 0; color: #2c3e50;"),
                    actionButton(
                      paste0(ns("add_body_weight_from_calendar_btn")),
                      label = "Add Body Weight",
                      class = "btn-success btn-sm",
                      onclick = paste0("Shiny.setInputValue('", ns("calendar_add_body_weight_clicked"), "', '", row$female_id, "', {priority: 'event'});")
                    )
                  ),
                  div(
                    style = "background: #fff8e1; border: 1px solid #fde68a; border-radius: 8px; padding: 12px; margin-bottom: 12px;",
                    h5("Pregnancy Fit", style = "margin: 0 0 8px 0; color: #b45309;"),
                    div(
                      style = "color: #92400e; font-weight: 600;",
                      build_prediction_plot_label(current_prediction)
                    ),
                    {
                      timing_lines <- build_prediction_timing_lines(current_prediction)
                      tagList(
                        if (length(timing_lines) > 0) {
                          div(
                            style = "margin-top: 8px; color: #92400e; font-size: 0.92em; line-height: 1.4;",
                            tagList(lapply(timing_lines, function(line_text) {
                              div(
                                style = "margin-top: 6px; font-weight: 700; color: #7c2d12; background: rgba(245, 158, 11, 0.18); border-left: 3px solid #f59e0b; padding: 4px 8px; border-radius: 6px;",
                                line_text
                              )
                            }))
                          )
                        }
                      )
                    }
                  ),
                  div(
                    id = paste0(ns("calendar_body_weight_preview_plot_container")),
                    style = "height: 400px; flex: 1;",
                    plotlyOutput(paste0(ns("calendar_body_weight_preview_plot_"), row$female_id))
                  )
                )
              } else {
                div(
                  style = "border-radius: 8px; padding: 12px; display: flex; align-items: center; justify-content: center; height: 100%; min-height: 400px;",
                  div(
                    style = "text-align: center; color: #6c757d;",
                    h5("📈 No Body Weight Data", style = "margin-bottom: 10px;"),
                    p("No body weight records found for this mouse."),
                    div(
                      style = "background: #fff8e1; border: 1px solid #fde68a; border-radius: 8px; padding: 12px; margin: 12px 0; text-align: left;",
                      div(
                        style = "color: #92400e; font-weight: 600;",
                        build_prediction_plot_label(current_prediction)
                      ),
                      {
                        timing_lines <- build_prediction_timing_lines(current_prediction)
                        tagList(
                          if (length(timing_lines) > 0) {
                            div(
                              style = "margin-top: 8px; color: #92400e; font-size: 0.92em; line-height: 1.4;",
                              tagList(lapply(timing_lines, function(line_text) {
                                div(
                                  style = "margin-top: 6px; font-weight: 700; color: #7c2d12; background: rgba(245, 158, 11, 0.18); border-left: 3px solid #f59e0b; padding: 4px 8px; border-radius: 6px;",
                                  line_text
                                )
                              }))
                            )
                          }
                        )
                      }
                    ),
                    actionButton(
                      paste0(ns("add_body_weight_from_calendar_btn")),
                      label = "Add First Record",
                      class = "btn-success btn-sm",
                      onclick = paste0("Shiny.setInputValue('", ns("calendar_add_body_weight_clicked"), "', '", row$female_id, "', {priority: 'event'});")
                    )
                  )
                )
              }
            )
          )
        )
      }
    })
    
    # Render body weight chart when in details view
    observe({
      if (view_mode() == "details") {
        req(selected_mouse_data())
        data <- selected_mouse_data()
        row <- data$plugging
        female_body_weight_history <- data$body_weight_history
        female_plugging_history <- data$plugging_history
        current_prediction <- build_calendar_current_prediction(data)
        
        if (nrow(female_body_weight_history) > 0) {
          # Use existing render function logic but with calendar-specific output ID
          output[[paste0("calendar_body_weight_preview_plot_", row$female_id)]] <- renderPlotly({
            # Create the base plotly chart
            weight_data <- female_body_weight_history
            
            # Robust date conversion - handle various date formats
            weight_data$measurement_date <- tryCatch({
              if (is.character(weight_data$measurement_date)) {
                as.Date(weight_data$measurement_date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d"))
              } else {
                as.Date(weight_data$measurement_date)
              }
            }, error = function(e) {
              # If date conversion fails, use current date as fallback
              rep(Sys.Date(), nrow(weight_data))
            })
            
            p <- plot_ly(
              data = weight_data,
              x = ~measurement_date,
              y = ~weight_grams,
              type = "scatter",
              mode = "lines",
              line = list(color = "#2196f3", width = 2),
              name = "Body Weight",
              showlegend = TRUE,
              hovertemplate = paste(
                "<b>Date:</b> %{x}<br>",
                "<b>Weight:</b> %{y} grams<br>",
                "<extra></extra>"
              )
            )

            if (!is.na(current_prediction$anchor$date) && nrow(current_prediction$fitted_curve) > 0) {
              fitted_curve_data <- current_prediction$fitted_curve[current_prediction$fitted_curve$day_since_anchor >= 0, , drop = FALSE]
              fitted_curve_data$measurement_date <- as.POSIXct(current_prediction$anchor$date) + fitted_curve_data$day_since_anchor * 86400

              p <- add_lines(
                p,
                data = fitted_curve_data,
                x = ~measurement_date,
                y = ~predicted_weight,
                name = "Pregnancy Date Curve",
                line = list(color = "#f59e0b", width = 2, dash = "dash"),
                hovertemplate = "<b>Date:</b> %{x}<br><b>Curve:</b> %{y:.2f} grams<br><extra></extra>"
              )
            }

            y_range <- build_body_weight_plot_y_range(
              actual_weights = weight_data$weight_grams,
              fitted_weights = if (exists("fitted_curve_data")) fitted_curve_data$predicted_weight else numeric(0)
            )
            
            # Initialize shapes list for layout
            shapes_list <- list()
            
            # Add plugging events if they exist
            if (nrow(female_plugging_history) > 0) {
              # Process each plugging record
              for (i in seq_len(nrow(female_plugging_history))) {
                plug_row <- female_plugging_history[i, ]
                
                # Add gray shaded area for pairing period
                if (!is.na(plug_row$pairing_start_date) && plug_row$pairing_start_date != "") {
                  start_date <- tryCatch({
                    if (is.character(plug_row$pairing_start_date)) {
                      as.Date(plug_row$pairing_start_date, 
                             tryFormats = c("%Y-%m-%d", "%m/%d/%Y", 
                                           "%d/%m/%Y", "%Y/%m/%d"))
                    } else {
                      as.Date(plug_row$pairing_start_date)
                    }
                  }, error = function(e) {
                    Sys.Date()  # Fallback to today
                  })
                  
                  end_date <- if (!is.na(plug_row$pairing_end_date) && plug_row$pairing_end_date != "") {
                    tryCatch({
                      if (is.character(plug_row$pairing_end_date)) {
                        as.Date(plug_row$pairing_end_date, 
                             tryFormats = c("%Y-%m-%d", "%m/%d/%Y", 
                                           "%d/%m/%Y", "%Y/%m/%d"))
                      } else {
                        as.Date(plug_row$pairing_end_date)
                      }
                    }, error = function(e) {
                      max(weight_data$measurement_date, Sys.Date())
                    })
                  } else {
                    max(weight_data$measurement_date, Sys.Date())
                  }
                  
                  # Add rectangle shape for pairing period
                  shapes_list[[length(shapes_list) + 1]] <- list(
                    type = "rect",
                    x0 = start_date, x1 = end_date,
                    y0 = 0, y1 = 1, yref = "paper",
                    fillcolor = "rgba(128, 128, 128, 0.3)",
                    line = list(color = "rgba(0,0,0,0)", width = 0),
                    layer = "below"
                  )
                }
                
                # Add plug observed markers
                if (!is.na(plug_row$plug_observed_date) && plug_row$plug_observed_date != "") {
                  plug_date <- tryCatch({
                    if (is.character(plug_row$plug_observed_date)) {
                      as.Date(plug_row$plug_observed_date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d"))
                    } else {
                      as.Date(plug_row$plug_observed_date)
                    }
                  }, error = function(e) {
                    NULL  # Skip this marker if date parsing fails
                  })
                  
                  if (!is.null(plug_date)) {
                    # Determine color based on plugging status
                    plug_color <- if (plug_row$plugging_status == "Collected") {
                      "#388e3c"  # Success - Green
                    } else if (plug_row$plugging_status %in% c("Empty", "Not Pregnant (Confirmed)", "Not Pregnant")) {
                      "#d32f2f"  # Failed - Red
                    } else {
                      "#ff9800"  # Pending - Orange
                    }
                    
                    # Add line shape for vertical line
                    shapes_list[[length(shapes_list) + 1]] <- list(
                      type = "line",
                      x0 = plug_date, x1 = plug_date,
                      y0 = 0, y1 = 1, yref = "paper",
                      line = list(color = plug_color, width = 2)
                    )
                  }
                }
              }
            }
            
            # Calculate x-axis range
            all_dates <- weight_data$measurement_date
            if (length(all_dates) > 0) {
              date_range <- range(all_dates, na.rm = TRUE)
              date_diff <- as.numeric(diff(date_range))
              
              # For single data point or very short periods, use minimum 7-day window
              if (date_diff <= 3) {
                center_date <- mean(date_range)
                x_range <- c(center_date - 3.5, center_date + 3.5)
              } else {
                date_padding <- date_diff * 0.15
                x_range <- c(date_range[1] - date_padding, 
                             date_range[2] + date_padding)
              }
            } else {
              x_range <- NULL
            }
            
            # Apply layout with shapes
            p <- p %>% layout(
              xaxis = list(
                title = "",
                showgrid = TRUE,
                gridcolor = "#e0e0e0",
                tickformat = "%d-%b",
                range = x_range
              ),
              yaxis = list(
                title = "Weight (grams)",
                showgrid = TRUE,
                gridcolor = "#e0e0e0",
                range = y_range
              ),
              shapes = shapes_list,
              hovermode = "closest",
              plot_bgcolor = "rgba(0,0,0,0)",
              paper_bgcolor = "rgba(0,0,0,0)",
              margin = list(t = 20, b = 30, l = 50, r = 20),
              showlegend = TRUE,
              legend = list(orientation = "h", y = -0.2)
            )
            
            return(p)
          })
        }
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
        actionButton(paste0(id, "-close_or_back_btn"), "Close", class = "btn btn-default")
      ),
      tags$script(HTML("
        $(document).ready(function() {
          setTimeout(function() {
            var $modal = $('.modal').last();
            $modal.off('hidden.bs.modal.calendarReset');
            $modal.on('hidden.bs.modal.calendarReset', function() {
              Shiny.setInputValue('", paste0(id, "-calendar_modal_closed"), "', Date.now(), {priority: 'event'});
            });
            $modal.find('.modal-dialog').css({
              'max-width': '70%',
              'width': '70%'
            });
          }, 50);
        });
      "))
    )
  ))
  plugging_calendar_modal_server(id, db_path, shared_plugging_state)
}
