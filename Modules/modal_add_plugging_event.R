# Add Plugging Event Modal Module
# UI and server logic for the Add Plugging Event modal, extracted from tab_plugging.R

add_plugging_modal_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("show_plugging_modal_btn"), "➕ Add Plugging Event", 
                class = "btn-primary", 
                style = "padding: 8px 16px; font-size: 14px; border-radius: 6px; font-weight: 500;")
  )
}

add_plugging_modal_server <- function(id, get_live_mice, get_mouse_info, validate_mice_active_status, db_connect, db_disconnect, log_audit_trail, auto_update_plugging_status_to_unknown, plugging_state, is_system_locked, global_refresh_trigger, force_refresh_all_mice_table = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$show_plugging_modal_btn, {
      mice_data <- get_live_mice()
      
      # Check if validation failed (NULL returned)
      if (is.null(mice_data)) {
        # Modal will not open - validation warnings were already shown by validate_selection_for_plugging()
        return()
      }
      
      male_choices <- if(nrow(mice_data$males) > 0) {
        setNames(
          mice_data$males$asu_id,
          paste(mice_data$males$asu_id, "-", mice_data$males$breeding_line)
        )
      } else {
        c("No live males available" = "")
      }
      female_choices <- if(nrow(mice_data$females) > 0) {
        setNames(mice_data$females$asu_id, paste(mice_data$females$asu_id, "-", mice_data$females$breeding_line))
      } else {
        c("No live females available" = "")
      }
      
      # Auto-preselect mice if available
      selected_male <- if(nrow(mice_data$males) > 0) mice_data$males$asu_id[1] else ""
      selected_female1 <- if(nrow(mice_data$females) > 0) mice_data$females$asu_id[1] else ""
      selected_female2 <- if(nrow(mice_data$females) >= 2) mice_data$females$asu_id[2] else ""
      selected_female3 <- if(nrow(mice_data$females) >= 3) mice_data$females$asu_id[3] else ""
      
      showModal(modalDialog(
        title = "Add Plugging Event",
        size = "xl",
        tagList(
          tags$script(HTML(sprintf(
            "setTimeout(function() {
              var input = document.getElementById('%s');
              if (!input) {
                return;
              }
              var dialog = input.closest('.modal-dialog');
              if (!dialog) {
                return;
              }
              dialog.style.width = '72vw';
              dialog.style.maxWidth = '1350px';
              var body = dialog.querySelector('.modal-body');
              if (body) {
                body.style.maxHeight = '80vh';
                body.style.overflowY = 'auto';
              }
            }, 0);",
            ns("plugging_male")
          ))),
          fluidRow(
            column(3, selectInput(ns("plugging_male"), "Male (ASU ID)", choices = male_choices, selected = selected_male)),
            column(3, selectInput(ns("plugging_female1"), "Female 1 (ASU ID)", choices = female_choices, selected = selected_female1)),
            column(3, selectInput(ns("plugging_female2"), "Female 2 (ASU ID)", choices = c("Optional" = "", female_choices), selected = selected_female2)),
            column(3, selectInput(ns("plugging_female3"), "Female 3 (ASU ID)", choices = c("Optional" = "", female_choices), selected = selected_female3))
          ),
          fluidRow(
            column(3, uiOutput(ns("plugging_male_info_panel"))),
            column(3, uiOutput(ns("plugging_female1_info_panel"))),
            column(3, uiOutput(ns("plugging_female2_info_panel"))),
            column(3, uiOutput(ns("plugging_female3_info_panel")))
          ),
          fluidRow(
            column(12, uiOutput(ns("active_plugging_warning_text")))
          ),
          fluidRow(
            column(3, dateInput(ns("pairing_start_date"), "Start", value = Sys.Date(), width = "100%")),
            column(3, dateInput(ns("pairing_end_date"), "End", value = Sys.Date(), width = "100%")),
            column(3, selectInput(ns("plugging_status"), "Status", choices = PLUGGING_STATUSES, selected = "Ongoing", width = "100%")),
            column(3, textInput(ns("plugging_cage"), "Cage", placeholder = "Optional", width = "100%"))
          ),
          textAreaInput(ns("plugging_notes"), "Notes", "", rows = 1, placeholder = "Optional notes")
        ),
        footer = tagList(
          div(
            style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
            modalButton("Cancel"),
            uiOutput(ns("add_plugging_btn_ui"))
          )
        )
      ))
    })

    # Define active plugging statuses for each gender
    active_plugging_statuses_male <- c("Ongoing")
    active_plugging_statuses_female <- c("Ongoing", "Plugged", "Plug Confirmed", "Not Observed (Waiting for confirmation)", "Surprising Plug!!")

    active_plugging_explanation <- tagList(
      br(),
      tags$span(
        style = "display: block; margin-top: 6px; font-size: 11px;",
        "Active plugging count rules: males count Ongoing only; females count Ongoing, Plugged, Plug Confirmed, Not Observed (Waiting for confirmation), and Surprising Plug!!."
      )
    )

    is_selected_optional_female <- function(female_id) {
      !is.null(female_id) && female_id != "" && female_id != "Optional"
    }

    get_female_warning_state <- function(female_id) {
      info <- get_mouse_info(female_id)
      has_warnings <- FALSE

      if (!is.null(info) && is_selected_optional_female(female_id)) {
        plugging_check <- validate_mice_active_status(
          mice_id = female_id,
          table_name = "plugging_history",
          status_column = "plugging_status",
          mouse_role = "female"
        )
        stock_check <- validate_mice_active_status(
          mice_id = female_id,
          table_name = "mice_stock",
          status_column = "status"
        )
        active_plugging_count <- sum(plugging_check$all_statuses %in% active_plugging_statuses_female)

        has_warnings <- (active_plugging_count >= 1) ||
          (!any(stock_check$all_statuses == "Alive")) ||
          (info$age_weeks < 7)
      }

      list(info = info, has_warnings = has_warnings)
    }

    get_male_warning_state <- function(male_id) {
      info <- get_mouse_info(male_id)

      if (is.null(info)) {
        return(list(info = NULL, active_plugging_count = 0, has_blocking_warning = FALSE, has_caution = FALSE))
      }

      plugging_check <- validate_mice_active_status(
        mice_id = male_id,
        table_name = "plugging_history",
        status_column = "plugging_status",
        mouse_role = "male"
      )
      stock_check <- validate_mice_active_status(
        mice_id = male_id,
        table_name = "mice_stock",
        status_column = "status"
      )
      active_plugging_count <- sum(plugging_check$all_statuses %in% active_plugging_statuses_male)

      list(
        info = info,
        stock_check = stock_check,
        active_plugging_count = active_plugging_count,
        has_blocking_warning = (!any(stock_check$all_statuses == "Alive")) || (info$age_weeks < 7) || (active_plugging_count > 3),
        has_caution = active_plugging_count > 0 && active_plugging_count <= 3
      )
    }

    render_female_info_panel <- function(female_id, label) {
      female_info <- get_mouse_info(female_id)
      if (is.null(female_info) || !is_selected_optional_female(female_id)) return(NULL)

      plugging_check <- validate_mice_active_status(
        mice_id = female_id,
        table_name = "plugging_history",
        status_column = "plugging_status",
        mouse_role = "female"
      )

      stock_check <- validate_mice_active_status(
        mice_id = female_id,
        table_name = "mice_stock",
        status_column = "status"
      )

      active_plugging_count <- sum(plugging_check$all_statuses %in% active_plugging_statuses_female)
      warning_messages <- list()

      if (active_plugging_count >= 1) {
        warning_messages[["plugging"]] <- div(
          style = "color: #d32f2f; background-color: #ffebee; padding: 8px; border-radius: 4px; margin: 8px 0; font-size: 12px;",
          tags$strong("⚠️ WARNING: "),
          paste("This female has", active_plugging_count, "active plugging records."),
          active_plugging_explanation
        )
      }

      if (!any(stock_check$all_statuses == "Alive")) {
        warning_messages[["status"]] <- div(
          style = "color: #d32f2f; background-color: #ffebee; padding: 8px; border-radius: 4px; margin: 8px 0; font-size: 12px;",
          tags$strong("⚠️ WARNING: "),
          paste("This female is not alive. Current status:", stock_check$status_summary)
        )
      }

      if (female_info$age_weeks < 7) {
        warning_messages[["age"]] <- div(
          style = "color: #ff9800; background-color: #fff3e0; padding: 8px; border-radius: 4px; margin: 8px 0; font-size: 12px;",
          tags$strong("⚠️ AGE WARNING: "),
          paste("This female is only", female_info$age_weeks, "weeks old (minimum recommended: 7 weeks)")
        )
      }

      wellPanel(
        tagList(
          tags$b(paste0("Selected ", label, " Info:")), br(),
          paste("ASU ID:", female_info$asu_id), br(),
          paste("Age (weeks):", female_info$age_weeks), br(),
          paste("Breeding Line:", female_info$breeding_line), br(),
          paste("Genotype:", female_info$genotype),
          if (length(warning_messages) > 0) warning_messages
        )
      )
    }

    # Reactive expression to track warnings across all mice
    warnings_exist <- reactive({
      male_state <- get_male_warning_state(input$plugging_male)
      
      female1_state <- get_female_warning_state(input$plugging_female1)
      female2_state <- get_female_warning_state(input$plugging_female2)
      female3_state <- get_female_warning_state(input$plugging_female3)

      return(male_state$has_blocking_warning || female1_state$has_warnings || female2_state$has_warnings || female3_state$has_warnings)
    })

    # Mouse info panels with enhanced validation
    output$plugging_male_info_panel <- renderUI({
      male_state <- get_male_warning_state(input$plugging_male)
      info <- male_state$info
      if (is.null(info)) return(NULL)
      stock_check <- male_state$stock_check
      active_plugging_count <- male_state$active_plugging_count
      
      # Create warning messages
      warning_messages <- list()
      
      if (active_plugging_count > 3) {
        warning_messages[["plugging"]] <- div(
          style = "color: #d32f2f; background-color: #ffebee; padding: 8px; border-radius: 4px; margin: 8px 0; font-size: 12px;",
          tags$strong("⚠️ WARNING: "),
          paste("This male has", active_plugging_count, "active plugging records and cannot be selected."),
          active_plugging_explanation
        )
      } else if (active_plugging_count > 0) {
        warning_messages[["plugging"]] <- div(
          style = "color: #d32f2f; background-color: #ffebee; padding: 8px; border-radius: 4px; margin: 8px 0; font-size: 12px;",
          tags$strong("⚠️ WARNING: "),
          paste("This male already has", active_plugging_count, "active plugging records but is still allowed because the limit is 3 active records."),
          active_plugging_explanation
        )
      }
      
      # Stock status warnings - show warning if not alive
      if (!any(stock_check$all_statuses == "Alive")) {
        warning_messages[["status"]] <- div(
          style = "color: #d32f2f; background-color: #ffebee; padding: 8px; border-radius: 4px; margin: 8px 0; font-size: 12px;",
          tags$strong("⚠️ WARNING: "),
          paste("This male is not alive. Current status:", stock_check$status_summary)
        )
      }
      
      # Age warnings - show warning if younger than 7 weeks
      if (info$age_weeks < 7) {
        warning_messages[["age"]] <- div(
          style = "color: #ff9800; background-color: #fff3e0; padding: 8px; border-radius: 4px; margin: 8px 0; font-size: 12px;",
          tags$strong("⚠️ AGE WARNING: "),
          paste("This male is only", info$age_weeks, "weeks old (minimum recommended: 7 weeks)")
        )
      }
      
      wellPanel(
        tagList(
          tags$b("Selected Male Info:"), br(),
          paste("ASU ID:", info$asu_id), br(),
          paste("Age (weeks):", info$age_weeks), br(),
          paste("Breeding Line:", info$breeding_line), br(),
          paste("Genotype:", info$genotype),
          if (length(warning_messages) > 0) warning_messages
        )
      )
    })
    
    output$plugging_female1_info_panel <- renderUI({
      render_female_info_panel(input$plugging_female1, "Female 1")
    })

    output$plugging_female2_info_panel <- renderUI({
      render_female_info_panel(input$plugging_female2, "Female 2")
    })

    output$plugging_female3_info_panel <- renderUI({
      render_female_info_panel(input$plugging_female3, "Female 3")
    })

    # Render the active plugging explanation text and emphasize it when warnings exist
    output$active_plugging_warning_text <- renderUI({
      warnings_value <- warnings_exist()

      div(
        style = if (warnings_value) {
          "color: #d32f2f; font-size: 11px; margin-bottom: 15px; text-align: left; background-color: #ffebee; padding: 8px 10px; border-radius: 4px;"
        } else {
          "color: #6c757d; font-size: 11px; margin-bottom: 15px; text-align: left; background-color: #f8f9fa; padding: 8px 10px; border-radius: 4px;"
        },
        tagList(
          tags$strong("Active plugging records include:"),
          br(),
          "For Males: Ongoing",
          br(),
          "For Females: Ongoing, Plugged, Plug Confirmed, Not Observed (Waiting for confirmation), and Surprising Plug!!"
        )
      )
    })

    # Render the add plugging button with warning-based state
    output$add_plugging_btn_ui <- renderUI({
      if (warnings_exist()) {
        actionButton(ns("add_plugging_btn"), "Add Plugging Event", 
                    class = "btn-primary", disabled = TRUE,
                    style = "margin-left: 10px;",
                    title = "Button disabled due to warnings")
      } else {
        actionButton(ns("add_plugging_btn"), "Add Plugging Event", 
                    class = "btn-primary",
                    style = "margin-left: 10px;")
      }
    })

    # Internal refresh function that can refresh both tables
    refresh_all_tables <- function() {
      # Refresh plugging tab
      if (!is.null(plugging_state)) {
        plugging_state$reload <- Sys.time()
      }
      
      # Refresh All Mice tab via global trigger
      if (!is.null(global_refresh_trigger)) {
        global_refresh_trigger(Sys.time())
      }
      
      # Call custom refresh function if provided
      if (!is.null(force_refresh_all_mice_table) && is.function(force_refresh_all_mice_table)) {
        force_refresh_all_mice_table()
      }
      
      # Force immediate database refresh by re-querying
      tryCatch({
        con <- db_connect()
        # Force a small database operation to ensure changes are committed
        DBI::dbGetQuery(con, "SELECT COUNT(*) FROM plugging_history")
        db_disconnect(con)
      }, error = function(e) {
        # Ignore errors in refresh
      })
    }

    # Add plugging event
    observeEvent(input$add_plugging_btn, {
      # Validation
      if (is.null(input$plugging_male) || input$plugging_male == "") {
        showNotification("Please select a male", type = "error")
        return()
      }
      
      if (is.null(input$plugging_female1) || input$plugging_female1 == "") {
        showNotification("Please select at least one female", type = "error")
        return()
      }
      
      if (input$plugging_male == input$plugging_female1) {
        showNotification("Male and female 1 cannot be the same mouse", type = "error")
        return()
      }
      
      # Check if optional females are selected and validate
      has_female2 <- !is.null(input$plugging_female2) && input$plugging_female2 != "" && input$plugging_female2 != "Optional"
      has_female3 <- !is.null(input$plugging_female3) && input$plugging_female3 != "" && input$plugging_female3 != "Optional"
      
      if (has_female2) {
        if (input$plugging_male == input$plugging_female2) {
          showNotification("Male and female 2 cannot be the same mouse", type = "error")
          return()
        }
        
        if (input$plugging_female1 == input$plugging_female2) {
          showNotification("Female 1 and female 2 cannot be the same mouse", type = "error")
          return()
        }
      }

      if (has_female3) {
        if (input$plugging_male == input$plugging_female3) {
          showNotification("Male and female 3 cannot be the same mouse", type = "error")
          return()
        }

        if (input$plugging_female1 == input$plugging_female3) {
          showNotification("Female 1 and female 3 cannot be the same mouse", type = "error")
          return()
        }

        if (has_female2 && input$plugging_female2 == input$plugging_female3) {
          showNotification("Female 2 and female 3 cannot be the same mouse", type = "error")
          return()
        }
      }
      
      con <- db_connect()
      tryCatch({
        # Create list of females to process
        females_to_process <- c(input$plugging_female1)
        if (has_female2) {
          females_to_process <- c(females_to_process, input$plugging_female2)
        }
        if (has_female3) {
          females_to_process <- c(females_to_process, input$plugging_female3)
        }
        
        # Insert plugging events for each female
        for (female_id in females_to_process) {
          DBI::dbExecute(con, 
            "INSERT INTO plugging_history (male_id, female_id, cage_id, pairing_start_date, 
             pairing_end_date, plugging_status, notes) VALUES (?, ?, ?, ?, ?, ?, ?)",
            params = list(
              input$plugging_male,
              female_id,
              input$plugging_cage,
              as.character(input$pairing_start_date),
              as.character(input$pairing_end_date),
              input$plugging_status,
              input$plugging_notes
            )
          )
          
          # Get last inserted id
          new_id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid()")[[1]]
          
          # Log to audit trail
          log_audit_trail(
            "plugging_history",
            new_id,
            "INSERT",
            NULL,
            list(
              male_id = input$plugging_male,
              female_id = female_id,
              cage_id = input$plugging_cage,
              pairing_start_date = as.character(input$pairing_start_date),
              pairing_end_date = as.character(input$pairing_end_date),
              plugging_status = input$plugging_status,
              notes = input$plugging_notes
            )
          )
        }
        
        # Show success message
        if (length(females_to_process) > 1) {
          showNotification(paste("Plugging events added for male", input$plugging_male, "with", length(females_to_process), "females!"), type = "message")
        } else {
          showNotification("Plugging event added!", type = "message")
        }
        
        removeModal()
        
        # Small delay to ensure database changes are committed
        Sys.sleep(1)
        
        # Run auto-update function
        auto_update_plugging_status_to_unknown()
        
        # Refresh all tables
        refresh_all_tables()
        
      }, error = function(e) {
        showNotification(paste("Error adding plugging event:", e$message), type = "error")
      }, finally = {
        db_disconnect(con)
      })
    })
  })
} 