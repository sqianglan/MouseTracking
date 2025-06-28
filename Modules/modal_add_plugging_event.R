# Add Plugging Event Modal Module
# UI and server logic for the Add Plugging Event modal, extracted from tab_plugging.R

add_plugging_modal_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("show_plugging_modal_btn"), "Add Plugging Event", class = "btn-primary", style = "margin-right: 10px;")
  )
}

add_plugging_modal_server <- function(id, get_live_mice, get_mouse_info, validate_mice_active_status, db_connect, db_disconnect, log_audit_trail, auto_update_plugging_status_to_unknown, plugging_state, is_system_locked, global_refresh_trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$show_plugging_modal_btn, {
      mice_data <- get_live_mice()
      male_choices <- if(nrow(mice_data$males) > 0) {
        setNames(mice_data$males$asu_id, paste(mice_data$males$asu_id, "-", mice_data$males$breeding_line))
      } else {
        c("No live males available" = "")
      }
      female_choices <- if(nrow(mice_data$females) > 0) {
        setNames(mice_data$females$asu_id, paste(mice_data$females$asu_id, "-", mice_data$females$breeding_line))
      } else {
        c("No live females available" = "")
      }
      showModal(modalDialog(
        title = "Add Plugging Event",
        size = "xl",
        tagList(
          fluidRow(
            column(4, selectInput(ns("plugging_male"), "Male (ASU ID)", choices = male_choices)),
            column(4, selectInput(ns("plugging_female1"), "Female 1 (ASU ID)", choices = female_choices)),
            column(4, selectInput(ns("plugging_female2"), "Female 2 (ASU ID)", choices = c("Optional" = "", female_choices)))
          ),
          fluidRow(
            column(4, uiOutput(ns("plugging_male_info_panel"))),
            column(4, uiOutput(ns("plugging_female1_info_panel"))),
            column(4, uiOutput(ns("plugging_female2_info_panel")))
          ),
          fluidRow(
            column(12, 
              div(
                style = "color: #d32f2f; font-size: 11px; margin-bottom: 15px; text-align: left;",
                "*Active plugging records include: Ongoing, Plugged and Not Observed (Waiting for confirmation)"
              )
            )
          ),
          fluidRow(
            column(6, dateInput(ns("pairing_start_date"), "Pairing Start Date", value = Sys.Date())),
            column(6, dateInput(ns("pairing_end_date"), "Pairing End Date", value = Sys.Date()))
          ),
          fluidRow(
            column(6, selectInput(ns("plugging_status"), "Plugging Status", choices = PLUGGING_STATUSES, selected = "Ongoing")),
            column(6, textInput(ns("plugging_cage"), "Cage ID"))
          ),
          textAreaInput(ns("plugging_notes"), "Notes", "", rows = 2)
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

    # Reactive expression to track warnings across all mice
    warnings_exist <- reactive({
      # Check male warnings
      male_info <- get_mouse_info(input$plugging_male)
      male_has_warnings <- FALSE
      if (!is.null(male_info)) {
        plugging_check <- validate_mice_active_status(
          mice_id = input$plugging_male,
          table_name = "plugging_history", 
          status_column = "plugging_status",
          mouse_role = "male"
        )
        stock_check <- validate_mice_active_status(
          mice_id = input$plugging_male,
          table_name = "mice_stock",
          status_column = "status"
        )
        active_plugging_statuses <- c("Ongoing", "Plugged", "Not Observed (Waiting for confirmation)")
        active_plugging_count <- sum(plugging_check$all_statuses %in% active_plugging_statuses)
        
        male_has_warnings <- (active_plugging_count > 2) || 
                           (!any(stock_check$all_statuses == "Alive")) || 
                           (male_info$age_weeks < 7)
      }
      
      # Check female1 warnings
      female1_info <- get_mouse_info(input$plugging_female1)
      female1_has_warnings <- FALSE
      if (!is.null(female1_info)) {
        plugging_check <- validate_mice_active_status(
          mice_id = input$plugging_female1,
          table_name = "plugging_history", 
          status_column = "plugging_status",
          mouse_role = "female"
        )
        stock_check <- validate_mice_active_status(
          mice_id = input$plugging_female1,
          table_name = "mice_stock",
          status_column = "status"
        )
        active_plugging_statuses <- c("Ongoing", "Plugged", "Not Observed (Waiting for confirmation)")
        active_plugging_count <- sum(plugging_check$all_statuses %in% active_plugging_statuses)
        
        female1_has_warnings <- (active_plugging_count >= 1) || 
                              (!any(stock_check$all_statuses == "Alive")) || 
                              (female1_info$age_weeks < 7)
      }
      
      # Check female2 warnings
      female2_info <- get_mouse_info(input$plugging_female2)
      female2_has_warnings <- FALSE
      if (!is.null(female2_info) && input$plugging_female2 != "Optional") {
        plugging_check <- validate_mice_active_status(
          mice_id = input$plugging_female2,
          table_name = "plugging_history", 
          status_column = "plugging_status",
          mouse_role = "female"
        )
        stock_check <- validate_mice_active_status(
          mice_id = input$plugging_female2,
          table_name = "mice_stock",
          status_column = "status"
        )
        active_plugging_statuses <- c("Ongoing", "Plugged", "Not Observed (Waiting for confirmation)")
        active_plugging_count <- sum(plugging_check$all_statuses %in% active_plugging_statuses)
        
        female2_has_warnings <- (active_plugging_count >= 1) || 
                              (!any(stock_check$all_statuses == "Alive")) || 
                              (female2_info$age_weeks < 7)
      }
      
      return(male_has_warnings || female1_has_warnings || female2_has_warnings)
    })

    # Mouse info panels with enhanced validation
    output$plugging_male_info_panel <- renderUI({
      info <- get_mouse_info(input$plugging_male)
      if (is.null(info)) return(NULL)
      
      # Check plugging history statuses
      plugging_check <- validate_mice_active_status(
        mice_id = input$plugging_male,
        table_name = "plugging_history", 
        status_column = "plugging_status",
        mouse_role = "male"
      )
      
      # Check mice_stock status
      stock_check <- validate_mice_active_status(
        mice_id = input$plugging_male,
        table_name = "mice_stock",
        status_column = "status"
      )
      
      # Define active plugging statuses
      active_plugging_statuses <- c("Ongoing", "Plugged", "Not Observed (Waiting for confirmation)")
      
      # Filter active plugging records
      active_plugging_count <- sum(plugging_check$all_statuses %in% active_plugging_statuses)
      
      # Create warning messages
      warning_messages <- list()
      
      # Plugging status warnings - show warning if male has 2 or more active records
      if (active_plugging_count >= 2) {
        warning_messages[["plugging"]] <- div(
          style = "color: #d32f2f; background-color: #ffebee; padding: 8px; border-radius: 4px; margin: 8px 0; font-size: 12px;",
          tags$strong("⚠️ WARNING: "),
          paste("This male has", active_plugging_count, "active plugging records.")
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
      female1_info <- get_mouse_info(input$plugging_female1)
      if (is.null(female1_info)) return(NULL)
      
      # Check plugging history statuses
      plugging_check <- validate_mice_active_status(
        mice_id = input$plugging_female1,
        table_name = "plugging_history", 
        status_column = "plugging_status",
        mouse_role = "female"
      )
      
      # Check mice_stock status
      stock_check <- validate_mice_active_status(
        mice_id = input$plugging_female1,
        table_name = "mice_stock",
        status_column = "status"
      )
      
      # Define active plugging statuses
      active_plugging_statuses <- c("Ongoing", "Plugged", "Not Observed (Waiting for confirmation)")
      
      # Filter active plugging records
      active_plugging_count <- sum(plugging_check$all_statuses %in% active_plugging_statuses)
      
      # Create warning messages
      warning_messages <- list()
      
      # Plugging status warnings - show warning if female has 1 or more active records
      if (active_plugging_count >= 1) {
        warning_messages[["plugging"]] <- div(
          style = "color: #d32f2f; background-color: #ffebee; padding: 8px; border-radius: 4px; margin: 8px 0; font-size: 12px;",
          tags$strong("⚠️ WARNING: "),
          paste("This female has", active_plugging_count, "active plugging records.")
        )
      }
      
      # Stock status warnings - show warning if not alive
      if (!any(stock_check$all_statuses == "Alive")) {
        warning_messages[["status"]] <- div(
          style = "color: #d32f2f; background-color: #ffebee; padding: 8px; border-radius: 4px; margin: 8px 0; font-size: 12px;",
          tags$strong("⚠️ WARNING: "),
          paste("This female is not alive. Current status:", stock_check$status_summary)
        )
      }
      
      # Age warnings - show warning if younger than 7 weeks
      if (female1_info$age_weeks < 7) {
        warning_messages[["age"]] <- div(
          style = "color: #ff9800; background-color: #fff3e0; padding: 8px; border-radius: 4px; margin: 8px 0; font-size: 12px;",
          tags$strong("⚠️ AGE WARNING: "),
          paste("This female is only", female1_info$age_weeks, "weeks old (minimum recommended: 7 weeks)")
        )
      }
      
      wellPanel(
        tagList(
          tags$b("Selected Female 1 Info:"), br(),
          paste("ASU ID:", female1_info$asu_id), br(),
          paste("Age (weeks):", female1_info$age_weeks), br(),
          paste("Breeding Line:", female1_info$breeding_line), br(),
          paste("Genotype:", female1_info$genotype),
          if (length(warning_messages) > 0) warning_messages
        )
      )
    })
    
    output$plugging_female2_info_panel <- renderUI({
      female2_info <- get_mouse_info(input$plugging_female2)
      if (is.null(female2_info) || input$plugging_female2 == "Optional") return(NULL)
      
      # Check plugging history statuses
      plugging_check <- validate_mice_active_status(
        mice_id = input$plugging_female2,
        table_name = "plugging_history", 
        status_column = "plugging_status",
        mouse_role = "female"
      )
      
      # Check mice_stock status
      stock_check <- validate_mice_active_status(
        mice_id = input$plugging_female2,
        table_name = "mice_stock",
        status_column = "status"
      )
      
      # Define active plugging statuses
      active_plugging_statuses <- c("Ongoing", "Plugged", "Not Observed (Waiting for confirmation)")
      
      # Filter active plugging records
      active_plugging_count <- sum(plugging_check$all_statuses %in% active_plugging_statuses)
      
      # Create warning messages
      warning_messages <- list()
      
      # Plugging status warnings - show warning if female has 1 or more active records
      if (active_plugging_count >= 1) {
        warning_messages[["plugging"]] <- div(
          style = "color: #d32f2f; background-color: #ffebee; padding: 8px; border-radius: 4px; margin: 8px 0; font-size: 12px;",
          tags$strong("⚠️ WARNING: "),
          paste("This female has", active_plugging_count, "active plugging records.")
        )
      }
      
      # Stock status warnings - show warning if not alive
      if (!any(stock_check$all_statuses == "Alive")) {
        warning_messages[["status"]] <- div(
          style = "color: #d32f2f; background-color: #ffebee; padding: 8px; border-radius: 4px; margin: 8px 0; font-size: 12px;",
          tags$strong("⚠️ WARNING: "),
          paste("This female is not alive. Current status:", stock_check$status_summary)
        )
      }
      
      # Age warnings - show warning if younger than 7 weeks
      if (female2_info$age_weeks < 7) {
        warning_messages[["age"]] <- div(
          style = "color: #ff9800; background-color: #fff3e0; padding: 8px; border-radius: 4px; margin: 8px 0; font-size: 12px;",
          tags$strong("⚠️ AGE WARNING: "),
          paste("This female is only", female2_info$age_weeks, "weeks old (minimum recommended: 7 weeks)")
        )
      }
      
      wellPanel(
        tagList(
          tags$b("Selected Female 2 Info:"), br(),
          paste("ASU ID:", female2_info$asu_id), br(),
          paste("Age (weeks):", female2_info$age_weeks), br(),
          paste("Breeding Line:", female2_info$breeding_line), br(),
          paste("Genotype:", female2_info$genotype),
          if (length(warning_messages) > 0) warning_messages
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
      
      # Check if female2 is selected and validate
      has_female2 <- !is.null(input$plugging_female2) && input$plugging_female2 != "" && input$plugging_female2 != "Optional"
      
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
      
      con <- db_connect()
      tryCatch({
        # Create list of females to process
        females_to_process <- c(input$plugging_female1)
        if (has_female2) {
          females_to_process <- c(females_to_process, input$plugging_female2)
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
        if (has_female2) {
          showNotification(paste("Plugging events added for male", input$plugging_male, "with 2 females!"), type = "message")
        } else {
          showNotification("Plugging event added!", type = "message")
        }
        
        removeModal()
        Sys.sleep(1)
        auto_update_plugging_status_to_unknown()
        
        plugging_state$reload <- Sys.time()
        
      }, error = function(e) {
        showNotification(paste("Error adding plugging event:", e$message), type = "error")
      }, finally = {
        db_disconnect(con)
      })
    })
  })
} 