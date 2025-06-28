# Optimized Plugging Tab Module
# This module provides comprehensive plugging event management with improved performance

library(shiny)
library(DBI)
library(RSQLite)
library(DT)
library(jsonlite)

# Constants
PLUGGING_STATUSES <- c("Ongoing", "Plugged", "Plug Confirmed", "Not Pregnant", "Not Observed (Waiting for confirmation)", "Empty", "Not Observed (Confirmed)")

# UI Function
plugging_tab_ui <- function() {
  fluidPage(
    h3("Plugging Management"),
    div(
      class = "action-buttons",
      actionButton("show_plugging_modal_btn", "Add Plugging Event", 
                  class = "btn-primary", style = "margin-right: 10px;"),
      actionButton("show_calendar_btn", "Event Calendar", 
                  class = "btn-info", style = "margin-right: 10px;")
    ),
    div(
      tags$i(tags$b("Record plugging events for breeding pairs.")),
      style = "margin: 20px 0; color: #888; font-size: 14px;"
    ),
    hr(),
    h4("Plugging History"),
    uiOutput("plugging_history_controls"),
    DT::dataTableOutput("plugging_history_table")
  )
}

# Server Function
plugging_tab_server <- function(input, output, session, is_system_locked = NULL, global_refresh_trigger = NULL, all_mice_table = NULL) {
  
  # Default lock function if not provided
  if (is.null(is_system_locked)) {
    is_system_locked <- function() FALSE
  }
  
  # Default refresh trigger if not provided
  if (is.null(global_refresh_trigger)) {
    global_refresh_trigger <- reactiveVal(Sys.time())
  }
  
  # Default all_mice_table if not provided
  if (is.null(all_mice_table)) {
    all_mice_table <- reactiveVal(NULL)
  }
  
  # Reactive values for state management
  plugging_state <- reactiveValues(
    reload = NULL,
    viewing_id = NULL,
    editing_id = NULL,
    confirming_id = NULL
  )
  
  # Add a reactiveVal to store the pending delete plugging_id
  pending_delete_id <- reactiveVal(NULL)
  
  # Database connection helper
  db_connect <- function() {
    DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  }
  
  db_disconnect <- function(con) {
    if (!is.null(con)) {
      tryCatch(DBI::dbDisconnect(con), error = function(e) NULL)
    }
  }
  
  # Audit trail helper - use the enhanced audit trail system
  log_audit_trail <- function(table_name, record_id, action, old_values, new_values, user_id = NULL, note = NULL) {
    con <- db_connect()
    tryCatch({
      # Use the enhanced audit trail function from audit_trail.R
      result <- log_audit_action(con, table_name, action, record_id, 
                      if (action == "INSERT") new_values else list(old = old_values, new = new_values),
                      user = ifelse(is.null(user_id), "system", user_id), 
                      operation_details = ifelse(is.null(note), "", note))
      
      return(result)
    }, error = function(e) {
      cat("Audit trail logging failed:", e$message, "\n")
      return(FALSE)
    }, finally = {
      db_disconnect(con)
    })
  }
  
  # Get live mice for selection (cached)
  get_live_mice <- reactive({
    # Add dependency on global refresh trigger
    global_refresh_trigger()
    
    con <- db_connect()
    tryCatch({
      mice <- DBI::dbGetQuery(con, 
        "SELECT asu_id, animal_id, gender, breeding_line, genotype 
         FROM mice_stock 
         WHERE status = 'Alive' 
         ORDER BY asu_id")
      
      list(
        males = mice[mice$gender == "Male", ],
        females = mice[mice$gender == "Female", ]
      )
    }, finally = {
      db_disconnect(con)
    })
  })
  
  # Get mouse info helper
  get_mouse_info <- function(asu_id) {
    if (is.null(asu_id) || asu_id == "") return(NULL)
    
    con <- db_connect()
    tryCatch({
      mouse <- DBI::dbGetQuery(con, 
        "SELECT asu_id, dob, breeding_line, genotype 
         FROM mice_stock 
         WHERE asu_id = ? 
         LIMIT 1",
        params = list(asu_id))
      
      if (nrow(mouse) == 0) return(NULL)
      
      mouse$age_weeks <- round(as.numeric(Sys.Date() - as.Date(mouse$dob)) / 7, 1)
      return(mouse)
    }, finally = {
      db_disconnect(con)
    })
  }
  
  # Show plugging modal
  observeEvent(input$show_plugging_modal_btn, {
    mice_data <- get_live_mice()
    
    male_choices <- if(nrow(mice_data$males) > 0) {
      setNames(mice_data$males$asu_id, 
               paste(mice_data$males$asu_id, "-", mice_data$males$breeding_line))
    } else {
      c("No live males available" = "")
    }
    
    female_choices <- if(nrow(mice_data$females) > 0) {
      setNames(mice_data$females$asu_id, 
               paste(mice_data$females$asu_id, "-", mice_data$females$breeding_line))
    } else {
      c("No live females available" = "")
    }
    
    showModal(modalDialog(
      title = "Add Plugging Event",
      size = "l",
      tagList(
        fluidRow(
          column(6, selectInput("plugging_male", "Male (ASU ID)", choices = male_choices)),
          column(6, selectInput("plugging_female", "Female (ASU ID)", choices = female_choices))
        ),
        fluidRow(
          column(6, uiOutput("plugging_male_info_panel")),
          column(6, uiOutput("plugging_female_info_panel"))
        ),
        fluidRow(
          column(6, dateInput("pairing_start_date", "Pairing Start Date", value = Sys.Date())),
          column(6, dateInput("pairing_end_date", "Pairing End Date", value = Sys.Date()))
        ),
        fluidRow(
          column(6, selectInput("plugging_status", "Plugging Status", 
                               choices = PLUGGING_STATUSES, selected = "Ongoing")),
          column(6, textInput("plugging_cage", "Cage ID"))
        ),
        textAreaInput("plugging_notes", "Notes", "", rows = 2)
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("add_plugging_btn", "Add Plugging Event", class = "btn-primary")
      )
    ))
  })
  
  # Mouse info panels
  output$plugging_male_info_panel <- renderUI({
    info <- get_mouse_info(input$plugging_male)
    if (is.null(info)) return(NULL)
    
    wellPanel(
      tags$b("Selected Male Info:"), br(),
      paste("ASU ID:", info$asu_id), br(),
      paste("Age (weeks):", info$age_weeks), br(),
      paste("Breeding Line:", info$breeding_line), br(),
      paste("Genotype:", info$genotype)
    )
  })
  
  output$plugging_female_info_panel <- renderUI({
    info <- get_mouse_info(input$plugging_female)
    if (is.null(info)) return(NULL)
    
    wellPanel(
      tags$b("Selected Female Info:"), br(),
      paste("ASU ID:", info$asu_id), br(),
      paste("Age (weeks):", info$age_weeks), br(),
      paste("Breeding Line:", info$breeding_line), br(),
      paste("Genotype:", info$genotype)
    )
  })
  
  # Add plugging event
  observeEvent(input$add_plugging_btn, {
    # Validation
    if (is.null(input$plugging_male) || input$plugging_male == "") {
      showNotification("Please select a male", type = "error")
      return()
    }
    
    if (is.null(input$plugging_female) || input$plugging_female == "") {
      showNotification("Please select a female", type = "error")
      return()
    }
    
    if (input$plugging_male == input$plugging_female) {
      showNotification("Male and female cannot be the same mouse", type = "error")
      return()
    }
    
    con <- db_connect()
    tryCatch({
      # Insert plugging event
      DBI::dbExecute(con, 
        "INSERT INTO plugging_history (male_id, female_id, cage_id, pairing_start_date, 
         pairing_end_date, plugging_status, notes) VALUES (?, ?, ?, ?, ?, ?, ?)",
        params = list(
          input$plugging_male,
          input$plugging_female,
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
          female_id = input$plugging_female,
          cage_id = input$plugging_cage,
          pairing_start_date = as.character(input$pairing_start_date),
          pairing_end_date = as.character(input$pairing_end_date),
          plugging_status = input$plugging_status,
          notes = input$plugging_notes
        )
      )
      
      showNotification("Plugging event added!", type = "message")
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
  
  # Plugging history table
  output$plugging_history_controls <- renderUI({
    tagList(
      checkboxInput("show_finished_plugging_history", "Show Inactive Records (Unsuccessful Plugs, Empty Plugs and Euthanized Mice)", value = FALSE),
      checkboxInput("show_deleted_plugging_history", "Show Deleted Records (Entries by mistake)", value = FALSE)
    )
  })
  
  output$plugging_history_table <- DT::renderDataTable({
    plugging_state$reload
    
    con <- db_connect()
    tryCatch({
      # Get plugging data with mouse info in a single query
      pluggings <- DBI::dbGetQuery(con, 
        "SELECT ph.*, 
                m.dob as male_dob, m.breeding_line as male_breeding_line, m.genotype as male_genotype, m.status as male_status,
                f.dob as female_dob, f.breeding_line as female_breeding_line, f.genotype as female_genotype, f.status as female_status
         FROM plugging_history ph
         LEFT JOIN mice_stock m ON ph.male_id = m.asu_id
         LEFT JOIN mice_stock f ON ph.female_id = f.asu_id
         ORDER BY ph.id DESC")
      
      # Filtering logic
      show_deleted <- isTRUE(input$show_deleted_plugging_history)
      show_finished <- isTRUE(input$show_finished_plugging_history)
      
      # show_deleted <- F #debug
      # show_finished <- F #degug

      ### Default values 
      filtered <- pluggings[
          pluggings$plugging_status %in% c("Ongoing", "Plugged", "Not Observed (Waiting for confirmation)", "Plug Confirmed") &
          (is.na(pluggings$female_status) | pluggings$female_status == "Alive"),
        ]
      
      ###add empty plugs or euthanized mice if show_finished is true
      ### potential problem if the mice has revived for mistake, the record will take both record. but reality is if the mice has been deceased, it will not shown anymore.
      euthanized_ids <- c()
      if (show_finished) {
        audit_con <- db_connect()
        euthanized_rows <- DBI::dbGetQuery(audit_con, "SELECT record_id, new_values FROM audit_trail WHERE table_name = 'mice_stock' AND action = 'UPDATE'")
        db_disconnect(audit_con)
        if (nrow(euthanized_rows) > 0) {
        
          euthanized_ids <- sapply(seq_len(nrow(euthanized_rows)), function(i) {
            # i=3 #debug
            new_vals <- tryCatch(jsonlite::fromJSON(euthanized_rows$new_values[i]), error = function(e) list())
            if (!is.null(new_vals$source) && new_vals$source == 'Plugging Tab' && new_vals$status == 'Deceased') {
              female_id <- euthanized_rows$record_id[i]
              which(pluggings$female_id == female_id)
            } else {
              integer(0)
            }
          })
          euthanized_ids <- unique(unlist(euthanized_ids))
        }
        filtered <- rbind(filtered, pluggings[pluggings$plugging_status == 'Empty' | pluggings$plugging_status == 'Not Observed (Confirmed)' | pluggings$plugging_status == 'Not Pregnant' | seq_len(nrow(pluggings)) %in% euthanized_ids, ])
      
      }
      
      ###add deleted plugs if show_deleted is true
      if (show_deleted) {
        deleted_plugs <- pluggings[pluggings$plugging_status == 'Deleted', ]
        filtered <- rbind(filtered, deleted_plugs)
      } 
      
     
      # Calculate ages
      filtered$male_age <- floor(as.numeric(Sys.Date() - as.Date(filtered$male_dob)) / 7)
      filtered$female_age <- floor(as.numeric(Sys.Date() - as.Date(filtered$female_dob)) / 7)
      
      # Handle NULL dates
      filtered$pairing_start_date[is.na(filtered$pairing_start_date) | filtered$pairing_start_date == ""] <- "Unknown"
      filtered$pairing_end_date[is.na(filtered$pairing_end_date) | filtered$pairing_end_date == ""] <- "Unknown"
      filtered$plug_observed_date[is.na(filtered$plug_observed_date) | filtered$plug_observed_date == ""] <- "Unknown"
      
      # All missing dates are now consistently shown as "Unknown"
      
      # Create display table - show only females (one animal per row)
      display_data <- filtered[, c("id", "female_id", "female_age", "female_breeding_line", "female_genotype", 
                                   "pairing_start_date", "pairing_end_date", "plug_observed_date", 
                                   "plugging_status", "expected_age_for_harvesting", "notes")]
      
      # Add action buttons column
      display_data$actions <- sapply(seq_len(nrow(display_data)), function(i) {
        row <- display_data[i, ]
        btns <- c()
        if (row$plugging_status %in% c("Not Observed (Waiting for confirmation)", "Plugged")) {
          btns <- c(btns, paste0('<button class="btn btn-sm btn-success quick-confirm-btn" data-id="', row$id, '">Confirm</button>'))
        }
        # Add Delete button for all except already deleted or done, but only if system is unlocked
        if (!row$plugging_status %in% c("Deleted", "Not Observed (Waiting for confirmation)") && !is_system_locked()) {
          btns <- c(btns, paste0('<button class="btn btn-sm btn-danger quick-delete-plugging-btn" data-id="', row$id, '">Delete</button>'))
        }
        paste(btns, collapse = ' ')
      })
      
      # Move actions column to second position (after id)
      display_data <- display_data[, c("id", "actions", "female_id", "female_age", "female_breeding_line", "female_genotype", 
                                       "pairing_start_date", "pairing_end_date", "plug_observed_date", 
                                       "plugging_status", "expected_age_for_harvesting", "notes")]
      
      # Store the IDs for double-click functionality
      row_ids <- filtered$id
      
      DT::datatable(
        display_data,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          order = list(list(0, 'desc')),
          columnDefs = list(list(visible = FALSE, targets = 0)) # hide id column
        ),
        rownames = FALSE,
        colnames = c("ID", "Actions", "Female ID", "Age (wks)", "Breeding Line", "Genotype", 
                     "Pairing Start", "Pairing End", "Plug Observed", "Status", "Harvesting @", "Notes"),
        selection = "single",
        escape = FALSE,
        callback = JS(
          'table.on("dblclick", "tr", function() {',
          '  var data = table.row(this).data();',
          '  if(data !== undefined) {',
          '    Shiny.setInputValue("plugging_history_table_row_dblclicked", data[0], {priority: "event"});',
          '  }',
          '});',
          'table.on("click", ".quick-confirm-btn", function() {',
          '  var id = $(this).data("id");',
          '  Shiny.setInputValue("quick_confirm_plugging_btn", id, {priority: "event"});',
          '});',
          'table.on("click", ".quick-delete-plugging-btn", function() {',
          '  var id = $(this).data("id");',
          '  Shiny.setInputValue("quick_delete_plugging_btn", id, {priority: "event"});',
          '});'
        )
      )
    }, finally = {
      db_disconnect(con)
    })
  })
  
  # Helper to check if status is Plugged
  is_plugged_status <- function(status) {
    is.null(status) || status %in% c("Plugged", "Plug Confirmed", "Not Observed (Waiting for confirmation)")
  }
  
  # Helper to check if status allows further actions (not final states)
  is_active_status <- function(status) {
    status %in% c("Ongoing", "Plugged", "Plug Confirmed", "Not Observed (Waiting for confirmation)")
  }
  
  # Helper to check if status is Not Observed (Confirmed) or Not Pregnant
  is_not_observed_confirmed <- function(status) {
    status %in% c("Not Observed (Confirmed)", "Not Pregnant")
  }
  
  # Helper to check if status allows "Plug is Empty" action
  can_mark_empty <- function(status) {
    status %in% c("Plugged", "Plug Confirmed", "Not Observed (Waiting for confirmation)")
  }
  
  # Helper to check if plug observed date is valid (not NA, empty, or "Unknown")
  is_valid_plug_date <- function(date_value) {
    if (is.na(date_value) || date_value == "" || date_value == "Unknown") {
      return(FALSE)
    }
    # Try to parse as date, return FALSE if it fails
    tryCatch({
      as.Date(date_value)
      return(TRUE)
    }, error = function(e) {
      return(FALSE)
    })
  }
  
  # --- Modification History UI ---
  output$modification_history_ui <- renderUI({
    req(plugging_state$viewing_id)
    
    con <- db_connect()
    plugging <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_state$viewing_id))
    db_disconnect(con)
    
    if (nrow(plugging) == 0) {
      return(NULL)
    }
    
    female_id <- plugging$female_id[1]
    
    # Get all relevant audit trail entries
    tryCatch({
      history <- get_plugging_modification_history(plugging_state$viewing_id)
      
      if (nrow(history) == 0) {
        return(tags$div("No modification history found."))
      }
      
      # Filter for plugging_status or status changes, and include INSERTs for plugging_history
      filtered <- lapply(seq_len(nrow(history)), function(i) {
        row <- history[i, ]
        
        old <- tryCatch({
          if (is.null(row$old_values) || row$old_values == "" || is.na(row$old_values)) {
            list()
          } else {
            jsonlite::fromJSON(row$old_values)
          }
        }, error = function(e) {
          list()
        })
        
        new <- tryCatch({
          if (is.null(row$new_values) || row$new_values == "" || is.na(row$new_values)) {
            list()
          } else {
            jsonlite::fromJSON(row$new_values)
          }
        }, error = function(e) {
          list()
        })
        
        if (row$table_name == "plugging_history" && row$action == "DELETE") {
          list(user = row$user_id, date = row$timestamp, action = "Deleted")
        } else if (row$table_name == "plugging_history" && row$action == "INSERT") {
          list(user = row$user_id, date = row$timestamp, action = "Plugging Record Created")
        } else if (row$table_name == "plugging_history" && !is.null(old$plugging_status) && !is.null(new$plugging_status) && old$plugging_status != new$plugging_status) {
          # Highlight auto modifications
          action_label <- paste0("Plugging Status: ", old$plugging_status, "  →  ", new$plugging_status)
          if (row$user_id == "system(auto)") {
            action_label <- paste0(action_label, " (Auto)")
          }
          list(user = row$user_id, date = row$timestamp, action = action_label)
        } else if (row$table_name == "mice_stock" && !is.null(old$status) && !is.null(new$status) && old$status != new$status) {
          if (!is.null(new$source) && new$source == "Plugging Tab" && new$status == "Deceased") {
            list(user = row$user_id, date = row$timestamp, action = "Euthanized")
          } else {
            list(user = row$user_id, date = row$timestamp, action = paste0("Mouse Status: ", old$status, "  →  ", new$status))
          }
        } else if (row$table_name == "mice_stock" && row$action == "UPDATE") {
          # Check if this is an euthanasia action based on operation details
          if (!is.null(row$operation_details) && grepl("Euthanasia", row$operation_details, ignore.case = TRUE)) {
            list(user = row$user_id, date = row$timestamp, action = "Euthanized")
          } else {
            list(user = row$user_id, date = row$timestamp, action = paste0("Mouse Update: ", ifelse(is.null(row$operation_details) || row$operation_details == "", "Status change", row$operation_details)))
          }
        } else {
          NULL
        }
      })
      
      filtered <- Filter(Negate(is.null), filtered)
      
      if (length(filtered) == 0) {
        return(tags$div("No status modification history found."))
      }
      
      df <- data.frame(
        User = sapply(filtered, function(x) x$user),
        Date = sapply(filtered, function(x) {
          if (!is.null(history$formatted_time)) {
            idx <- which(history$timestamp == x$date)
            if (length(idx) > 0) history$formatted_time[idx[1]] else as.character(x$date)
          } else {
            as.character(x$date)
          }
        }),
        Action = sapply(filtered, function(x) x$action),
        stringsAsFactors = FALSE
      )
      
      dt <- DT::datatable(df, rownames = FALSE, options = list(dom = 't', ordering = FALSE), escape = FALSE, selection = 'none')
      # Add color formatting: yellow for Plugging Status, blue for Mouse Status and Euthanized, red for Deleted, gray for Auto
      DT::formatStyle(
        dt,
        'Action',
        target = 'row',
        backgroundColor = DT::styleEqual(
          c('Plugging Record Created',
            unique(df$Action[grepl('Plugging Status:', df$Action) & !grepl('\\(Auto\\)$', df$Action)]),
            unique(df$Action[grepl('Mouse Status:', df$Action)]),
            'Euthanized',
            'Deleted',
            unique(df$Action[grepl('Plugging Status:.*\\(Auto\\)$', df$Action)])),
          c('#fffbe6', # light yellow for Plugging Record Created and Plugging Status
            rep('#fffbe6', length(unique(df$Action[grepl('Plugging Status:', df$Action) & !grepl('\\(Auto\\)$', df$Action)]))),
            rep('#e6f0ff', length(unique(df$Action[grepl('Mouse Status:', df$Action)]))), # light blue for Mouse Status
            '#e6f0ff', # light blue for Euthanized
            '#ffeaea', # light red for Deleted
            rep('#eeeeee', length(unique(df$Action[grepl('Plugging Status:.*\\(Auto\\)$', df$Action)]))) # gray for Auto
          )
        )
      )
    }, error = function(e) {
      tags$div(paste("Error loading modification history:", e$message))
    })
  })
  
  # Edit plugging event - show detailed information
  observeEvent(input$plugging_history_table_row_dblclicked, {
    plugging_id <- input$plugging_history_table_row_dblclicked
    if (is.null(plugging_id)) return()
    
    con <- db_connect()
    tryCatch({
      plugging <- DBI::dbGetQuery(con, 
        "SELECT ph.*, 
                m.dob as male_dob, m.breeding_line as male_breeding_line, m.genotype as male_genotype, m.status as male_status,
                f.dob as female_dob, f.breeding_line as female_breeding_line, f.genotype as female_genotype, f.status as female_status
         FROM plugging_history ph
         LEFT JOIN mice_stock m ON ph.male_id = m.asu_id
         LEFT JOIN mice_stock f ON ph.female_id = f.asu_id
         WHERE ph.id = ?",
        params = list(plugging_id))
      
      if (nrow(plugging) == 0) return()
      
      row <- plugging[1, ]
      
      # Calculate ages
      male_age <- if(!is.na(row$male_dob)) round(as.numeric(Sys.Date() - as.Date(row$male_dob)) / 7, 1) else NA
      female_age <- if(!is.na(row$female_dob)) round(as.numeric(Sys.Date() - as.Date(row$female_dob)) / 7, 1) else NA
      
      showModal(modalDialog(
        title = paste("Plugging Event Details (", row$female_id, ")"),
        size = "m",
        tagList(
          fluidRow(
            column(6,
              wellPanel(
                tags$h4("Female Information"),
                tags$b("ASU ID:"), row$female_id, br(),
                tags$b("Age (weeks):"), female_age, br(),
                tags$b("Breeding Line:"), row$female_breeding_line, br(),
                tags$b("Genotype:"), row$female_genotype, br(),
                tags$b("Status:"), row$female_status
              )
            ),
            column(6, 
              wellPanel(
                tags$h4("Male Information"),
                tags$b("ASU ID:"), row$male_id, br(),
                tags$b("Age (weeks):"), male_age, br(),
                tags$b("Breeding Line:"), row$male_breeding_line, br(),
                tags$b("Genotype:"), row$male_genotype, br(),
                tags$b("Status:"), row$male_status
              )
            )
          ),
          fluidRow(
            column(12,
              wellPanel(
                tags$h4("Plugging Details"),
                fluidRow(
                  column(6, tags$b("Pairing Start Date:"), br(), 
                         if(!is.na(row$pairing_start_date) && row$pairing_start_date != "") row$pairing_start_date else "Unknown"),
                  column(6, tags$b("Pairing End Date:"), br(), 
                         if(!is.na(row$pairing_end_date) && row$pairing_end_date != "") row$pairing_end_date else "Unknown")
                ),
                fluidRow(
                  column(6, tags$b("Plug Observed Date:"), br(), 
                         if(is_valid_plug_date(row$plug_observed_date)) row$plug_observed_date else "Unknown"),
                  column(6, tags$b("Status:"), br(), row$plugging_status)
                ),
                fluidRow(
                  column(6, tags$b("Expected Age for Harvesting:"), br(), 
                         if(!is.null(row$expected_age_for_harvesting) && !is.na(row$expected_age_for_harvesting) && row$expected_age_for_harvesting != "") row$expected_age_for_harvesting else "Not decided")
                ),
                fluidRow(
                  column(12, tags$b("Notes:"), br(), ifelse(is.na(row$notes) || row$notes == "", "No notes", row$notes))
                )
              )
            )
          ),
          tags$h4("Modification History"),
          uiOutput("modification_history_ui"),
        ),
        footer = tagList(
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            div(
              style = "margin-top: 15px;",
              if (row$female_status != "Deceased" && row$plugging_status != "Deleted" && is_active_status(row$plugging_status)) {
                tagList(
                  # Show "Mark as Plugged" for Ongoing status
                  if (row$plugging_status == "Ongoing") {
                    actionButton("mark_plug_observed_btn", "Plugged", class = "btn-success")
                  },
                  # Show "Euthanized" for all active statuses (always allowed)
                  actionButton("euthanize_mice_btn", "Euthanized", class = "btn-warning"),
                  # Show "Empty Plug" for statuses that can be marked as empty (always allowed)
                  if (can_mark_empty(row$plugging_status)) {
                    actionButton("set_status_empty_btn", "Empty Plug", class = "btn-info")
                  }
                )
              }
            ),
            div(
              actionButton("edit_plugging_details_btn", "Edit", class = "btn-primary"),
              # Only hide the Delete button when locked, allow other actions
              if (!is_not_observed_confirmed(row$plugging_status) && row$female_status != "Deceased" && row$plugging_status != "Deleted" && row$plugging_status != "Not Observed (Waiting for confirmation)" && !is_system_locked()) {
                actionButton("delete_plugging_btn", "Delete", class = "btn-danger")
              },
              modalButton("Close")
            )
          )
        )
      ))
      
      plugging_state$viewing_id <- plugging_id
      
    }, finally = {
      db_disconnect(con)
    })
    
    # Reset the input so the same row can be double-clicked again
    session$sendInputMessage("plugging_history_table_row_dblclicked", NULL)
  })
  
  # Edit button from details view
  observeEvent(input$edit_plugging_details_btn, {
    plugging_id <- plugging_state$viewing_id
    if (is.null(plugging_id)) return()
    
    con <- db_connect()
    tryCatch({
      plugging <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      
      if (nrow(plugging) == 0) return()
      row <- plugging[1, ]
      
      # Get available mice for dropdown
      mice_data <- get_live_mice()
      
      male_choices <- setNames(mice_data$males$asu_id, 
                              paste(mice_data$males$asu_id, "-", mice_data$males$breeding_line, "(", mice_data$males$genotype, ")"))
      female_choices <- setNames(mice_data$females$asu_id, 
                                paste(mice_data$females$asu_id, "-", mice_data$females$breeding_line, "(", mice_data$females$genotype, ")"))
      
      # Close the details modal first
      removeModal()
      
      # Show edit modal
      showModal(modalDialog(
        title = paste("Edit Plugging Event (", row$id, ")"),
        size = "l",
        tagList(
          fluidRow(
            column(6, selectInput("edit_plugging_male", "Male", choices = male_choices, selected = row$male_id)),
            column(6, selectInput("edit_plugging_female", "Female", choices = female_choices, selected = row$female_id))
          ),
          fluidRow(
            column(6, dateInput("edit_pairing_start_date", "Pairing Start Date", 
                                value = if(!is.na(row$pairing_start_date) && row$pairing_start_date != "") as.Date(row$pairing_start_date) else Sys.Date())),
            column(6, dateInput("edit_pairing_end_date", "Pairing End Date", 
                                value = if(!is.na(row$pairing_end_date) && row$pairing_end_date != "") as.Date(row$pairing_end_date) else Sys.Date()))
          ),
          fluidRow(
            column(6, 
              radioButtons("edit_plug_observed_type", "Plug Observed Date Type", 
                          choices = c("Specific Date" = "date", "Unknown" = "unknown"),
                          selected = if(is_valid_plug_date(row$plug_observed_date)) "date" else "unknown"),
              conditionalPanel(
                condition = "input.edit_plug_observed_type == 'date'",
                dateInput("edit_plug_observed_date", "Plug Observed Date", 
                          value = if(is_valid_plug_date(row$plug_observed_date)) as.Date(row$plug_observed_date) else Sys.Date())
              )
            ),
            column(6, selectInput("edit_plugging_status", "Plugging Status", 
                                 choices = PLUGGING_STATUSES, selected = row$plugging_status))
          ),
          textAreaInput("edit_plugging_notes", "Notes", value = row$notes, rows = 3)
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("save_plugging_edit_btn", "Save Changes", class = "btn-primary")
        )
      ))
      
      plugging_state$editing_id <- plugging_id
      
    }, finally = {
      db_disconnect(con)
    })
  })
  
  # Save plugging edit
  observeEvent(input$save_plugging_edit_btn, {
    plugging_id <- plugging_state$editing_id
    if (is.null(plugging_id)) return()
    
    # Validation
    if (is.null(input$edit_plugging_male) || input$edit_plugging_male == "") {
      showNotification("Please select a male", type = "error")
      return()
    }
    
    if (is.null(input$edit_plugging_female) || input$edit_plugging_female == "") {
      showNotification("Please select a female", type = "error")
      return()
    }
    
    if (input$edit_plugging_male == input$edit_plugging_female) {
      showNotification("Male and female cannot be the same mouse", type = "error")
      return()
    }
    
    con <- db_connect()
    tryCatch({
      # Get current values for audit trail
      current <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      
      if (nrow(current) == 0) {
        showNotification("Plugging event not found", type = "error")
        return()
      }
      
      old_values <- current[1, ]
      
      # Determine plug observed date value based on radio button selection
      plug_observed_date_value <- if(input$edit_plug_observed_type == "unknown") {
        "Unknown"
      } else {
        as.character(input$edit_plug_observed_date)
      }
      
      # Update the plugging event
      result <- DBI::dbExecute(con, 
        "UPDATE plugging_history SET 
         male_id = ?,
         female_id = ?,
         pairing_start_date = ?,
         pairing_end_date = ?,
         plug_observed_date = ?, 
         plugging_status = ?, 
         notes = ?,
         updated_at = DATETIME('now')
         WHERE id = ?",
        params = list(
          input$edit_plugging_male,
          input$edit_plugging_female,
          as.character(input$edit_pairing_start_date),
          as.character(input$edit_pairing_end_date),
          plug_observed_date_value,
          input$edit_plugging_status,
          input$edit_plugging_notes,
          plugging_id
        )
      )
      
      if (result > 0) {
        # Log to audit trail
        log_audit_trail(
          "plugging_history",
          plugging_id,
          "UPDATE",
          old_values,
          list(
            male_id = input$edit_plugging_male,
            female_id = input$edit_plugging_female,
            pairing_start_date = as.character(input$edit_pairing_start_date),
            pairing_end_date = as.character(input$edit_pairing_end_date),
            plug_observed_date = plug_observed_date_value,
            plugging_status = input$edit_plugging_status,
            notes = input$edit_plugging_notes
          )
        )
        
        showNotification("Plugging event updated successfully", type = "message")
        removeModal()
        Sys.sleep(1)
        auto_update_plugging_status_to_unknown()
        plugging_state$editing_id <- NULL
        plugging_state$reload <- Sys.time()
      } else {
        showNotification("Failed to update plugging event", type = "error")
      }
      
    }, error = function(e) {
      showNotification(paste("Error updating plugging event:", e$message), type = "error")
    }, finally = {
      db_disconnect(con)
    })
  })
  
  # Mark plug as observed
  observeEvent(input$mark_plug_observed_btn, {
    showModal(modalDialog(
      title = "Mark Plug as Observed",
      size = "s",
      tagList(
        div(
          style = "text-align: center; padding: 20px;",
          tags$h4("When was the plug observed?"),
          radioButtons("plug_observed_type", "Plug Observed Date Type", 
                      choices = c("Specific Date" = "date", "Unknown" = "unknown"),
                      selected = "date"),
          conditionalPanel(
            condition = "input.plug_observed_type == 'date'",
            dateInput("plug_observed_date_input", "", value = Sys.Date(), width = "100%"),
          ),
          br(),
          textInput("expected_age_for_harvesting_input", "Expected Age for Harvesting (weeks, e.g. 14)", value = "", width = "100%"),
          br(),
          textAreaInput("plug_observed_notes_input", "Notes (optional)", value = "", rows = 3, width = "100%"),
          br(),
          tags$p("This will update the plugging status to 'Plugged' and set the plug observed date.")
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_plug_observed_btn", "Confirm", class = "btn-success")
      )
    ))
  })
  
  # Confirm plug observed
  observeEvent(input$confirm_plug_observed_btn, {
    plugging_id <- plugging_state$viewing_id
    if (is.null(plugging_id)) return()
    
    con <- db_connect()
    tryCatch({
      # Get current values for audit trail
      current <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      
      if (nrow(current) == 0) {
        showNotification("Plugging event not found", type = "error")
        return()
      }
      
      old_values <- current[1, ]
      
      # Determine plug observed date value based on radio button selection
      plug_observed_date_value <- if(input$plug_observed_type == "unknown") {
        "Unknown"
      } else {
        as.character(input$plug_observed_date_input)
      }
      
      # Update the plugging event
      result <- DBI::dbExecute(con, 
        "UPDATE plugging_history SET 
         plug_observed_date = ?,
         plugging_status = 'Plugged',
         expected_age_for_harvesting = ?,
         notes = CASE 
           WHEN notes IS NULL OR notes = '' THEN ?
           ELSE notes || '\n' || ?
         END,
         updated_at = DATETIME('now')
         WHERE id = ?",
        params = list(
          plug_observed_date_value,
          input$expected_age_for_harvesting_input,
          input$plug_observed_notes_input,
          input$plug_observed_notes_input,
          plugging_id
        )
      )
      
      if (result > 0) {
        # Log to audit trail
        log_audit_trail(
          "plugging_history",
          plugging_id,
          "UPDATE",
          old_values,
          list(
            plug_observed_date = plug_observed_date_value,
            plugging_status = "Plugged",
            expected_age_for_harvesting = input$expected_age_for_harvesting_input,
            notes = input$plug_observed_notes_input
          )
        )
        
        showNotification("Plug marked as observed successfully!", type = "message")
        removeModal()
        plugging_state$viewing_id <- NULL
        plugging_state$reload <- Sys.time()
        
        # Trigger global refresh for cross-module updates
        global_refresh_trigger(Sys.time())
        
        # Refresh all_mice_table if available
        if (!is.null(all_mice_table)) {
          con_refresh <- db_connect()
          tryCatch({
            all_data <- DBI::dbGetQuery(con_refresh, "SELECT * FROM mice_stock ORDER BY asu_id")
            all_mice_table(all_data)
          }, finally = {
            db_disconnect(con_refresh)
          })
        }
      } else {
        showNotification("Failed to update plugging event", type = "error")
      }
      
    }, error = function(e) {
      showNotification(paste("Error updating plugging event:", e$message), type = "error")
    }, finally = {
      db_disconnect(con)
    })
  })
  
  # Mark plug as done
  observeEvent(input$mark_plug_done_btn, {
    showModal(modalDialog(
      title = "Mark Plug as Done",
      size = "s",
      tagList(
        div(
          style = "text-align: center; padding: 20px;",
          tags$h4("Mark the plugging process as complete?"),
          dateInput("plug_done_date_input", "Completion Date", value = Sys.Date(), width = "100%"),
          br(),
          textAreaInput("plug_done_notes_input", "Notes (optional)", value = "", rows = 3, width = "100%"),
          br(),
          tags$p("This will update the plugging status to 'Not Observed (Waiting for confirmation)'.")
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_plug_done_btn", "Confirm", class = "btn-success")
      )
    ))
  })
  
  # Confirm plug done
  observeEvent(input$confirm_plug_done_btn, {
    plugging_id <- plugging_state$viewing_id
    if (is.null(plugging_id)) return()
    
    con <- db_connect()
    tryCatch({
      # Get current values for audit trail
      current <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      
      if (nrow(current) == 0) {
        showNotification("Plugging event not found", type = "error")
        return()
      }
      
      old_values <- current[1, ]
      
      # Update the plugging event
      result <- DBI::dbExecute(con, 
        "UPDATE plugging_history SET 
         plugging_status = 'Not Observed (Waiting for confirmation)',
         updated_at = DATETIME('now'),
         notes = CASE 
           WHEN notes IS NULL OR notes = '' THEN ?
           ELSE notes || '\n' || ?
         END
         WHERE id = ?",
        params = list(
          paste0("[Plug marked as Not Observed (Waiting for confirmation) on ", as.character(input$plug_done_date_input), "]"),
          paste0("[Plug marked as Not Observed (Waiting for confirmation) on ", as.character(input$plug_done_date_input), "]"),
          plugging_id
        )
      )
      
      if (result > 0) {
        # Log to audit trail
        log_audit_trail(
          "plugging_history",
          plugging_id,
          "UPDATE",
          old_values,
          list(
            plugging_status = "Not Observed (Waiting for confirmation)",
            completion_date = as.character(input$plug_done_date_input),
            notes = input$plug_done_notes_input
          )
        )
        
        showNotification("Plug marked as Not Observed (Waiting for confirmation) successfully!", type = "message")
        removeModal()
        Sys.sleep(1)
        auto_update_plugging_status_to_unknown()
        
        plugging_state$viewing_id <- NULL
        plugging_state$reload <- Sys.time()
      } else {
        showNotification("Failed to update plugging event", type = "error")
      }
      
    }, error = function(e) {
      showNotification(paste("Error updating plugging event:", e$message), type = "error")
    }, finally = {
      db_disconnect(con)
    })
  })
  
  # Euthanize mice
  observeEvent(input$euthanize_mice_btn, {
    plugging_id <- plugging_state$viewing_id
    if (is.null(plugging_id)) return()
    
    con <- db_connect()
    tryCatch({
      plugging <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      
      if (nrow(plugging) == 0) return()
      row <- plugging[1, ]
      
      # Get female mouse information
      female_info <- DBI::dbGetQuery(con, 
        "SELECT asu_id, dob, breeding_line, genotype FROM mice_stock WHERE asu_id = ?", 
        params = list(row$female_id))
      
      if (nrow(female_info) == 0) return()
      
      # Calculate female age
      female_age <- round(as.numeric(Sys.Date() - as.Date(female_info$dob)) / 7, 1)
      
      showModal(modalDialog(
        title = "Confirm Euthanasia",
        size = "m",
        tagList(
          fluidRow(
            column(6, 
              wellPanel(
                tags$h4("Female Mouse Information"),
                tags$b("ASU ID:"), female_info$asu_id, br(),
                tags$b("Age (weeks):"), female_age, br(),
                tags$b("Breeding Line:"), female_info$breeding_line, br(),
                tags$b("Genotype:"), female_info$genotype
              )
            ),
            column(6,
              dateInput("euthanasia_date_input", "Date of Death", value = Sys.Date()),
              textAreaInput("euthanasia_notes_input", "Notes (optional)", value = "", rows = 2),
              checkboxInput("euthanasia_plug_empty", "Plug is Empty", value = FALSE)
            )
          ),
          div(
            style = "text-align: center; margin-top: 15px;",
            tags$p(tags$i("Are you sure you want to mark this female mouse as deceased? This action cannot be undone."))
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_euthanasia_btn", "Confirm Euthanasia", class = "btn-danger")
        )
      ))
      
    }, finally = {
      db_disconnect(con)
    })
  })
  
  # Confirm euthanasia
  observeEvent(input$confirm_euthanasia_btn, {
    plugging_id <- plugging_state$viewing_id
    if (is.null(plugging_id)) return()
    
    con <- db_connect()
    tryCatch({
      plugging <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      
      if (nrow(plugging) == 0) return()
      row <- plugging[1, ]
      
      # If Plug is Empty is checked, handle plugging status first, then mouse status
      if (isTRUE(input$euthanasia_plug_empty)) {
        # First: Update plugging status to Empty
        con2 <- db_connect()
        old_plug <- DBI::dbGetQuery(con2, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
        DBI::dbExecute(con2, 
          "UPDATE plugging_history SET plugging_status = 'Empty', plug_observed_date = NULL WHERE id = ?",
          params = list(plugging_id)
        )
        # Log to audit trail for plugging_history change
        plug_audit_result <- log_audit_trail(
          "plugging_history",
          plugging_id,
          "UPDATE",
          old_plug[1, ],
          list(
            plugging_status = "Empty",
            plug_observed_date = NA
          ),
          note = "Plug marked as Empty due to euthanasia"
        )
        db_disconnect(con2)
        
        # Wait 1 second before updating mouse status
        Sys.sleep(1)
        
        # Then: Update female mouse status to Deceased
        mouse_result <- DBI::dbExecute(con, 
          "UPDATE mice_stock SET 
           status = 'Deceased',
           date_of_death = ?,
           deceased_timestamp = DATETIME('now'),
           last_updated = DATETIME('now')
           WHERE asu_id = ?",
          params = list(
            as.character(input$euthanasia_date_input),
            row$female_id
          )
        )
        
        if (mouse_result > 0) {
          # Log to audit trail for female mouse
          audit_result <- log_audit_trail(
            "mice_stock",
            row$female_id,
            "UPDATE",
            list(status = "Alive"),
            list(
              status = "Deceased",
              date_of_death = as.character(input$euthanasia_date_input),
              notes = input$euthanasia_notes_input,
              source = "Plugging Tab"
            ),
            note = "Euthanasia from Plugging Tab"
          )
          
          showNotification("Female mouse marked as deceased successfully!", type = "message")
          removeModal()
          plugging_state$viewing_id <- NULL
          plugging_state$reload <- Sys.time()
          
          # Trigger global refresh for cross-module updates
          global_refresh_trigger(Sys.time())
          
          # Refresh all_mice_table if available
          if (!is.null(all_mice_table)) {
            con_refresh <- db_connect()
            tryCatch({
              all_data <- DBI::dbGetQuery(con_refresh, "SELECT * FROM mice_stock ORDER BY asu_id")
              all_mice_table(all_data)
            }, finally = {
              db_disconnect(con_refresh)
            })
          }
        } else {
          showNotification("Failed to update female mouse status", type = "error")
        }
        
      } else {
        # If Plug is Empty is NOT checked, update mouse status first, then plugging status
        # Update female mouse status to Deceased
        result <- DBI::dbExecute(con, 
          "UPDATE mice_stock SET 
           status = 'Deceased',
           date_of_death = ?,
           deceased_timestamp = DATETIME('now'),
           last_updated = DATETIME('now')
           WHERE asu_id = ?",
          params = list(
            as.character(input$euthanasia_date_input),
            row$female_id
          )
        )
        
        if (result > 0) {
          # Log to audit trail for female mouse
          audit_result <- log_audit_trail(
            "mice_stock",
            row$female_id,
            "UPDATE",
            list(status = "Alive"),
            list(
              status = "Deceased",
              date_of_death = as.character(input$euthanasia_date_input),
              notes = input$euthanasia_notes_input,
              source = "Plugging Tab"
            ),
            note = "Euthanasia from Plugging Tab"
          )
          
          # Update plugging_status to 'Not Observed (Waiting for confirmation)'
          con2 <- db_connect()
          old_plug <- DBI::dbGetQuery(con2, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
          DBI::dbExecute(con2, 
            "UPDATE plugging_history SET 
             plugging_status = 'Not Observed (Waiting for confirmation)',
             updated_at = DATETIME('now'),
             notes = CASE 
               WHEN notes IS NULL OR notes = '' THEN ?
               ELSE notes || '\n' || ?
             END
             WHERE id = ?",
            params = list(
              paste0("[Plug marked as Not Observed (Waiting for confirmation) due to euthanasia on ", as.character(input$euthanasia_date_input), "]"),
              paste0("[Plug marked as Not Observed (Waiting for confirmation) due to euthanasia on ", as.character(input$euthanasia_date_input), "]"),
              plugging_id
            )
          )
          # Log to audit trail for plugging_history change
          plug_audit_result <- log_audit_trail(
            "plugging_history",
            plugging_id,
            "UPDATE",
            old_plug[1, ],
            list(
              plugging_status = "Not Observed (Waiting for confirmation)",
              completion_date = as.character(input$euthanasia_date_input),
              notes = paste0("Euthanasia completed on ", as.character(input$euthanasia_date_input))
            ),
            note = "Plug marked as Not Observed due to euthanasia"
          )
          db_disconnect(con2)
          
          showNotification("Female mouse marked as deceased successfully!", type = "message")
          removeModal()
          plugging_state$viewing_id <- NULL
          plugging_state$reload <- Sys.time()
          
          # Trigger global refresh for cross-module updates
          global_refresh_trigger(Sys.time())
          
          # Refresh all_mice_table if available
          if (!is.null(all_mice_table)) {
            con_refresh <- db_connect()
            tryCatch({
              all_data <- DBI::dbGetQuery(con_refresh, "SELECT * FROM mice_stock ORDER BY asu_id")
              all_mice_table(all_data)
            }, finally = {
              db_disconnect(con_refresh)
            })
          }
        } else {
          showNotification("Failed to update female mouse status", type = "error")
        }
      }
      
    }, error = function(e) {
      showNotification(paste("Error updating mouse status:", e$message), type = "error")
    }, finally = {
      db_disconnect(con)
    })
  })
  
  # Show calendar modal when Event Calendar button is clicked
  observeEvent(input$show_calendar_btn, {
    show_plugging_calendar_modal()
  })

  # Delete plugging record: show confirmation modal
  observeEvent(input$delete_plugging_btn, {
    showModal(modalDialog(
      title = "Delete Plugging Record",
      "Are you sure you want to delete this plugging record? This action cannot be undone.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_plugging_btn", "Delete", class = "btn-danger")
      )
    ))
  })

  # Confirm delete plugging record
  observeEvent(input$confirm_delete_plugging_btn, {
    plugging_id <- plugging_state$viewing_id
    if (is.null(plugging_id)) return()
    con <- db_connect()
    tryCatch({
      # Get old record for audit
      old_row <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      if (nrow(old_row) == 0) return()
      # Set plugging_status to 'Deleted'
      DBI::dbExecute(con, "UPDATE plugging_history SET plugging_status = 'Deleted' WHERE id = ?", params = list(plugging_id))
      # Log plugging_status change to Deleted
      log_audit_trail(
        "plugging_history",
        plugging_id,
        "UPDATE",
        as.list(old_row[1, ]),
        modifyList(as.list(old_row[1, ]), list(plugging_status = 'Deleted'))
      )
      showNotification("Plugging record marked as deleted.", type = "message")
      removeModal()
      plugging_state$viewing_id <- NULL
      plugging_state$reload <- Sys.time()
    }, finally = {
      db_disconnect(con)
    })
  })
  
  # Quick confirm plugging status
  observeEvent(input$quick_confirm_plugging_btn, {
    plugging_id <- input$quick_confirm_plugging_btn
    if (is.null(plugging_id)) return()
    
    # Show confirmation modal with status options
    showModal(modalDialog(
      title = "Confirm Plugging Status",
      size = "s",
      tagList(
        div(
          style = "margin-bottom: 15px;",
          tags$strong("Please select the final status for this plugging record:")
        ),
        div(
          style = "margin-bottom: 10px;",
          radioButtons("confirm_status_choice", "Status Options:",
                      choices = c(
                        "Not Pregnant" = "Not Pregnant",
                        "Plug Confirmed" = "Plug Confirmed", 
                        "Not Observed (Confirmed)" = "Not Observed (Confirmed)"
                      ),
                      selected = "Plug Confirmed"
          )
        ),
        div(
          style = "background-color: #e3f2fd; border: 1px solid #2196f3; padding: 10px; border-radius: 5px;",
          tags$strong("Note:"), "This will finalize the plugging record status."
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_status_btn", "Confirm Status", class = "btn-primary")
      )
    ))
    
    # Store the plugging_id for use in the confirmation
    plugging_state$confirming_id <- plugging_id
  })
  
  # Handle the status confirmation
  observeEvent(input$confirm_status_btn, {
    plugging_id <- plugging_state$confirming_id
    selected_status <- input$confirm_status_choice
    
    if (is.null(plugging_id) || is.null(selected_status)) {
      showNotification("Missing information for confirmation", type = "error")
      return()
    }
    
    con <- db_connect()
    tryCatch({
      # Get current values for audit trail
      current <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      
      if (nrow(current) == 0) {
        showNotification("Plugging event not found", type = "error")
        return()
      }
      
      # Check if status is eligible for confirmation
      if (!current$plugging_status[1] %in% c("Not Observed (Waiting for confirmation)", "Plugged")) {
        showNotification("This record is not eligible for confirmation", type = "error")
        return()
      }
      
      old_values <- current[1, ]
      
      # Update the plugging event
      result <- DBI::dbExecute(con, 
        "UPDATE plugging_history SET 
         plugging_status = ?,
         updated_at = DATETIME('now'),
         notes = CASE 
           WHEN notes IS NULL OR notes = '' THEN ?
           ELSE notes || '\n' || ?
         END
         WHERE id = ?",
        params = list(
          selected_status,
          paste0("[Status confirmed as '", selected_status, "' on ", as.character(Sys.Date()), "]"),
          paste0("[Status confirmed as '", selected_status, "' on ", as.character(Sys.Date()), "]"),
          plugging_id
        )
      )
      
      if (result > 0) {
        # Log to audit trail
        log_audit_trail(
          "plugging_history",
          plugging_id,
          "UPDATE",
          old_values,
          list(
            plugging_status = selected_status,
            confirmation_date = as.character(Sys.Date()),
            notes = paste0("Status confirmed as '", selected_status, "' by ASU staff")
          )
        )
        
        showNotification(paste("Plugging status confirmed as '", selected_status, "' successfully!", sep = ""), type = "message")
        removeModal()
        plugging_state$confirming_id <- NULL
        ##Sys.sleep(1)
        auto_update_plugging_status_to_unknown()
        plugging_state$reload <- Sys.time()
      } else {
        showNotification("Failed to update plugging status", type = "error")
      }
      
    }, error = function(e) {
      showNotification(paste("Error updating plugging status:", e$message), type = "error")
    }, finally = {
      db_disconnect(con)
    })
  })
  
  # Set plugging status to Empty
  observeEvent(input$set_status_empty_btn, {
    showModal(modalDialog(
      title = "Confirm Set Status to Empty",
      size = "s",
      tagList(
        dateInput("set_status_empty_date_input", "Date of Confirmation", value = Sys.Date()),
        div(style = "margin-top: 10px;", tags$p("Are you sure you want to set the plugging status to 'Empty'?"))
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_set_status_empty_btn", "Confirm", class = "btn-warning")
      )
    ))
  })
  
  # Confirm set status empty
  observeEvent(input$confirm_set_status_empty_btn, {
    plugging_id <- plugging_state$viewing_id
    if (is.null(plugging_id)) return()
    
    con <- db_connect()
    tryCatch({
      # Get current values for audit trail
      current <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      
      if (nrow(current) == 0) {
        showNotification("Plugging event not found", type = "error")
        return()
      }
      old_values <- current[1, ]
      
      # Update the plugging event
      result <- DBI::dbExecute(con, 
        "UPDATE plugging_history SET 
         plugging_status = 'Empty',
         updated_at = DATETIME('now'),
         notes = CASE 
           WHEN notes IS NULL OR notes = '' THEN ?
           ELSE notes || '\n' || ?
         END
         WHERE id = ?",
        params = list(
          paste0("[Plug set to Empty on ", as.character(input$set_status_empty_date_input), "]"),
          paste0("[Plug set to Empty on ", as.character(input$set_status_empty_date_input), "]"),
          plugging_id
        )
      )
      
      if (result > 0) {
        # Log to audit trail
        log_audit_trail(
          "plugging_history",
          plugging_id,
          "UPDATE",
          old_values,
          list(
            plugging_status = "Empty",
            confirmation_date = as.character(input$set_status_empty_date_input)
          )
        )
        
        showNotification("Plugging status set to Empty!", type = "message")
        removeModal()
        plugging_state$viewing_id <- NULL
        plugging_state$reload <- Sys.time()
      } else {
        showNotification("Failed to update plugging status", type = "error")
      }
      
    }, error = function(e) {
      showNotification(paste("Error updating plugging status:", e$message), type = "error")
    }, finally = {
      db_disconnect(con)
    })
  })

  # Function to auto-update plugging_status to 'Not Observed (Waiting for confirmation)' if pairing period is over and status is still 'Ongoing'
  auto_update_plugging_status_to_unknown <- function() {
    con <- db_connect()
    tryCatch({
      today <- as.character(Sys.Date())
      # Find all ongoing records where pairing_end_date is before today (not equal to today)
      records <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE plugging_status = 'Ongoing' AND pairing_end_date IS NOT NULL AND pairing_end_date < ?", params = list(today))
      for (i in seq_len(nrow(records))) {
        rec <- records[i, ]
        # Update status to Not Observed (Waiting for confirmation)
        DBI::dbExecute(con, "UPDATE plugging_history SET plugging_status = 'Not Observed (Waiting for confirmation)', updated_at = DATETIME('now') WHERE id = ?", params = list(rec$id))
        # Log to audit trail (use list for new_values, just like user actions, use user_id = 'system(auto)' )
        log_audit_trail(
          "plugging_history",
          rec$id,
          "UPDATE",
          as.list(rec),
          list(
            plugging_status = "Not Observed (Waiting for confirmation)",
            confirmation_date = as.character(Sys.Date()),
            notes = "Auto-update: pairing period ended, status set to Not Observed (Waiting for confirmation)"
          ),
          user_id = "system(auto)"
        )
      }
    }, finally = {
      db_disconnect(con)
    })
  }

  # Auto-update plugging_status to Unknown if needed
  auto_update_plugging_status_to_unknown()

  # Add observeEvent for quick_delete_plugging_btn
  observeEvent(input$quick_delete_plugging_btn, {
    plugging_id <- input$quick_delete_plugging_btn
    if (is.null(plugging_id)) return()
    pending_delete_id(plugging_id)
    showModal(modalDialog(
      title = "Confirm Delete Plugging Record",
      #size = "s",
      "Are you sure you want to delete this plugging record? This action cannot be undone.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_quick_delete_plugging_btn", "Delete", class = "btn-danger")
      )
    ))
  })

  # Register the confirmation observer only once
  observeEvent(input$confirm_quick_delete_plugging_btn, {
    plugging_id <- pending_delete_id()
    if (is.null(plugging_id)) return()
    removeModal()
    con <- db_connect()
    tryCatch({
      old_row <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      if (nrow(old_row) == 0) return()
      DBI::dbExecute(con, "UPDATE plugging_history SET plugging_status = 'Deleted' WHERE id = ?", params = list(plugging_id))
      log_audit_trail(
        "plugging_history",
        plugging_id,
        "UPDATE",
        as.list(old_row[1, ]),
        modifyList(as.list(old_row[1, ]), list(plugging_status = 'Deleted'))
      )
      showNotification("Plugging record marked as deleted.", type = "message")
      plugging_state$reload <- Sys.time()
    }, finally = {
      db_disconnect(con)
    })
    pending_delete_id(NULL)
  })
} 