# Optimized Plugging Tab Module
# This module provides comprehensive plugging event management with improved performance

suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RSQLite)
  library(DT)
  library(jsonlite)
})

# Source the new modal module
source("Modules/modal_add_plugging_event.R")

# Constants
PLUGGING_STATUSES <- c("Ongoing", "Plugged", "Plug Confirmed", "Not Pregnant", "Not Observed (Waiting for confirmation)", "Empty", "Not Observed (Confirmed)", "Surprising Plug!!", "Collected")

# UI Function
plugging_tab_ui <- function() {
  fluidPage(
    div(
      style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 16px;",
      h3("♂️🐭 x 🐭♀️ Plugging Management", style = "margin: 0; font-size: 1.8em; color: #2c3e50; font-weight: 700;"),
      div(
        style = "display: flex; align-items: center; gap: 8px;",
        div(
          style = "font-size: 12px; color: #6c757d; background: #e9ecef; padding: 4px 8px; border-radius: 4px;",
          "💡 Manage breeding pairs and track plugging events"
        )
      )
    ),
    div(
      style = "background: white; border-radius: 8px; padding: 16px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); margin-bottom: 16px;",
      div(
        class = "action-buttons",
        style = "margin-bottom: 16px;",
        add_plugging_modal_ui("add_plugging_modal"),
        actionButton("show_calendar_btn", "📅 Event Calendar", 
                    class = "btn-info", style = "margin-left: 8px; padding: 8px 16px; font-size: 14px; border-radius: 6px;")
      ),
      div(
        style = "padding: 12px; background: linear-gradient(135deg, #e8f5e8 0%, #d4edda 100%); border-radius: 6px; border-left: 4px solid #28a745;",
        "📊 Record plugging events for breeding pairs or trios and track their progress through different stages."
      )
    ),
    div(
      style = "background: white; border-radius: 8px; padding: 16px; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
      h4("📋 Plugging History", style = "margin: 8px 0 16px 0; font-size: 1.3em; color: #2c3e50;"),
      uiOutput("plugging_history_controls"),
      div(
        style = "position: relative;",
        uiOutput("plugging_loading_overlay"),
        DT::dataTableOutput("plugging_history_table")
      )
    )
  )
}

# Server Function
plugging_tab_server <- function(input, output, session, is_system_locked = NULL, global_refresh_trigger = NULL, all_mice_table = NULL, shared_plugging_state = NULL) {
  
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
  
  # Use shared plugging state if provided, otherwise create a new one
  if (is.null(shared_plugging_state)) {
    plugging_state <- reactiveValues(
      reload = NULL,
      viewing_id = NULL,
      editing_id = NULL,
      confirming_id = NULL
    )
  } else {
    plugging_state <- shared_plugging_state
  }
  
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
  
  # Unified modal function for Empty (Alive) status
  show_empty_alive_modal <- function(female_info, female_age, default_date, default_notes, confirm_id, confirm_btn_id) {
    modalDialog(
      title = "Confirm Set Status to Empty (Alive)",
      size = "m",
      tagList(
        fluidRow(
          column(6, 
            wellPanel(
              tags$h4("Female Mouse Information"),
              tags$b("ASU ID:"), if(!is.null(female_info) && nrow(female_info) > 0) female_info$asu_id[1] else "N/A", br(),
              tags$b("Age (weeks):"), if(!is.na(female_age)) female_age else "N/A", br(),
              tags$b("Breeding Line:"), if(!is.null(female_info) && nrow(female_info) > 0) female_info$breeding_line[1] else "N/A", br(),
              tags$b("Genotype:"), if(!is.null(female_info) && nrow(female_info) > 0) female_info$genotype[1] else "N/A"
            )
          ),
          column(6,
            dateInput(confirm_id, "Date of Confirmation", value = default_date),
            textAreaInput(paste0(confirm_id, "_notes"), "Notes", value = default_notes, rows = 2)
          )
        ),
        div(
          style = "text-align: center; margin-top: 15px;",
          tags$p(tags$i("This will set the plugging status to 'Empty' but keep the female mouse alive."))
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(confirm_btn_id, "Confirm", class = "btn-success")
      )
    )
  }
  
  # Use the enhanced audit trail system from audit_trail.R
  
  # Get live mice for selection - using centralized function directly
  get_live_mice <- reactive({
    # Get all live mice directly using centralized function
    all_mice <- get_live_mice_by_gender(gender = NULL, global_refresh_trigger = global_refresh_trigger)()
    
    # Return in the expected format for the modal
    list(
      males = all_mice[all_mice$gender == "Male", ],
      females = all_mice[all_mice$gender == "Female", ]
    )
  })
  
  # Plugging history table
  output$plugging_history_controls <- renderUI({
    div(
      style = "display: flex; align-items: center; gap: 16px;",
      div(
        style = "display: flex; flex-direction: column; margin-right: 16px;",
        checkboxInput("show_finished_plugging_history", "Show Archived Records", value = FALSE),
        div(
          style = "font-size: 11px; color: #666; margin-left: 20px; margin-top: -12px;",
          "(Unsuccessful Plugs, Empty Plugs and Euthanized Mice)"
        )
      ),
      div(
        style = "display: flex; flex-direction: column;",
        checkboxInput("show_deleted_plugging_history", "Show Deleted Records", value = FALSE),
        div(
          style = "font-size: 11px; color: #666; margin-left: 20px; margin-top: -12px;",
          "(Entries by mistake)"
        )
      ),
      div(
        style = "margin-left: auto;",
        actionButton("refresh_plugging_table_btn", "🔄 Refresh Table", 
                    class = "btn-secondary", 
                    style = "padding: 6px 12px; font-size: 12px; border-radius: 4px;")
      )
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
          pluggings$plugging_status %in% c("Ongoing", "Plugged", "Not Observed (Waiting for confirmation)", "Plug Confirmed", "Surprising Plug!!") &
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
        filtered <- rbind(filtered, pluggings[pluggings$plugging_status == 'Empty' | pluggings$plugging_status == 'Not Observed (Confirmed)' | pluggings$plugging_status == 'Not Pregnant' | pluggings$plugging_status == 'Collected' | seq_len(nrow(pluggings)) %in% euthanized_ids, ])
      
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
        
        # Add Update button for all active status mice
        if (is_active_status(row$plugging_status)) {
          btns <- c(btns, paste0('<button class="btn btn-sm btn-success quick-confirm-btn" data-id="', row$id, '">Update</button>'))
        }
        
        # Add Delete button for all except already deleted or active statuses, but only if system is unlocked
        if (!row$plugging_status %in% c("Deleted") && !is_active_status(row$plugging_status) && !is_system_locked()) {
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
          pageLength = 100,
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
    is.null(status) || status %in% c("Plugged", "Plug Confirmed", "Not Observed (Waiting for confirmation)", "Surprising Plug!!")
  }
  
  # Helper to check if status allows further actions (not final states)
  is_active_status <- function(status) {
    status %in% c("Ongoing", "Plugged", "Plug Confirmed", "Not Observed (Waiting for confirmation)", "Surprising Plug!!")
  }
  
  # Helper to check if status is Not Observed (Confirmed) or Not Pregnant
  is_not_observed_confirmed <- function(status) {
    status %in% c("Not Observed (Confirmed)", "Not Pregnant")
  }
  
  # Helper to check if status allows "Plug is Empty" action
  can_mark_empty <- function(status) {
    status %in% c("Plugged", "Plug Confirmed")
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
      
      # Group entries by timestamp to handle transactions properly
      history$timestamp_group <- as.character(history$timestamp)
      timestamp_groups <- unique(history$timestamp_group)
      
      filtered <- list()
      
      for (timestamp_group in timestamp_groups) {
        # Get all entries with the same timestamp
        same_time_entries <- history[history$timestamp_group == timestamp_group, ]
        
        # Check if any of these entries involve euthanasia
        has_euthanasia <- FALSE
        for (j in seq_len(nrow(same_time_entries))) {
          entry <- same_time_entries[j, ]
          if (entry$table_name == "mice_stock") {
            new_vals <- tryCatch({
              if (!is.null(entry$new_values) && entry$new_values != "" && !is.na(entry$new_values)) {
                jsonlite::fromJSON(entry$new_values)
              } else {
                list()
              }
            }, error = function(e) list())
            
            if (!is.null(new_vals$status) && new_vals$status == "Deceased" && 
                !is.null(new_vals$source) && new_vals$source == "Plugging Tab" &&
                !is.null(new_vals$source_event_id) && as.character(new_vals$source_event_id) == as.character(plugging_state$viewing_id)) {
              has_euthanasia <- TRUE
              break
            }
          }
        }
        
        # Process each entry in this timestamp group
        for (j in seq_len(nrow(same_time_entries))) {
          row <- same_time_entries[j, ]
          
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
          
          # Get plug observed date and date of death if available
          plug_observed_date <- NA
          date_of_death <- NA
          if (row$table_name == "plugging_history") {
            if (!is.null(new$plug_observed_date) && new$plug_observed_date != "" && new$plug_observed_date != "Unknown") {
              plug_observed_date <- new$plug_observed_date
            }
          }
          if (row$table_name == "mice_stock") {
            if (!is.null(new$date_of_death) && new$date_of_death != "") {
              date_of_death <- new$date_of_death
            }
          }
          # Format extra info
          extra_info <- c()
          if (!is.na(plug_observed_date) && plug_observed_date != "") {
            extra_info <- c(extra_info, paste0("Plug Observed: ", format(as.Date(plug_observed_date), "%d-%b-%Y")))
          }
          if (!is.na(date_of_death) && date_of_death != "") {
            extra_info <- c(extra_info, paste0("Date of Death: ", format(as.Date(date_of_death), "%d-%b-%Y")))
          }
          extra_info_str <- if (length(extra_info) > 0) paste0(" (", paste(extra_info, collapse = ", "), ")") else ""
          
          if (row$table_name == "plugging_history" && row$action == "DELETE") {
            filtered[[length(filtered) + 1]] <- list(user = row$user_id, date = row$timestamp, action = paste0("Deleted", extra_info_str))
          } else if (row$table_name == "plugging_history" && row$action == "INSERT") {
            filtered[[length(filtered) + 1]] <- list(user = row$user_id, date = row$timestamp, action = paste0("Plugging Record Created", extra_info_str))
          } else if (row$table_name == "plugging_history" && !is.null(old$plugging_status) && !is.null(new$plugging_status) && old$plugging_status != new$plugging_status) {
            # Special handling for Empty status: use the euthanasia flag from the same transaction
            if (new$plugging_status == "Empty") {
              action_label <- paste0("Plugging Status: ", old$plugging_status, "  →  ", ifelse(has_euthanasia, "Empty (Euthanized)", "Empty (Alive)"))
            } else {
              action_label <- paste0("Plugging Status: ", old$plugging_status, "  →  ", new$plugging_status)
            }
            if (row$user_id == "system(auto)") {
              action_label <- paste0(action_label, " (Auto)")
            }
            filtered[[length(filtered) + 1]] <- list(user = row$user_id, date = row$timestamp, action = paste0(action_label, extra_info_str))
          } else if (row$table_name == "mice_stock" && !is.null(old$status) && !is.null(new$status) && old$status != new$status) {
            if (!is.null(new$source) && new$source == "Plugging Tab" && new$status == "Deceased") {
              filtered[[length(filtered) + 1]] <- list(user = row$user_id, date = row$timestamp, action = paste0("Euthanized", extra_info_str))
            } else {
              filtered[[length(filtered) + 1]] <- list(user = row$user_id, date = row$timestamp, action = paste0("Mouse Status: ", old$status, "  →  ", new$status, extra_info_str))
            }
          } else if (row$table_name == "mice_stock" && row$action == "UPDATE") {
            if (!is.null(row$operation_details) && grepl("Euthanasia", row$operation_details, ignore.case = TRUE)) {
              filtered[[length(filtered) + 1]] <- list(user = row$user_id, date = row$timestamp, action = paste0("Euthanized", extra_info_str))
            } else {
              filtered[[length(filtered) + 1]] <- list(user = row$user_id, date = row$timestamp, action = paste0("Mouse Update: ", ifelse(is.null(row$operation_details) || row$operation_details == "", "Status change", row$operation_details), extra_info_str))
            }
          }
        }
      }
      
      filtered <- Filter(Negate(is.null), filtered)
      
      if (length(filtered) == 0) {
        return(tags$div("No status modification history found."))
      }
      
      df <- data.frame(
        User = sapply(filtered, function(x) x$user),
        Date = sapply(filtered, function(x) {
          tryCatch({
            if (!is.null(history$formatted_time)) {
              idx <- which(history$timestamp == x$date)
              if (length(idx) > 0) history$formatted_time[idx[1]] else as.character(x$date)
            } else {
              as.character(x$date)
            }
          }, error = function(e) {
            as.character(x$date)
          })
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
              style = "margin-top: 0;",
              if (row$female_status != "Deceased" && row$plugging_status != "Deleted" && is_active_status(row$plugging_status)) {
                tagList(
                  # Show "Mark as Plugged" for Ongoing status
                  if (row$plugging_status == "Ongoing") {
                    actionButton("mark_plug_observed_btn", "Plugged", class = "btn-success")
                  },
                  # Show "Surprising Plug!! 😱" for Not Observed (Waiting for confirmation) status
                  if (row$plugging_status == "Not Observed (Waiting for confirmation)") {
                    actionButton("mark_surprising_plug_btn", "Surprising Plug!!😱", class = "btn-success", style = "background-color: #ff6b6b; border-color: #ff6b6b;")
                  },
                  # Show "Euthanized" for all active statuses (always allowed)
                  actionButton("euthanize_mice_btn", "Euthanized", class = "btn-warning"),
                  # Show "Empty Plug" for statuses that can be marked as empty (always allowed)
                  if (can_mark_empty(row$plugging_status)) {
                    actionButton("set_status_empty_btn", "Empty Plug (Alive)", class = "btn-info")
                  }
                )
              }
            ),
            div(
              actionButton("edit_plugging_details_btn", "Edit", class = "btn-primary", style = "margin-left: 6px;"),
              # Only hide the Delete button when locked, allow other actions
              if (!is_not_observed_confirmed(row$plugging_status) && row$female_status != "Deceased" && row$plugging_status != "Deleted" && row$plugging_status != "Not Observed (Waiting for confirmation)" && !is_system_locked()) {
                actionButton("delete_plugging_btn", "Delete", class = "btn-danger")
              },
              modalButton("Cancel")
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
      
      # Get available LIVE mice for dropdown
      live_mice_data <- get_live_mice()
      
      # Get the full details of the mice currently in the record,
      # as they might be deceased and not in the live list.
      con_mice <- db_connect()
      current_male <- dbGetQuery(con_mice, "SELECT * FROM mice_stock WHERE asu_id = ?", params = list(row$male_id))
      current_female <- dbGetQuery(con_mice, "SELECT * FROM mice_stock WHERE asu_id = ?", params = list(row$female_id))
      db_disconnect(con_mice)

      # --- Male Choices ---
      # Combine live males with the current male if it's not already in the list
      all_males <- live_mice_data$males
      if (nrow(current_male) > 0 && !(current_male$asu_id[1] %in% all_males$asu_id)) {
        # Ensure columns match before rbind
        if (nrow(all_males) > 0) {
          all_males <- rbind(current_male[, colnames(all_males)], all_males)
        } else {
          all_males <- current_male
        }
      }
      male_choices <- setNames(all_males$asu_id, 
                              paste(all_males$asu_id, "-", all_males$breeding_line, "(", all_males$genotype, ")"))

      # --- Female Choices ---
      # Combine live females with the current female if it's not already in the list
      all_females <- live_mice_data$females
      if (nrow(current_female) > 0 && !(current_female$asu_id[1] %in% all_females$asu_id)) {
        # Ensure columns match before rbind
        if (nrow(all_females) > 0) {
          all_females <- rbind(current_female[, colnames(all_females)], all_females)
        } else {
          all_females <- current_female
        }
      }
      female_choices <- setNames(all_females$asu_id, 
                                paste(all_females$asu_id, "-", all_females$breeding_line, "(", all_females$genotype, ")"))
      
      # Close the details modal first
      removeModal()
      
      # Show edit modal
      showModal(modalDialog(
        title = paste("Edit Plugging Event - Female: ", row$female_id),
        size = "l",
        tagList(
          div(
            style = "background-color: #e3f2fd; border-left: 4px solid #2196f3; padding: 10px; margin-bottom: 15px; border-radius: 4px;",
            tags$strong("Note: "), 
            "This edit modal is for the specific plugging event with female ", tags$code(row$female_id), 
            ". For trio mating, each female has a separate plugging record."
          ),
          fluidRow(
            column(6, selectInput("edit_plugging_female", "Female ID", choices = female_choices, selected = row$female_id)),
            column(6, selectInput("edit_plugging_male", "Male ID", choices = male_choices, selected = row$male_id))
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
                div(style = "margin-top: -10px;", dateInput("edit_plug_observed_date", "Plug Observed Date", 
                          value = if(is_valid_plug_date(row$plug_observed_date)) as.Date(row$plug_observed_date) else Sys.Date()))
              )
            ),
            column(6, selectInput("edit_plugging_status", "Plugging Status", 
                                 choices = PLUGGING_STATUSES, selected = row$plugging_status)),
            column(6, textInput("edit_expected_age_for_harvesting", "Expected Age for Harvesting (Embryonic Days, e.g. 14)", value = if(!is.null(row$expected_age_for_harvesting) && !is.na(row$expected_age_for_harvesting)) row$expected_age_for_harvesting else "", width = "100%"))
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
  
  # Helper function to perform the actual save operation, callable from multiple places
  perform_plugging_update <- function(plugging_id, update_data) {
    con <- db_connect()
    tryCatch({
      current <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      if (nrow(current) == 0) {
        showNotification("Plugging event not found", type = "error")
        return()
      }
      old_values <- current[1, ]
      
      result <- DBI::dbExecute(con, 
        "UPDATE plugging_history SET 
         male_id = ?, female_id = ?, pairing_start_date = ?, pairing_end_date = ?,
         plug_observed_date = ?, plugging_status = ?, expected_age_for_harvesting = ?,
         notes = ?, updated_at = DATETIME('now')
         WHERE id = ?",
        params = list(
          update_data$male_id, update_data$female_id, update_data$pairing_start_date,
          update_data$pairing_end_date, update_data$plug_observed_date, update_data$plugging_status,
          update_data$expected_age_for_harvesting, update_data$notes, plugging_id
        )
      )
      
      if (result > 0) {
        log_audit_trail("plugging_history", plugging_id, "UPDATE", old_values, update_data)
        showNotification("Plugging event updated successfully", type = "message")
        removeModal() # Close confirmation modal if open
        removeModal() # Close edit modal
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
  }

  # Save plugging edit - Step 1: Validate and check for ID changes
  observeEvent(input$save_plugging_edit_btn, {
    plugging_id <- plugging_state$editing_id
    if (is.null(plugging_id)) return()
    
    # Validation
    if (is.null(input$edit_plugging_male) || input$edit_plugging_male == "" || is.null(input$edit_plugging_female) || input$edit_plugging_female == "") {
      showNotification("Please select both a male and a female mouse.", type = "error")
      return()
    }
    if (input$edit_plugging_male == input$edit_plugging_female) {
      showNotification("Male and female cannot be the same mouse", type = "error")
      return()
    }
    
    con <- db_connect()
    original_record <- tryCatch({
      DBI::dbGetQuery(con, "SELECT male_id, female_id FROM plugging_history WHERE id = ?", params = list(plugging_id))
    }, finally = {
      db_disconnect(con)
    })
    
    if (nrow(original_record) == 0) {
      showNotification("Original record not found.", type = "error")
      return()
    }

    id_changed <- (original_record$male_id[1] != input$edit_plugging_male) || (original_record$female_id[1] != input$edit_plugging_female)
    
    plug_observed_date_value <- if(input$edit_plug_observed_type == "unknown") "Unknown" else as.character(input$edit_plug_observed_date)

    update_data <- list(
      male_id = input$edit_plugging_male,
      female_id = input$edit_plugging_female,
      pairing_start_date = as.character(input$edit_pairing_start_date),
      pairing_end_date = as.character(input$edit_pairing_end_date),
      plug_observed_date = plug_observed_date_value,
      plugging_status = input$edit_plugging_status,
      expected_age_for_harvesting = input$edit_expected_age_for_harvesting,
      notes = input$edit_plugging_notes
    )

    if (id_changed) {
      plugging_state$pending_edit_data <- update_data
      
      # Build a more informative warning message
      changes <- list()
      if (original_record$female_id[1] != input$edit_plugging_female) {
        changes <- c(changes, list(tags$li(HTML(paste0("<b>Female ID:</b> ", original_record$female_id[1], " → ", input$edit_plugging_female)))))
      }
      if (original_record$male_id[1] != input$edit_plugging_male) {
        changes <- c(changes, list(tags$li(HTML(paste0("<b>Male ID:</b> ", original_record$male_id[1], " → ", input$edit_plugging_male)))))
      }
      
      warning_message <- tagList(
        p("You are about to change the mouse ID(s) for this record. This is a significant change. Please confirm the changes below:"),
        tags$ul(changes),
        p("Are you sure you want to proceed?")
      )
      
      showModal(modalDialog(
        title = "Confirm Mouse ID Change",
        warning_message,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_id_change_and_save_btn", "Confirm and Save", class = "btn-danger")
        )
      ))
    } else {
      perform_plugging_update(plugging_id, update_data)
    }
  })

  # Save plugging edit - Step 2: Final confirmation after ID change warning
  observeEvent(input$confirm_id_change_and_save_btn, {
    plugging_id <- plugging_state$editing_id
    update_data <- plugging_state$pending_edit_data
    
    if (is.null(plugging_id) || is.null(update_data)) {
      showNotification("Could not save. Required information is missing.", type = "error")
      return()
    }
    
    perform_plugging_update(plugging_id, update_data)
    plugging_state$pending_edit_data <- NULL # Clean up
  })

  # Mark plug as observed
  observeEvent(input$mark_plug_observed_btn, {
    plugging_id <- plugging_state$viewing_id
    if (is.null(plugging_id)) return()
    
    # Get current notes for the modal
    con <- db_connect()
    current_notes <- ""
    tryCatch({
      current_record <- DBI::dbGetQuery(con, "SELECT notes FROM plugging_history WHERE id = ?", params = list(plugging_id))
      if (nrow(current_record) > 0) {
        current_notes <- ifelse(is.na(current_record$notes) || current_record$notes == "", "", current_record$notes)
      }
    }, finally = {
      db_disconnect(con)
    })
    
    showModal(modalDialog(
      title = "Mark Plug as Observed",
      size = "m",
      tagList(
        fluidRow(
          column(6,
            div(style = "margin-bottom: 0px;",
              tags$h5("Plug Observed Date"),
              radioButtons("plug_observed_type", NULL, 
                choices = c("Specific Date" = "date", "Unknown" = "unknown"),
                selected = "date"
              ),
              conditionalPanel(style = "margin-top: -10px;",
                condition = "input.plug_observed_type == 'date'",
                dateInput("plug_observed_date_input", "", value = Sys.Date(), width = "100%")
              )
            ),
            div(style = "margin-bottom: 12px;",
              tags$h5("Expected Age for Harvesting (E. days)"),
              textInput("expected_age_for_harvesting_input", NULL, value = "", width = "100%")
            )
          ),
          column(6,
            div(style = "margin-bottom: 12px;",
              tags$h5("Notes"),
              textAreaInput("plug_observed_notes_input", NULL, value = current_notes, rows = 5, width = "100%")
            ),
            div(style = "margin-top: 18px; color: #888; font-size: 0.98em;",
              tags$p("This will update the plugging status to 'Plugged' and set the plug observed date.")
            )
          )
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
        title = "Confirm Plug Status for Euthanasia",
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
              textAreaInput("euthanasia_notes_input", "Notes", value = ifelse(is.na(plugging$notes) || plugging$notes == "", "", plugging$notes), rows = 2),
              radioButtons("euthanasia_status_choice", "Plugging Status after Euthanasia:",
                choices = c("Empty" = "Empty", "Sample Collected" = "Collected"),
                selected = "Collected"
              )
            )
          ),
          div(
            style = "text-align: center; margin-top: 15px;",
            tags$p(tags$i("Are you sure you want to mark this female mouse as deceased? This action cannot be undone."))
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_euthanasia_btn", "Confirm", class = "btn-danger")
        )
      ))
      
    }, finally = {
      db_disconnect(con)
    })
  })

  # Confirm euthanasia (updated logic)
  observeEvent(input$confirm_euthanasia_btn, {
    plugging_id <- plugging_state$viewing_id
    if (is.null(plugging_id)) return()
    selected_status <- input$euthanasia_status_choice
    if (is.null(selected_status)) return()
    
    con <- db_connect()
    tryCatch({
      plugging <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      if (nrow(plugging) == 0) return()
      row <- plugging[1, ]
      # Always update mouse to Deceased
      result <- DBI::dbExecute(con, 
        "UPDATE mice_stock SET \
         status = 'Deceased',\
         date_of_death = ?,\
         deceased_timestamp = DATETIME('now'),\
         last_updated = DATETIME('now')\
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
            source = "Plugging Tab",
            source_event_id = plugging_id
          )
        )
        # Update plugging status to selected value
        con2 <- db_connect()
        old_plug <- DBI::dbGetQuery(con2, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
        DBI::dbExecute(con2, 
          "UPDATE plugging_history SET \
           plugging_status = ?,\
           updated_at = DATETIME('now'),\
           notes = CASE \
             WHEN notes IS NULL OR notes = '' THEN ?\
             ELSE notes || '\n' || ?\
           END\
           WHERE id = ?",
          params = list(
            selected_status,
            input$euthanasia_notes_input,
            input$euthanasia_notes_input,
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
            plugging_status = selected_status,
            completion_date = as.character(input$euthanasia_date_input),
            notes = input$euthanasia_notes_input
          )
        )
        db_disconnect(con2)
        showNotification("Female mouse marked as deceased and plugging status updated!", type = "message")
        removeModal()
        plugging_state$viewing_id <- NULL
        plugging_state$reload <- Sys.time()
        global_refresh_trigger(Sys.time())
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
    
    # Get current plugging record to determine available options
    con <- db_connect()
    current_record <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
    db_disconnect(con)
    
    if (nrow(current_record) == 0) {
      showNotification("Plugging record not found", type = "error")
      return()
    }
    
    current_status <- current_record$plugging_status[1]
    
    # Define available status options based on current status
    status_choices <- switch(current_status,
      "Ongoing" = c(
        "Plugged" = "Plugged",
        "Not Observed (Waiting for confirmation)" = "Not Observed (Waiting for confirmation)"
        #"Not Pregnant" = "Not Pregnant"
      ),
      "Plugged" = c(
        "Plug Confirmed" = "Plug Confirmed",
        "Not Pregnant" = "Not Pregnant",
        "Empty Plug! (Euthanized) 🔴" = "Empty",
        "Empty Plug (Alive) 🟢" = "Empty_Alive_UI",
        "Sample Collected" = "Collected"
      ),
      "Plug Confirmed" = c(
        "Not Pregnant" = "Not Pregnant",
        "Empty Plug! (Euthanized) 🔴" = "Empty",
        "Empty Plug (Alive) 🟢" = "Empty_Alive_UI",
        "Sample Collected" = "Collected"
      ),
      "Not Observed (Waiting for confirmation)" = c(
        "Not Observed (Confirmed)" = "Not Observed (Confirmed)",
        "Not Pregnant" = "Not Pregnant",
        "Plugged (Report Delayed)" = "Plugged",
        "Surprising Plug!! 😱" = "Surprising Plug!!",
        #"Empty Plug! (Euthanized) 🔴" = "Empty",
        "Sample Collected" = "Collected"
      ),
      "Not Observed (Confirmed)" = c(
        "Surprising Plug!! 😱" = "Surprising Plug!!",
        "Empty Plug! (Euthanized) 🔴" = "Empty",
        "Empty Plug (Alive) 🟢" = "Empty_Alive_UI",
        "Sample Collected" = "Collected"
      ),
      "Surprising Plug!!" = c(
        "Empty Plug! (Euthanized) 🔴" = "Empty",
        "Empty Plug (Alive) 🟢" = "Empty_Alive_UI",
        "Sample Collected" = "Collected"
      ),
      # Default options for any other active status
      c(
        "Plug Confirmed" = "Plug Confirmed",
        "Not Pregnant" = "Not Pregnant",
        "Surprising Plug!!" = "Surprising Plug!!",
        "Not Observed (Confirmed)" = "Not Observed (Confirmed)",
        "Empty Plug! (Euthanized) 🔴" = "Empty",
        "Empty Plug (Alive) 🟢" = "Empty_Alive_UI",
        "Sample Collected" = "Collected"
      )
    )
    
    # Get current notes, expected age, and plug observed date for the modal
    con <- db_connect()
    current_notes <- ""
    current_expected_age <- ""
    current_plug_observed_date <- ""
    tryCatch({
      current_record <- DBI::dbGetQuery(con, "SELECT notes, expected_age_for_harvesting, plug_observed_date FROM plugging_history WHERE id = ?", params = list(plugging_id))
      if (nrow(current_record) > 0) {
        current_notes <- ifelse(is.na(current_record$notes) || current_record$notes == "", "", current_record$notes)
        current_expected_age <- ifelse(is.na(current_record$expected_age_for_harvesting) || current_record$expected_age_for_harvesting == "", "", current_record$expected_age_for_harvesting)
        current_plug_observed_date <- ifelse(is.na(current_record$plug_observed_date) || current_record$plug_observed_date == "", "", current_record$plug_observed_date)
      }
    }, finally = {
      db_disconnect(con)
    })
    
    # Determine default radio selection for plug observed date
    plug_observed_type_default <- if (current_plug_observed_date == "Unknown" || current_plug_observed_date == "") "unknown" else "date"
    plug_observed_date_value <- if (plug_observed_type_default == "date") current_plug_observed_date else as.character(Sys.Date())
    
    # Show confirmation modal with status options
    showModal(modalDialog(
      title = paste("Update Plugging Status - Current:", current_status),
      size = "m",
      tagList(
        div(
          style = "margin-bottom: 15px;",
          tags$strong("Please select the new status for this plugging record:")
        ),
        div(
          style = "margin-bottom: 10px;",
          radioButtons("confirm_status_choice", "Status Options:",
                      choices = status_choices,
                      selected = names(status_choices)[1],
                      width = NULL,
                      inline = FALSE
          ),
          # Show expected age and plug observed date input only if Plugged (Report Delayed) is selected
          conditionalPanel(
            condition = "input.confirm_status_choice == 'Plugged'",
            tagList(
              radioButtons("confirm_plug_observed_type", "Plug Observed Date Type", 
                choices = c("Specific Date" = "date", "Unknown" = "unknown"),
                selected = plug_observed_type_default
              ),
              conditionalPanel(
                condition = "input.confirm_plug_observed_type == 'date'",
                dateInput("confirm_plug_observed_date", "Plug Observed Date", value = plug_observed_date_value, width = "100%")
              ),
              textInput("confirm_expected_age_for_harvesting", "Expected Age for Harvesting (Embryonic Days, e.g. 14)", value = current_expected_age, width = "100%")
            )
          )
        ),
        div(
          style = "margin-bottom: 10px;",
          textAreaInput("quick_status_notes_input", "Notes", value = current_notes, rows = 3, width = "100%")
        ),
        div(
          style = "font-size: 12px; color: #555; margin-bottom: 10px;",
          tags$em(
            tags$strong("Not Pregnant:"), " False pregnant without Euthanizing.", tags$br(),
            tags$strong("Empty Plug:"), " Euthanized without embryos, or Alive",tags$br(),
            tags$strong("Sample Collected:"), " Euthanized with embryos."
          )
        ),
        div(
          style = "background-color: #e3f2fd; border: 1px solid #2196f3; padding: 10px; border-radius: 5px;",
          tags$strong("Note:"), "This will update the plugging record status."
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_status_btn", "Update Status", class = "btn-primary")
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
    
    # Helper to open the correct modal for statuses that require extra info
    open_status_detail_modal <- function(status) {
      plugging_id <- plugging_state$confirming_id
      con <- db_connect()
      plugging_row <- NULL
      female_info <- NULL
      female_age <- NA
      if (!is.null(plugging_id)) {
        plugging_row <- tryCatch({
          DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
        }, error = function(e) NULL)
        if (!is.null(plugging_row) && nrow(plugging_row) > 0) {
          female_id <- plugging_row$female_id[1]
          female_info <- tryCatch({
            DBI::dbGetQuery(con, "SELECT * FROM mice_stock WHERE asu_id = ?", params = list(female_id))
          }, error = function(e) NULL)
          if (!is.null(female_info) && nrow(female_info) > 0) {
            dob <- female_info$dob[1]
            if (!is.na(dob) && dob != "") {
              female_age <- floor(as.numeric(Sys.Date() - as.Date(dob)) / 7)
            }
          }
        }
      }
      db_disconnect(con)
      if (status == "Plugged") {
        # Get current notes for the modal
        current_notes <- ""
        if (!is.null(plugging_row) && nrow(plugging_row) > 0) {
          current_notes <- ifelse(is.na(plugging_row$notes) || plugging_row$notes == "", "", plugging_row$notes)
        }
        
        showModal(modalDialog(
          title = "Mark Plug as Observed",
          size = "m",
          tagList(
            fluidRow(
              column(6,
                div(style = "margin-bottom: 12px;",
                  tags$h5("Plug Observed Date"),
                  radioButtons("plug_observed_type", NULL, 
                    choices = c("Specific Date" = "date", "Unknown" = "unknown"),
                    selected = "date"
                  ),
                  conditionalPanel(style = "margin-top: -10px;",
                    condition = "input.plug_observed_type == 'date'",
                    dateInput("plug_observed_date_input", "", value = Sys.Date(), width = "100%")
                  )
                ),
                div(style = "margin-bottom: 12px;",
                  tags$h5("Expected Age for Harvesting (Embryonic Days)"),
                  textInput("expected_age_for_harvesting_input", NULL, value = "", width = "100%")
                )
              ),
              column(6,
                div(style = "margin-bottom: 12px;",
                  tags$h5("Notes"),
                  textAreaInput("plug_observed_notes_input", NULL, value = current_notes, rows = 5, width = "100%")
                ),
                div(style = "margin-top: 18px; color: #888; font-size: 0.98em;",
                  tags$p("This will update the plugging status to 'Plugged' and set the plug observed date.")
                )
              )
            )
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_plug_observed_btn", "Confirm", class = "btn-success")
          )
        ))
      } else if (status == "Surprising Plug!!") {
        # Get current notes for the modal
        current_notes <- ""
        if (!is.null(plugging_row) && nrow(plugging_row) > 0) {
          current_notes <- ifelse(is.na(plugging_row$notes) || plugging_row$notes == "", "", plugging_row$notes)
        }
        
        showModal(modalDialog(
          title = "Mark as Surprising Plug!!😱😮‍💨",
          size = "m",
          tagList(
            div(
              style = "text-align: center; padding: 20px;",
              tags$p("This will set the plug observed date to 'Unknown' and estimate the plug date using the pairing start date for calendar purposes."),
              br(),
              textInput("expected_age_for_harvesting_surprising_input_quick", "Expected Age for Harvesting (Embryonic Days, e.g. 14)", value = "", width = "100%"),
              br(),
              textAreaInput("surprising_plug_notes_input_quick", "Notes", value = current_notes, rows = 3, width = "100%"),
              br(),
              tags$p("This will update the plugging status to 'Surprising Plug!!' and set the plug observed date to 'Unknown'.")
            )
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_surprising_plug_btn_quick", "Confirm Surprising Plug!!", class = "btn-success")
          )
        ))
      } else if (status == "Empty_Alive_UI") {
        # Get current notes for the modal
        current_notes <- ""
        if (!is.null(plugging_row) && nrow(plugging_row) > 0) {
          current_notes <- ifelse(is.na(plugging_row$notes) || plugging_row$notes == "", "", plugging_row$notes)
        }
        
        showModal(show_empty_alive_modal(
          female_info, female_age, Sys.Date(), current_notes, 
          "quick_empty_alive_date_input", "confirm_quick_empty_alive_btn"
        ))
      } else if (status %in% c("Empty", "Collected")) {
        # Get current notes for the modal
        current_notes <- ""
        if (!is.null(plugging_row) && nrow(plugging_row) > 0) {
          current_notes <- ifelse(is.na(plugging_row$notes) || plugging_row$notes == "", "", plugging_row$notes)
        }
        
        showModal(modalDialog(
          title = ifelse(status == "Empty", "Confirm Set Status to Empty (Euthanized)", "Confirm Sample Collected (Euthanized)"),
          size = "m",
          tagList(
            fluidRow(
              column(6, 
                wellPanel(
                  tags$h4("Female Mouse Information"),
                  tags$b("ASU ID:"), if(!is.null(female_info) && nrow(female_info) > 0) female_info$asu_id[1] else "N/A", br(),
                  tags$b("Age (weeks):"), if(!is.na(female_age)) female_age else "N/A", br(),
                  tags$b("Breeding Line:"), if(!is.null(female_info) && nrow(female_info) > 0) female_info$breeding_line[1] else "N/A", br(),
                  tags$b("Genotype:"), if(!is.null(female_info) && nrow(female_info) > 0) female_info$genotype[1] else "N/A"
                )
              ),
              column(6,
                dateInput("quick_euthanasia_date_input", "Date of Death", value = Sys.Date()),
                textAreaInput("quick_euthanasia_notes_input", "Notes", value = current_notes, rows = 2)
              )
            ),
            div(
              style = "text-align: center; margin-top: 15px;",
              tags$p(tags$i("Are you sure you want to mark this female mouse as deceased? This action cannot be undone."))
            )
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_quick_euthanasia_btn", "Confirm", class = "btn-danger")
          )
        ))
      }
    }

    # Determine if the selected status requires extra info
    if (selected_status %in% c("Plugged", "Surprising Plug!!", "Euthanized", "Empty", "Empty_Alive_UI", "Collected")) {
      # For Plugged, if coming from quick confirm, update expected_age_for_harvesting and plug_observed_date as well
      if (selected_status == "Plugged" && (!is.null(input$confirm_expected_age_for_harvesting) || !is.null(input$confirm_plug_observed_type))) {
        con <- db_connect()
        tryCatch({
          current <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
          if (nrow(current) == 0) {
            showNotification("Plugging event not found", type = "error")
            return()
          }
          old_values <- current[1, ]
          notes_input <- if (is.null(input$quick_status_notes_input) || length(input$quick_status_notes_input) == 0) {
            ""
          } else {
            as.character(input$quick_status_notes_input)
          }
          expected_age <- input$confirm_expected_age_for_harvesting
          plug_observed_date_value <- if (input$confirm_plug_observed_type == "unknown") {
            "Unknown"
          } else {
            as.character(input$confirm_plug_observed_date)
          }
          result <- DBI::dbExecute(con, 
            "UPDATE plugging_history SET plugging_status = ?, expected_age_for_harvesting = ?, plug_observed_date = ?, updated_at = DATETIME('now'), notes = CASE WHEN notes IS NULL OR notes = '' THEN ? ELSE notes || '\n' || ? END WHERE id = ?",
            params = list(
              selected_status,
              expected_age,
              plug_observed_date_value,
              notes_input,
              notes_input,
              plugging_id
            )
          )
          log_audit_trail(
            "plugging_history",
            plugging_id,
            "UPDATE",
            old_values,
            list(
              plugging_status = selected_status,
              expected_age_for_harvesting = expected_age,
              plug_observed_date = plug_observed_date_value,
              confirmation_date = as.character(Sys.Date()),
              notes = notes_input
            )
          )
          showNotification(paste("Plugging status updated to '", selected_status, "' successfully!", sep = ""), type = "message")
          removeModal()
          plugging_state$confirming_id <- NULL
          Sys.sleep(1)
          auto_update_plugging_status_to_unknown()
          plugging_state$reload <- Sys.time()
          if (!is.null(global_refresh_trigger)) {
            global_refresh_trigger(Sys.time())
          }
          invalidateLater(100, session)
        }, error = function(e) {
          showNotification(paste("Error updating plugging status:", e$message), type = "error")
        }, finally = {
          db_disconnect(con)
        })
        return()
      }
      open_status_detail_modal(selected_status)
      return()
    }

    # For other statuses, update immediately as before
    con <- db_connect()
    tryCatch({
      current <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      if (nrow(current) == 0) {
        showNotification("Plugging event not found", type = "error")
        return()
      }
      old_values <- current[1, ]
      # Handle notes input safely
      notes_input <- if (is.null(input$quick_status_notes_input) || length(input$quick_status_notes_input) == 0) {
        ""
      } else {
        as.character(input$quick_status_notes_input)
      }
      
      update_query <- "UPDATE plugging_history SET plugging_status = ?, updated_at = DATETIME('now'), notes = CASE WHEN notes IS NULL OR notes = '' THEN ? ELSE notes || '\n' || ? END WHERE id = ?"
      update_params <- list(
        selected_status,
        notes_input,
        notes_input,
        plugging_id
      )
      result <- DBI::dbExecute(con, update_query, params = update_params)
      if (result > 0) {
        # If status is set to Collected, also set female mouse to Deceased
        if (selected_status == "Collected") {
          female_id <- old_values$female_id
          DBI::dbExecute(con, 
            "UPDATE mice_stock SET status = 'Deceased', date_of_death = ?, deceased_timestamp = DATETIME('now'), last_updated = DATETIME('now') WHERE asu_id = ? AND status != 'Deceased'",
            params = list(as.character(Sys.Date()), female_id)
          )
        }
        log_audit_trail(
          "plugging_history",
          plugging_id,
          "UPDATE",
          old_values,
          list(
            plugging_status = selected_status,
            confirmation_date = as.character(Sys.Date()),
            notes = notes_input
          )
        )
        showNotification(paste("Plugging status updated to '", selected_status, "' successfully!", sep = ""), type = "message")
        removeModal()
        plugging_state$confirming_id <- NULL
        Sys.sleep(1)
        auto_update_plugging_status_to_unknown()
        plugging_state$reload <- Sys.time()
        if (!is.null(global_refresh_trigger)) {
          global_refresh_trigger(Sys.time())
        }
        invalidateLater(100, session)
      } else {
        showNotification("Failed to update plugging status", type = "error")
      }
    }, error = function(e) {
      showNotification(paste("Error updating plugging status:", e$message), type = "error")
    }, finally = {
      db_disconnect(con)
    })
  })
  
  # Set plugging status to Empty (Alive) - using unified modal
  observeEvent(input$set_status_empty_btn, {
    plugging_id <- plugging_state$viewing_id
    if (is.null(plugging_id)) return()
    con <- db_connect()
    tryCatch({
      plugging_row <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      if (nrow(plugging_row) == 0) return()
      female_id <- plugging_row$female_id[1]
      female_info <- DBI::dbGetQuery(con, "SELECT * FROM mice_stock WHERE asu_id = ?", params = list(female_id))
      female_age <- NA
      if (!is.null(female_info) && nrow(female_info) > 0) {
        dob <- female_info$dob[1]
        if (!is.na(dob) && dob != "") {
          female_age <- floor(as.numeric(Sys.Date() - as.Date(dob)) / 7)
        }
      }
      # Get current notes for the modal
      current_notes <- ifelse(is.na(plugging_row$notes) || plugging_row$notes == "", "", plugging_row$notes)
      showModal(show_empty_alive_modal(female_info, female_age, Sys.Date(), current_notes, "set_status_empty_date_input", "confirm_set_status_empty_btn"))
    }, finally = {
      db_disconnect(con)
    })
  })
  
  # Confirm set status empty
  observeEvent(input$confirm_set_status_empty_btn, {
    plugging_id <- plugging_state$viewing_id
    if (is.null(plugging_id)) return()
    date_of_death <- input$set_status_empty_date_input
    notes <- input$set_status_empty_notes_input
    
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
        "UPDATE plugging_history SET \
         plugging_status = 'Empty',\
         updated_at = DATETIME('now'),\
         notes = CASE \
           WHEN notes IS NULL OR notes = '' THEN ?\
           ELSE notes || '\n' || ?\
         END\
         WHERE id = ?",
        params = list(
          notes,
          notes,
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
            confirmation_date = as.character(date_of_death),
            notes = notes
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
            notes = NULL
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

  # Mark plug as Surprising Plug!!
  observeEvent(input$mark_surprising_plug_btn, {
    plugging_id <- plugging_state$viewing_id
    if (is.null(plugging_id)) return()
    
    # Get current notes for the modal
    con <- db_connect()
    current_notes <- ""
    tryCatch({
      current_record <- DBI::dbGetQuery(con, "SELECT notes FROM plugging_history WHERE id = ?", params = list(plugging_id))
      if (nrow(current_record) > 0) {
        current_notes <- ifelse(is.na(current_record$notes) || current_record$notes == "", "", current_record$notes)
      }
    }, finally = {
      db_disconnect(con)
    })
    
    showModal(modalDialog(
      title = "Mark as Surprising Plug!!😱😮‍💨",
      size = "m",
      tagList(
        div(
          style = "text-align: center; padding: 20px;",
          tags$p("This will set the plug observed date to 'Unknown' and estimate the plug date using the pairing start date for calendar purposes."),
          br(),
          textInput("expected_age_for_harvesting_surprising_input", "Expected Age for Harvesting (Embryonic Days, e.g. 14)", value = "", width = "100%"),
          br(),
          textAreaInput("surprising_plug_notes_input", "Notes", value = current_notes, rows = 3, width = "100%"),
          br(),
          tags$p("This will update the plugging status to 'Surprising Plug!!' and set the plug observed date to 'Unknown'.")
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_surprising_plug_btn", "Confirm Surprising Plug!!", class = "btn-success")
      )
    ))
  })
  
  # Confirm Surprising Plug!!
  observeEvent(input$confirm_surprising_plug_btn, {
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
         plug_observed_date = 'Unknown',
         plugging_status = 'Surprising Plug!!',
         expected_age_for_harvesting = ?,
         notes = CASE 
           WHEN notes IS NULL OR notes = '' THEN ?
           ELSE notes || '\n' || ?
         END,
         updated_at = DATETIME('now')
         WHERE id = ?",
        params = list(
          input$expected_age_for_harvesting_surprising_input,
          input$surprising_plug_notes_input,
          input$surprising_plug_notes_input,
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
            plug_observed_date = "Unknown",
            plugging_status = "Surprising Plug!!",
            expected_age_for_harvesting = input$expected_age_for_harvesting_surprising_input,
            notes = input$surprising_plug_notes_input
          )
        )
        
        showNotification("🎉 Marked as Surprising Plug!! successfully!", type = "message")
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

  # Remove the old modal logic and call the new module server
  add_plugging_modal_server(
    "add_plugging_modal",
    get_live_mice = get_live_mice,
    get_mouse_info = function(asu_id) {
      # Use unified function from validation.R (without status for modal display)
      get_mouse_info(asu_id, include_status = FALSE)
    },
    validate_mice_active_status = validate_mice_active_status,
    db_connect = db_connect,
    db_disconnect = db_disconnect,
    log_audit_trail = log_audit_trail,
    auto_update_plugging_status_to_unknown = auto_update_plugging_status_to_unknown,
    plugging_state = plugging_state,
    is_system_locked = is_system_locked,
    global_refresh_trigger = global_refresh_trigger
  )
  
  # Refresh button observer
  observeEvent(input$refresh_plugging_table_btn, {
    plugging_state$reload <- Sys.time()
    if (!is.null(global_refresh_trigger)) {
      global_refresh_trigger(Sys.time())
    }
    showNotification("Table refreshed!", type = "message")
  })

  # Quick update modal: Confirm Plugged
  observeEvent(input$confirm_plug_observed_btn_quick, {
    plugging_id <- plugging_state$confirming_id
    if (is.null(plugging_id)) return()
    con <- db_connect()
    tryCatch({
      current <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      if (nrow(current) == 0) {
        showNotification("Plugging event not found", type = "error")
        return()
      }
      old_values <- current[1, ]
      plug_observed_date_value <- if(input$plug_observed_type == "unknown") {
        "Unknown"
      } else {
        as.character(input$plug_observed_date_input)
      }
      result <- DBI::dbExecute(con, 
        "UPDATE plugging_history SET \
         plug_observed_date = ?,\
         plugging_status = 'Plugged',\
         expected_age_for_harvesting = ?,\
         notes = CASE \
           WHEN notes IS NULL OR notes = '' THEN ?\
           ELSE notes || '\n' || ?\
         END,\
         updated_at = DATETIME('now')\
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
        plugging_state$confirming_id <- NULL
        plugging_state$reload <- Sys.time()
        if (!is.null(global_refresh_trigger)) global_refresh_trigger(Sys.time())
      } else {
        showNotification("Failed to update plugging event", type = "error")
      }
    }, error = function(e) {
      showNotification(paste("Error updating plugging event:", e$message), type = "error")
    }, finally = {
      db_disconnect(con)
    })
  })

  # Quick update modal: Confirm Surprising Plug!!
  observeEvent(input$confirm_surprising_plug_btn_quick, {
    plugging_id <- plugging_state$confirming_id
    if (is.null(plugging_id)) return()
    con <- db_connect()
    tryCatch({
      current <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      if (nrow(current) == 0) {
        showNotification("Plugging event not found", type = "error")
        return()
      }
      old_values <- current[1, ]
      result <- DBI::dbExecute(con, 
        "UPDATE plugging_history SET \
         plug_observed_date = 'Unknown',\
         plugging_status = 'Surprising Plug!!',\
         expected_age_for_harvesting = ?,\
         notes = CASE \
           WHEN notes IS NULL OR notes = '' THEN ?\
           ELSE notes || '\n' || ?\
         END,\
         updated_at = DATETIME('now')\
         WHERE id = ?",
        params = list(
          input$expected_age_for_harvesting_surprising_input_quick,
          input$surprising_plug_notes_input_quick,
          input$surprising_plug_notes_input_quick,
          plugging_id
        )
      )
      if (result > 0) {
        log_audit_trail(
          "plugging_history",
          plugging_id,
          "UPDATE",
          old_values,
          list(
            plug_observed_date = "Unknown",
            plugging_status = "Surprising Plug!!",
            expected_age_for_harvesting = input$expected_age_for_harvesting_surprising_input_quick,
            notes = input$surprising_plug_notes_input_quick
          )
        )
        showNotification("🎉 Marked as Surprising Plug!! successfully!", type = "message")
        removeModal()
        plugging_state$confirming_id <- NULL
        plugging_state$reload <- Sys.time()
        if (!is.null(global_refresh_trigger)) global_refresh_trigger(Sys.time())
      } else {
        showNotification("Failed to update plugging event", type = "error")
      }
    }, error = function(e) {
      showNotification(paste("Error updating plugging event:", e$message), type = "error")
    }, finally = {
      db_disconnect(con)
    })
  })

  # Quick update modal: Confirm Euthanized
  observeEvent(input$confirm_euthanasia_btn_quick, {
    plugging_id <- plugging_state$confirming_id
    if (is.null(plugging_id)) return()
    con <- db_connect()
    tryCatch({
      plugging <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      if (nrow(plugging) == 0) return()
      row <- plugging[1, ]
      # Update female mouse status to Deceased
      result <- DBI::dbExecute(con, 
        "UPDATE mice_stock SET status = 'Deceased', date_of_death = ?, deceased_timestamp = DATETIME('now'), last_updated = DATETIME('now') WHERE asu_id = ?",
        params = list(
          as.character(input$euthanasia_date_input_quick),
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
            date_of_death = as.character(input$euthanasia_date_input_quick),
            notes = input$euthanasia_notes_input_quick,
            source = "Plugging Tab",
            source_event_id = plugging_id
          )
        )
        # Update plugging_status to selected value (Empty or Collected)
        selected_status <- input$euthanasia_status_choice
        DBI::dbExecute(con, 
          "UPDATE plugging_history SET plugging_status = ?, updated_at = DATETIME('now') WHERE id = ?",
          params = list(selected_status, plugging_id)
        )
        showNotification(paste("Female mouse marked as deceased and plug set to", selected_status, "!"), type = "message")
        removeModal()
        plugging_state$confirming_id <- NULL
        plugging_state$reload <- Sys.time()
        if (!is.null(global_refresh_trigger)) global_refresh_trigger(Sys.time())
      } else {
        showNotification("Failed to update female mouse status", type = "error")
      }
    }, error = function(e) {
      showNotification(paste("Error updating mouse status:", e$message), type = "error")
    }, finally = {
      db_disconnect(con)
    })
  })

  # Quick update modal: Confirm Empty Plug
  observeEvent(input$confirm_set_status_empty_btn_quick, {
    plugging_id <- plugging_state$confirming_id
    if (is.null(plugging_id)) return()
    notes <- input$set_status_empty_notes_input_quick
    con <- db_connect()
    tryCatch({
      current <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      if (nrow(current) == 0) {
        showNotification("Plugging event not found", type = "error")
        return()
      }
      old_values <- current[1, ]
      result <- DBI::dbExecute(con, 
        "UPDATE plugging_history SET \
         plugging_status = 'Empty',\
         updated_at = DATETIME('now'),\
         notes = CASE \
           WHEN notes IS NULL OR notes = '' THEN ?\
           ELSE notes || '\n' || ?\
         END\
         WHERE id = ?",
        params = list(
          notes,
          notes,
          plugging_id
        )
      )
      if (result > 0) {
        log_audit_trail(
          "plugging_history",
          plugging_id,
          "UPDATE",
          old_values,
          list(
            plugging_status = "Empty",
            confirmation_date = as.character(input$set_status_empty_date_input_quick),
            notes = notes
          )
        )
        showNotification("Plugging status set to Empty!", type = "message")
        removeModal()
        plugging_state$confirming_id <- NULL
        plugging_state$reload <- Sys.time()
        if (!is.null(global_refresh_trigger)) global_refresh_trigger(Sys.time())
      } else {
        showNotification("Failed to update plugging status", type = "error")
      }
    }, error = function(e) {
      showNotification(paste("Error updating plugging status:", e$message), type = "error")
    }, finally = {
      db_disconnect(con)
    })
  })

  # Add new observer for quick euthanasia/collected confirmation
  observeEvent(input$confirm_quick_euthanasia_btn, {
    plugging_id <- plugging_state$confirming_id
    if (is.null(plugging_id)) return()
    selected_status <- input$confirm_status_choice
    date_of_death <- input$quick_euthanasia_date_input
    notes <- input$quick_euthanasia_notes_input
    if (is.null(selected_status) || !(selected_status %in% c("Empty", "Collected"))) return()
    con <- db_connect()
    tryCatch({
      current <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      if (nrow(current) == 0) {
        showNotification("Plugging event not found", type = "error")
        return()
      }
      old_values <- current[1, ]
      # Update plugging status
      DBI::dbExecute(con, 
        "UPDATE plugging_history SET plugging_status = ?, updated_at = DATETIME('now'), notes = CASE WHEN notes IS NULL OR notes = '' THEN ? ELSE notes || '\n' || ? END WHERE id = ?",
        params = list(
          selected_status,
          paste0("[Status updated to '", selected_status, "' on ", as.character(date_of_death), "]"),
          paste0("[Status updated to '", selected_status, "' on ", as.character(date_of_death), "]"),
          plugging_id
        )
      )
      # Update female mouse to Deceased
      female_id <- old_values$female_id
      DBI::dbExecute(con, 
        "UPDATE mice_stock SET status = 'Deceased', date_of_death = ?, deceased_timestamp = DATETIME('now'), last_updated = DATETIME('now'), notes = CASE WHEN notes IS NULL OR notes = '' THEN ? ELSE notes || '\n' || ? END WHERE asu_id = ? AND status != 'Deceased'",
        params = list(
          as.character(date_of_death),
          notes,
          notes,
          female_id
        )
      )
      # Log to audit trail for female mouse status change
      log_audit_trail(
        "mice_stock",
        female_id,
        "UPDATE",
        list(status = "Alive"),
        list(
          status = "Deceased",
          date_of_death = as.character(date_of_death),
          notes = notes,
          source = "Plugging Tab",
          source_event_id = plugging_id
        )
      )
      log_audit_trail(
        "plugging_history",
        plugging_id,
        "UPDATE",
        old_values,
        list(
          plugging_status = selected_status,
          confirmation_date = as.character(date_of_death),
          notes = notes
        )
      )
      showNotification(paste("Plugging status updated to '", selected_status, "' and mouse marked as deceased!", sep = ""), type = "message")
      removeModal()
      plugging_state$confirming_id <- NULL
      Sys.sleep(1)
      auto_update_plugging_status_to_unknown()
      plugging_state$reload <- Sys.time()
      if (!is.null(global_refresh_trigger)) {
        global_refresh_trigger(Sys.time())
      }
      invalidateLater(100, session)
    }, error = function(e) {
      showNotification(paste("Error updating plugging status:", e$message), type = "error")
    }, finally = {
      db_disconnect(con)
    })
  })

  # Add new observer for quick Empty (Alive) confirmation
  observeEvent(input$confirm_quick_empty_alive_btn, {
    plugging_id <- plugging_state$confirming_id
    if (is.null(plugging_id)) return()
    date_of_confirmation <- input$quick_empty_alive_date_input
    notes <- input$quick_empty_alive_notes_input
    con <- db_connect()
    tryCatch({
      current <- DBI::dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", params = list(plugging_id))
      if (nrow(current) == 0) {
        showNotification("Plugging event not found", type = "error")
        return()
      }
      old_values <- current[1, ]
      # Update plugging status to Empty (but don't mark mouse as deceased)
      DBI::dbExecute(con, 
        "UPDATE plugging_history SET plugging_status = 'Empty', updated_at = DATETIME('now'), notes = CASE WHEN notes IS NULL OR notes = '' THEN ? ELSE notes || '\n' || ? END WHERE id = ?",
        params = list(
          paste0("[Status updated to 'Empty (Alive)' on ", as.character(date_of_confirmation), "]"),
          paste0("[Status updated to 'Empty (Alive)' on ", as.character(date_of_confirmation), "]"),
          plugging_id
        )
      )
      log_audit_trail(
        "plugging_history",
        plugging_id,
        "UPDATE",
        old_values,
        list(
          plugging_status = "Empty",
          confirmation_date = as.character(date_of_confirmation),
          notes = notes
        )
      )
      showNotification("Plugging status updated to 'Empty (Alive)' successfully!", type = "message")
      removeModal()
      plugging_state$confirming_id <- NULL
      Sys.sleep(1)
      auto_update_plugging_status_to_unknown()
      plugging_state$reload <- Sys.time()
      if (!is.null(global_refresh_trigger)) {
        global_refresh_trigger(Sys.time())
      }
      invalidateLater(100, session)
    }, error = function(e) {
      showNotification(paste("Error updating plugging status:", e$message), type = "error")
    }, finally = {
      db_disconnect(con)
    })
  })

} 

