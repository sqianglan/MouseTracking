# modal_mice_history.R
# Function to show mouse history tracing modal

show_mouse_history_tracing <- function(input, output, session, asu_id, all_mice_table) {
  # Get mouse details
  current_data <- all_mice_table()
  mouse_info <- current_data[current_data$asu_id == asu_id, ]
  
  if (nrow(mouse_info) == 0) {
    showNotification("Mouse information not found.", type = "error", duration = 3)
    return()
  }
  
  # Fetch additional mouse details from database (including last_updated)
  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  
  # Get complete mouse information including last_updated
  mouse_details_query <- paste0(
    "SELECT asu_id, animal_id, gender, dob, breeding_line, genotype, responsible_person, stock_category, status, last_updated, notes\n     FROM mice_stock \n     WHERE asu_id = '", asu_id, "'"
  )
  
  mouse_details <- tryCatch({
    DBI::dbGetQuery(con, mouse_details_query)
  }, error = function(e) {
    # Fallback to current data if database query fails
    mouse_info
  })
  
  # Use database data if available, otherwise use current data
  if (nrow(mouse_details) > 0) {
    mouse_info <- mouse_details[1, ]
  }
  
  # Fetch breeding history
  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  
  # Get breeding history where this mouse is involved
  breeding_query <- paste0(
    "SELECT bh.*, 
            l.dob as litter_dob, l.num_pups, l.notes as litter_notes
     FROM breeding_history bh
     LEFT JOIN litter l ON bh.id = l.breeding_id
     WHERE bh.male_id = '", asu_id, "' OR bh.female1_id = '", asu_id, "' OR bh.female2_id = '", asu_id, "'
     ORDER BY bh.start_date DESC, l.dob DESC"
  )
  
  breeding_history <- tryCatch({
    DBI::dbGetQuery(con, breeding_query)
  }, error = function(e) {
    data.frame()
  })
  
  # Get plugging history where this mouse is involved
  plugging_query <- paste0(
    "SELECT * FROM plugging_history 
     WHERE (male_id = '", asu_id, "' OR female_id = '", asu_id, "')
     AND plugging_status != 'Deleted'
     ORDER BY pairing_start_date DESC"
  )
  
  plugging_history <- tryCatch({
    DBI::dbGetQuery(con, plugging_query)
  }, error = function(e) {
    data.frame()
  })
  
  DBI::dbDisconnect(con)
  
  # Create breeding history table HTML
  breeding_table_html <- ""
  if (nrow(breeding_history) > 0) {
    # Format breeding history for display (removed breeding_id and cage)
    display_breeding <- breeding_history[, c("male_id", "female1_id", "female2_id", "start_date", "end_date", "breeding_status", "litter_dob", "num_pups")]
    colnames(display_breeding) <- c("Male", "Female 1", "Female 2", "Start Date", "End Date", "Status", "Litter DOB", "Pups")
    
    # Format dates
    display_breeding$`Start Date` <- sapply(display_breeding$`Start Date`, function(date) {
      if (is.na(date) || date == "" || date == "Unknown") {
        "N/A"
      } else {
        tryCatch({
          format(as.Date(date), "%d-%b-%Y")
        }, error = function(e) {
          "N/A"
        })
      }
    })
    display_breeding$`End Date` <- sapply(display_breeding$`End Date`, function(date) {
      if (is.na(date) || date == "" || date == "Unknown") {
        "Ongoing"
      } else {
        tryCatch({
          format(as.Date(date), "%d-%b-%Y")
        }, error = function(e) {
          "Ongoing"
        })
      }
    })
    display_breeding$`Litter DOB` <- sapply(display_breeding$`Litter DOB`, function(date) {
      if (is.na(date) || date == "" || date == "Unknown") {
        "N/A"
      } else {
        tryCatch({
          format(as.Date(date), "%d-%b-%Y")
        }, error = function(e) {
          "N/A"
        })
      }
    })
    
    # Create HTML table rows
    table_rows <- apply(display_breeding, 1, function(row) {
      paste0('<tr>', paste0('<td>', row, '</td>', collapse = ''), '</tr>')
    })
    
    # Create HTML table
    breeding_table_html <- paste0(
      '<table class="table table-striped table-bordered table-hover" style="width: 100%;">',
      '<thead><tr>',
      paste0('<th>', colnames(display_breeding), '</th>', collapse = ''),
      '</tr></thead><tbody>',
      paste0(table_rows, collapse = ''),
      '</tbody></table>'
    )
  }
  
  # Create plugging history table HTML
  plugging_table_html <- ""
  if (nrow(plugging_history) > 0) {
    # Format plugging history for display (removed plugging_id and cage)
    display_plugging <- plugging_history[, c("male_id", "female_id", "pairing_start_date", "pairing_end_date", "plug_observed_date", "plugging_status")]
    colnames(display_plugging) <- c("Male", "Female", "Pairing Start", "Pairing End", "Plug Date", "Status")
    
    # Format dates
    display_plugging$`Pairing Start` <- sapply(display_plugging$`Pairing Start`, function(date) {
      if (is.na(date) || date == "" || date == "Unknown") {
        "N/A"
      } else {
        tryCatch({
          format(as.Date(date), "%d-%b-%Y")
        }, error = function(e) {
          "N/A"
        })
      }
    })
    display_plugging$`Pairing End` <- sapply(display_plugging$`Pairing End`, function(date) {
      if (is.na(date) || date == "" || date == "Unknown") {
        "Ongoing"
      } else {
        tryCatch({
          format(as.Date(date), "%d-%b-%Y")
        }, error = function(e) {
          "Ongoing"
        })
      }
    })
    display_plugging$`Plug Date` <- sapply(display_plugging$`Plug Date`, function(date) {
      if (is.na(date) || date == "" || date == "Unknown") {
        "N/A"
      } else {
        tryCatch({
          format(as.Date(date), "%d-%b-%Y")
        }, error = function(e) {
          "N/A"
        })
      }
    })
    
    # Create HTML table rows
    table_rows <- apply(display_plugging, 1, function(row) {
      paste0('<tr>', paste0('<td>', row, '</td>', collapse = ''), '</tr>')
    })
    
    # Create HTML table
    plugging_table_html <- paste0(
      '<table class="table table-striped table-bordered table-hover" style="width: 100%;">',
      '<thead><tr>',
      paste0('<th>', colnames(display_plugging), '</th>', collapse = ''),
      '</tr></thead><tbody>',
      paste0(table_rows, collapse = ''),
      '</tbody></table>'
    )
  }
  
  # Create modal content
  modal_content <- div(
    style = "max-height: 70vh; overflow-y: auto;",
    
    # Mouse information header
    div(
      style = "background-color: #f5f5f5; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
      #h4(paste("Mouse History Tracing:", asu_id)),
      div(
        style = "display: grid; grid-template-columns: 1fr 1fr; gap: 10px;",
        div(
          strong("Animal ID:"), mouse_info$animal_id, br(),
          strong("Gender:"), mouse_info$gender, br(),
          strong("Date of Birth:"), ifelse(is.na(mouse_info$dob) || mouse_info$dob == "", "N/A", mouse_info$dob), br(),
          strong("Genotype:"), ifelse(is.null(mouse_info$genotype) || is.na(mouse_info$genotype), "N/A", mouse_info$genotype), br(),
          strong("Breeding Line:"), ifelse(is.null(mouse_info$breeding_line) || is.na(mouse_info$breeding_line), "N/A", mouse_info$breeding_line)
        ),
        div(
          strong("Status:"), mouse_info$status, br(),
          strong("Responsible Person:"), ifelse(is.na(mouse_info$responsible_person), "N/A", mouse_info$responsible_person), br(),
          strong("Stock Category:"), ifelse(is.na(mouse_info$stock_category), "N/A", mouse_info$stock_category), br(),
          strong("Last Updated:"), ifelse(is.na(mouse_info$last_updated) || mouse_info$last_updated == "", "N/A", mouse_info$last_updated)
        )
      ),
      # Notes Section
      div(
        style = "margin: 10px 0 20px 0; padding: 10px; background: #f8f9fa; border-left: 4px solid #1976d2; border-radius: 6px; color: #333;",
        h4("Notes", style = "font-size: 1.1em; color: #1976d2; margin-bottom: 6px;"),
        if (!is.null(mouse_info$notes) && !is.na(mouse_info$notes) && mouse_info$notes != "") {
          div(style = "white-space: pre-line; font-size: 1em;", mouse_info$notes)
        } else {
          div(style = "color: #888; font-size: 0.95em; font-style: italic;", "No notes for this mouse.")
        }
      )
    ),
    
    # Breeding History Section
    # div(
    #   style = "margin-bottom: 30px;",
    #   h4("Breeding History", style = "color: #1976d2; border-bottom: 2px solid #1976d2; padding-bottom: 5px;"),
    #   if (nrow(breeding_history) > 0) {
    #     div(
    #       style = "overflow-x: auto;",
    #       HTML(breeding_table_html)
    #     )
    #   } else {
    #     div(
    #       style = "padding: 20px; text-align: center; color: #666; background-color: #f9f9f9; border-radius: 5px;",
    #       "No breeding history found for this mouse."
    #     )
    #   }
    # ),
    
    # Plugging History Section
    div(
      style = "margin-bottom: 20px;",
      h4("Plugging History", style = "color: #ff9800; border-bottom: 2px solid #ff9800; padding-bottom: 5px;"),
      if (nrow(plugging_history) > 0) {
        div(
          style = "overflow-x: auto;",
          HTML(plugging_table_html)
        )
      } else {
        div(
          style = "padding: 20px; text-align: center; color: #666; background-color: #f9f9f9; border-radius: 5px;",
          "No plugging history found for this mouse."
        )
      }
    )
  )
  
  # Show modal
  showModal(modalDialog(
    title = div(
      style = "font-size: 2rem; font-weight: bold;",
      paste("Mouse History Tracing - ", asu_id)
    ),
    size = "xl",
    modal_content,
    footer = modalButton("Close")
  ))
} 