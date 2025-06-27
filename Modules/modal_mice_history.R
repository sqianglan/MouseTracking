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
    display_breeding$`Start Date` <- format(as.Date(display_breeding$`Start Date`), "%Y-%m-%d")
    display_breeding$`End Date` <- ifelse(is.na(display_breeding$`End Date`), "Ongoing", format(as.Date(display_breeding$`End Date`), "%Y-%m-%d"))
    display_breeding$`Litter DOB` <- ifelse(is.na(display_breeding$`Litter DOB`), "N/A", format(as.Date(display_breeding$`Litter DOB`), "%Y-%m-%d"))
    
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
    display_plugging$`Pairing Start` <- format(as.Date(display_plugging$`Pairing Start`), "%Y-%m-%d")
    display_plugging$`Pairing End` <- ifelse(is.na(display_plugging$`Pairing End`), "Ongoing", format(as.Date(display_plugging$`Pairing End`), "%Y-%m-%d"))
    display_plugging$`Plug Date` <- ifelse(is.na(display_plugging$`Plug Date`), "N/A", format(as.Date(display_plugging$`Plug Date`), "%Y-%m-%d"))
    
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
          strong("Breeding Line:"), ifelse(is.na(mouse_info$breeding_line), "N/A", mouse_info$breeding_line), br(),
          strong("Genotype:"), ifelse(is.null(mouse_info$genotype) || is.na(mouse_info$genotype), "N/A", mouse_info$genotype), br(),
          strong("Transgene:"), ifelse(is.null(mouse_info$transgenes) || is.na(mouse_info$transgenes), "N/A", mouse_info$transgenes)
        ),
        div(
          strong("Date of Birth:"), mouse_info$dob, br(),
          strong("Status:"), mouse_info$status, br(),
          strong("Responsible Person:"), ifelse(is.na(mouse_info$responsible_person), "N/A", mouse_info$responsible_person)
        )
      )
    ),
    
    # Breeding History Section
    div(
      style = "margin-bottom: 30px;",
      h4("Breeding History", style = "color: #1976d2; border-bottom: 2px solid #1976d2; padding-bottom: 5px;"),
      if (nrow(breeding_history) > 0) {
        div(
          style = "overflow-x: auto;",
          HTML(breeding_table_html)
        )
      } else {
        div(
          style = "padding: 20px; text-align: center; color: #666; background-color: #f9f9f9; border-radius: 5px;",
          "No breeding history found for this mouse."
        )
      }
    ),
    
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