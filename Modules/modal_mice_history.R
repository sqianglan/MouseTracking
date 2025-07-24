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
  
  # Use a single DB connection for all queries
  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
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
  
  # Get breeding history where this mouse is involved
  breeding_query <- paste0(
    "SELECT bh.*, 
            l.dob as litter_dob, l.num_pups, l.notes as litter_notes
     FROM breeding_history bh
     LEFT JOIN litter l ON bh.id = l.breeding_id
     WHERE bh.male_id = '", asu_id, "' OR bh.female1_id = '", asu_id, "' OR bh.female2_id = '", asu_id, "'\n     ORDER BY bh.start_date DESC, l.dob DESC"
  )
  
  breeding_history <- tryCatch({
    DBI::dbGetQuery(con, breeding_query)
  }, error = function(e) {
    data.frame()
  })
  
  # Get plugging history where this mouse is involved
  plugging_query <- paste0(
    "SELECT * FROM plugging_history \
     WHERE (male_id = '", asu_id, "' OR female_id = '", asu_id, "')\
     AND plugging_status != 'Deleted'"
  )
  
  plugging_history <- tryCatch({
    DBI::dbGetQuery(con, plugging_query)
  }, error = function(e) {
    data.frame()
  })
  
  # Sort plugging_history by updated_at (or last_updated/created_at) descending
  if (nrow(plugging_history) > 0) {
    if ("updated_at" %in% colnames(plugging_history)) {
      plugging_history <- plugging_history[order(as.POSIXct(plugging_history$updated_at, tz = "UTC"), decreasing = TRUE), ]
    } else if ("last_updated" %in% colnames(plugging_history)) {
      plugging_history <- plugging_history[order(as.POSIXct(plugging_history$last_updated, tz = "UTC"), decreasing = TRUE), ]
    } else if ("created_at" %in% colnames(plugging_history)) {
      plugging_history <- plugging_history[order(as.POSIXct(plugging_history$created_at, tz = "UTC"), decreasing = TRUE), ]
    }
  }
  
  # Get body weight history for this mouse
  body_weight_query <- paste0(
    "SELECT * FROM body_weight_history \
     WHERE asu_id = '", asu_id, "' \
     ORDER BY measurement_date DESC"
  )
  
  body_weight_history <- tryCatch({
    DBI::dbGetQuery(con, body_weight_query)
  }, error = function(e) {
    data.frame()
  })
  
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
    # Create simplified summary classification
    summary_classification <- sapply(plugging_history$plugging_status, function(status) {
      if (status == "Collected") {
        "Success"
      } else if (status == "Empty" || status == "Not Pregnant (Confirmed)" || status == "Not Pregnant") {
        "Failed"
      } else {
        "Pending"
      }
    })
    
    # Compute summary counts
    summary_counts <- table(factor(summary_classification, levels = c("Success", "Failed", "Pending")))
    
    # Create summary bar
    summary_bar <- div(
      style = "display: flex; justify-content: space-between; margin-bottom: 16px; font-size: 1.1em; align-items: center; width: 100%;",
      div(style = "background: #e8f5e9; border-radius: 10px; flex: 1; margin: 0 8px; min-height: 60px; display: flex; flex-direction: column; align-items: center; justify-content: center; box-shadow: 0 1px 4px #388e3c22; border: 2px solid #388e3c;",
        span(style = "font-weight: bold; color: #388e3c; font-size: 1.2em;", summary_counts[["Success"]]),
        span(style = "color: #388e3c; font-size: 0.95em;", "Success")
      ),
      div(style = "background: #ffebee; border-radius: 10px; flex: 1; margin: 0 8px; min-height: 60px; display: flex; flex-direction: column; align-items: center; justify-content: center; box-shadow: 0 1px 4px #d32f2f22; border: 2px solid #d32f2f;",
        span(style = "font-weight: bold; color: #d32f2f; font-size: 1.2em;", summary_counts[["Failed"]]),
        span(style = "color: #d32f2f; font-size: 0.95em;", "Failed")
      ),
      div(style = "background: #fffde7; border-radius: 10px; flex: 1; margin: 0 8px; min-height: 60px; display: flex; flex-direction: column; align-items: center; justify-content: center; box-shadow: 0 1px 4px #ff980022; border: 2px solid #ff9800;",
        span(style = "font-weight: bold; color: #ff9800; font-size: 1.2em;", summary_counts[["Pending"]]),
        span(style = "color: #ff9800; font-size: 0.95em;", "Pending")
      )
    )
    
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
    
    # Body Weight History Section
    div(
      style = "margin-bottom: 20px;",
      h4("Body Weight History", style = "color: #2196f3; border-bottom: 2px solid #2196f3; padding-bottom: 5px;"),
      if (nrow(body_weight_history) > 0) {
        div(
          style = "height: 300px; margin-bottom: 20px;",
          plotlyOutput(paste0("body_weight_plot_", asu_id))
        )
      } else {
        div(
          style = "padding: 20px; text-align: center; color: #666; background-color: #f9f9f9; border-radius: 5px;",
          "No body weight records found for this mouse. Click 'Add Body Weight' to start tracking."
        )
      }
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
        summary_bar
      },
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
  
  # Render body weight plot if data exists
  if (nrow(body_weight_history) > 0) {
    output[[paste0("body_weight_plot_", asu_id)]] <- renderPlotly({
      # Create the plot
      p <- plot_ly(
        data = body_weight_history,
        x = ~as.Date(measurement_date),
        y = ~weight_grams,
        type = 'scatter',
        mode = 'lines+markers',
        marker = list(size = 8, color = '#2196f3'),
        line = list(color = '#2196f3', width = 2),
        hovertemplate = paste(
          '<b>Date:</b> %{x}<br>',
          '<b>Weight:</b> %{y} grams<br>',
          '<extra></extra>'
        )
      ) %>%
      layout(
        title = list(
          text = paste("Body Weight Trend for", asu_id),
          font = list(size = 16)
        ),
        xaxis = list(
          title = "Date",
          showgrid = TRUE,
          gridcolor = '#e0e0e0'
        ),
        yaxis = list(
          title = "Weight (grams)",
          showgrid = TRUE,
          gridcolor = '#e0e0e0'
        ),
        hovermode = 'closest',
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
      
      return(p)
    })
  }
  
  # Show modal
  showModal(modalDialog(
    title = div(
      style = "font-size: 2rem; font-weight: bold;",
      paste("Mouse History Tracing - ", asu_id)
    ),
    size = "xl",
    modal_content,
    footer = div(
      style = "display: flex; justify-content: space-between; align-items: center;",
      actionButton(
        inputId = paste0("add_body_weight_", asu_id),
        label = "Add Body Weight",
        class = "btn-primary",
        style = "margin-right: 10px;"
      ),
      modalButton("Close")
    )
  ))
}

# Function to show body weight input modal
show_body_weight_input <- function(input, output, session, asu_id) {
  # Show body weight input modal
  showModal(modalDialog(
    title = div(
      style = "font-size: 1.5rem; font-weight: bold; color: #2196f3;",
      paste("Add Body Weight Record -", asu_id)
    ),
    size = "m",
    div(
      style = "padding: 20px;",
      fluidRow(
        column(6,
          dateInput(
            inputId = "body_weight_date",
            label = "Measurement Date:",
            value = Sys.Date(),
            format = "yyyy-mm-dd"
          )
        ),
        column(6,
          numericInput(
            inputId = "body_weight_grams",
            label = "Weight (grams):",
            value = NULL,
            min = 0,
            max = 100,
            step = 0.1
          )
        )
      ),
      br(),
      textAreaInput(
        inputId = "body_weight_notes",
        label = "Notes (optional):",
        rows = 3,
        placeholder = "Any additional notes about the measurement..."
      )
    ),
    footer = div(
      style = "display: flex; justify-content: flex-end; gap: 10px;",
      modalButton("Cancel"),
      actionButton(
        inputId = paste0("save_body_weight_", asu_id),
        label = "Save Record",
        class = "btn-success"
      )
    )
  ))
}

# Function to save body weight record
save_body_weight_record <- function(asu_id, weight_grams, measurement_date, notes = "") {
  # Validate inputs
  if (is.na(weight_grams) || weight_grams <= 0) {
    showNotification("Please enter a valid weight greater than 0.", type = "error", duration = 3)
    return(FALSE)
  }
  
  if (is.na(measurement_date)) {
    showNotification("Please select a valid measurement date.", type = "error", duration = 3)
    return(FALSE)
  }
  
  # Use a single DB connection for the insert
  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  tryCatch({
    # Insert the body weight record
    DBI::dbExecute(con, 
      "INSERT INTO body_weight_history (asu_id, weight_grams, measurement_date, notes, created_at, updated_at) 
       VALUES (?, ?, ?, ?, datetime('now'), datetime('now'))",
      params = list(asu_id, weight_grams, as.character(measurement_date), notes)
    )
    
    showNotification(
      paste("Body weight record saved successfully for", asu_id),
      type = "message",
      duration = 3
    )
    
    return(TRUE)
  }, error = function(e) {
    showNotification(
      paste("Error saving body weight record:", e$message),
      type = "error",
      duration = 5
    )
    return(FALSE)
  })
}