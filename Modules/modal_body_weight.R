# modal_body_weight.R
# Body weight management functions for mouse history tracking

# Function to show enhanced body weight input modal with existing records and plot preview
show_body_weight_input <- function(input, output, session, asu_id) {
  # Get existing body weight data
  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  existing_data <- tryCatch({
    DBI::dbGetQuery(con, paste0("SELECT * FROM body_weight_history WHERE asu_id = '", asu_id, "' ORDER BY measurement_date DESC"))
  }, error = function(e) {
    data.frame()
  })
  
  # Get plugging history for context
  plugging_data <- tryCatch({
    DBI::dbGetQuery(con, paste0("SELECT * FROM plugging_history WHERE (male_id = '", asu_id, "' OR female_id = '", asu_id, "') AND plugging_status != 'Deleted'"))
  }, error = function(e) {
    data.frame()
  })
  
  # Create existing records table HTML with edit/delete buttons
  records_table_html <- ""
  if (nrow(existing_data) > 0) {
    display_data <- existing_data[, c("id", "measurement_date", "weight_grams", "notes")]
    
    # Format the data for display
    display_rows <- lapply(1:nrow(display_data), function(i) {
      row <- display_data[i, ]
      
      # Format date
      formatted_date <- if (is.na(row$measurement_date) || row$measurement_date == "") {
        "N/A"
      } else {
        tryCatch({
          format(as.Date(row$measurement_date), "%d-%b-%Y")
        }, error = function(e) {
          row$measurement_date
        })
      }
      
      # Handle empty notes
      formatted_notes <- if (is.na(row$notes) || row$notes == "") "-" else row$notes
      
      # Create action buttons (smaller and more compact, in same row)
      edit_btn <- paste0('<button class="btn btn-xs btn-warning" style="margin-right: 3px; padding: 1px 6px; font-size: 11px;" onclick="Shiny.setInputValue(\'edit_body_weight_record\', \'', row$id, '\', {priority: \'event\'});">Edit</button>')
      delete_btn <- paste0('<button class="btn btn-xs btn-danger" style="padding: 1px 6px; font-size: 11px;" onclick="Shiny.setInputValue(\'delete_body_weight_record\', \'', row$id, '\', {priority: \'event\'});">Del</button>')
      actions <- paste0('<div style="white-space: nowrap;">', edit_btn, delete_btn, '</div>')
      
      # Create table row with optimized column widths
      paste0(
        '<tr>',
        '<td style="width: 25%; text-align: center; vertical-align: middle;">', formatted_date, '</td>',
        '<td style="width: 17%; text-align: center; vertical-align: middle;">', row$weight_grams, '</td>',
        '<td style="width: 30%; max-width: 150px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; text-align: center; vertical-align: middle;" title="', htmltools::htmlEscape(formatted_notes), '">', formatted_notes, '</td>',
        '<td style="width: 23%; min-width: 70px; text-align: center; vertical-align: middle;">', actions, '</td>',
        '</tr>'
      )
    })
    
    records_table_html <- paste0(
      '<table class="table table-striped table-bordered table-hover" style="width: 100%; font-size: 1em; table-layout: fixed;">',
      '<thead><tr>',
      '<th style="background-color: #f8f9fa; width: 20%; text-align: center; font-size: 0.9em;">Date</th>',
      '<th style="background-color: #f8f9fa; width: 17%; text-align: center; font-size: 0.9em;">Weight (g)</th>',
      '<th style="background-color: #f8f9fa; width: 30%; text-align: center; font-size: 0.9em;">Notes</th>',
      '<th style="background-color: #f8f9fa; width: 23%; text-align: center; font-size: 0.9em;">Actions</th>',
      '</tr></thead><tbody>',
      paste0(display_rows, collapse = ''),
      '</tbody></table>'
    )
  } else {
    records_table_html <- '<div style="text-align: center; color: #666; padding: 20px; background-color: #f9f9f9; border-radius: 5px;">No existing body weight records found.</div>'
  }
  
  # Always render the preview plot (it will handle empty data gracefully)
  render_body_weight_preview_chart(output, asu_id, existing_data, plugging_data)
  
  # Show enhanced modal
  showModal(modalDialog(
    title = div(
      style = "font-size: 1.5rem; font-weight: bold; color: #2196f3;",
      paste("Body Weight Management -", asu_id)
    ),
    size = "xl",
    div(
      style = "max-height: 80vh; overflow-y: auto;",
      
      # Input section
      div(
        style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
        h4("Add New Record", style = "color: #2196f3; margin-bottom: 15px;"),
        fluidRow(
          column(4,
            dateInput(
              inputId = "body_weight_date",
              label = "Measurement Date",
              value = Sys.Date(),
              format = "yyyy-mm-dd"
            )
          ),
          column(4,
            numericInput(
              inputId = "body_weight_grams",
              label = "Weight (grams)",
              value = NULL,
              min = 0,
              max = 100,
              step = 0.1
            )
          ),
          column(4,
            br(),
            actionButton(
              inputId = "save_body_weight_btn",
              label = "Add Record",
              class = "btn-success",
              style = "margin-top: 5px;",
              onclick = paste0("Shiny.setInputValue('save_body_weight_clicked', '", asu_id, "', {priority: 'event'});")
            )
          )
        ),
        fluidRow(
          column(12,
            textAreaInput(
              inputId = "body_weight_notes",
              label = "Notes (optional)",
              rows = 2,
              placeholder = "Any additional notes about the measurement..."
            )
          )
        )
      ),
      
      # Existing records section
      div(
        style = "margin-bottom: 20px; margin-right: 15px;",
        h4("Existing Records", style = "color: #ff9800; border-bottom: 2px solid #ff9800; padding-bottom: 5px;"),
        div(
          id = "body_weight_records_table",
          style = "overflow-x: auto;",
          HTML(records_table_html)
        )
      ),
      
      # Plot preview section - always show but hide if no data
      div(
        style = paste0("margin-bottom: 20px;", if (nrow(existing_data) == 0) " display: none;" else ""),
        h4("Weight Trend Preview", style = "color: #4caf50; border-bottom: 2px solid #4caf50; padding-bottom: 5px;"),
        div(
          id = "body_weight_preview_plot_container",
          style = "height: 350px;",
          plotlyOutput(paste0("body_weight_preview_plot_", asu_id))
        )
      )
    ),
    footer = div(
      style = "display: flex; justify-content: space-between; align-items: center; padding: 10px;",
      div(
        style = "color: #666; font-size: 0.9em;",
        paste("Total records:", nrow(existing_data))
      ),
      div(
        style = "display: flex; gap: 10px;",
        actionButton(
          inputId = "body_weight_back_btn",
          label = "← Back",
          class = "btn-secondary",
          style = "font-size: 0.9em; padding: 8px 16px;",
          onclick = paste0("Shiny.setInputValue('body_weight_back_clicked', '", asu_id, "', {priority: 'event'});")
        ),
        actionButton(
          inputId = "body_weight_close_btn", 
          label = "✕ Close",
          class = "btn-outline-secondary",
          style = "font-size: 0.9em; padding: 8px 16px;",
          onclick = "Shiny.setInputValue('body_weight_close_clicked', true, {priority: 'event'});"
        )
      )
    )
  ))
}

# Function to render body weight preview chart in modal
render_body_weight_preview_chart <- function(output, asu_id, body_weight_history, plugging_history) {
  output[[paste0("body_weight_preview_plot_", asu_id)]] <- renderPlotly({
    # Handle empty data case
    if (nrow(body_weight_history) == 0) {
      # Create empty plot with message
      p <- plot_ly() %>%
        add_annotations(
          text = "No body weight data available yet.<br>Add your first record above to see the trend chart.",
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 14, color = "#666666")
        ) %>%
        layout(
          xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
          yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
          plot_bgcolor = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)",
          margin = list(t = 10, b = 10, l = 10, r = 10),
          showlegend = FALSE
        )
      return(p)
    }
    
    # Prepare body weight data
    weight_data <- body_weight_history
    weight_data$measurement_date <- as.Date(weight_data$measurement_date)
    
    # Create the base plotly chart
    p <- plot_ly(
      data = weight_data,
      x = ~measurement_date,
      y = ~weight_grams,
      type = "scatter",
      mode = "lines+markers",
      marker = list(size = 6, color = "#2196f3"),
      line = list(color = "#2196f3", width = 2),
      name = "Body Weight",
      hovertemplate = paste(
        "<b>Date:</b> %{x}<br>",
        "<b>Weight:</b> %{y} grams<br>",
        "<extra></extra>"
      )
    )
    
    # Initialize shapes list for layout
    shapes_list <- list()
    
    # Add plugging events if they exist (simplified for preview)
    if (nrow(plugging_history) > 0) {
      # Get y-axis range for event marker placement
      y_range <- range(weight_data$weight_grams)
      y_top <- y_range[2] + (y_range[2] - y_range[1]) * 0.1
      
      # Process each plugging record (simplified)
      for (i in seq_len(nrow(plugging_history))) {
        row <- plugging_history[i, ]
        
        # Add gray shaded area for pairing period
        if (!is.na(row$pairing_start_date) && row$pairing_start_date != "") {
          start_date <- as.Date(row$pairing_start_date)
          end_date <- if (!is.na(row$pairing_end_date) && row$pairing_end_date != "") {
            as.Date(row$pairing_end_date)
          } else {
            max(weight_data$measurement_date, Sys.Date())
          }
          
          # Add rectangle shape for pairing period
          shapes_list[[length(shapes_list) + 1]] <- list(
            type = "rect",
            x0 = start_date, x1 = end_date,
            y0 = y_range[1], y1 = y_range[2],
            fillcolor = "rgba(128, 128, 128, 0.15)",
            line = list(color = "rgba(0,0,0,0)", width = 0),
            layer = "below"
          )
        }
        
        # Add plug observed markers
        if (!is.na(row$plug_observed_date) && row$plug_observed_date != "") {
          plug_date <- as.Date(row$plug_observed_date)
          
          # Determine color based on plugging status
          plug_color <- if (row$plugging_status == "Collected") {
            "#388e3c"  # Success - Green
          } else if (row$plugging_status %in% c("Empty", "Not Pregnant (Confirmed)", "Not Pregnant")) {
            "#d32f2f"  # Failed - Red
          } else {
            "#ff9800"  # Pending - Orange
          }
          
          # Add line shape for vertical line
          shapes_list[[length(shapes_list) + 1]] <- list(
            type = "line",
            x0 = plug_date, x1 = plug_date,
            y0 = y_range[1], y1 = y_range[2],
            line = list(color = plug_color, width = 2)
          )
        }
      }
    }
    
    # Apply layout with shapes (simplified for preview)
    p <- p %>% layout(
      xaxis = list(
        title = "",
        showgrid = TRUE,
        gridcolor = "#e0e0e0",
        tickformat = "%Y-%m-%d"
      ),
      yaxis = list(
        title = "Weight (grams)",
        showgrid = TRUE,
        gridcolor = "#e0e0e0"
      ),
      shapes = shapes_list,
      hovermode = "closest",
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      margin = list(t = 10, b = 30, l = 50, r = 20),
      showlegend = FALSE
    )
    
    return(p)
  })
}

# Function to save body weight record and refresh modal content
save_body_weight_record <- function(asu_id, weight_grams, measurement_date, notes = "", refresh_modal = TRUE) {
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
    
    # The modal will need to be refreshed by the calling function
    
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

# Function to show edit body weight record modal
show_edit_body_weight_modal <- function(input, output, session, record_id, asu_id) {
  # Get the record to edit
  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  record <- tryCatch({
    DBI::dbGetQuery(con, paste0("SELECT * FROM body_weight_history WHERE id = ", record_id))
  }, error = function(e) {
    data.frame()
  })
  
  if (nrow(record) == 0) {
    showNotification("Record not found.", type = "error", duration = 3)
    return()
  }
  
  # Show edit modal
  showModal(modalDialog(
    title = div(
      style = "font-size: 1.3rem; font-weight: bold; color: #ff9800;",
      paste("Edit Body Weight Record - ID:", record_id)
    ),
    size = "m",
    div(
      style = "padding: 15px;",
      fluidRow(
        column(6,
          dateInput(
            inputId = "edit_body_weight_date",
            label = "Measurement Date",
            value = as.Date(record$measurement_date[1]),
            format = "yyyy-mm-dd"
          )
        ),
        column(6,
          numericInput(
            inputId = "edit_body_weight_grams",
            label = "Weight (grams)",
            value = record$weight_grams[1],
            min = 0,
            max = 100,
            step = 0.1
          )
        )
      ),
      fluidRow(
        column(12,
          textAreaInput(
            inputId = "edit_body_weight_notes",
            label = "Notes (optional)",
            value = if (is.na(record$notes[1])) "" else record$notes[1],
            rows = 3,
            placeholder = "Any additional notes about the measurement..."
          )
        )
      )
    ),
    footer = div(
      style = "display: flex; justify-content: flex-end; gap: 10px;",
      modalButton("Cancel"),
      actionButton(
        inputId = "update_body_weight_btn",
        label = "Update Record",
        class = "btn-success",
        onclick = paste0("Shiny.setInputValue('update_body_weight_clicked', '", record_id, "_", asu_id, "', {priority: 'event'});")
      )
    )
  ))
}

# Function to update existing body weight record
update_body_weight_record <- function(record_id, asu_id, weight_grams, measurement_date, notes = "") {
  # Validate inputs
  if (is.na(weight_grams) || weight_grams <= 0) {
    showNotification("Please enter a valid weight greater than 0.", type = "error", duration = 3)
    return(FALSE)
  }
  
  if (is.na(measurement_date)) {
    showNotification("Please select a valid measurement date.", type = "error", duration = 3)
    return(FALSE)
  }
  
  # Use a single DB connection for the update
  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  tryCatch({
    # Update the body weight record
    DBI::dbExecute(con, 
      "UPDATE body_weight_history SET weight_grams = ?, measurement_date = ?, notes = ?, updated_at = datetime('now') WHERE id = ?",
      params = list(weight_grams, as.character(measurement_date), notes, record_id)
    )
    
    showNotification(
      "Body weight record updated successfully",
      type = "message",
      duration = 3
    )
    
    return(TRUE)
  }, error = function(e) {
    showNotification(
      paste("Error updating body weight record:", e$message),
      type = "error",
      duration = 5
    )
    return(FALSE)
  })
}

# Function to delete body weight record
delete_body_weight_record <- function(record_id, asu_id) {
  # Show confirmation modal
  showModal(modalDialog(
    title = "Confirm Deletion",
    div(
      style = "padding: 15px;",
      p("Are you sure you want to delete this body weight record?"),
      p("This action cannot be undone.", style = "color: #d32f2f; font-weight: bold;")
    ),
    footer = div(
      style = "display: flex; justify-content: flex-end; gap: 10px;",
      modalButton("Cancel"),
      actionButton(
        inputId = "confirm_delete_body_weight_btn",
        label = "Delete",
        class = "btn-danger",
        onclick = paste0("Shiny.setInputValue('confirm_delete_body_weight_clicked', '", record_id, "_", asu_id, "', {priority: 'event'});")
      )
    )
  ))
}

# Function to confirm delete body weight record
confirm_delete_body_weight_record <- function(record_id, asu_id) {
  # Use a single DB connection for the delete
  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  tryCatch({
    # Delete the body weight record
    result <- DBI::dbExecute(con, 
      "DELETE FROM body_weight_history WHERE id = ?",
      params = list(record_id)
    )
    
    if (result > 0) {
      showNotification(
        "Body weight record deleted successfully",
        type = "message",
        duration = 3
      )
      return(TRUE)
    } else {
      showNotification(
        "No record found to delete",
        type = "warning",
        duration = 3
      )
      return(FALSE)
    }
  }, error = function(e) {
    showNotification(
      paste("Error deleting body weight record:", e$message),
      type = "error",
      duration = 5
    )
    return(FALSE)
  })
}

# Function to refresh modal content without closing it
refresh_body_weight_modal_content <- function(output, session, asu_id) {
  # Get updated body weight data
  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  existing_data <- tryCatch({
    DBI::dbGetQuery(con, paste0("SELECT * FROM body_weight_history WHERE asu_id = '", asu_id, "' ORDER BY measurement_date DESC"))
  }, error = function(e) {
    data.frame()
  })
  
  # Get plugging history for context
  plugging_data <- tryCatch({
    DBI::dbGetQuery(con, paste0("SELECT * FROM plugging_history WHERE (male_id = '", asu_id, "' OR female_id = '", asu_id, "') AND plugging_status != 'Deleted'"))
  }, error = function(e) {
    data.frame()
  })
  
  # Create updated records table HTML
  records_table_html <- ""
  if (nrow(existing_data) > 0) {
    display_data <- existing_data[, c("id", "measurement_date", "weight_grams", "notes")]
    
    # Format the data for display
    display_rows <- lapply(1:nrow(display_data), function(i) {
      row <- display_data[i, ]
      
      # Format date
      formatted_date <- if (is.na(row$measurement_date) || row$measurement_date == "") {
        "N/A"
      } else {
        tryCatch({
          format(as.Date(row$measurement_date), "%d-%b-%Y")
        }, error = function(e) {
          row$measurement_date
        })
      }
      
      # Handle empty notes
      formatted_notes <- if (is.na(row$notes) || row$notes == "") "-" else row$notes
      
      # Create action buttons
      edit_btn <- paste0('<button class="btn btn-xs btn-warning" style="margin-right: 3px; padding: 1px 6px; font-size: 11px;" onclick="Shiny.setInputValue(\'edit_body_weight_record\', \'', row$id, '\', {priority: \'event\'});">Edit</button>')
      delete_btn <- paste0('<button class="btn btn-xs btn-danger" style="padding: 1px 6px; font-size: 11px;" onclick="Shiny.setInputValue(\'delete_body_weight_record\', \'', row$id, '\', {priority: \'event\'});">Del</button>')
      actions <- paste0('<div style="white-space: nowrap;">', edit_btn, delete_btn, '</div>')
      
      # Create table row
      paste0(
        '<tr>',
        '<td style="width: 25%; text-align: center; vertical-align: middle;">', formatted_date, '</td>',
        '<td style="width: 17%; text-align: center; vertical-align: middle;">', row$weight_grams, '</td>',
        '<td style="width: 30%; max-width: 150px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; text-align: center; vertical-align: middle;" title="', htmltools::htmlEscape(formatted_notes), '">', formatted_notes, '</td>',
        '<td style="width: 23%; min-width: 70px; text-align: center; vertical-align: middle;">', actions, '</td>',
        '</tr>'
      )
    })
    
    records_table_html <- paste0(
      '<table class="table table-striped table-bordered table-hover" style="width: 100%; font-size: 1em; table-layout: fixed;">',
      '<thead><tr>',
      '<th style="background-color: #f8f9fa; width: 20%; text-align: center; font-size: 0.9em;">Date</th>',
      '<th style="background-color: #f8f9fa; width: 17%; text-align: center; font-size: 0.9em;">Weight (g)</th>',
      '<th style="background-color: #f8f9fa; width: 30%; text-align: center; font-size: 0.9em;">Notes</th>',
      '<th style="background-color: #f8f9fa; width: 23%; text-align: center; font-size: 0.9em;">Actions</th>',
      '</tr></thead><tbody>',
      paste0(display_rows, collapse = ''),
      '</tbody></table>'
    )
  } else {
    records_table_html <- '<div style="text-align: center; color: #666; padding: 20px; background-color: #f9f9f9; border-radius: 5px;">No existing body weight records found.</div>'
  }
  
  # Update the records table content using JavaScript
  session$sendCustomMessage(
    type = "updateBodyWeightTable",
    message = list(
      html = records_table_html,
      record_count = nrow(existing_data),
      has_records = nrow(existing_data) > 0,
      show_plot = nrow(existing_data) > 0  # Add this to control plot visibility
    )
  )
  
  # Always render the plot (it will handle empty data gracefully)
  render_body_weight_preview_chart(output, asu_id, existing_data, plugging_data)
  
  # Send custom message to update plot container visibility
  session$sendCustomMessage(
    type = "updateBodyWeightPlotContainer",
    message = list(
      asu_id = asu_id,
      show_plot = nrow(existing_data) > 0
    )
  )
}

# Function to render body weight plotly chart
render_body_weight_chart <- function(output, asu_id, body_weight_history, plugging_history) {
  output[[paste0("body_weight_plot_", asu_id)]] <- renderPlotly({
    # Prepare body weight data
    weight_data <- body_weight_history
    weight_data$measurement_date <- as.Date(weight_data$measurement_date)
    
    # Create the base plotly chart
    p <- plot_ly(
      data = weight_data,
      x = ~measurement_date,
      y = ~weight_grams,
      type = 'scatter',
      mode = 'lines+markers',
      marker = list(size = 8, color = '#2196f3'),
      line = list(color = '#2196f3', width = 3),
      name = 'Body Weight',
      hovertemplate = paste(
        '<b>Date:</b> %{x}<br>',
        '<b>Weight:</b> %{y} grams<br>',
        '<extra></extra>'
      )
    )
    
    # Initialize shapes list for layout
    shapes_list <- list()
    
    # Add plugging events if they exist
    if (nrow(plugging_history) > 0) {
      # Get y-axis range for event marker placement
      y_range <- range(weight_data$weight_grams)
      y_top <- y_range[2] + (y_range[2] - y_range[1]) * 0.15
      
      # Process each plugging record
      for (i in seq_len(nrow(plugging_history))) {
        row <- plugging_history[i, ]
        
        # Create gray shaded area for pairing period
        start_date <- NULL
        end_date <- NULL
        
        if (!is.na(row$pairing_start_date) && row$pairing_start_date != "") {
          start_date <- as.Date(row$pairing_start_date)
        }
        
        if (!is.na(row$pairing_end_date) && row$pairing_end_date != "") {
          end_date <- as.Date(row$pairing_end_date)
        }
        
        # Add gray shaded area for pairing period
        if (!is.null(start_date)) {
          # If no end date, extend to current date or last weight measurement
          if (is.null(end_date)) {
            end_date <- max(weight_data$measurement_date, Sys.Date())
          }
          
          # Add rectangle shape for pairing period
          shapes_list[[length(shapes_list) + 1]] <- list(
            type = "rect",
            x0 = start_date, x1 = end_date,
            y0 = y_range[1], y1 = y_range[2],
            fillcolor = "rgba(128, 128, 128, 0.2)",
            line = list(color = "rgba(0,0,0,0)", width = 0),
            layer = "below"
          )
        }
        
        # Plug observed - Arrow with color based on plugging status
        if (!is.na(row$plug_observed_date) && row$plug_observed_date != "") {
          plug_date <- as.Date(row$plug_observed_date)
          
          # Determine color based on plugging status (same as summary)
          plug_color <- if (row$plugging_status == "Collected") {
            "#388e3c"  # Success - Green
          } else if (row$plugging_status %in% c("Empty", "Not Pregnant (Confirmed)", "Not Pregnant")) {
            "#d32f2f"  # Failed - Red
          } else {
            "#ff9800"  # Pending - Orange
          }
          
          p <- p %>% add_trace(
            x = plug_date,
            y = y_top,
            type = "scatter",
            mode = "markers+text",
            marker = list(size = 15, color = plug_color, symbol = "triangle-down"),
            text = "Plug Observed",
            textposition = "top center",
            textfont = list(color = plug_color, size = 10, family = "Arial Black"),
            name = "Plug Observed",
            hovertemplate = paste("<b>Plug Observed</b><br>Date: %{x}<br>Status: ", row$plugging_status, "<extra></extra>"),
            showlegend = FALSE
          )
          
          # Add line shape for vertical line
          shapes_list[[length(shapes_list) + 1]] <- list(
            type = "line",
            x0 = plug_date, x1 = plug_date,
            y0 = y_range[1], y1 = y_range[2],
            line = list(color = plug_color, width = 3)
          )
        }
      }
    }
    
    # Apply layout with shapes
    p <- p %>% layout(
      xaxis = list(
        title = "",
        showgrid = TRUE,
        gridcolor = "#e0e0e0",
        tickformat = "%Y-%m-%d"
      ),
      yaxis = list(
        title = "Weight (grams)",
        showgrid = TRUE,
        gridcolor = "#e0e0e0"
      ),
      shapes = shapes_list,
      hovermode = "closest",
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      margin = list(t = 20, b = 15, l = 60, r = 40)
    )
    
    return(p)
  })
}