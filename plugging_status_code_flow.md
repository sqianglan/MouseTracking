# Plugging Status Code Implementation Flow

## 1. Table Display Logic (tab_plugging.R)

```javascript
// Action Button Generation
display_data$actions <- sapply(seq_len(nrow(display_data)), function(i) {
  row <- display_data[i, ]
  btns <- c()
  
  // Update Button Logic
  if (is_active_status(row$plugging_status)) {
    btns <- c(btns, '<button class="btn btn-sm btn-success quick-confirm-btn">Update</button>')
  }
  
  // Delete Button Logic
  if (!row$plugging_status %in% c("Deleted") && 
      !is_active_status(row$plugging_status) && 
      !is_system_locked()) {
    btns <- c(btns, '<button class="btn btn-sm btn-danger quick-delete-plugging-btn">Delete</button>')
  }
  
  return(paste(btns, collapse = ' '))
})
```

## 2. Helper Functions

```javascript
// Active Status Check
is_active_status <- function(status) {
  status %in% c("Ongoing", "Plugged", "Plug Confirmed", 
                "Not Observed (Waiting for confirmation)", "Surprising Plug!!")
}

// Plugged Status Check
is_plugged_status <- function(status) {
  is.null(status) || status %in% c("Plugged", "Plug Confirmed", 
                                  "Not Observed (Waiting for confirmation)", "Surprising Plug!!")
}

// Can Mark Empty Check
can_mark_empty <- function(status) {
  status %in% c("Plugged", "Plug Confirmed", "Not Observed (Waiting for confirmation)")
}
```

## 3. Update Button Click Handler

```javascript
// Quick Confirm Button Click
observeEvent(input$quick_confirm_plugging_btn, {
  plugging_id <- input$quick_confirm_plugging_btn
  
  // Get current record
  current_record <- dbGetQuery(con, "SELECT * FROM plugging_history WHERE id = ?", plugging_id)
  current_status <- current_record$plugging_status[1]
  
  // Define status options based on current status
  status_choices <- switch(current_status,
    "Ongoing" = c(
      "Not Observed (Waiting for confirmation)" = "Not Observed (Waiting for confirmation)",
      "Plugged" = "Plugged",
      "Plug Confirmed" = "Plug Confirmed",
      "Not Pregnant" = "Not Pregnant",
      "Empty" = "Empty",
      "Surprising Plug!!" = "Surprising Plug!!"
    ),
    "Plugged" = c(
      "Plug Confirmed" = "Plug Confirmed",
      "Not Pregnant" = "Not Pregnant",
      "Empty" = "Empty",
      "Surprising Plug!!" = "Surprising Plug!!"
    ),
    // ... other status mappings
  )
  
  // Show modal with options
  showModal(modalDialog(
    title = paste("Update Plugging Status - Current:", current_status),
    radioButtons("confirm_status_choice", "Status Options:", choices = status_choices),
    footer = actionButton("confirm_status_btn", "Update Status")
  ))
  
  plugging_state$confirming_id <- plugging_id
})
```

## 4. Status Update Logic

```javascript
// Confirm Status Update
observeEvent(input$confirm_status_btn, {
  plugging_id <- plugging_state$confirming_id
  selected_status <- input$confirm_status_choice
  
  // Validate eligibility
  if (!is_active_status(current$plugging_status[1])) {
    showNotification("This record is not eligible for status updates", type = "error")
    return()
  }
  
  // Prepare update based on selected status
  if (selected_status == "Surprising Plug!!") {
    // Special handling for Surprising Plug!!
    update_query <- "UPDATE plugging_history SET 
     plugging_status = ?, plug_observed_date = 'Unknown', updated_at = DATETIME('now')"
  } else if (selected_status == "Empty") {
    // Special handling for Empty
    update_query <- "UPDATE plugging_history SET 
     plugging_status = ?, plug_observed_date = ?, updated_at = DATETIME('now')"
  } else {
    // Standard update
    update_query <- "UPDATE plugging_history SET 
     plugging_status = ?, updated_at = DATETIME('now')"
  }
  
  // Execute update
  result <- dbExecute(con, update_query, params)
  
  if (result > 0) {
    // Log to audit trail
    log_audit_trail("plugging_history", plugging_id, "UPDATE", old_values, new_values)
    
    // Refresh table
    plugging_state$reload <- Sys.time()
    global_refresh_trigger(Sys.time())
  }
})
```

## 5. Auto-Update Logic

```javascript
// Auto-update function (called daily)
auto_update_plugging_status_to_unknown <- function() {
  today <- as.character(Sys.Date())
  
  // Find ongoing records where pairing period ended
  records <- dbGetQuery(con, 
    "SELECT * FROM plugging_history 
     WHERE plugging_status = 'Ongoing' 
     AND pairing_end_date IS NOT NULL 
     AND pairing_end_date < ?", 
    params = list(today))
  
  for (i in seq_len(nrow(records))) {
    rec <- records[i, ]
    
    // Update to Not Observed (Waiting for confirmation)
    dbExecute(con, 
      "UPDATE plugging_history 
       SET plugging_status = 'Not Observed (Waiting for confirmation)', 
           updated_at = DATETIME('now') 
       WHERE id = ?", 
      params = list(rec$id))
    
    // Log to audit trail
    log_audit_trail(
      "plugging_history", rec$id, "UPDATE", as.list(rec),
      list(plugging_status = "Not Observed (Waiting for confirmation)"),
      user_id = "system(auto)"
    )
  }
}
```

## 6. Calendar Events Logic

```javascript
// Calendar events generation
create_calendar_events <- function(pluggings) {
  for (i in seq_len(nrow(pluggings))) {
    row <- pluggings[i, ]
    
    // Calculate base date based on status
    base_date_result <- calculate_base_date(
      row$plugging_status, 
      row$plug_observed_date, 
      row$pairing_start_date
    )
    
    if (row$plugging_status == "Plugged" || row$plugging_status == "Plug Confirmed") {
      // Use plug_observed_date
      base_date <- as.Date(row$plug_observed_date)
    } else if (row$plugging_status == "Not Observed (Waiting for confirmation)" || 
               row$plugging_status == "Surprising Plug!!") {
      // Use pairing_start_date + 1
      base_date <- as.Date(row$pairing_start_date) + 1
    }
    
    // Generate events for each expected age
    for (age in expected_ages) {
      harvest_date <- base_date + floor(age)
      // Create calendar event...
    }
  }
}
```

## 7. Busy Status Logic (validation.R)

```javascript
// Busy status determination
mice_status_tag_all_mice <- function(asu_id) {
  // Check active plugging records
  active_count <- dbGetQuery(con, 
    "SELECT COUNT(*) as active_count 
     FROM plugging_history 
     WHERE female_id = ? 
     AND plugging_status IN ('Ongoing', 'Plugged', 'Plug Confirmed', 
                            'Not Observed (Waiting for confirmation)', 'Surprising Plug!!')",
    params = list(asu_id))$active_count
  
  if (mouse_role == "male") {
    // For males: Busy if 2+ active records
    return(active_count >= 2 ? "Busy" : "Free")
  } else {
    // For females: Busy if 1+ active records
    return(active_count >= 1 ? "Busy" : "Free")
  }
}
```

## 8. Data Flow Summary

```
User Action → Button Click → Modal Display → Status Selection → 
Database Update → Audit Trail → Table Refresh → UI Update
```

### Key Decision Points:

1. **Button Display**: `is_active_status()` determines Update vs Delete button
2. **Modal Options**: Current status determines available next statuses
3. **Update Logic**: Selected status determines special handling (Empty, Surprising Plug!!)
4. **Auto-Update**: Daily check for Ongoing records with expired pairing periods
5. **Calendar Events**: Status determines base date calculation method
6. **Busy Status**: Active statuses count toward busy indicator

### Error Handling:

- Invalid status transitions are prevented by modal options
- Database errors are caught and user is notified
- System lock prevents unauthorized updates
- Audit trail ensures all changes are logged 