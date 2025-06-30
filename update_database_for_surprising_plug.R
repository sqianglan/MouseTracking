# Script to update database for "Surprising Plug!!" status
# Run this script to add the new plugging status to existing databases

library(DBI)
library(RSQLite)

# Set database path (adjust as needed)
if (Sys.getenv("MOUSE_DB_DIR") != "") {
  DB_DIR <- Sys.getenv("MOUSE_DB_DIR")
} else {
  DB_DIR <- getwd()
}

if (Sys.getenv("MOUSE_DB_NAME") != "") {
  DB_NAME <- Sys.getenv("MOUSE_DB_NAME")
} else {
  DB_NAME <- "mice_colony_test.db"
}

DB_PATH <- file.path(DB_DIR, DB_NAME)
DB_PATH <- "~/Documents/MouseManagement/database/mice_colony.db"

cat("Updating database:", DB_PATH, "\n")

# Check if database exists
if (!file.exists(DB_PATH)) {
  cat("Database does not exist. Please run the main application first to create the database.\n")
  quit()
}

# Connect to database
con <- dbConnect(RSQLite::SQLite(), DB_PATH)

# Check if plugging_history table exists
tables <- dbListTables(con)
if (!"plugging_history" %in% tables) {
  cat("plugging_history table does not exist. Please run the main application first.\n")
  dbDisconnect(con)
  quit()
}

# Get current table info
table_info <- dbGetQuery(con, "PRAGMA table_info(plugging_history)")
cat("Current table structure:\n")
print(table_info)

# Check current status values
current_statuses <- dbGetQuery(con, "SELECT DISTINCT plugging_status FROM plugging_history ORDER BY plugging_status")
cat("Current plugging statuses:\n")
print(current_statuses)

# Backup existing data
cat("Creating backup...\n")
dbExecute(con, "CREATE TABLE IF NOT EXISTS plugging_history_backup AS SELECT * FROM plugging_history")

# Get existing data
existing_data <- dbGetQuery(con, "SELECT * FROM plugging_history")
cat("Backed up", nrow(existing_data), "records\n")

# Drop the existing table
cat("Dropping existing table...\n")
dbExecute(con, "DROP TABLE plugging_history")

# Recreate the table with the new status in the CHECK constraint
cat("Creating new table with updated constraint...\n")
dbExecute(con, paste0(
  "CREATE TABLE plugging_history (",
  "id INTEGER PRIMARY KEY AUTOINCREMENT,",
  "male_id TEXT NOT NULL,",
  "female_id TEXT NOT NULL,",
  "cage_id TEXT,",
  "pairing_start_date DATE,",
  "pairing_end_date DATE,",
  "plug_observed_date TEXT,",
  "plugging_status TEXT DEFAULT 'Ongoing' CHECK (plugging_status IN ('Ongoing', 'Plugged', 'Plug Confirmed', 'Not Pregnant', 'Not Observed (Waiting for confirmation)', 'Empty', 'Deleted', 'Not Observed (Confirmed)', 'Surprising Plug!!')),",
  "notes TEXT,",
  "created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,",
  "updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,",
  "expected_age_for_harvesting TEXT",
  ")"
))

# Restore the data
if (nrow(existing_data) > 0) {
  cat("Restoring data...\n")
  dbWriteTable(con, "plugging_history", existing_data, append = TRUE, row.names = FALSE)
}

# Drop the backup table
cat("Cleaning up backup...\n")
dbExecute(con, "DROP TABLE plugging_history_backup")

# Verify the update
new_statuses <- dbGetQuery(con, "SELECT DISTINCT plugging_status FROM plugging_history ORDER BY plugging_status")
cat("Updated plugging statuses:\n")
print(new_statuses)

# Test inserting a record with the new status
cat("Testing new status insertion...\n")
test_result <- tryCatch({
  dbExecute(con, "INSERT INTO plugging_history (male_id, female_id, plugging_status) VALUES ('TEST_MALE', 'TEST_FEMALE', 'Surprising Plug!!')")
  cat("✓ Successfully inserted test record with 'Surprising Plug!!' status\n")
  
  # Clean up test record
  dbExecute(con, "DELETE FROM plugging_history WHERE male_id = 'TEST_MALE' AND female_id = 'TEST_FEMALE'")
  cat("✓ Cleaned up test record\n")
}, error = function(e) {
  cat("✗ Error testing new status:", e$message, "\n")
})

dbDisconnect(con)

cat("Database update completed successfully!\n")
cat("The new 'Surprising Plug!!' status is now available in the plugging system.\n") 
