# db_check.R
library(DBI)
library(RSQLite)
library(stringr)

# Set default database path and name
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

DEFAULT_DB_NAME <- file.path(DB_DIR, DB_NAME)
TABLE_NAME <- "mice_stock"

# Ensure database directory exists
ensure_db_directory <- function() {
  if (!dir.exists(DB_DIR)) {
    dir.create(DB_DIR, recursive = TRUE, showWarnings = FALSE)
    cat("Created database directory:", DB_DIR, "\n")
  }
}

# Create the mice_stock table with full schema if DB does not exist
initialize_db <- function() {
  # Ensure database directory exists
  ensure_db_directory()
  
  if (!file.exists(DEFAULT_DB_NAME)) {
    con <- dbConnect(SQLite(), DEFAULT_DB_NAME)
    dbExecute(con, paste0(
      "CREATE TABLE IF NOT EXISTS ", TABLE_NAME, " (",
      "asu_id TEXT PRIMARY KEY,",
      "animal_id TEXT,",
      "ear_mark TEXT,",
      "gender TEXT CHECK (gender IN ('Male', 'Female')),",
      "dob DATE NOT NULL,",
      "genotype TEXT,",
      "transgenes TEXT,",
      "strain TEXT DEFAULT 'C57BL/6J',",
      "breeding_line TEXT,",
      "dam TEXT,",
      "sire TEXT,",
      "cage_id TEXT,",
      "room TEXT,",
      "project_code TEXT,",
      "responsible_person TEXT,",
      "protocol TEXT,",
      "stock_category TEXT DEFAULT 'Experiment' CHECK (stock_category IN ('Experiment', 'Breeding', 'Charles River')),",
      "status TEXT DEFAULT 'Alive' CHECK (status IN ('Alive', 'Deceased', 'Deleted')),",
      "date_of_death DATE,",
      "age_at_death_weeks REAL,",
      "max_severity TEXT,",
      "procedure TEXT,",
      "stage TEXT,",
      "deceased_timestamp TIMESTAMP,",
      "notes TEXT,",
      "imported_from_excel BOOLEAN DEFAULT 0,",
      "date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,",
      "last_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP"
      ,")"
    ))
    dbDisconnect(con)
  } else {
    # Update existing database to allow 'Deleted' status
    update_existing_db()
  }
}

# Function to update existing database schema
update_existing_db <- function() {
  con <- dbConnect(SQLite(), DEFAULT_DB_NAME)
  
  # Check if the table exists
  tables <- dbListTables(con)
  if (TABLE_NAME %in% tables) {
    # Get current table info
    table_info <- dbGetQuery(con, paste0("PRAGMA table_info(", TABLE_NAME, ")"))
    
    # Check if status column exists and what constraints it has
    status_col <- table_info[table_info$name == "status", ]
    
    if (nrow(status_col) > 0) {
      # Check if 'Deleted' is already allowed in the constraint
      # Since SQLite doesn't easily allow constraint modification, 
      # we'll create a new table with the updated schema and migrate data
      
      # Get all data from existing table
      existing_data <- dbGetQuery(con, paste0("SELECT * FROM ", TABLE_NAME))
      
      # Drop the old table
      dbExecute(con, paste0("DROP TABLE ", TABLE_NAME))
      
      # Create new table with updated schema
      dbExecute(con, paste0(
        "CREATE TABLE ", TABLE_NAME, " (",
        "asu_id TEXT PRIMARY KEY,",
        "animal_id TEXT,",
        "ear_mark TEXT,",
        "gender TEXT CHECK (gender IN ('Male', 'Female')),",
        "dob DATE NOT NULL,",
        "genotype TEXT,",
        "transgenes TEXT,",
        "strain TEXT DEFAULT 'C57BL/6J',",
        "breeding_line TEXT,",
        "dam TEXT,",
        "sire TEXT,",
        "cage_id TEXT,",
        "room TEXT,",
        "project_code TEXT,",
        "responsible_person TEXT,",
        "protocol TEXT,",
        "stock_category TEXT DEFAULT 'Experiment' CHECK (stock_category IN ('Experiment', 'Breeding', 'Charles River')),",
        "status TEXT DEFAULT 'Alive' CHECK (status IN ('Alive', 'Deceased', 'Deleted')),",
        "date_of_death DATE,",
        "age_at_death_weeks REAL,",
        "max_severity TEXT,",
        "procedure TEXT,",
        "stage TEXT,",
        "deceased_timestamp TIMESTAMP,",
        "notes TEXT,",
        "imported_from_excel BOOLEAN DEFAULT 0,",
        "date_created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,",
        "last_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP"
        ,")"
      ))
      
      # Re-insert the data
      if (nrow(existing_data) > 0) {
        dbWriteTable(con, TABLE_NAME, existing_data, append = TRUE, row.names = FALSE)
      }
      
      cat("Database schema updated to allow 'Deleted' status\n")
    }
  }
  
  dbDisconnect(con)
}

# Add breeding_history and litter tables if not exist
add_breeding_tables <- function() {
  con <- dbConnect(SQLite(), DEFAULT_DB_NAME)
  # Breeding history table
  dbExecute(con, paste0(
    "CREATE TABLE IF NOT EXISTS breeding_history (",
    "id INTEGER PRIMARY KEY AUTOINCREMENT,",
    "male_id TEXT NOT NULL,",
    "female1_id TEXT NOT NULL,",
    "female2_id TEXT,",
    "cage_id TEXT,",
    "start_date DATE,",
    "end_date DATE,",
    "notes TEXT,",
    "created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,",
    "updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP"
    ,")"
  ))
  # Litter table
  dbExecute(con, paste0(
    "CREATE TABLE IF NOT EXISTS litter (",
    "id INTEGER PRIMARY KEY AUTOINCREMENT,",
    "breeding_id INTEGER NOT NULL,",
    "dob DATE,",
    "num_pups INTEGER,",
    "notes TEXT,",
    "created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,",
    "FOREIGN KEY(breeding_id) REFERENCES breeding_history(id) ON DELETE CASCADE"
    ,")"
  ))
  dbDisconnect(con)
}

# Add breeding_status column to breeding_history if not exists, and remove old status column if present
add_breeding_status_column <- function() {
  con <- dbConnect(SQLite(), DEFAULT_DB_NAME)
  cols <- dbListFields(con, "breeding_history")
  if (!"breeding_status" %in% cols) {
    dbExecute(con, "ALTER TABLE breeding_history ADD COLUMN breeding_status TEXT DEFAULT 'Ongoing'")
  }
  # Optionally, keep 'status' for backward compatibility, or remove if you want
  dbDisconnect(con)
}

# Add plugging_history table if not exists
add_plugging_tables <- function() {
  con <- dbConnect(SQLite(), DEFAULT_DB_NAME)
  # Plugging history table
  dbExecute(con, paste0(
    "CREATE TABLE IF NOT EXISTS plugging_history (",
    "id INTEGER PRIMARY KEY AUTOINCREMENT,",
    "male_id TEXT NOT NULL,",
    "female_id TEXT NOT NULL,",
    "cage_id TEXT,",
    "pairing_start_date DATE,",
    "pairing_end_date DATE,",
    "plug_observed_date TEXT,",
    "plugging_status TEXT DEFAULT 'Ongoing' CHECK (plugging_status IN ('Ongoing', 'Plugged', 'Plug Confirmed', 'Not Pregnant', 'Not Observed (Waiting for confirmation)', 'Empty', 'Deleted', 'Not Observed (Confirmed)')),",
    "notes TEXT,",
    "created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,",
    "updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,",
    "expected_age_for_harvesting TEXT"
    ,")"
  ))
  
  # Check if old columns exist and migrate data if needed
  cols <- dbListFields(con, "plugging_history")
  if ("plugging_date" %in% cols && !"plug_observed_date" %in% cols) {
    dbExecute(con, "ALTER TABLE plugging_history ADD COLUMN plug_observed_date DATE")
    dbExecute(con, "UPDATE plugging_history SET plug_observed_date = plugging_date")
    dbExecute(con, "ALTER TABLE plugging_history DROP COLUMN plugging_date")
  }
  if (!"pairing_start_date" %in% cols) {
    dbExecute(con, "ALTER TABLE plugging_history ADD COLUMN pairing_start_date DATE")
  }
  if (!"pairing_end_date" %in% cols) {
    dbExecute(con, "ALTER TABLE plugging_history ADD COLUMN pairing_end_date DATE")
  }
  
  # Migrate old status values to new ones
  dbExecute(con, "UPDATE plugging_history SET plugging_status = 'Plugged' WHERE plugging_status = 'Plugged'")
  dbExecute(con, "UPDATE plugging_history SET plugging_status = 'Not Observed (Waiting for confirmation)' WHERE plugging_status = 'Not Plugged'")
  dbExecute(con, "UPDATE plugging_history SET plugging_status = 'Ongoing' WHERE plugging_status IS NULL")
  dbExecute(con, "UPDATE plugging_history SET plugging_status = 'Empty' WHERE plugging_status = ''")
  
  # Update the CHECK constraint for plugging_status
  tryCatch({
    existing_data <- dbGetQuery(con, "SELECT * FROM plugging_history")
    
    # Update "Confirmed" to "Plug Confirmed" in the data before recreating table
    existing_data$plugging_status[existing_data$plugging_status == "Confirmed"] <- "Plug Confirmed"
    
    dbExecute(con, "DROP TABLE plugging_history")
    dbExecute(con, paste0(
      "CREATE TABLE plugging_history (",
      "id INTEGER PRIMARY KEY AUTOINCREMENT,",
      "male_id TEXT NOT NULL,",
      "female_id TEXT NOT NULL,",
      "cage_id TEXT,",
      "pairing_start_date DATE,",
      "pairing_end_date DATE,",
      "plug_observed_date TEXT,",
      "plugging_status TEXT DEFAULT 'Ongoing' CHECK (plugging_status IN ('Ongoing', 'Plugged', 'Plug Confirmed', 'Not Pregnant', 'Not Observed (Waiting for confirmation)', 'Empty', 'Deleted', 'Not Observed (Confirmed)')),",
      "notes TEXT,",
      "created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,",
      "updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,",
      "expected_age_for_harvesting TEXT"
      ,")"
    ))
    if (nrow(existing_data) > 0) {
      dbWriteTable(con, "plugging_history", existing_data, append = TRUE, row.names = FALSE)
    }
    cat("Plugging history table updated with new status constraints (including 'Plug Confirmed')\n")
  }, error = function(e) {
    cat("Error updating plugging history table:", e$message, "\n")
  })
  
  dbDisconnect(con)
}

# Add audit_trail table if not exists
add_audit_trail_table <- function() {
  # This function is now handled by the enhanced audit trail system
  # The enhanced audit trail table is created in audit_trail.R
  cat("Audit trail table initialization handled by enhanced audit trail system.\n")
}

# Call this after initialize_db
initialize_db()
add_breeding_tables()
add_breeding_status_column()
add_plugging_tables()
# add_audit_trail_table() # Removed - now handled by enhanced audit trail
cat("Breeding and plugging tables ready.\n")

DB_PATH <<- normalizePath(DEFAULT_DB_NAME)

cat(paste("Database ready at", DB_PATH, "\n"))

# Note: Audit trail functions are now provided by the enhanced audit_trail.R module
# The following functions are maintained for backward compatibility but delegate to the enhanced system
# - log_audit_action() - Use the enhanced version from audit_trail.R
# - get_mouse_modification_history() - Use the enhanced version from audit_trail.R

# Utility: Extract ASU ID from animal_id
extract_asu_id <- function(animal_id) {
  # Split by '/', '//' or space
  parts <- unlist(strsplit(animal_id, "/|//| "))
  # Find all numeric substrings
  nums <- regmatches(parts, gregexpr("[0-9]+", parts))
  nums <- unlist(nums)
  if (length(nums) == 0) return(NA)
  # Return the longest numeric substring
  nums[which.max(nchar(nums))]
}

# Utility: Parse Excel data frame to match mice_stock schema with flexible column mapping
parse_excel_to_mice_stock <- function(df) {
  # Define required and optional field mappings with suggestions
  field_mappings <- list(
    # Required fields
    required = list(
      animal_id = c("Animal-ID", "Animal ID", "AnimalID", "ID", "Animal"),
      asu_id = c("ASU ID", "ASUID", "ASU_ID", "ASU")
    ),
    # Optional fields with suggestions
    optional = list(
      ear_mark = c("Ear Mark", "EarMark", "Ear_Mark", "Ear", "Mark"),
      gender = c("Gender", "Sex", "S", "G"),
      dob = c("DoB", "Date of Birth", "Birth Date", "BirthDate", "Date", "DOB"),
      genotype = c("Genotype", "Geno"),
      transgenes = c("Transgenes", "Transgene", "Trans"),
      strain = c("Strain", "Mouse Strain", "Strain Type"),
      breeding_line = c("Breeding line", "Breeding Line", "BreedingLine", "Breeding_Line", "Line", "Breeding"),
      dam = c("Dam", "Mother", "Female Parent", "Dam ID"),
      sire = c("Sire", "Father", "Male Parent", "Sire ID"),
      cage_id = c("Cage-ID", "Cage ID", "CageID", "Cage_ID", "Cage"),
      room = c("Room", "Room Number", "RoomNum", "Location"),
      project_code = c("Project code", "Project Code", "ProjectCode", "Project_Code", "Project", "Code"),
      responsible_person = c("Responsible Person", "ResponsiblePerson", "Responsible_Person", "Team", "Person", "Responsible"),
      protocol = c("19b protocol", "Protocol", "Protocol Number", "ProtocolNum", "Protocol_Number"),
      notes = c("Notes", "Note", "Comments", "Comment", "Description")
    )
  )
  
  # Function to find best matching column
  find_best_match <- function(field_name, suggestions, available_columns) {
    # Exact matches first
    for (suggestion in suggestions) {
      if (suggestion %in% available_columns) {
        return(suggestion)
      }
    }
    
    # Partial matches (case insensitive)
    available_lower <- tolower(available_columns)
    for (suggestion in suggestions) {
      suggestion_lower <- tolower(suggestion)
      for (i in seq_along(available_columns)) {
        if (grepl(suggestion_lower, available_lower[i], fixed = TRUE) || 
            grepl(available_lower[i], suggestion_lower, fixed = TRUE)) {
          return(available_columns[i])
        }
      }
    }
    
    return(NULL)
  }
  
  # Find mappings for all fields
  available_columns <- names(df)
  mappings <- list()
  
  # Process required fields
  for (field in names(field_mappings$required)) {
    suggestions <- field_mappings$required[[field]]
    best_match <- find_best_match(field, suggestions, available_columns)
    mappings[[field]] <- best_match
  }
  
  # Process optional fields
  for (field in names(field_mappings$optional)) {
    suggestions <- field_mappings$optional[[field]]
    best_match <- find_best_match(field, suggestions, available_columns)
    mappings[[field]] <- best_match
  }
  
  # Return mappings for user confirmation
  return(list(
    mappings = mappings,
    available_columns = available_columns,
    field_suggestions = field_mappings
  ))
}

# Function to apply confirmed mappings and create the data frame
apply_mappings_and_create_df <- function(df, confirmed_mappings, stock_category = 'Experiment') {
  # Ensure animal_id from confirmed mapping or Animal-ID
  if (!is.null(confirmed_mappings$animal_id) && confirmed_mappings$animal_id %in% names(df)) {
    df$animal_id <- as.character(df[[confirmed_mappings$animal_id]])
  } else if ("Animal-ID" %in% names(df)) {
    df$animal_id <- as.character(df$`Animal-ID`)
  } else {
    stop("No animal_id column found or mapped")
  }
  
  # Always extract ASU ID from animal_id
  df$asu_id <- vapply(df$animal_id, extract_asu_id, character(1))
  
  # Build output data frame with confirmed mappings
  out <- data.frame(
    asu_id = df$asu_id,
    animal_id = df$animal_id,
    ear_mark = if (!is.null(confirmed_mappings$ear_mark)) as.character(df[[confirmed_mappings$ear_mark]]) else NA,
    gender = if (!is.null(confirmed_mappings$gender)) as.character(df[[confirmed_mappings$gender]]) else NA,
    dob = if (!is.null(confirmed_mappings$dob)) as.character(df[[confirmed_mappings$dob]]) else NA,
    genotype = if (!is.null(confirmed_mappings$genotype)) as.character(df[[confirmed_mappings$genotype]]) else NA,
    transgenes = if (!is.null(confirmed_mappings$transgenes)) as.character(df[[confirmed_mappings$transgenes]]) else NA,
    strain = if (!is.null(confirmed_mappings$strain)) as.character(df[[confirmed_mappings$strain]]) else 'C57BL/6J',
    breeding_line = if (!is.null(confirmed_mappings$breeding_line)) as.character(df[[confirmed_mappings$breeding_line]]) else NA,
    dam = if (!is.null(confirmed_mappings$dam)) as.character(df[[confirmed_mappings$dam]]) else NA,
    sire = if (!is.null(confirmed_mappings$sire)) as.character(df[[confirmed_mappings$sire]]) else NA,
    cage_id = if (!is.null(confirmed_mappings$cage_id)) as.character(df[[confirmed_mappings$cage_id]]) else NA,
    room = if (!is.null(confirmed_mappings$room)) as.character(df[[confirmed_mappings$room]]) else NA,
    project_code = if (!is.null(confirmed_mappings$project_code)) as.character(df[[confirmed_mappings$project_code]]) else NA,
    responsible_person = if (!is.null(confirmed_mappings$responsible_person)) as.character(df[[confirmed_mappings$responsible_person]]) else NA,
    protocol = if (!is.null(confirmed_mappings$protocol)) as.character(df[[confirmed_mappings$protocol]]) else NA,
    stock_category = stock_category,
    status = 'Alive',
    date_of_death = NA,
    age_at_death_weeks = NA,
    max_severity = NA,
    procedure = NA,
    stage = NA,
    deceased_timestamp = NA,
    notes = if (!is.null(confirmed_mappings$notes)) as.character(df[[confirmed_mappings$notes]]) else NA,
    imported_from_excel = TRUE,
    stringsAsFactors = FALSE
  )
  
  # Standardize gender values
  out$gender <- ifelse(
    tolower(out$gender) %in% c("male", "m"), "Male",
    ifelse(tolower(out$gender) %in% c("female", "f"), "Female", NA)
  )
  
  return(out)
}

# Utility: Process duplicates based on user actions
process_duplicates <- function(parsed_df, comparison_data, import_duplicates, db_conflicts, user_actions) {
  # Process each duplicate based on action
  final_df <- data.frame()
  skipped_count <- 0
  modified_count <- 0
  overwritten_count <- 0
  
  for (i in 1:nrow(comparison_data)) {
    # Get the action from user_actions
    action <- user_actions[[paste0("action_", i)]]
    if (is.null(action)) action <- "Skip"
    
    asu_id <- comparison_data$ASU_ID[i]
    
    if (action == "Skip") {
      skipped_count <- skipped_count + 1
      next  # Skip this record
    } else if (action == "Modify") {
      # Generate new ASU ID
      row_data <- parsed_df[parsed_df$asu_id == asu_id, ]
      if (nrow(row_data) > 0) {
        row_data$asu_id <- paste0(asu_id, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
        final_df <- rbind(final_df, row_data)
        modified_count <- modified_count + 1
      }
    } else if (action == "Overwrite") {
      # Delete existing record and add new one
      con <- dbConnect(SQLite(), DB_PATH)
      dbExecute(con, paste0("DELETE FROM ", TABLE_NAME, " WHERE asu_id = '", asu_id, "'"))
      dbDisconnect(con)
      
      row_data <- parsed_df[parsed_df$asu_id == asu_id, ]
      if (nrow(row_data) > 0) {
        final_df <- rbind(final_df, row_data)
        overwritten_count <- overwritten_count + 1
      }
    } else if (action == "Keep Both") {
      # Generate new ASU ID and keep both
      row_data <- parsed_df[parsed_df$asu_id == asu_id, ]
      if (nrow(row_data) > 0) {
        row_data$asu_id <- paste0(asu_id, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
        final_df <- rbind(final_df, row_data)
        modified_count <- modified_count + 1
      }
    }
  }
  
  # Add non-duplicate records
  non_duplicates <- parsed_df[!parsed_df$asu_id %in% c(import_duplicates, db_conflicts), ]
  final_df <- rbind(final_df, non_duplicates)
  
  return(list(
    final_df = final_df,
    skipped_count = skipped_count,
    modified_count = modified_count,
    overwritten_count = overwritten_count
  ))
}

# Function to check for duplicates and conflicts
check_duplicates_and_conflicts <- function(parsed_df) {
  # Check for duplicates in import file
  import_duplicates <- parsed_df$asu_id[duplicated(parsed_df$asu_id)]
  
  # Check for conflicts with existing database
  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  existing_ids <- DBI::dbGetQuery(con, "SELECT asu_id FROM mice_stock")$asu_id
  DBI::dbDisconnect(con)
  db_conflicts <- parsed_df$asu_id[parsed_df$asu_id %in% existing_ids]
  
  # Create detailed comparison data
  comparison_data <- data.frame()
  exact_matches <- data.frame()
  
  # Handle import file duplicates
  if (length(import_duplicates) > 0) {
    dup_rows <- parsed_df[parsed_df$asu_id %in% import_duplicates, ]
    for (i in 1:nrow(dup_rows)) {
      comparison_data <- rbind(comparison_data, data.frame(
        Type = "Import Duplicate",
        ASU_ID = dup_rows$asu_id[i],
        Animal_ID = dup_rows$animal_id[i],
        Gender = dup_rows$gender[i],
        Breeding_Line = dup_rows$breeding_line[i],
        Genotype = dup_rows$genotype[i],
        DoB = dup_rows$dob[i],
        Age = as.numeric(Sys.Date() - as.Date(dup_rows$dob[i])) / 7, # Age in weeks
        Action = "Skip",
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Handle database conflicts
  if (length(db_conflicts) > 0) {
    # Get existing data from database
    con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
    existing_data <- DBI::dbGetQuery(con, paste0(
      "SELECT asu_id, animal_id, gender, breeding_line, genotype, dob FROM ", TABLE_NAME, 
      " WHERE asu_id IN ('", paste(db_conflicts, collapse = "','"), "')"
    ))
    DBI::dbDisconnect(con)
    
    # Get import data for conflicts
    conflict_import <- parsed_df[parsed_df$asu_id %in% db_conflicts, ]
    
    for (i in 1:nrow(conflict_import)) {
      asu_id <- conflict_import$asu_id[i]
      existing_row <- existing_data[existing_data$asu_id == asu_id, ]
      
      # Compare fields and find differences
      differences <- list()
      has_differences <- FALSE
      
      # Compare each field
      fields_to_compare <- c("animal_id", "gender", "breeding_line", "genotype", "dob")
      for (field in fields_to_compare) {
        import_val <- conflict_import[[field]][i]
        db_val <- existing_row[[field]]
        
        # Handle NA comparisons
        if (is.na(import_val) && !is.na(db_val)) {
          # Import has NA but DB has data - this is not a conflict
          next
        } else if (!is.na(import_val) && is.na(db_val)) {
          # Import has data but DB has NA - this is a difference
          differences[[field]] <- paste("Import:", import_val, "| DB: NA")
          has_differences <- TRUE
        } else if (!is.na(import_val) && !is.na(db_val) && import_val != db_val) {
          # Both have data but different - this is a difference
          differences[[field]] <- paste("Import:", import_val, "| DB:", db_val)
          has_differences <- TRUE
        }
      }
      
      if (has_differences) {
        # Show differences in comparison table
        comparison_data <- rbind(comparison_data, data.frame(
          Type = "Database Conflict",
          ASU_ID = asu_id,
          Animal_ID = if ("animal_id" %in% names(differences)) differences$animal_id else conflict_import$animal_id[i],
          Gender = if ("gender" %in% names(differences)) differences$gender else conflict_import$gender[i],
          Breeding_Line = if ("breeding_line" %in% names(differences)) differences$breeding_line else conflict_import$breeding_line[i],
          Genotype = if ("genotype" %in% names(differences)) differences$genotype else conflict_import$genotype[i],
          DoB = if ("dob" %in% names(differences)) differences$dob else conflict_import$dob[i],
          Age = if ("dob" %in% names(differences)) {
            paste("Import:", round(as.numeric(Sys.Date() - as.Date(conflict_import$dob[i])) / 7, 1), 
                  "| DB:", round(as.numeric(Sys.Date() - as.Date(existing_row$dob)) / 7, 1))
          } else {
            round(as.numeric(Sys.Date() - as.Date(conflict_import$dob[i])) / 7, 1)
          },
          Action = "Skip",
          stringsAsFactors = FALSE
        ))
      } else {
        # Exact match or import has NA but DB has data
        exact_matches <- rbind(exact_matches, data.frame(
          ASU_ID = asu_id,
          Animal_ID = conflict_import$animal_id[i],
          Status = "Exactly the same or import has NA",
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  return(list(
    comparison_data = comparison_data,
    exact_matches = exact_matches,
    import_duplicates = import_duplicates,
    db_conflicts = db_conflicts,
    has_duplicates = length(import_duplicates) > 0 || length(db_conflicts) > 0
  ))
}

# Function to import data to database
import_data_to_db <- function(parsed_df) {
  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  result <- tryCatch({
    # Use direct SQL INSERT for each row to properly set timestamps
    for (i in 1:nrow(parsed_df)) {
      row <- parsed_df[i, ]
      DBI::dbExecute(con, 
        "INSERT INTO mice_stock (
          asu_id, animal_id, ear_mark, gender, dob, genotype, transgenes, strain, 
          breeding_line, dam, sire, cage_id, room, project_code, responsible_person, 
          protocol, stock_category, status, date_of_death, age_at_death_weeks, 
          max_severity, procedure, stage, deceased_timestamp, notes, imported_from_excel, 
          date_created, last_updated
        ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, DATETIME('now'), DATETIME('now'))",
        params = list(
          row$asu_id,
          row$animal_id,
          row$ear_mark,
          row$gender,
          as.character(row$dob),
          row$genotype,
          row$transgenes,
          row$strain,
          row$breeding_line,
          row$dam,
          row$sire,
          row$cage_id,
          row$room,
          row$project_code,
          row$responsible_person,
          row$protocol,
          row$stock_category,
          row$status,
          row$date_of_death,
          row$age_at_death_weeks,
          row$max_severity,
          row$procedure,
          row$stage,
          row$deceased_timestamp,
          row$notes,
          TRUE # imported_from_excel
        )
      )
    }
    TRUE
  }, error = function(e) {
    FALSE
  }, finally = {
    DBI::dbDisconnect(con)
  })
  return(result)
}

# Function to create column mapping UI
create_column_mapping_ui <- function(mapping_result, df) {
  mappings <- mapping_result$mappings
  available_columns <- mapping_result$available_columns
  field_suggestions <- mapping_result$field_suggestions
  
  # Get first 5 rows for preview
  preview_data <- head(df, 5)
  
  ui_elements <- list()
  
  # Add preview of Excel data
  ui_elements[[length(ui_elements) + 1]] <- div(
    style = "margin-bottom: 20px;",
    tags$h4("Preview of Excel Data (First 5 rows):"),
    div(
      style = "max-height: 300px; overflow-x: auto; border: 1px solid #ddd; padding: 10px; background-color: #f9f9f9;",
      renderTable(preview_data, striped = TRUE, bordered = TRUE, hover = TRUE)
    )
  )
  
  # Add mapping instructions
  ui_elements[[length(ui_elements) + 1]] <- div(
    style = "margin-bottom: 20px; padding: 10px; background-color: #e3f2fd; border-radius: 5px;",
    tags$h5("Column Mapping Instructions:"),
    tags$ul(
      tags$li("Select which Excel column should be mapped to each database field"),
      tags$li("Choose 'NA' if you don't want to import that field"),
      tags$li("Required fields (marked with *) must be mapped to proceed"),
      tags$li("ASU ID will be automatically extracted from Animal ID")
    )
  )
  
  # Add required fields
  ui_elements[[length(ui_elements) + 1]] <- tags$h4("Required Fields (*):")
  for (field in names(field_suggestions$required)) {
    # Skip asu_id since it's automatically extracted from animal_id
    if (field == "asu_id") next
    
    current_mapping <- mappings[[field]]
    suggestions <- field_suggestions$required[[field]]
    
    # Convert field name to human-readable format
    human_name <- switch(field,
      "animal_id" = "Animal ID",
      "asu_id" = "ASU ID",
      field
    )
    
    ui_elements[[length(ui_elements) + 1]] <- fluidRow(
      column(4, tags$strong(paste0(human_name, " *:"))),
      column(8, selectInput(
        inputId = paste0("mapping_", field),
        label = NULL,
        choices = c("NA", available_columns),
        selected = ifelse(is.null(current_mapping), "NA", current_mapping),
        width = "100%"
      ))
    )
    
    # Add suggestions
    if (!is.null(current_mapping)) {
      ui_elements[[length(ui_elements) + 1]] <- div(
        style = "margin-left: 20px; font-size: 12px; color: #666; margin-bottom: 10px;",
        paste("Suggested:", paste(suggestions, collapse = ", "))
      )
    }
  }
  
  # Add optional fields
  ui_elements[[length(ui_elements) + 1]] <- tags$h4("Optional Fields:")
  for (field in names(field_suggestions$optional)) {
    current_mapping <- mappings[[field]]
    suggestions <- field_suggestions$optional[[field]]
    
    # Convert field name to human-readable format
    human_name <- switch(field,
      "ear_mark" = "Ear Mark",
      "gender" = "Gender",
      "dob" = "Date of Birth",
      "genotype" = "Genotype",
      "transgenes" = "Transgenes",
      "strain" = "Strain",
      "breeding_line" = "Breeding Line",
      "dam" = "Dam",
      "sire" = "Sire",
      "cage_id" = "Cage ID",
      "room" = "Room",
      "project_code" = "Project Code",
      "responsible_person" = "Responsible Person",
      "protocol" = "Protocol",
      "notes" = "Notes",
      field
    )
    
    ui_elements[[length(ui_elements) + 1]] <- fluidRow(
      column(4, tags$strong(paste0(human_name, ":"))),
      column(8, selectInput(
        inputId = paste0("mapping_", field),
        label = NULL,
        choices = c("NA", available_columns),
        selected = ifelse(is.null(current_mapping), "NA", current_mapping),
        width = "100%"
      ))
    )
    
    # Add suggestions
    if (!is.null(current_mapping)) {
      ui_elements[[length(ui_elements) + 1]] <- div(
        style = "margin-left: 20px; font-size: 12px; color: #666; margin-bottom: 10px;",
        paste("Suggested:", paste(suggestions, collapse = ", "))
      )
    }
  }
  
  do.call(tagList, ui_elements)
}

# Independent function: Get modification history for a mouse by ASU ID
get_mouse_modification_history <- function(asu_id, db_path = DB_PATH) {
  # This function is now handled by the enhanced audit trail system
  # Use the enhanced get_mouse_modification_history from audit_trail.R instead
  cat("Using enhanced audit trail system from audit_trail.R\n")
} 