# db_check.R
suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(stringr)
})

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
      "study_plan TEXT DEFAULT 'SP2500090' CHECK (study_plan IN ('SP2500090', 'SP2500083', 'SP2500082', 'SP2500081')),",
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
    
    # Check if study_plan column exists
    if (!"study_plan" %in% table_info$name) {
      dbExecute(con, paste0("ALTER TABLE ", TABLE_NAME, " ADD COLUMN study_plan TEXT DEFAULT 'SP2500090' CHECK (study_plan IN ('SP2500090', 'SP2500083', 'SP2500082', 'SP2500081'))"))
    }
    
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
        "study_plan TEXT DEFAULT 'SP2500090' CHECK (study_plan IN ('SP2500090', 'SP2500083', 'SP2500082', 'SP2500081')),",
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
    "plugging_status TEXT DEFAULT 'Ongoing' CHECK (plugging_status IN ('Ongoing', 'Plugged', 'Plug Confirmed', 'Not Pregnant', 'Not Observed (Waiting for confirmation)', 'Empty', 'Deleted', 'Not Observed (Confirmed)', 'Surprising Plug!!', 'Collected')),",
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
  dbExecute(con, "UPDATE plugging_history SET plugging_status = 'Collected' WHERE plugging_status = 'Collected'")
  
  # Update the CHECK constraint for plugging_status to include the new status
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
      "plugging_status TEXT DEFAULT 'Ongoing' CHECK (plugging_status IN ('Ongoing', 'Plugged', 'Plug Confirmed', 'Not Pregnant', 'Not Observed (Waiting for confirmation)', 'Empty', 'Deleted', 'Not Observed (Confirmed)', 'Surprising Plug!!', 'Collected')),",
      "notes TEXT,",
      "created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,",
      "updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,",
      "expected_age_for_harvesting TEXT"
      ,")"
    ))
    if (nrow(existing_data) > 0) {
      dbWriteTable(con, "plugging_history", existing_data, append = TRUE, row.names = FALSE)
    }
  }, error = function(e) {
    cat("Error updating plugging history table:", e$message, "\n")
  })
  
  dbDisconnect(con)
}

# Add body_weight_history table if not exists
add_body_weight_table <- function() {
  con <- dbConnect(SQLite(), DEFAULT_DB_NAME)
  
  # Create body weight history table with correct column names (only if it doesn't exist)
  dbExecute(con, paste0(
    "CREATE TABLE IF NOT EXISTS body_weight_history (",
    "id INTEGER PRIMARY KEY AUTOINCREMENT,",
    "asu_id TEXT NOT NULL,",
    "weight_grams REAL NOT NULL,",
    "measurement_date DATE NOT NULL,",
    "notes TEXT,",
    "created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,",
    "updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,",
    "FOREIGN KEY(asu_id) REFERENCES mice_stock(asu_id) ON DELETE CASCADE"
    ,")"
  ))
  dbDisconnect(con)
}





# Call this after initialize_db
initialize_db()
add_breeding_tables()
add_breeding_status_column()
add_plugging_tables()
add_body_weight_table()
# add_audit_trail_table() # Removed - now handled by enhanced audit trail

DB_PATH <<- normalizePath(DEFAULT_DB_NAME)

# Note: Audit trail functions are now provided by the enhanced audit_trail.R module

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
    # Required fields (asu_id is automatically generated from animal_id)
    required = list(
      animal_id = c("Animal-ID", "Animal ID", "AnimalID", "ID", "Animal")
    ),
    # Optional fields with suggestions
    optional = list(
      #asu_id = c("ASU ID", "ASUID", "ASU_ID", "ASU"),  # Optional mapping, will be auto-generated from animal_id
      ear_mark = c("Ear Mark", "EarMark", "Ear_Mark", "Ear", "Mark"),
      gender = c("Gender", "Sex", "S", "G"),
      dob = c("DoB", "Date of Birth", "Birth Date", "BirthDate", "Date", "DOB"),
      genotype = c("Genotype", "Geno"),
      strain = c("Strain", "Mouse Strain", "Strain Type"),
      breeding_line = c("Breeding line", "Breeding Line", "BreedingLine", "Breeding_Line", "Line", "Breeding"),
      dam = c("Dam", "Mother", "Female Parent", "Dam ID"),
      sire = c("Sire", "Father", "Male Parent", "Sire ID"),
      cage_id = c("Cage-ID", "Cage ID", "CageID", "Cage_ID", "Cage"),
      room = c("Room", "Room Number", "RoomNum", "Location"),
      project_code = c("Project code", "Project Code", "ProjectCode", "Project_Code", "Project", "Code"),
      responsible_person = c("Responsible Person", "ResponsiblePerson", "Responsible_Person", "Team", "Person", "Responsible"),
      protocol = c("19b protocol", "Protocol", "Protocol Number", "ProtocolNum", "Protocol_Number"),
      study_plan = c("Study Plan", "StudyPlan", "Study_Plan", "Plan", "Study"),
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
  
  # Exclude specific columns that should not be imported
  excluded_columns <- c(
    "Age", "age", "AGE",
    "No. of animals", "No of animals", "Number of animals", "Count", "Num", "Animals",
    "Team", "TEAM", "team",
    "Cage Type", "Cage type", "cage type", "CageType", "CAGE TYPE"
  )
  
  # Filter out excluded columns (case-insensitive partial matching)
  available_columns <- available_columns[!sapply(available_columns, function(col) {
    col_lower <- tolower(col)
    any(sapply(excluded_columns, function(excl) {
      excl_lower <- tolower(excl)
      grepl(excl_lower, col_lower, fixed = TRUE) || grepl(col_lower, excl_lower, fixed = TRUE)
    }))
  })]
  
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
    study_plan = if (!is.null(confirmed_mappings$study_plan)) as.character(df[[confirmed_mappings$study_plan]]) else 'SP2500090',
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

# Utility: Process duplicates based on user actions with custom ASU IDs
process_duplicates_with_custom <- function(parsed_df, comparison_data, import_duplicates, db_conflicts, user_actions, custom_asu_map = list()) {
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
    } else if (action == "Keep Both") {
      # Generate new ASU ID and keep both (use custom if provided)
      row_data <- parsed_df[parsed_df$asu_id == asu_id, ]
      if (nrow(row_data) > 0) {
        new_asu_id <- if (!is.null(custom_asu_map[[asu_id]])) {
          custom_asu_map[[asu_id]]
        } else {
          paste0(asu_id, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
        }
        row_data$asu_id <- new_asu_id
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
    } else if (action == "Keep Both") {
      # Generate new ASU ID and keep both
      row_data <- parsed_df[parsed_df$asu_id == asu_id, ]
      if (nrow(row_data) > 0) {
        new_asu_id <- paste0(asu_id, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
        row_data$asu_id <- new_asu_id
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
      
      # Compare all fields to determine if records are identical or different
      fields_to_compare <- c("animal_id", "gender", "breeding_line", "genotype", "dob")
      is_identical <- TRUE
      
      # Check if all comparable fields are identical
      for (field in fields_to_compare) {
        import_val <- conflict_import[[field]][i]
        db_val <- existing_row[[field]]
        
        # Convert to character for comparison, treating NA as empty string
        import_str <- if (is.na(import_val)) "" else as.character(import_val)
        db_str <- if (is.na(db_val)) "" else as.character(db_val)
        
        if (import_str != db_str) {
          is_identical <- FALSE
          break
        }
      }
      
      if (is_identical) {
        # Truly identical records - skip them
        exact_matches <- rbind(exact_matches, data.frame(
          ASU_ID = asu_id,
          Animal_ID = conflict_import$animal_id[i],
          Status = "Identical data",
          stringsAsFactors = FALSE
        ))
      } else {
        # Records have differences - add to comparison for user decision
        comparison_data <- rbind(comparison_data, data.frame(
          Type = "Database Conflict",
          ASU_ID = asu_id,
          Import_Animal_ID = as.character(conflict_import$animal_id[i]),
          DB_Animal_ID = as.character(existing_row$animal_id),
          Import_Gender = as.character(conflict_import$gender[i]),
          DB_Gender = as.character(existing_row$gender),
          Import_Breeding_Line = as.character(conflict_import$breeding_line[i]),
          DB_Breeding_Line = as.character(existing_row$breeding_line),
          Import_Genotype = as.character(conflict_import$genotype[i]),
          DB_Genotype = as.character(existing_row$genotype),
          Import_DoB = as.character(conflict_import$dob[i]),
          DB_DoB = as.character(existing_row$dob),
          Action = "Skip",
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
  
  # Get first 3 rows for preview (df is already filtered upstream)
  preview_data <- head(df, 3)
  
  ui_elements <- list()
  
  # Add mapping instructions first
  ui_elements[[length(ui_elements) + 1]] <- div(
    style = "margin-bottom: 20px; padding: 10px; background-color: #e3f2fd; border-radius: 5px;",
    tags$h5("Column Mapping Instructions:"),
    tags$ul(
      tags$li("Use the dropdown above each column to map it to a database field"),
      tags$li("Choose 'NA' if you don't want to import that column"),
      tags$li("Required fields (marked with *) must be mapped to proceed"),
      tags$li("ASU ID will be automatically extracted from Animal ID")
    )
  )
  
  # Create all possible database field choices with human-readable names
  all_db_fields <- list(
    "NA" = "NA",
    "Animal ID *" = "animal_id",
    "Ear Mark" = "ear_mark", 
    "Gender" = "gender",
    "Date of Birth" = "dob",
    "Genotype" = "genotype",
    "Strain" = "strain",
    "Breeding Line" = "breeding_line",
    "Dam" = "dam",
    "Sire" = "sire", 
    "Cage ID" = "cage_id",
    "Room" = "room",
    "Project Code" = "project_code",
    "Responsible Person" = "responsible_person",
    "Protocol" = "protocol",
    "Study Plan" = "study_plan",
    "Notes" = "notes"
  )
  
  # Function to check if a column contains all NA values
  is_column_all_na <- function(col_name) {
    if (col_name %in% names(df)) {
      column_data <- df[[col_name]]
      # Check if all values are NA, empty strings, whitespace only, or boolean values
      all_na <- all(is.na(column_data) | 
                   trimws(as.character(column_data)) == "" | 
                   trimws(as.character(column_data)) == "NA" |
                   toupper(trimws(as.character(column_data))) %in% c("TRUE", "FALSE"))
      return(all_na)
    }
    return(FALSE)
  }
  
  # Function to find best suggestion for a column (enhanced version)
  find_column_suggestion <- function(col_name) {
    # First check if the column contains all NA values
    if (is_column_all_na(col_name)) {
      return("NA")
    }
    
    # Use the original smart mapping logic from the mappings result
    if (!is.null(mappings)) {
      # First check if this column was already mapped in the original suggestions
      for (field in names(mappings)) {
        if (!is.null(mappings[[field]]) && mappings[[field]] == col_name) {
          return(field)
        }
      }
    }
    
    # Fallback to pattern matching
    col_lower <- tolower(col_name)
    
    # Check all field suggestions with exact matches first
    all_suggestions <- c(field_suggestions$required, field_suggestions$optional)
    for (field in names(all_suggestions)) {
      suggestions <- all_suggestions[[field]]
      for (suggestion in suggestions) {
        # Exact match (case insensitive)
        if (tolower(suggestion) == col_lower) {
          return(field)
        }
      }
    }
    
    # Then check partial matches
    for (field in names(all_suggestions)) {
      suggestions <- all_suggestions[[field]]
      for (suggestion in suggestions) {
        # Partial matches
        if (grepl(tolower(suggestion), col_lower, fixed = TRUE) || 
            grepl(col_lower, tolower(suggestion), fixed = TRUE)) {
          return(field)
        }
      }
    }
    
    return("NA")
  }
  
  # Add warning area for duplicate mappings
  ui_elements[[length(ui_elements) + 1]] <- div(
    id = "mapping_warnings_container",
    style = "margin-top: 10px; margin-bottom: 10px;",
    uiOutput("mapping_warnings")
  )
  
  # Create table with dropdowns above each column
  ui_elements[[length(ui_elements) + 1]] <- div(
    style = "margin-top: 20px; margin-bottom: 20px;",
    tags$h4("Column Mapping and Preview (first 3 rows):"),
    div(
      style = "border: 1px solid #ddd; background-color: #f9f9f9; overflow-x: auto; overflow-y: visible;",
      # Create dropdown header row
      div(
        style = "display: table; width: 100%; table-layout: fixed; background-color: #e9ecef;",
        div(
          style = "display: table-row;",
          lapply(1:ncol(preview_data), function(i) {
            col_name <- names(preview_data)[i]
            suggested_field <- find_column_suggestion(col_name)
            
            div(
              style = "display: table-cell; padding: 8px; border-right: 1px solid #ddd; vertical-align: top; min-width: 180px; max-width: 250px; width: 180px; box-sizing: border-box;",
              div(
                style = "font-weight: bold; margin-bottom: 5px; font-size: 12px; text-align: center; word-wrap: break-word;",
                col_name
              ),
              div(
                style = "margin-bottom: 0px;",
                selectInput(
                  inputId = paste0("mapping_col_", i),
                  label = NULL,
                  choices = all_db_fields,
                  selected = if (suggested_field != "NA") suggested_field else "NA",
                  width = "calc(100% - 0px)"
                )
              )
            )
          })
        )
      ),
      # Create data preview rows
      div(
        style = "display: table; width: 100%; table-layout: fixed;",
        # Header row with column names (hidden since we show them above)
        # Data rows
        lapply(1:nrow(preview_data), function(row_i) {
          div(
            style = paste0("display: table-row; ", 
                          if (row_i %% 2 == 0) "background-color: #f8f9fa;" else "background-color: white;"),
            lapply(1:ncol(preview_data), function(col_i) {
              # Smart date formatting for better display
              cell_value <- preview_data[row_i, col_i]
              
              # Check if this looks like a date column and try to format it properly
              display_value <- tryCatch({
                # Convert cell value to numeric if possible
                numeric_value <- NA
                
                # Handle different data types
                if (is.numeric(cell_value) && !is.na(cell_value)) {
                  numeric_value <- cell_value
                } else if (!is.na(cell_value)) {
                  # Convert to character and clean
                  char_value <- trimws(as.character(cell_value))
                  
                  # Only try to convert if it's not empty and looks like a number
                  if (char_value != "" && char_value != "NA") {
                    # Try numeric conversion with warning suppression
                    test_numeric <- suppressWarnings(as.numeric(char_value))
                    if (!is.na(test_numeric)) {
                      numeric_value <- test_numeric
                    }
                  }
                }
                
                # If it's numeric (or can be converted to numeric) and could be a date
                if (!is.na(numeric_value)) {
                  # Check for Unix timestamp (seconds since 1970-01-01)
                  if (numeric_value > 1000000000 && numeric_value < 2147483647) {
                    # Unix timestamp - convert from seconds
                    date_val <- as.Date(as.POSIXct(numeric_value, origin = "1970-01-01"))
                    format(date_val, "%Y-%b-%d")
                  } 
                  # Check for Unix timestamp in milliseconds
                  else if (numeric_value > 1000000000000 && numeric_value < 2147483647000) {
                    # Unix timestamp in milliseconds - convert from milliseconds
                    date_val <- as.Date(as.POSIXct(numeric_value/1000, origin = "1970-01-01"))
                    format(date_val, "%Y-%b-%d")
                  }
                  # Check for Excel date (between reasonable bounds)
                  else if (numeric_value > 25000 && numeric_value < 80000) {
                    # Convert Excel numeric date to R date
                    date_val <- as.Date(numeric_value, origin = "1899-12-30")
                    format(date_val, "%Y-%b-%d")
                  } else {
                    # Not a recognizable date format, return as character
                    as.character(cell_value)
                  }
                } else if (inherits(cell_value, "Date")) {
                  # Already a Date object
                  format(cell_value, "%Y-%b-%d")
                } else if (inherits(cell_value, "POSIXt")) {
                  # DateTime object
                  format(as.Date(cell_value), "%Y-%b-%d")
                } else {
                  # Default to character conversion
                  as.character(cell_value)
                }
              }, error = function(e) {
                # Fallback to character conversion if date parsing fails
                as.character(cell_value)
              })
              
              div(
                style = "display: table-cell; padding: 8px; border-right: 1px solid #ddd; border-bottom: 1px solid #ddd; vertical-align: top; min-width: 180px; max-width: 250px; width: 180px; text-align: center; word-wrap: break-word; box-sizing: border-box;",
                display_value
              )
            })
          )
        })
      )
    )
  )
  
  do.call(tagList, ui_elements)
}

# Function to validate column mappings and detect duplicates
validate_column_mappings <- function(input, column_count) {
  # Collect all selected mappings
  selected_mappings <- list()
  duplicate_fields <- character(0)
  
  for (i in seq_len(column_count)) {
    input_id <- paste0("mapping_col_", i)
    selected_value <- input[[input_id]]
    
    if (!is.null(selected_value) && selected_value != "NA") {
      if (selected_value %in% names(selected_mappings)) {
        # This field is already mapped to another column
        duplicate_fields <- c(duplicate_fields, selected_value)
      } else {
        selected_mappings[[selected_value]] <- i
      }
    }
  }
  
  return(list(
    is_valid = length(duplicate_fields) == 0,
    duplicate_fields = unique(duplicate_fields),
    selected_mappings = selected_mappings
  ))
}

# Function to generate warning CSS for duplicate mappings
generate_mapping_warnings <- function(validation_result, column_count, input = NULL) {
  warning_css <- ""
  
  if (!validation_result$is_valid && !is.null(input)) {
    # Find which input elements have duplicate field selections
    duplicate_input_ids <- character(0)
    
    # For each duplicate field, find all columns that selected it
    for (field in validation_result$duplicate_fields) {
      for (i in seq_len(column_count)) {
        input_id <- paste0("mapping_col_", i)
        selected_value <- input[[input_id]]
        
        if (!is.null(selected_value) && selected_value == field) {
          duplicate_input_ids <- c(duplicate_input_ids, input_id)
        }
      }
    }
    
    if (length(duplicate_input_ids) > 0) {
      warning_css <- paste0(
        "<style>",
        paste(sapply(unique(duplicate_input_ids), function(col_id) {
          paste0("#", col_id, " { border: 2px solid #ff6b6b !important; background-color: #ffe6e6 !important; }")
        }), collapse = " "),
        "</style>"
      )
    }
  }
  
  return(warning_css)
}

# Function to create warning message for duplicate mappings
create_duplicate_mapping_warning <- function(duplicate_fields) {
  if (length(duplicate_fields) == 0) {
    return(NULL)
  }
  
  # Get human-readable field names
  field_name_map <- list(
    "animal_id" = "Animal ID",
    "ear_mark" = "Ear Mark", 
    "gender" = "Gender",
    "dob" = "Date of Birth",
    "genotype" = "Genotype",
    "strain" = "Strain",
    "breeding_line" = "Breeding Line",
    "dam" = "Dam",
    "sire" = "Sire", 
    "cage_id" = "Cage ID",
    "room" = "Room",
    "project_code" = "Project Code",
    "responsible_person" = "Responsible Person",
    "protocol" = "Protocol",
    "study_plan" = "Study Plan",
    "notes" = "Notes"
  )
  
  readable_fields <- sapply(duplicate_fields, function(field) {
    if (field %in% names(field_name_map)) {
      field_name_map[[field]]
    } else {
      field
    }
  })
  
  div(
    style = "margin: 10px 0; padding: 10px; background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 4px; color: #856404;",
    tags$strong("⚠️ Warning: Duplicate Column Mappings Detected"),
    tags$br(),
    paste("The following database fields are mapped to multiple columns:", 
          paste(readable_fields, collapse = ", ")),
    tags$br(),
    "Please ensure each database field is mapped to only one Excel column."
  )
}

