# database_manager.R
# Database management functions for switching and managing hidden databases

suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RSQLite)
})

# Initialize all required directories
initialize_hidden_directories <- function() {
  tryCatch({
    # Define directories with fallbacks
    hidden_dir <- if (exists("HIDDEN_DIR") && !is.null(HIDDEN_DIR)) {
      HIDDEN_DIR
    } else {
      file.path(Sys.getenv("HOME"), ".mousemanagement_DontRemove")
    }
    
    backups_dir <- if (exists("BACKUPS_DIR") && !is.null(BACKUPS_DIR)) {
      BACKUPS_DIR
    } else {
      file.path(hidden_dir, "backups")
    }
    
    config_dir <- if (exists("CONFIG_DIR") && !is.null(CONFIG_DIR)) {
      CONFIG_DIR
    } else {
      file.path(hidden_dir, "config")
    }
    
    dirs_to_create <- c(hidden_dir, backups_dir, config_dir)
    
    for (dir_path in dirs_to_create) {
      if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
      }
    }
    
    return(TRUE)
  }, error = function(e) {
    warning(paste("Failed to initialize directories:", e$message))
    return(FALSE)
  })
}

# Get current database path (checks config file for temporary switches)
get_current_db_path <- function() {
  # Check if there's a temporary database set in config file
  config_file <- file.path(CONFIG_DIR, "current_db.conf")
  
  if (file.exists(config_file)) {
    tryCatch({
      temp_db_path <- readLines(config_file, n = 1, warn = FALSE)
      if (length(temp_db_path) > 0 && file.exists(temp_db_path)) {
        # Validate the temporary database
        validation <- validate_database_file(temp_db_path)
        if (validation$valid) {
          return(temp_db_path)
        }
      }
    }, error = function(e) {
      # If config file is corrupted, ignore and use default
    })
  }
  
  # Fall back to main database
  return(DB_PATH)
}

# List main database and all backup databases
list_hidden_databases <- function() {
  all_db_paths <- c()
  all_db_labels <- c()
  
  # Add main database
  main_db_path <- file.path(HIDDEN_DIR, "mice_colony.db")
  if (file.exists(main_db_path)) {
    size <- file.info(main_db_path)$size
    if (is.na(size)) size <- 0
    all_db_paths <- c(all_db_paths, main_db_path)
    all_db_labels <- c(all_db_labels, paste0("mice_colony.db (CURRENT - ", size, " bytes)"))
  }
  
  # Add backup databases
  if (!dir.exists(BACKUPS_DIR)) {
    dir.create(BACKUPS_DIR, recursive = TRUE, showWarnings = FALSE)
  }
  
  backup_files <- list.files(BACKUPS_DIR, pattern = "\\.db$", full.names = FALSE)
  
  if (length(backup_files) > 0) {
    backup_paths <- file.path(BACKUPS_DIR, backup_files)
    
    # Get backup labels with sizes
    backup_labels <- sapply(1:length(backup_files), function(i) {
      tryCatch({
        size <- file.info(backup_paths[i])$size
        if (is.na(size)) size <- 0
        paste0(backup_files[i], " (", size, " bytes)")
      }, error = function(e) {
        paste0(backup_files[i], " (unknown size)")
      })
    })
    
    all_db_paths <- c(all_db_paths, backup_paths)
    all_db_labels <- c(all_db_labels, backup_labels)
  }
  
  # Return combined list
  if (length(all_db_paths) > 0) {
    names(all_db_paths) <- all_db_labels
    return(all_db_paths)
  } else {
    return(list("No databases found" = ""))
  }
}

# Validate database file
validate_database_file <- function(db_path) {
  if (!file.exists(db_path)) {
    return(list(valid = FALSE, message = "Database file does not exist"))
  }
  
  tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    
    # Check for required tables
    required_tables <- c("mice_stock", "plugging_history", "body_weight_history")
    existing_tables <- DBI::dbListTables(con)
    
    missing_tables <- setdiff(required_tables, existing_tables)
    
    DBI::dbDisconnect(con)
    
    if (length(missing_tables) > 0) {
      return(list(
        valid = FALSE, 
        message = paste("Missing required tables:", paste(missing_tables, collapse = ", "))
      ))
    }
    
    return(list(valid = TRUE, message = "Database is valid"))
    
  }, error = function(e) {
    return(list(valid = FALSE, message = paste("Database error:", e$message)))
  })
}

# Switch to database temporarily (session only)
switch_database <- function(new_db_path) {
  validation <- validate_database_file(new_db_path)
  
  if (!validation$valid) {
    return(list(success = FALSE, message = validation$message))
  }
  
  tryCatch({
    # Ensure config directory exists
    if (!dir.exists(CONFIG_DIR)) {
      dir.create(CONFIG_DIR, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Update config file for current session only
    config_file <- file.path(CONFIG_DIR, "current_db.conf")
    writeLines(new_db_path, config_file)
    
    # Update global DB_PATH for current session
    DB_PATH <<- normalizePath(new_db_path)
    
    # Check if switching to main database or backup
    main_db_path <- file.path(HIDDEN_DIR, "mice_colony.db")
    is_main_db <- normalizePath(new_db_path) == normalizePath(main_db_path)
    
    if (is_main_db) {
      message_text <- "Switched to main database (mice_colony.db)"
    } else {
      message_text <- paste("Temporarily viewing backup:", basename(new_db_path))
    }
    
    return(list(
      success = TRUE, 
      message = message_text,
      is_main = is_main_db
    ))
    
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error switching database:", e$message)))
  })
}

# Create new database in main database location
create_new_database <- function(db_name) {
  if (!dir.exists(DB_DIR)) {
    dir.create(DB_DIR, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Clean up database name
  if (is.null(db_name) || trimws(db_name) == "") {
    return(list(success = FALSE, message = "Database name cannot be empty"))
  }
  
  db_name <- gsub("[^a-zA-Z0-9_-]", "_", db_name)
  if (!grepl("\\.db$", db_name)) {
    db_name <- paste0(db_name, ".db")
  }
  
  new_db_path <- file.path(HIDDEN_DIR, db_name)
  
  if (file.exists(new_db_path)) {
    return(list(success = FALSE, message = "Database already exists"))
  }
  
  tryCatch({
    # Create new database with schema
    con <- DBI::dbConnect(RSQLite::SQLite(), new_db_path)
    
    # Create mice_stock table
    DBI::dbExecute(con, paste0(
      "CREATE TABLE mice_stock (",
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
      "last_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP",
      ")"
    ))
    
    # Create plugging_history table
    DBI::dbExecute(con, paste0(
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
      "expected_age_for_harvesting TEXT",
      ")"
    ))
    
    # Create body_weight_history table
    DBI::dbExecute(con, paste0(
      "CREATE TABLE body_weight_history (",
      "id INTEGER PRIMARY KEY AUTOINCREMENT,",
      "asu_id TEXT NOT NULL,",
      "weight_grams REAL NOT NULL,",
      "measurement_date DATE NOT NULL,",
      "notes TEXT,",
      "created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,",
      "updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,",
      "FOREIGN KEY(asu_id) REFERENCES mice_stock(asu_id) ON DELETE CASCADE",
      ")"
    ))
    
    DBI::dbDisconnect(con)
    
    return(list(
      success = TRUE, 
      message = paste("Created new database:", db_name),
      path = new_db_path
    ))
    
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error creating database:", e$message)))
  })
}

# Import database file - always replaces main database with pre-import backup
import_database_to_main <- function(source_path) {
  if (!file.exists(source_path)) {
    return(list(success = FALSE, message = "Source database file does not exist"))
  }
  
  validation <- validate_database_file(source_path)
  if (!validation$valid) {
    return(list(success = FALSE, message = validation$message))
  }
  
  if (!dir.exists(DB_DIR)) {
    dir.create(DB_DIR, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Main database path is always mice_colony.db
  main_db_path <- file.path(HIDDEN_DIR, "mice_colony.db")
  
  tryCatch({
    # Create pre-import backup of existing database if it exists
    if (file.exists(main_db_path)) {
      backup_result <- create_backup(main_db_path, backup_type = "preimport")
      if (!backup_result$success) {
        return(list(success = FALSE, message = paste("Failed to create pre-import backup:", backup_result$message)))
      }
    }
    
    # Copy imported database to main location with mice_colony.db name
    file.copy(source_path, main_db_path, overwrite = TRUE)
    
    return(list(
      success = TRUE,
      message = "Database imported and replaced main database (mice_colony.db). Previous database backed up.",
      path = main_db_path
    ))
    
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error importing database:", e$message)))
  })
}

# Get database info
get_database_info <- function(db_path = DB_PATH) {
  if (!file.exists(db_path)) {
    return(list(error = "Database file not found"))
  }
  
  tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    
    # Get table info
    tables <- DBI::dbListTables(con)
    
    info <- list(
      path = db_path,
      name = basename(db_path),
      size = file.info(db_path)$size,
      modified = file.info(db_path)$mtime,
      tables = tables
    )
    
    # Get record counts
    if ("mice_stock" %in% tables) {
      info$mice_count <- DBI::dbGetQuery(con, "SELECT COUNT(*) FROM mice_stock")[[1]]
    }
    if ("plugging_history" %in% tables) {
      info$plugging_count <- DBI::dbGetQuery(con, "SELECT COUNT(*) FROM plugging_history")[[1]]
    }
    if ("body_weight_history" %in% tables) {
      info$weight_records_count <- DBI::dbGetQuery(con, "SELECT COUNT(*) FROM body_weight_history")[[1]]
    }
    
    DBI::dbDisconnect(con)
    
    return(info)
    
  }, error = function(e) {
    return(list(error = paste("Database error:", e$message)))
  })
}

# Create automatic backup
create_backup <- function(db_path = DB_PATH, backup_dir = BACKUPS_DIR, backup_type = "manual") {
  if (!file.exists(db_path)) {
    return(list(success = FALSE, message = "Source database not found"))
  }
  
  # Ensure backup directory exists
  if (!dir.exists(backup_dir)) {
    dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  tryCatch({
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    
    # Always use mice_colony as base name for backups
    if (backup_type == "preimport") {
      backup_name <- paste0("mice_colony_", timestamp, "_preimport.db")
    } else {
      backup_name <- paste0("mice_colony_", timestamp, ".db")
    }
    backup_path <- file.path(backup_dir, backup_name)
    
    file.copy(db_path, backup_path)
    
    return(list(
      success = TRUE,
      message = paste("Backup created:", backup_name),
      path = backup_path
    ))
    
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Backup failed:", e$message)))
  })
}

# Check if automatic backup is needed
should_create_auto_backup <- function(db_path = DB_PATH, backup_dir = BACKUPS_DIR, 
                                     backup_interval_hours = 6) {
  if (!file.exists(db_path)) {
    return(FALSE)
  }
  
  # Ensure backup directory exists
  if (!dir.exists(backup_dir)) {
    dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
    return(TRUE)  # First time, create backup
  }
  
  # Get last auto backup
  db_name <- tools::file_path_sans_ext(basename(db_path))
  auto_backups <- list.files(backup_dir, 
                            pattern = paste0(db_name, "_auto_.*\\.db$"), 
                            full.names = TRUE)
  
  if (length(auto_backups) == 0) {
    return(TRUE)  # No auto backups exist
  }
  
  # Check the most recent auto backup
  latest_backup <- auto_backups[which.max(file.info(auto_backups)$mtime)]
  last_backup_time <- file.info(latest_backup)$mtime
  
  # Check if enough time has passed
  time_since_backup <- as.numeric(difftime(Sys.time(), last_backup_time, units = "hours"))
  
  return(time_since_backup >= backup_interval_hours)
}

# Create automatic startup backup
create_startup_backup <- function(db_path = DB_PATH, backup_dir = BACKUPS_DIR) {
  if (!should_create_auto_backup(db_path, backup_dir)) {
    return(list(success = TRUE, message = "Recent backup exists, skipping"))
  }
  
  result <- create_backup(db_path, backup_dir, backup_type = "auto")
  
  if (result$success) {
    # Clean old auto backups (keep last 10)
    clean_old_auto_backups(backup_dir)
  }
  
  return(result)
}

# Clean old automatic backups (keep recent ones)
clean_old_auto_backups <- function(backup_dir = BACKUPS_DIR, keep_count = 10) {
  if (!dir.exists(backup_dir)) {
    return(list(success = TRUE, message = "Backup directory doesn't exist"))
  }
  
  tryCatch({
    # Find all auto backup files
    auto_backups <- list.files(backup_dir, 
                               pattern = ".*_auto_.*\\.db$", 
                               full.names = TRUE)
    
    if (length(auto_backups) <= keep_count) {
      return(list(success = TRUE, message = "No old backups to clean"))
    }
    
    # Sort by modification time (newest first)
    backup_info <- file.info(auto_backups)
    auto_backups <- auto_backups[order(backup_info$mtime, decreasing = TRUE)]
    
    # Remove old backups (keep only the most recent ones)
    old_backups <- auto_backups[(keep_count + 1):length(auto_backups)]
    file.remove(old_backups)
    
    return(list(
      success = TRUE,
      message = paste("Removed", length(old_backups), "old automatic backups")
    ))
    
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error cleaning auto backups:", e$message)))
  })
}

# Clean old backups (keep last N days)
clean_old_backups <- function(backup_dir = BACKUPS_DIR, keep_days = 30) {
  if (!dir.exists(backup_dir)) {
    return(list(success = TRUE, message = "Backup directory doesn't exist"))
  }
  
  tryCatch({
    backup_files <- list.files(backup_dir, pattern = "_backup_.*\\.db$", full.names = TRUE)
    
    if (length(backup_files) == 0) {
      return(list(success = TRUE, message = "No backup files found"))
    }
    
    # Get file modification times
    file_times <- file.info(backup_files)$mtime
    cutoff_time <- Sys.time() - (keep_days * 24 * 3600)
    
    old_files <- backup_files[file_times < cutoff_time]
    
    if (length(old_files) > 0) {
      file.remove(old_files)
      return(list(
        success = TRUE, 
        message = paste("Removed", length(old_files), "old backup files")
      ))
    } else {
      return(list(success = TRUE, message = "No old backup files to remove"))
    }
    
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error cleaning backups:", e$message)))
  })
}