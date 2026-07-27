# database_manager.R
# Database management functions for switching and managing hidden databases

suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RSQLite)
})

# Resolve the main database path for the current app location.
get_main_db_path <- function() {
  normalizePath(DEFAULT_DB_NAME, mustWork = FALSE)
}

# Check whether the current app state is viewing the main database.
is_viewing_main_database <- function() {
  normalizePath(get_current_db_path(), mustWork = FALSE) == get_main_db_path()
}

# Resolve the backup directory for the main database location.
get_backup_dir_for_db <- function(db_path = get_main_db_path()) {
  if (is.null(db_path) || identical(db_path, "")) {
    return(BACKUPS_DIR)
  }

  db_dir <- dirname(db_path)

  if (basename(db_dir) == ".mousemanagement_DontRemove") {
    return(file.path(db_dir, "backups"))
  }

  file.path(db_dir, ".mousemanagement_DontRemove", "backups")
}

# List all backup database files for the active database location.
list_backup_database_files <- function(db_path = get_current_db_path(), backup_dir = get_backup_dir_for_db(get_main_db_path())) {
  if (!dir.exists(backup_dir)) {
    return(character(0))
  }

  backup_files <- list.files(backup_dir, pattern = "\\.db$", full.names = TRUE)

  if (length(backup_files) == 0) {
    return(character(0))
  }

  backup_info <- file.info(backup_files)
  backup_times <- backup_info$mtime
  backup_times[is.na(backup_times)] <- as.POSIXct("1970-01-01", tz = "UTC")

  backup_files[order(backup_times, decreasing = TRUE)]
}

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

# Get current database path for the active session.
get_current_db_path <- function() {
  return(DB_PATH)
}

# Clear any temporary backup view and return to the main database.
return_to_main_database <- function() {
  tryCatch({
    DB_PATH <<- get_main_db_path()

    migration_result <- ensure_startup_database_schema(DB_PATH)
    if (!isTRUE(migration_result$success)) {
      return(list(success = FALSE, message = paste("Database migration failed:", migration_result$message)))
    }

    message_text <- "Returned to main database"
    if (!identical(migration_result$message, "Database schema already up to date")) {
      message_text <- paste(message_text, "-", migration_result$message)
    }

    list(success = TRUE, message = message_text, path = DB_PATH)
  }, error = function(e) {
    list(success = FALSE, message = paste("Error returning to main database:", e$message))
  })
}

# List main database and all backup databases
list_hidden_databases <- function() {
  current_db_path <- get_current_db_path()
  main_db_path <- get_main_db_path()
  backup_dir <- get_backup_dir_for_db(main_db_path)
  current_choices <- list()
  main_choices <- list()
  backup_choices <- list()
  
  # Add main database
  if (file.exists(main_db_path)) {
    size <- file.info(main_db_path)$size
    if (is.na(size)) size <- 0
    main_label <- if (normalizePath(main_db_path, mustWork = FALSE) == normalizePath(current_db_path, mustWork = FALSE)) {
      paste0(basename(main_db_path), " (Main database, ", size, " bytes)")
    } else {
      paste0(basename(main_db_path), " (Main database, ", size, " bytes)")
    }

    if (normalizePath(main_db_path, mustWork = FALSE) == normalizePath(current_db_path, mustWork = FALSE)) {
      current_choices[[main_label]] <- main_db_path
    } else {
      main_choices[[main_label]] <- main_db_path
    }
  }
  
  # Add backup databases
  if (!dir.exists(backup_dir)) {
    dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  backup_paths <- list_backup_database_files(current_db_path, backup_dir)
  backup_files <- basename(backup_paths)
  
  if (length(backup_files) > 0) {
    # Get backup labels with sizes
    backup_labels <- sapply(1:length(backup_files), function(i) {
      tryCatch({
        size <- file.info(backup_paths[i])$size
        if (is.na(size)) size <- 0
        if (normalizePath(backup_paths[i], mustWork = FALSE) == normalizePath(current_db_path, mustWork = FALSE)) {
          paste0(backup_files[i], " (Current view, ", size, " bytes)")
        } else {
          paste0(backup_files[i], " (Backup, ", size, " bytes)")
        }
      }, error = function(e) {
        paste0(backup_files[i], " (unknown size)")
      })
    })

    for (i in seq_along(backup_paths)) {
      normalized_backup <- normalizePath(backup_paths[i], mustWork = FALSE)
      if (normalized_backup == normalizePath(current_db_path, mustWork = FALSE)) {
        current_choices[[backup_labels[i]]] <- backup_paths[i]
      } else {
        backup_choices[[backup_labels[i]]] <- backup_paths[i]
      }
    }
  }

  grouped_choices <- list()

  if (length(current_choices) > 0) {
    grouped_choices[["Current"]] <- current_choices
  }

  if (length(main_choices) > 0) {
    grouped_choices[["Main"]] <- main_choices
  }

  if (length(backup_choices) > 0) {
    grouped_choices[["Backups"]] <- backup_choices
  }

  if (length(grouped_choices) > 0) {
    return(grouped_choices)
  }

  list("No databases found" = list("No databases found" = ""))
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

# Switch to database temporarily for viewing
switch_database <- function(new_db_path) {
  validation <- validate_database_file(new_db_path)
  
  if (!validation$valid) {
    return(list(success = FALSE, message = validation$message))
  }
  
  tryCatch({
    normalized_path <- normalizePath(new_db_path, mustWork = FALSE)
    main_db_path <- get_main_db_path()
    previous_db_path <- DB_PATH
    
    # Update global DB_PATH for current session
    DB_PATH <<- normalized_path

    migration_result <- ensure_startup_database_schema(DB_PATH)
    if (!isTRUE(migration_result$success)) {
      DB_PATH <<- previous_db_path
      return(list(success = FALSE, message = paste("Database migration failed:", migration_result$message)))
    }
    
    # Check if switching to main database or backup
    is_main_db <- normalized_path == main_db_path
    
    if (is_main_db) {
      message_text <- "Switched to main database (mice_colony.db)"
    } else {
      message_text <- paste("Temporarily viewing backup:", basename(new_db_path))
    }

    if (!identical(migration_result$message, "Database schema already up to date")) {
      message_text <- paste(message_text, "-", migration_result$message)
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

# Restore a backup as the main database while keeping both the chosen backup and a safety copy of the current main DB.
restore_backup_as_main <- function(backup_path) {
  validation <- validate_database_file(backup_path)

  if (!validation$valid) {
    return(list(success = FALSE, message = validation$message))
  }

  main_db_path <- get_main_db_path()
  normalized_backup <- normalizePath(backup_path, mustWork = FALSE)

  if (normalized_backup == main_db_path) {
    return(list(success = FALSE, message = "Selected database is already the main database"))
  }

  tryCatch({
    if (file.exists(main_db_path)) {
      safety_backup <- create_backup(main_db_path, backup_type = "preimport")
      if (!safety_backup$success) {
        return(list(success = FALSE, message = paste("Failed to back up current main database:", safety_backup$message)))
      }
    }

    if (!file.copy(normalized_backup, main_db_path, overwrite = TRUE)) {
      return(list(success = FALSE, message = "Failed to restore backup as main database"))
    }

    return_to_main_database()

    migration_result <- ensure_startup_database_schema(main_db_path)
    if (!isTRUE(migration_result$success)) {
      return(list(success = FALSE, message = paste("Backup restored but migration failed:", migration_result$message)))
    }

    DB_PATH <<- main_db_path

    message_text <- paste("Restored", basename(normalized_backup), "as the main database. Previous main database was backed up.")
    if (!identical(migration_result$message, "Database schema already up to date")) {
      message_text <- paste(message_text, migration_result$message)
    }

    list(
      success = TRUE,
      message = message_text,
      path = main_db_path
    )
  }, error = function(e) {
    list(success = FALSE, message = paste("Error restoring backup:", e$message))
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
      "expected_age_for_harvesting TEXT,",
      "final_report_date DATE,",
      "final_report_primary_age TEXT,",
      "final_report_primary_age_value REAL,",
      "final_report_total_embryos INTEGER,",
      "final_report_male_embryos INTEGER,",
      "final_report_female_embryos INTEGER,",
      "final_report_unknown_embryos INTEGER,",
      "final_report_mixed_age INTEGER DEFAULT 0,",
      "final_report_age_groups_json TEXT,",
      "final_report_notes TEXT",
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
    if (!file.copy(source_path, main_db_path, overwrite = TRUE)) {
      return(list(success = FALSE, message = "Failed to copy imported database into the main database location"))
    }

    migration_result <- ensure_startup_database_schema(main_db_path)
    if (!isTRUE(migration_result$success)) {
      return(list(success = FALSE, message = paste("Database imported but migration failed:", migration_result$message)))
    }

    DB_PATH <<- normalizePath(main_db_path, mustWork = FALSE)

    message_text <- "Database imported and replaced main database (mice_colony.db). Previous database backed up."
    if (!identical(migration_result$message, "Database schema already up to date")) {
      message_text <- paste(message_text, migration_result$message)
    }
    
    return(list(
      success = TRUE,
      message = message_text,
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
      tables = tables,
      is_main = normalizePath(db_path, mustWork = FALSE) == get_main_db_path()
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
create_backup <- function(db_path = get_main_db_path(), backup_dir = get_backup_dir_for_db(get_main_db_path()), backup_type = "manual") {
  if (!file.exists(db_path)) {
    return(list(success = FALSE, message = "Source database not found"))
  }
  
  # Ensure backup directory exists
  if (!dir.exists(backup_dir)) {
    dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  tryCatch({
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    
    backup_name <- switch(
      backup_type,
      auto = paste0("mice_colony_auto_", timestamp, ".db"),
      preimport = paste0("mice_colony_preimport_", timestamp, ".db"),
      paste0("mice_colony_backup_", timestamp, ".db")
    )
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
should_create_auto_backup <- function(db_path = get_main_db_path(), backup_dir = get_backup_dir_for_db(get_main_db_path()), 
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
create_startup_backup <- function(db_path = get_main_db_path(), backup_dir = get_backup_dir_for_db(get_main_db_path())) {
  create_backup(db_path, backup_dir, backup_type = "auto")
}

# Clean old automatic backups (keep recent ones)
clean_old_auto_backups <- function(backup_dir = get_backup_dir_for_db(), keep_count = 10) {
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

# Clean old backups (keep at least the most recent backups and anything from the last N days)
clean_old_backups <- function(backup_dir = get_backup_dir_for_db(), keep_days = 7, keep_count = 3) {
  if (!dir.exists(backup_dir)) {
    return(list(success = TRUE, message = "Backup directory doesn't exist"))
  }
  
  tryCatch({
    backup_files <- list_backup_database_files(backup_dir = backup_dir)
    
    if (length(backup_files) == 0) {
      return(list(success = TRUE, message = "No backup files found"))
    }
    
    # Get file modification times
    backup_info <- file.info(backup_files)
    file_times <- backup_info$mtime
    cutoff_time <- Sys.time() - (keep_days * 24 * 3600)
    
    # Sort by modification time (newest first)
    backup_files <- backup_files[order(file_times, decreasing = TRUE)]
    file_times <- file_times[order(file_times, decreasing = TRUE)]
    
    recent_indices <- which(file_times >= cutoff_time)
    newest_indices <- seq_len(min(keep_count, length(backup_files)))
    keep_indices <- sort(unique(c(recent_indices, newest_indices)))
    
    old_files <- backup_files[-keep_indices]
    
    if (length(old_files) > 0) {
      file.remove(old_files)
      return(list(
        success = TRUE, 
        message = paste(
          "Removed", length(old_files), "old backup files and kept",
          length(keep_indices), "recent backup(s)"
        )
      ))
    } else {
      return(list(success = TRUE, message = "No old backup files to remove under the current retention rule"))
    }
    
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error cleaning backups:", e$message)))
  })
}