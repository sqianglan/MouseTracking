# Pregnancy prediction and final-report parsing helpers

safe_analysis_date <- function(value) {
  if (is.null(value) || length(value) == 0 || is.na(value) || value == "" || identical(value, "Unknown")) {
    return(as.Date(NA))
  }

  if (inherits(value, "Date")) {
    return(value)
  }

  if (is.numeric(value)) {
    return(as.Date(value, origin = "1970-01-01"))
  }

  try_formats <- c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d", "%m-%d-%Y", "%d-%m-%Y")
  parsed <- tryCatch(as.Date(value, tryFormats = try_formats), error = function(e) as.Date(NA))
  unname(parsed)
}

normalize_embryo_age_input <- function(age_value) {
  if (is.null(age_value) || length(age_value) == 0 || is.na(age_value) || trimws(age_value) == "") {
    return(list(label = NA_character_, numeric = NA_real_))
  }

  normalized <- toupper(trimws(as.character(age_value)))
  normalized <- gsub("\\s+", "", normalized)
  normalized <- sub("^EMBRYONICAGE", "E", normalized)
  normalized <- sub("^AGE", "E", normalized)

  match <- regexec("^E?([0-9]+(?:\\.[0-9]+)?)$", normalized, perl = TRUE)
  captured <- regmatches(normalized, match)[[1]]

  if (length(captured) < 2) {
    return(list(label = NA_character_, numeric = NA_real_))
  }

  age_num <- suppressWarnings(as.numeric(captured[2]))
  if (is.na(age_num)) {
    return(list(label = NA_character_, numeric = NA_real_))
  }

  label <- if (age_num %% 1 == 0) {
    paste0("E", format(age_num, trim = TRUE, scientific = FALSE, nsmall = 0))
  } else {
    paste0("E", format(age_num, trim = TRUE, scientific = FALSE))
  }

  list(label = label, numeric = age_num)
}

parse_age_group_segment <- function(segment_text) {
  segment_text <- trimws(segment_text)
  if (segment_text == "") {
    return(NULL)
  }

  age_match <- regexec("(E\\s*[0-9]+(?:\\.[0-9]+)?)", toupper(segment_text), perl = TRUE)
  age_capture <- regmatches(toupper(segment_text), age_match)[[1]]
  if (length(age_capture) < 2) {
    return(NULL)
  }

  normalized_age <- normalize_embryo_age_input(age_capture[2])
  if (is.na(normalized_age$numeric)) {
    return(NULL)
  }

  count <- NA_integer_
  count_patterns <- c(
    "(?:X|COUNT[[:space:]]*=|COUNT[[:space:]]+|N[[:space:]]*=|N[[:space:]]+)([0-9]+)",
    "([0-9]+)[[:space:]]*(EMBRYOS?|PUPS?)",
    "^([0-9]+)[[:space:]]+"
  )

  for (pattern in count_patterns) {
    count_match <- regexec(pattern, toupper(segment_text), perl = TRUE)
    count_capture <- regmatches(toupper(segment_text), count_match)[[1]]
    if (length(count_capture) >= 2) {
      count <- suppressWarnings(as.integer(count_capture[2]))
      if (!is.na(count)) {
        break
      }
    }
  }

  data.frame(
    age_label = normalized_age$label,
    age_value = normalized_age$numeric,
    count = count,
    source_text = segment_text,
    stringsAsFactors = FALSE
  )
}

parse_age_groups_text <- function(text_value) {
  empty_result <- data.frame(
    age_label = character(0),
    age_value = numeric(0),
    count = integer(0),
    source_text = character(0),
    stringsAsFactors = FALSE
  )

  if (is.null(text_value) || length(text_value) == 0 || is.na(text_value) || trimws(text_value) == "") {
    return(empty_result)
  }

  normalized <- gsub("\r\n?|\n", ";", text_value, perl = TRUE)
  segments <- unlist(strsplit(normalized, "[;|]+"))
  parsed_segments <- lapply(segments, parse_age_group_segment)
  parsed_segments <- Filter(Negate(is.null), parsed_segments)

  if (length(parsed_segments) == 0) {
    return(empty_result)
  }

  combined <- do.call(rbind, parsed_segments)
  combined <- combined[!duplicated(paste(combined$age_label, combined$count, combined$source_text)), , drop = FALSE]
  rownames(combined) <- NULL
  combined
}

extract_integer_from_text <- function(text_value, patterns) {
  if (is.null(text_value) || length(text_value) == 0 || is.na(text_value) || trimws(text_value) == "") {
    return(NA_integer_)
  }

  for (pattern in patterns) {
    match <- regexec(pattern, text_value, ignore.case = TRUE, perl = TRUE)
    capture <- regmatches(text_value, match)[[1]]
    if (length(capture) >= 2) {
      parsed <- suppressWarnings(as.integer(capture[2]))
      if (!is.na(parsed)) {
        return(parsed)
      }
    }
  }

  NA_integer_
}

parse_final_report_notes <- function(note_text) {
  empty_age_groups <- data.frame(
    age_label = character(0),
    age_value = numeric(0),
    count = integer(0),
    source_text = character(0),
    stringsAsFactors = FALSE
  )

  empty_result <- list(
    primary_age_label = NA_character_,
    primary_age_value = NA_real_,
    total_embryos = NA_integer_,
    male_embryos = NA_integer_,
    female_embryos = NA_integer_,
    unknown_embryos = NA_integer_,
    mixed_age = FALSE,
    age_groups = empty_age_groups,
    parse_confidence = "none"
  )

  if (is.null(note_text) || length(note_text) == 0 || is.na(note_text) || trimws(note_text) == "") {
    return(empty_result)
  }

  normalized <- trimws(gsub("\r\n?|\n", ";", as.character(note_text), perl = TRUE))
  age_groups <- parse_age_groups_text(normalized)

  age_matches <- gregexpr("E\\s*[0-9]+(?:\\.[0-9]+)?", toupper(normalized), perl = TRUE)
  age_tokens <- character(0)
  if (length(age_matches) > 0 && length(age_matches[[1]]) > 0 && age_matches[[1]][1] != -1) {
    age_tokens <- regmatches(toupper(normalized), age_matches)[[1]]
    age_tokens <- unique(gsub("\\s+", "", age_tokens))
  }

  primary_age_label <- NA_character_
  primary_age_value <- NA_real_
  mixed_age <- FALSE

  if (nrow(age_groups) > 0) {
    primary_age_label <- age_groups$age_label[1]
    primary_age_value <- age_groups$age_value[1]
    mixed_age <- length(unique(age_groups$age_label)) > 1
  } else if (length(age_tokens) > 0) {
    normalized_age <- normalize_embryo_age_input(age_tokens[1])
    primary_age_label <- normalized_age$label
    primary_age_value <- normalized_age$numeric
    mixed_age <- length(unique(age_tokens)) > 1 || grepl("MIX|RANGE|DIFFER", toupper(normalized), perl = TRUE)
  }

  total_embryos <- extract_integer_from_text(
    normalized,
    c(
      "(?:TOTAL[[:space:]]*=|TOTAL[[:space:]]+)([0-9]+)",
      "([0-9]+)[[:space:]]*(EMBRYOS?|PUPS?)",
      "N[[:space:]]*=[[:space:]]*([0-9]+)"
    )
  )
  male_embryos <- extract_integer_from_text(
    normalized,
    c("([0-9]+)[[:space:]]*(MALES?|MALE\\b|M\\b)")
  )
  female_embryos <- extract_integer_from_text(
    normalized,
    c("([0-9]+)[[:space:]]*(FEMALES?|FEMALE\\b|F\\b)")
  )
  unknown_embryos <- extract_integer_from_text(
    normalized,
    c("([0-9]+)[[:space:]]*(UNKNOWN|UNK|UNSEXED|UNDETERMINED)")
  )

  parsed_field_count <- sum(!is.na(c(
    primary_age_value,
    total_embryos,
    male_embryos,
    female_embryos,
    unknown_embryos
  )))

  parse_confidence <- if (parsed_field_count >= 3 || nrow(age_groups) > 1) {
    "high"
  } else if (parsed_field_count >= 1 || length(age_tokens) > 0) {
    "partial"
  } else {
    "none"
  }

  list(
    primary_age_label = primary_age_label,
    primary_age_value = primary_age_value,
    total_embryos = total_embryos,
    male_embryos = male_embryos,
    female_embryos = female_embryos,
    unknown_embryos = unknown_embryos,
    mixed_age = mixed_age,
    age_groups = age_groups,
    parse_confidence = parse_confidence
  )
}

merge_final_report_details <- function(existing_row = NULL, status = NULL, final_report_date = NULL,
                                       primary_age_input = NULL, mixed_age_input = NULL,
                                       total_embryos = NA_integer_, male_embryos = NA_integer_,
                                       female_embryos = NA_integer_, unknown_embryos = NA_integer_,
                                       final_report_notes = NULL) {
  existing_notes <- ""
  existing_final_report_notes <- ""
  if (!is.null(existing_row) && nrow(existing_row) > 0 && "notes" %in% names(existing_row)) {
    existing_notes <- existing_row$notes[1]
  }
  if (!is.null(existing_row) && nrow(existing_row) > 0 && "final_report_notes" %in% names(existing_row)) {
    existing_final_report_notes <- existing_row$final_report_notes[1]
  }

  parse_note_source <- paste(
    unique(stats::na.omit(c(existing_final_report_notes, existing_notes))),
    collapse = "; "
  )
  if (trimws(parse_note_source) == "") {
    parse_note_source <- ""
  }

  parsed_notes <- parse_final_report_notes(parse_note_source)
  structured_age <- normalize_embryo_age_input(primary_age_input)
  structured_age_groups <- parse_age_groups_text(mixed_age_input)

  merged_age_label <- structured_age$label
  merged_age_value <- structured_age$numeric
  if (is.na(merged_age_value) && !is.null(existing_row) && nrow(existing_row) > 0 && "final_report_primary_age_value" %in% names(existing_row)) {
    merged_age_value <- suppressWarnings(as.numeric(existing_row$final_report_primary_age_value[1]))
  }
  if (is.na(merged_age_value)) {
    merged_age_value <- parsed_notes$primary_age_value
  }

  if (is.na(merged_age_value) && !is.na(structured_age$numeric)) {
    merged_age_value <- structured_age$numeric
  }

  if (is.na(merged_age_value) && !is.null(existing_row) && nrow(existing_row) > 0 && "final_report_primary_age" %in% names(existing_row)) {
    fallback_age <- normalize_embryo_age_input(existing_row$final_report_primary_age[1])
    merged_age_label <- fallback_age$label
    merged_age_value <- fallback_age$numeric
  }

  if (is.na(merged_age_value) && !is.na(parsed_notes$primary_age_value)) {
    merged_age_label <- parsed_notes$primary_age_label
    merged_age_value <- parsed_notes$primary_age_value
  }

  if (!is.na(structured_age$numeric)) {
    merged_age_label <- structured_age$label
    merged_age_value <- structured_age$numeric
  } else if (is.na(merged_age_label) && !is.na(merged_age_value)) {
    merged_age_label <- normalize_embryo_age_input(as.character(merged_age_value))$label
  }

  merged_total <- suppressWarnings(as.integer(total_embryos))
  if (is.na(merged_total) && !is.null(existing_row) && nrow(existing_row) > 0 && "final_report_total_embryos" %in% names(existing_row)) {
    merged_total <- suppressWarnings(as.integer(existing_row$final_report_total_embryos[1]))
  }
  if (is.na(merged_total)) {
    merged_total <- parsed_notes$total_embryos
  }

  merged_male <- suppressWarnings(as.integer(male_embryos))
  if (is.na(merged_male) && !is.null(existing_row) && nrow(existing_row) > 0 && "final_report_male_embryos" %in% names(existing_row)) {
    merged_male <- suppressWarnings(as.integer(existing_row$final_report_male_embryos[1]))
  }
  if (is.na(merged_male)) {
    merged_male <- parsed_notes$male_embryos
  }

  merged_female <- suppressWarnings(as.integer(female_embryos))
  if (is.na(merged_female) && !is.null(existing_row) && nrow(existing_row) > 0 && "final_report_female_embryos" %in% names(existing_row)) {
    merged_female <- suppressWarnings(as.integer(existing_row$final_report_female_embryos[1]))
  }
  if (is.na(merged_female)) {
    merged_female <- parsed_notes$female_embryos
  }

  merged_unknown <- suppressWarnings(as.integer(unknown_embryos))
  if (is.na(merged_unknown) && !is.null(existing_row) && nrow(existing_row) > 0 && "final_report_unknown_embryos" %in% names(existing_row)) {
    merged_unknown <- suppressWarnings(as.integer(existing_row$final_report_unknown_embryos[1]))
  }
  if (is.na(merged_unknown)) {
    merged_unknown <- parsed_notes$unknown_embryos
  }

  merged_age_groups <- structured_age_groups
  if (nrow(merged_age_groups) == 0 && !is.null(existing_row) && nrow(existing_row) > 0 && "final_report_age_groups_json" %in% names(existing_row)) {
    raw_json <- existing_row$final_report_age_groups_json[1]
    if (!is.null(raw_json) && !is.na(raw_json) && raw_json != "") {
      merged_age_groups <- tryCatch({
        parsed_json <- jsonlite::fromJSON(raw_json)
        as.data.frame(parsed_json, stringsAsFactors = FALSE)
      }, error = function(e) {
        merged_age_groups
      })
    }
  }
  if (nrow(merged_age_groups) == 0 && nrow(parsed_notes$age_groups) > 0) {
    merged_age_groups <- parsed_notes$age_groups
  }

  mixed_age_flag <- FALSE
  if (!is.null(existing_row) && nrow(existing_row) > 0 && "final_report_mixed_age" %in% names(existing_row)) {
    mixed_age_flag <- isTRUE(as.logical(existing_row$final_report_mixed_age[1]))
  }
  mixed_age_flag <- isTRUE(mixed_age_flag) || nrow(merged_age_groups) > 1 || isTRUE(parsed_notes$mixed_age)

  report_date <- safe_analysis_date(final_report_date)
  if (is.na(report_date) && !is.null(existing_row) && nrow(existing_row) > 0 && "final_report_date" %in% names(existing_row)) {
    report_date <- safe_analysis_date(existing_row$final_report_date[1])
  }

  report_notes <- if (!is.null(final_report_notes) && !is.na(final_report_notes) && trimws(final_report_notes) != "") {
    trimws(final_report_notes)
  } else if (!is.null(existing_row) && nrow(existing_row) > 0 && "final_report_notes" %in% names(existing_row) && !is.na(existing_row$final_report_notes[1]) && trimws(existing_row$final_report_notes[1]) != "") {
    trimws(existing_row$final_report_notes[1])
  } else {
    ""
  }

  age_groups_json <- if (nrow(merged_age_groups) > 0) {
    jsonlite::toJSON(merged_age_groups, auto_unbox = TRUE, dataframe = "rows", null = "null")
  } else {
    NA_character_
  }

  source_label <- if (!is.null(final_report_notes) || !is.null(primary_age_input) || !is.na(suppressWarnings(as.integer(total_embryos)))) {
    "structured"
  } else if (parsed_notes$parse_confidence != "none") {
    "parsed-notes"
  } else {
    "legacy"
  }

  list(
    status = status,
    final_report_date = if (is.na(report_date)) NA_character_ else as.character(report_date),
    final_report_primary_age = merged_age_label,
    final_report_primary_age_value = merged_age_value,
    final_report_total_embryos = merged_total,
    final_report_male_embryos = merged_male,
    final_report_female_embryos = merged_female,
    final_report_unknown_embryos = merged_unknown,
    final_report_mixed_age = mixed_age_flag,
    final_report_age_groups_json = age_groups_json,
    final_report_notes = report_notes,
    final_report_source = source_label,
    parsed_notes = parsed_notes,
    age_groups = merged_age_groups
  )
}

summarize_final_report_age_profile <- function(report_details) {
  empty_result <- list(
    age_mean_value = report_details$final_report_primary_age_value,
    age_min_value = report_details$final_report_primary_age_value,
    age_max_value = report_details$final_report_primary_age_value,
    age_span_value = 0,
    age_group_count = if (isTRUE(report_details$final_report_mixed_age)) 2L else 1L
  )

  raw_json <- report_details$final_report_age_groups_json
  if (is.null(raw_json) || is.na(raw_json) || raw_json == "") {
    return(empty_result)
  }

  age_groups <- tryCatch(jsonlite::fromJSON(raw_json), error = function(e) NULL)
  if (is.null(age_groups) || NROW(age_groups) == 0) {
    return(empty_result)
  }

  age_df <- as.data.frame(age_groups, stringsAsFactors = FALSE)
  if (!("age_value" %in% names(age_df))) {
    return(empty_result)
  }

  age_values <- suppressWarnings(as.numeric(age_df$age_value))
  age_values <- age_values[!is.na(age_values)]
  if (length(age_values) == 0) {
    return(empty_result)
  }

  counts <- rep(1, length(age_values))
  if ("count" %in% names(age_df)) {
    parsed_counts <- suppressWarnings(as.numeric(age_df$count))
    parsed_counts[is.na(parsed_counts) | parsed_counts <= 0] <- 1
    counts <- parsed_counts[!is.na(suppressWarnings(as.numeric(age_df$age_value)))]
  }

  weighted_mean <- sum(age_values * counts, na.rm = TRUE) / sum(counts, na.rm = TRUE)

  list(
    age_mean_value = weighted_mean,
    age_min_value = min(age_values, na.rm = TRUE),
    age_max_value = max(age_values, na.rm = TRUE),
    age_span_value = max(age_values, na.rm = TRUE) - min(age_values, na.rm = TRUE),
    age_group_count = length(unique(age_values))
  )
}

build_final_report_note_entry <- function(selected_status, report_details) {
  note_parts <- c(sprintf("[Status updated to '%s' on %s]", selected_status, report_details$final_report_date))

  if (!is.na(report_details$final_report_primary_age) && report_details$final_report_primary_age != "") {
    note_parts <- c(note_parts, paste("Embryo age:", report_details$final_report_primary_age))
  }

  if (!is.na(report_details$final_report_total_embryos)) {
    note_parts <- c(note_parts, paste("Total embryos:", report_details$final_report_total_embryos))
  }

  sex_parts <- character(0)
  if (!is.na(report_details$final_report_male_embryos)) {
    sex_parts <- c(sex_parts, paste("M", report_details$final_report_male_embryos, sep = "="))
  }
  if (!is.na(report_details$final_report_female_embryos)) {
    sex_parts <- c(sex_parts, paste("F", report_details$final_report_female_embryos, sep = "="))
  }
  if (!is.na(report_details$final_report_unknown_embryos)) {
    sex_parts <- c(sex_parts, paste("Unknown", report_details$final_report_unknown_embryos, sep = "="))
  }
  if (length(sex_parts) > 0) {
    note_parts <- c(note_parts, paste("Sex counts:", paste(sex_parts, collapse = ", ")))
  }

  if (isTRUE(report_details$final_report_mixed_age) && !is.na(report_details$final_report_age_groups_json)) {
    age_groups <- tryCatch(jsonlite::fromJSON(report_details$final_report_age_groups_json), error = function(e) NULL)
    if (!is.null(age_groups) && NROW(age_groups) > 0) {
      age_summaries <- apply(as.data.frame(age_groups, stringsAsFactors = FALSE), 1, function(group_row) {
        if (!is.na(group_row[["count"]]) && group_row[["count"]] != "") {
          paste0(group_row[["age_label"]], " x", group_row[["count"]])
        } else {
          group_row[["age_label"]]
        }
      })
      note_parts <- c(note_parts, paste("Mixed ages:", paste(age_summaries, collapse = "; ")))
    }
  }

  if (!is.null(report_details$final_report_notes) && !is.na(report_details$final_report_notes) && trimws(report_details$final_report_notes) != "") {
    note_parts <- c(note_parts, report_details$final_report_notes)
  }

  paste(note_parts, collapse = " | ")
}

extract_plugging_final_report <- function(plugging_row, legacy_backfill = FALSE) {
  report_details <- merge_final_report_details(existing_row = plugging_row)

  # Keep plugging_history as the canonical source for event-level analysis.
  # Legacy fallback from mice_stock or audit_trail should only happen in an
  # explicit one-time backfill path, not during normal prediction reads.
  if (!isTRUE(legacy_backfill)) {
    return(report_details)
  }

  report_details
}

build_body_weight_features_for_event <- function(weight_history, plugging_row, report_details) {
  empty_features <- list(
    baseline_weight = NA_real_,
    last_pre_plug_weight = NA_real_,
    first_post_plug_weight = NA_real_,
    latest_post_plug_weight = NA_real_,
    weight_change_after_plug = NA_real_,
    max_weight_gain = NA_real_,
    recent_weight_gain = NA_real_,
    recent_relative_gain = NA_real_,
    recent_day_offset = NA_real_,
    measurement_count = 0L,
    pre_measurement_count = 0L,
    post_measurement_count = 0L,
    average_daily_gain = NA_real_
  )

  if (nrow(weight_history) == 0) {
    return(empty_features)
  }

  weight_history$measurement_date <- as.Date(
    unname(vapply(weight_history$measurement_date, safe_analysis_date, as.Date(NA))),
    origin = "1970-01-01"
  )
  weight_history$weight_grams <- suppressWarnings(as.numeric(weight_history$weight_grams))
  weight_history <- weight_history[!is.na(weight_history$measurement_date) & !is.na(weight_history$weight_grams), , drop = FALSE]

  if (nrow(weight_history) == 0) {
    return(empty_features)
  }

  pairing_start <- safe_analysis_date(plugging_row$pairing_start_date[1])
  plug_observed <- safe_analysis_date(plugging_row$plug_observed_date[1])
  collection_date <- safe_analysis_date(report_details$final_report_date)
  anchor_date <- if (!is.na(collection_date)) collection_date else if (!is.na(plug_observed)) plug_observed else pairing_start

  if (is.na(anchor_date)) {
    anchor_date <- max(weight_history$measurement_date, na.rm = TRUE)
  }

  pre_weights <- weight_history[weight_history$measurement_date <= anchor_date, , drop = FALSE]
  post_weights <- weight_history[weight_history$measurement_date >= anchor_date, , drop = FALSE]
  baseline_candidates <- pre_weights
  if (!is.na(pairing_start)) {
    baseline_candidates <- weight_history[weight_history$measurement_date <= pairing_start, , drop = FALSE]
  }

  baseline_weight <- if (nrow(baseline_candidates) > 0) baseline_candidates$weight_grams[nrow(baseline_candidates)] else if (nrow(pre_weights) > 0) pre_weights$weight_grams[1] else NA_real_
  last_pre_plug <- if (nrow(pre_weights) > 0) pre_weights$weight_grams[nrow(pre_weights)] else NA_real_
  first_post_plug <- if (nrow(post_weights) > 0) post_weights$weight_grams[1] else NA_real_
  latest_post_plug <- if (nrow(post_weights) > 0) post_weights$weight_grams[nrow(post_weights)] else NA_real_
  latest_post_date <- if (nrow(post_weights) > 0) post_weights$measurement_date[nrow(post_weights)] else as.Date(NA)
  max_weight_gain <- if (!is.na(baseline_weight)) max(weight_history$weight_grams - baseline_weight, na.rm = TRUE) else NA_real_
  recent_weight_gain <- if (!is.na(baseline_weight) && !is.na(latest_post_plug)) latest_post_plug - baseline_weight else NA_real_
  recent_relative_gain <- if (!is.na(recent_weight_gain) && !is.na(baseline_weight) && baseline_weight > 0) recent_weight_gain / baseline_weight else NA_real_
  recent_day_offset <- if (!is.na(latest_post_date) && !is.na(anchor_date)) as.numeric(latest_post_date - anchor_date) else NA_real_

  average_daily_gain <- NA_real_
  if (!is.na(baseline_weight) && nrow(post_weights) > 0) {
    last_post_date <- max(post_weights$measurement_date, na.rm = TRUE)
    last_post_weight <- post_weights$weight_grams[which.max(post_weights$measurement_date)]
    days_elapsed <- as.numeric(last_post_date - anchor_date)
    if (!is.na(days_elapsed) && days_elapsed > 0) {
      average_daily_gain <- (last_post_weight - baseline_weight) / days_elapsed
    }
  }

  list(
    baseline_weight = baseline_weight,
    last_pre_plug_weight = last_pre_plug,
    first_post_plug_weight = first_post_plug,
    latest_post_plug_weight = latest_post_plug,
    weight_change_after_plug = if (!is.na(last_pre_plug) && !is.na(first_post_plug)) first_post_plug - last_pre_plug else NA_real_,
    max_weight_gain = max_weight_gain,
    recent_weight_gain = recent_weight_gain,
    recent_relative_gain = recent_relative_gain,
    recent_day_offset = recent_day_offset,
    measurement_count = nrow(weight_history),
    pre_measurement_count = nrow(pre_weights),
    post_measurement_count = nrow(post_weights),
    average_daily_gain = average_daily_gain
  )
}

build_plugging_prediction_dataset <- function(db_path = DB_PATH, legacy_backfill = FALSE) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  plugging_query <- paste(
    "SELECT ph.*,",
    "f.dob AS female_dob, f.breeding_line AS female_breeding_line, f.genotype AS female_genotype,",
    "f.status AS female_status, f.date_of_death",
    "FROM plugging_history ph",
    "LEFT JOIN mice_stock f ON ph.female_id = f.asu_id",
    "WHERE ph.plugging_status != 'Deleted'"
  )

  plugging_history <- tryCatch(DBI::dbGetQuery(con, plugging_query), error = function(e) data.frame())
  body_weight_history <- tryCatch(DBI::dbGetQuery(con, "SELECT * FROM body_weight_history ORDER BY asu_id, measurement_date"), error = function(e) data.frame())

  if (nrow(plugging_history) == 0) {
    return(data.frame())
  }

  dataset_rows <- lapply(seq_len(nrow(plugging_history)), function(index) {
    plugging_row <- plugging_history[index, , drop = FALSE]
    report_details <- extract_plugging_final_report(plugging_row, legacy_backfill = legacy_backfill)
    age_profile <- summarize_final_report_age_profile(report_details)

    female_plugging_history <- plugging_history[plugging_history$female_id == plugging_row$female_id[1], , drop = FALSE]
    mouse_weights <- body_weight_history[body_weight_history$asu_id == plugging_row$female_id[1], , drop = FALSE]
    event_weights <- build_event_weight_window(mouse_weights, plugging_row, female_plugging_history)
    body_weight_features <- build_body_weight_features_for_event(event_weights, plugging_row, report_details)

    female_age_weeks <- NA_real_
    female_dob <- safe_analysis_date(plugging_row$female_dob[1])
    pairing_start <- safe_analysis_date(plugging_row$pairing_start_date[1])
    if (!is.na(female_dob) && !is.na(pairing_start)) {
      female_age_weeks <- round(as.numeric(pairing_start - female_dob) / 7, 1)
    }

    has_positive_final_report_signal <- !is.na(report_details$final_report_total_embryos) && report_details$final_report_total_embryos > 0
    has_positive_final_report_signal <- isTRUE(has_positive_final_report_signal) ||
      !is.na(report_details$final_report_primary_age_value) ||
      !is.na(report_details$final_report_male_embryos) ||
      !is.na(report_details$final_report_female_embryos) ||
      !is.na(report_details$final_report_unknown_embryos) ||
      identical(report_details$parsed_notes$parse_confidence, "high") ||
      identical(report_details$parsed_notes$parse_confidence, "partial")

    outcome_label <- if (plugging_row$plugging_status[1] == "Collected") {
      "pregnant"
    } else if (plugging_row$plugging_status[1] %in% c("Empty", "Not Pregnant", "Not Observed (Confirmed)")) {
      "not_pregnant"
    } else {
      "unknown"
    }

    data.frame(
      plugging_id = plugging_row$id[1],
      female_id = plugging_row$female_id[1],
      male_id = plugging_row$male_id[1],
      plugging_status = plugging_row$plugging_status[1],
      female_breeding_line = if ("female_breeding_line" %in% names(plugging_row)) plugging_row$female_breeding_line[1] else NA_character_,
      female_genotype = if ("female_genotype" %in% names(plugging_row)) plugging_row$female_genotype[1] else NA_character_,
      female_age_weeks = female_age_weeks,
      pairing_start_date = ifelse(is.na(pairing_start), NA_character_, as.character(pairing_start)),
      plug_observed_date = ifelse(is.na(safe_analysis_date(plugging_row$plug_observed_date[1])), NA_character_, as.character(safe_analysis_date(plugging_row$plug_observed_date[1]))),
      expected_age_for_harvesting = if ("expected_age_for_harvesting" %in% names(plugging_row)) plugging_row$expected_age_for_harvesting[1] else NA_character_,
      notes = if ("notes" %in% names(plugging_row)) plugging_row$notes[1] else NA_character_,
      final_report_date = report_details$final_report_date,
      final_report_primary_age = report_details$final_report_primary_age,
      final_report_primary_age_value = report_details$final_report_primary_age_value,
      final_report_age_mean_value = age_profile$age_mean_value,
      final_report_age_min_value = age_profile$age_min_value,
      final_report_age_max_value = age_profile$age_max_value,
      final_report_age_span_value = age_profile$age_span_value,
      final_report_age_group_count = age_profile$age_group_count,
      final_report_notes = report_details$final_report_notes,
      final_report_total_embryos = report_details$final_report_total_embryos,
      final_report_male_embryos = report_details$final_report_male_embryos,
      final_report_female_embryos = report_details$final_report_female_embryos,
      final_report_unknown_embryos = report_details$final_report_unknown_embryos,
      final_report_mixed_age = report_details$final_report_mixed_age,
      final_report_source = report_details$final_report_source,
      parse_confidence = report_details$parsed_notes$parse_confidence,
      outcome_label = outcome_label,
      baseline_weight = body_weight_features$baseline_weight,
      last_pre_plug_weight = body_weight_features$last_pre_plug_weight,
      first_post_plug_weight = body_weight_features$first_post_plug_weight,
      latest_post_plug_weight = body_weight_features$latest_post_plug_weight,
      weight_change_after_plug = body_weight_features$weight_change_after_plug,
      max_weight_gain = body_weight_features$max_weight_gain,
      recent_weight_gain = body_weight_features$recent_weight_gain,
      recent_relative_gain = body_weight_features$recent_relative_gain,
      recent_day_offset = body_weight_features$recent_day_offset,
      measurement_count = body_weight_features$measurement_count,
      pre_measurement_count = body_weight_features$pre_measurement_count,
      post_measurement_count = body_weight_features$post_measurement_count,
      average_daily_gain = body_weight_features$average_daily_gain,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, dataset_rows)
}

compute_pregnancy_correlation_map <- function(prediction_dataset) {
  if (is.null(prediction_dataset) || nrow(prediction_dataset) == 0) {
    return(data.frame())
  }

  numeric_columns <- vapply(prediction_dataset, is.numeric, logical(1))
  numeric_data <- prediction_dataset[, numeric_columns, drop = FALSE]
  numeric_data <- numeric_data[, vapply(numeric_data, function(column) any(!is.na(column)), logical(1)), drop = FALSE]

  if (ncol(numeric_data) < 2) {
    return(data.frame())
  }

  correlation_matrix <- stats::cor(numeric_data, use = "pairwise.complete.obs")
  correlation_table <- as.data.frame(as.table(correlation_matrix), stringsAsFactors = FALSE)
  names(correlation_table) <- c("feature_x", "feature_y", "correlation")
  correlation_table
}

normalize_prediction_breeding_line_mode <- function(mode = NULL) {
  normalized_mode <- if (is.null(mode) || length(mode) == 0 || is.na(mode) || trimws(as.character(mode)[1]) == "") {
    "feature"
  } else {
    tolower(trimws(as.character(mode)[1]))
  }

  alias_map <- c(
    mixed = "pooled",
    ignore = "pooled",
    ignored = "pooled",
    none = "pooled",
    default = "feature",
    feature = "feature",
    line = "line_specific",
    line_specific = "line_specific",
    `line-specific` = "line_specific",
    germline = "line_specific",
    germline_specific = "line_specific",
    `germline-specific` = "line_specific"
  )

  if (normalized_mode %in% names(alias_map)) {
    normalized_mode <- unname(alias_map[[normalized_mode]])
  }

  if (!normalized_mode %in% c("pooled", "feature", "line_specific")) {
    normalized_mode <- "feature"
  }

  normalized_mode
}

filter_training_dataset_by_current_breeding_line <- function(prediction_dataset, breeding_line_mode = "feature",
                                                             current_breeding_line = NA_character_) {
  normalized_mode <- normalize_prediction_breeding_line_mode(breeding_line_mode)

  if (!identical(normalized_mode, "line_specific") || is.null(prediction_dataset) || nrow(prediction_dataset) == 0) {
    return(prediction_dataset)
  }

  if (!("female_breeding_line" %in% names(prediction_dataset))) {
    return(prediction_dataset[0, , drop = FALSE])
  }

  if (is.null(current_breeding_line) || length(current_breeding_line) == 0 || is.na(current_breeding_line) || trimws(current_breeding_line) == "") {
    return(prediction_dataset[0, , drop = FALSE])
  }

  prediction_dataset[prediction_dataset$female_breeding_line == current_breeding_line, , drop = FALSE]
}

describe_prediction_breeding_line_mode <- function(mode = "feature", current_breeding_line = NA_character_) {
  normalized_mode <- normalize_prediction_breeding_line_mode(mode)

  if (identical(normalized_mode, "pooled")) {
    return("Pooled model across breeding lines")
  }

  if (identical(normalized_mode, "line_specific")) {
    if (!is.null(current_breeding_line) && length(current_breeding_line) > 0 && !is.na(current_breeding_line) && trimws(current_breeding_line) != "") {
      return(paste0("Germline-specific model for ", current_breeding_line))
    }

    return("Germline-specific model")
  }

  "Pooled model with breeding line as a feature"
}

get_prediction_models_dir <- function() {
  configured_models_dir <- Sys.getenv("MOUSE_MODELS_DIR", unset = "")
  if (!is.null(configured_models_dir) && nzchar(configured_models_dir)) {
    return(configured_models_dir)
  }

  file.path(dirname(HIDDEN_DIR), "prediction_models")
}

get_prediction_model_backups_dir <- function() {
  file.path(get_prediction_models_dir(), "backups")
}

get_prediction_model_registry_path <- function() {
  file.path(get_prediction_models_dir(), "pregnancy_prediction_models.rds")
}

list_prediction_model_registry_files <- function() {
  ensure_prediction_model_directories()

  active_path <- get_prediction_model_registry_path()
  backup_dir <- get_prediction_model_backups_dir()
  backup_paths <- list.files(backup_dir, pattern = "\\.rds$", full.names = TRUE)
  candidate_paths <- unique(c(active_path, backup_paths))
  candidate_paths[file.exists(candidate_paths)]
}

describe_prediction_model_registry_file <- function(registry_path) {
  if (is.null(registry_path) || length(registry_path) == 0 || !file.exists(registry_path)) {
    return(NULL)
  }

  registry <- load_prediction_model_registry(registry_path)
  file_info <- file.info(registry_path)
  active_path <- normalizePath(get_prediction_model_registry_path(), winslash = "/", mustWork = FALSE)
  normalized_path <- normalizePath(registry_path, winslash = "/", mustWork = FALSE)
  is_active <- identical(normalized_path, active_path)

  saved_at <- if (!is.null(registry) && !is.null(registry$metadata$saved_at) && !is.na(registry$metadata$saved_at) && registry$metadata$saved_at != "") {
    registry$metadata$saved_at
  } else if (!is.null(file_info$mtime) && !is.na(file_info$mtime)) {
    format(file_info$mtime, "%Y-%m-%d %H:%M:%S")
  } else {
    "Unknown"
  }

  description <- if (!is.null(registry) && !is.null(registry$metadata$breeding_line_description) && !is.na(registry$metadata$breeding_line_description) && registry$metadata$breeding_line_description != "") {
    registry$metadata$breeding_line_description
  } else {
    "Unknown model mode"
  }

  training_summary <- if (!is.null(registry) && !is.null(registry$training_summary) && is.list(registry$training_summary)) {
    registry$training_summary
  } else {
    list()
  }

  total_events <- if (!is.null(training_summary$total_events) && is.finite(suppressWarnings(as.numeric(training_summary$total_events)))) {
    as.integer(training_summary$total_events)
  } else {
    NA_integer_
  }

  pregnant_events <- if (!is.null(training_summary$pregnant_events) && is.finite(suppressWarnings(as.numeric(training_summary$pregnant_events)))) {
    as.integer(training_summary$pregnant_events)
  } else {
    NA_integer_
  }

  not_pregnant_events <- if (!is.null(training_summary$not_pregnant_events) && is.finite(suppressWarnings(as.numeric(training_summary$not_pregnant_events)))) {
    as.integer(training_summary$not_pregnant_events)
  } else {
    NA_integer_
  }

  label_prefix <- if (is_active) "Active" else "Backup"
  label <- paste0(label_prefix, ": ", description, " (", saved_at, ")")
  if (!is_active) {
    label <- paste0(label, " [", basename(registry_path), "]")
  }

  list(
    path = registry_path,
    normalized_path = normalized_path,
    is_active = is_active,
    saved_at = saved_at,
    description = description,
    label = label,
    registry = registry,
    total_events = total_events,
    pregnant_events = pregnant_events,
    not_pregnant_events = not_pregnant_events,
    file_name = basename(registry_path),
    modified_time = if (!is.null(file_info$mtime) && !is.na(file_info$mtime)) as.POSIXct(file_info$mtime) else as.POSIXct(NA)
  )
}

list_prediction_model_registry_choices <- function() {
  registry_descriptions <- lapply(list_prediction_model_registry_files(), describe_prediction_model_registry_file)
  registry_descriptions <- Filter(Negate(is.null), registry_descriptions)

  if (length(registry_descriptions) == 0) {
    return(list())
  }

  active_entries <- Filter(function(entry) isTRUE(entry$is_active), registry_descriptions)
  backup_entries <- Filter(function(entry) !isTRUE(entry$is_active), registry_descriptions)

  if (length(backup_entries) > 1) {
    backup_entries <- backup_entries[order(vapply(backup_entries, function(entry) entry$saved_at, character(1)), decreasing = TRUE)]
  }

  c(active_entries, backup_entries)
}

activate_prediction_model_registry <- function(source_registry_path,
                                               registry_path = get_prediction_model_registry_path(),
                                               backup_existing = TRUE) {
  ensure_prediction_model_directories()

  if (is.null(source_registry_path) || length(source_registry_path) == 0 || is.na(source_registry_path) || trimws(source_registry_path) == "") {
    stop("No prediction model registry was selected.")
  }

  if (!file.exists(source_registry_path)) {
    stop("The selected prediction model registry file does not exist.")
  }

  source_registry <- load_prediction_model_registry(source_registry_path)
  if (is.null(source_registry) || is.null(source_registry$models) || is.null(source_registry$models$classifier)) {
    stop("The selected prediction model registry is not valid.")
  }

  normalized_source <- normalizePath(source_registry_path, winslash = "/", mustWork = FALSE)
  normalized_target <- normalizePath(registry_path, winslash = "/", mustWork = FALSE)

  if (identical(normalized_source, normalized_target)) {
    return(list(path = registry_path, backup_path = NULL, changed = FALSE))
  }

  backup_path <- NULL
  if (isTRUE(backup_existing) && file.exists(registry_path)) {
    backup_path <- backup_prediction_model_registry(registry_path)
  }

  copied <- file.copy(source_registry_path, registry_path, overwrite = TRUE)
  if (!isTRUE(copied)) {
    stop("Failed to activate the selected prediction model registry.")
  }

  list(path = registry_path, backup_path = backup_path, changed = TRUE)
}

delete_prediction_model_registry <- function(source_registry_path,
                                             registry_path = get_prediction_model_registry_path()) {
  ensure_prediction_model_directories()

  if (is.null(source_registry_path) || length(source_registry_path) == 0 || is.na(source_registry_path) || trimws(source_registry_path) == "") {
    stop("No prediction model registry was selected.")
  }

  if (!file.exists(source_registry_path)) {
    stop("The selected prediction model registry file does not exist.")
  }

  normalized_source <- normalizePath(source_registry_path, winslash = "/", mustWork = FALSE)
  normalized_target <- normalizePath(registry_path, winslash = "/", mustWork = FALSE)

  if (identical(normalized_source, normalized_target)) {
    stop("The active prediction model cannot be deleted. Load a different saved model first if you want to replace it.")
  }

  deleted <- file.remove(source_registry_path)
  if (!isTRUE(deleted)) {
    stop("Failed to delete the selected prediction model registry.")
  }

  invisible(TRUE)
}

ensure_prediction_model_directories <- function() {
  dirs_to_create <- c(get_prediction_models_dir(), get_prediction_model_backups_dir())

  for (dir_path in dirs_to_create) {
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    }
  }

  invisible(TRUE)
}

backup_prediction_model_registry <- function(registry_path = get_prediction_model_registry_path()) {
  ensure_prediction_model_directories()

  if (!file.exists(registry_path)) {
    return(NULL)
  }

  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup_path <- file.path(
    get_prediction_model_backups_dir(),
    paste0("pregnancy_prediction_models_", timestamp, ".rds")
  )

  copied <- file.copy(registry_path, backup_path, overwrite = TRUE)
  if (!isTRUE(copied)) {
    stop("Failed to back up the existing prediction model registry.")
  }

  backup_path
}

load_prediction_model_registry <- function(registry_path = get_prediction_model_registry_path()) {
  if (!file.exists(registry_path)) {
    return(NULL)
  }

  raw_registry <- tryCatch(readRDS(registry_path), error = function(e) NULL)
  normalize_prediction_model_registry(raw_registry)
}

normalize_prediction_model_registry <- function(model_registry = NULL) {
  if (is.null(model_registry)) {
    return(NULL)
  }

  if (!is.list(model_registry) || is.null(model_registry$metadata)) {
    inferred_mode <- if (!is.null(model_registry$breeding_line_mode)) {
      model_registry$breeding_line_mode
    } else if (isTRUE(model_registry$breeding_line_used)) {
      "feature"
    } else {
      "pooled"
    }

    normalized_mode <- normalize_prediction_breeding_line_mode(inferred_mode)

    return(list(
      metadata = list(
        registry_version = 1L,
        saved_at = NA_character_,
        breeding_line_mode = normalized_mode,
        current_breeding_line = NA_character_,
        breeding_line_description = describe_prediction_breeding_line_mode(normalized_mode)
      ),
      models = model_registry,
      training_summary = NULL
    ))
  }

  current_breeding_line <- if (!is.null(model_registry$metadata$current_breeding_line)) {
    as.character(model_registry$metadata$current_breeding_line)[1]
  } else {
    NA_character_
  }
  normalized_mode <- normalize_prediction_breeding_line_mode(model_registry$metadata$breeding_line_mode)

  model_registry$metadata$registry_version <- if (is.null(model_registry$metadata$registry_version)) 1L else as.integer(model_registry$metadata$registry_version)
  model_registry$metadata$saved_at <- if (is.null(model_registry$metadata$saved_at)) NA_character_ else as.character(model_registry$metadata$saved_at)
  model_registry$metadata$breeding_line_mode <- normalized_mode
  model_registry$metadata$current_breeding_line <- current_breeding_line
  model_registry$metadata$breeding_line_description <- describe_prediction_breeding_line_mode(normalized_mode, current_breeding_line)

  if (is.null(model_registry$models) && !is.null(model_registry$model_bundle)) {
    model_registry$models <- model_registry$model_bundle
  }

  if (is.null(model_registry$training_summary)) {
    model_registry$training_summary <- NULL
  }

  model_registry
}

create_prediction_model_registry <- function(model_bundle, breeding_line_mode = "feature",
                                             current_breeding_line = NA_character_, training_summary = NULL,
                                             trained_at = Sys.time()) {
  normalized_mode <- normalize_prediction_breeding_line_mode(breeding_line_mode)

  normalize_prediction_model_registry(list(
    metadata = list(
      registry_version = 1L,
      saved_at = format(trained_at, "%Y-%m-%d %H:%M:%S"),
      breeding_line_mode = normalized_mode,
      current_breeding_line = current_breeding_line,
      breeding_line_description = describe_prediction_breeding_line_mode(normalized_mode, current_breeding_line)
    ),
    models = model_bundle,
    training_summary = training_summary
  ))
}

is_prediction_model_registry_compatible <- function(model_registry, breeding_line_mode = "feature",
                                                    current_breeding_line = NA_character_) {
  normalized_registry <- normalize_prediction_model_registry(model_registry)
  if (is.null(normalized_registry) || is.null(normalized_registry$metadata) || is.null(normalized_registry$models)) {
    return(FALSE)
  }

  normalized_mode <- normalize_prediction_breeding_line_mode(breeding_line_mode)
  registry_mode <- normalize_prediction_breeding_line_mode(normalized_registry$metadata$breeding_line_mode)
  if (!identical(normalized_mode, registry_mode)) {
    return(FALSE)
  }

  if (!identical(normalized_mode, "line_specific")) {
    return(TRUE)
  }

  if (is.null(current_breeding_line) || length(current_breeding_line) == 0 || is.na(current_breeding_line) || trimws(current_breeding_line) == "") {
    return(FALSE)
  }

  identical(
    trimws(as.character(normalized_registry$metadata$current_breeding_line)[1]),
    trimws(as.character(current_breeding_line)[1])
  )
}

resolve_prediction_model_bundle <- function(training_dataset, current_breeding_line = NA_character_,
                                            breeding_line_mode = "feature",
                                            registry_path = get_prediction_model_registry_path()) {
  normalized_mode <- normalize_prediction_breeding_line_mode(breeding_line_mode)
  saved_registry <- load_prediction_model_registry(registry_path)

  if (is_prediction_model_registry_compatible(saved_registry, normalized_mode, current_breeding_line)) {
    saved_model_bundle <- saved_registry$models
    if (!is.null(saved_model_bundle) && !is.null(saved_model_bundle$classifier) && !is.null(saved_model_bundle$classifier_formula)) {
      saved_at_label <- if (!is.null(saved_registry$metadata$saved_at) && !is.na(saved_registry$metadata$saved_at) && saved_registry$metadata$saved_at != "") {
        saved_registry$metadata$saved_at
      } else {
        "unknown time"
      }

      return(list(
        model_bundle = saved_model_bundle,
        source = "saved_model",
        source_label = paste0("Saved model loaded (", saved_at_label, ")"),
        registry = saved_registry
      ))
    }
  }

  refit_bundle <- build_pregnancy_ml_models(
    training_dataset,
    current_breeding_line = current_breeding_line,
    breeding_line_mode = normalized_mode
  )

  list(
    model_bundle = refit_bundle,
    source = "live_refit",
    source_label = "Live refit from current training data",
    registry = NULL
  )
}

save_prediction_model_registry <- function(model_registry, registry_path = get_prediction_model_registry_path(), backup_existing = TRUE) {
  ensure_prediction_model_directories()

  normalized_registry <- normalize_prediction_model_registry(model_registry)
  if (is.null(normalized_registry$metadata$saved_at) || is.na(normalized_registry$metadata$saved_at) || normalized_registry$metadata$saved_at == "") {
    normalized_registry$metadata$saved_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  }

  backup_path <- NULL
  if (isTRUE(backup_existing) && file.exists(registry_path)) {
    backup_path <- backup_prediction_model_registry(registry_path)
  }

  saveRDS(normalized_registry, registry_path)

  list(path = registry_path, backup_path = backup_path)
}

filter_prediction_training_dataset <- function(prediction_dataset, breeding_lines = NULL,
                                               min_age_weeks = NA_real_, max_age_weeks = NA_real_,
                                               breeding_line_mode = "feature", current_breeding_line = NA_character_) {
  if (is.null(prediction_dataset) || nrow(prediction_dataset) == 0) {
    return(data.frame())
  }

  filtered <- prediction_dataset[prediction_dataset$outcome_label != "unknown", , drop = FALSE]

  if (!is.null(breeding_lines) && length(breeding_lines) > 0 && !"All" %in% breeding_lines) {
    filtered <- filtered[filtered$female_breeding_line %in% breeding_lines, , drop = FALSE]
  }

  numeric_age <- suppressWarnings(as.numeric(filtered$female_age_weeks))
  if (!is.na(min_age_weeks)) {
    filtered <- filtered[is.na(numeric_age) | numeric_age >= min_age_weeks, , drop = FALSE]
    numeric_age <- suppressWarnings(as.numeric(filtered$female_age_weeks))
  }
  if (!is.na(max_age_weeks)) {
    filtered <- filtered[is.na(numeric_age) | numeric_age <= max_age_weeks, , drop = FALSE]
  }

  filtered <- filter_training_dataset_by_current_breeding_line(
    filtered,
    breeding_line_mode = breeding_line_mode,
    current_breeding_line = current_breeding_line
  )

  filtered
}

summarize_prediction_training_data <- function(prediction_dataset, breeding_lines = NULL,
                                               min_age_weeks = NA_real_, max_age_weeks = NA_real_,
                                               breeding_line_mode = "feature", current_breeding_line = NA_character_) {
  filtered <- filter_prediction_training_dataset(
    prediction_dataset,
    breeding_lines,
    min_age_weeks,
    max_age_weeks,
    breeding_line_mode = breeding_line_mode,
    current_breeding_line = current_breeding_line
  )

  outcome_counts <- table(factor(filtered$outcome_label, levels = c("pregnant", "not_pregnant")))
  status_counts <- table(filtered$plugging_status)

  list(
    filtered_dataset = filtered,
    total_events = nrow(filtered),
    unique_females = length(unique(filtered$female_id)),
    pregnant_events = unname(outcome_counts[["pregnant"]]),
    not_pregnant_events = unname(outcome_counts[["not_pregnant"]]),
    with_weight_ge_3 = sum(filtered$measurement_count >= 3, na.rm = TRUE),
    with_age_text = sum(!is.na(filtered$final_report_primary_age_value)),
    with_count_text = sum(!is.na(filtered$final_report_total_embryos)),
    with_sex_text = sum(!is.na(filtered$final_report_male_embryos) | !is.na(filtered$final_report_female_embryos) | !is.na(filtered$final_report_unknown_embryos)),
    status_breakdown = as.data.frame(status_counts, stringsAsFactors = FALSE)
  )
}

calculate_plugging_anchor <- function(plugging_row) {
  observed_plug <- safe_analysis_date(plugging_row$plug_observed_date[1])
  if (!is.na(observed_plug)) {
    return(list(date = observed_plug, type = "Observed Plug"))
  }

  pairing_start <- safe_analysis_date(plugging_row$pairing_start_date[1])
  if (!is.na(pairing_start)) {
    return(list(date = pairing_start + 1, type = "Presumed Plug"))
  }

  pairing_end <- safe_analysis_date(plugging_row$pairing_end_date[1])
  if (!is.na(pairing_end)) {
    return(list(date = pairing_end, type = "Presumed Plug"))
  }

  list(date = as.Date(NA), type = "Unknown Anchor")
}

prepare_anchor_weight_series <- function(weight_history, anchor_date) {
  empty_result <- data.frame(
    measurement_date = as.Date(character(0)),
    weight_grams = numeric(0),
    day_since_anchor = numeric(0),
    stringsAsFactors = FALSE
  )

  if (is.null(weight_history) || nrow(weight_history) == 0 || is.na(anchor_date)) {
    return(empty_result)
  }

  series <- weight_history
  series$measurement_date <- as.Date(
    unname(vapply(series$measurement_date, safe_analysis_date, as.Date(NA))),
    origin = "1970-01-01"
  )
  series$weight_grams <- suppressWarnings(as.numeric(series$weight_grams))
  series <- series[!is.na(series$measurement_date) & !is.na(series$weight_grams), , drop = FALSE]
  if (nrow(series) == 0) {
    return(empty_result)
  }

  series$day_since_anchor <- as.numeric(series$measurement_date - anchor_date)
  series <- series[order(series$measurement_date), , drop = FALSE]
  rownames(series) <- NULL
  series
}

build_event_weight_window <- function(all_weights, plugging_row, female_plugging = NULL,
                                      pre_days = 3, post_days = 25) {
  if (is.null(all_weights) || nrow(all_weights) == 0) {
    return(data.frame())
  }

  event_anchor <- calculate_plugging_anchor(plugging_row)
  pairing_start <- safe_analysis_date(plugging_row$pairing_start_date[1])
  final_report_date <- safe_analysis_date(plugging_row$final_report_date[1])

  window_start <- if (!is.na(pairing_start)) pairing_start - pre_days else if (!is.na(event_anchor$date)) event_anchor$date - pre_days else as.Date(NA)
  window_end <- if (!is.na(final_report_date)) final_report_date + 3 else if (!is.na(event_anchor$date)) event_anchor$date + post_days else as.Date(NA)

  if (!is.null(female_plugging) && nrow(female_plugging) > 0 && !is.na(pairing_start)) {
    female_plugging$pairing_start_date <- unname(vapply(female_plugging$pairing_start_date, safe_analysis_date, as.Date(NA)))
    later_events <- female_plugging[
      female_plugging$id != plugging_row$id[1] & !is.na(female_plugging$pairing_start_date) & female_plugging$pairing_start_date > pairing_start,
      ,
      drop = FALSE
    ]

    if (nrow(later_events) > 0) {
      next_event_start <- min(later_events$pairing_start_date, na.rm = TRUE)
      if (!is.na(next_event_start)) {
        window_end <- min(window_end, next_event_start - 1, na.rm = TRUE)
      }
    }
  }

  all_weights$measurement_date <- unname(vapply(all_weights$measurement_date, safe_analysis_date, as.Date(NA)))
  filtered_weights <- all_weights[!is.na(all_weights$measurement_date), , drop = FALSE]

  if (!is.na(window_start)) {
    filtered_weights <- filtered_weights[filtered_weights$measurement_date >= window_start, , drop = FALSE]
  }
  if (!is.na(window_end)) {
    filtered_weights <- filtered_weights[filtered_weights$measurement_date <= window_end, , drop = FALSE]
  }

  if (nrow(filtered_weights) == 0) {
    filtered_weights <- all_weights
    filtered_weights$measurement_date <- unname(vapply(filtered_weights$measurement_date, safe_analysis_date, as.Date(NA)))
    filtered_weights <- filtered_weights[!is.na(filtered_weights$measurement_date), , drop = FALSE]
  }

  filtered_weights[order(filtered_weights$measurement_date), , drop = FALSE]
}

build_presumable_weight_curve <- function(baseline_weight, training_dataset, max_days = 21) {
  if (is.na(baseline_weight) || is.null(training_dataset) || nrow(training_dataset) == 0) {
    return(data.frame(day_since_anchor = numeric(0), predicted_weight = numeric(0), stringsAsFactors = FALSE))
  }

  positive_cases <- training_dataset[
    training_dataset$outcome_label == "pregnant" & !is.na(training_dataset$average_daily_gain),
    ,
    drop = FALSE
  ]

  average_gain <- suppressWarnings(stats::median(positive_cases$average_daily_gain, na.rm = TRUE))
  if (is.na(average_gain) || !is.finite(average_gain) || average_gain <= 0) {
    average_gain <- 0.15
  }

  curve_days <- seq.int(0, max_days)
  data.frame(
    day_since_anchor = curve_days,
    predicted_weight = baseline_weight + curve_days * average_gain,
    stringsAsFactors = FALSE
  )
}

build_actual_trend_line <- function(weight_series) {
  if (is.null(weight_series) || nrow(weight_series) < 2) {
    return(data.frame(day_since_anchor = numeric(0), trend_weight = numeric(0), stringsAsFactors = FALSE))
  }

  fit <- tryCatch(stats::lm(weight_grams ~ day_since_anchor, data = weight_series), error = function(e) NULL)
  if (is.null(fit)) {
    return(data.frame(day_since_anchor = numeric(0), trend_weight = numeric(0), stringsAsFactors = FALSE))
  }

  prediction_days <- seq(min(weight_series$day_since_anchor), max(weight_series$day_since_anchor), by = 1)
  data.frame(
    day_since_anchor = prediction_days,
    trend_weight = as.numeric(stats::predict(fit, newdata = data.frame(day_since_anchor = prediction_days))),
    stringsAsFactors = FALSE
  )
}

estimate_potential_pregnancy_anchor <- function(anchor, weight_series, baseline_weight, training_dataset,
                                                max_shift_days = 3, shift_step = 0.5) {
  empty_result <- list(
    offset_days = 0,
    fitted_curve = data.frame(day_since_anchor = numeric(0), predicted_weight = numeric(0), stringsAsFactors = FALSE),
    aligned_score = NA_real_,
    average_gain = NA_real_,
    anchor_label = if (!is.na(anchor$date)) as.character(anchor$date) else "Unknown"
  )

  if (is.na(anchor$date) || is.na(baseline_weight) || is.null(weight_series) || nrow(weight_series) == 0 ||
      is.null(training_dataset) || nrow(training_dataset) == 0) {
    return(empty_result)
  }

  positive_cases <- training_dataset[
    training_dataset$outcome_label == "pregnant" & !is.na(training_dataset$average_daily_gain),
    ,
    drop = FALSE
  ]

  average_gain <- suppressWarnings(stats::median(positive_cases$average_daily_gain, na.rm = TRUE))
  if (is.na(average_gain) || !is.finite(average_gain) || average_gain <= 0) {
    average_gain <- 0.15
  }

  post_anchor_series <- weight_series[weight_series$day_since_anchor >= 0, , drop = FALSE]
  if (nrow(post_anchor_series) < 2) {
    fitted_days <- seq.int(
      floor(min(weight_series$day_since_anchor, na.rm = TRUE)),
      ceiling(max(weight_series$day_since_anchor, na.rm = TRUE)),
      by = 1
    )

    empty_result$fitted_curve <- data.frame(
      day_since_anchor = fitted_days,
      predicted_weight = baseline_weight + pmax(fitted_days, 0) * average_gain,
      stringsAsFactors = FALSE
    )
    empty_result$average_gain <- average_gain
    return(empty_result)
  }

  shift_candidates <- seq(-max_shift_days, max_shift_days, by = shift_step)
  shift_scores <- vapply(shift_candidates, function(offset_days) {
    aligned_days <- pmax(post_anchor_series$day_since_anchor - offset_days, 0)
    predicted_weight <- baseline_weight + aligned_days * average_gain
    mean((post_anchor_series$weight_grams - predicted_weight) ^ 2, na.rm = TRUE)
  }, numeric(1))

  best_index <- which.min(shift_scores)
  best_offset <- shift_candidates[best_index]
  fitted_days <- seq.int(
    floor(min(weight_series$day_since_anchor, na.rm = TRUE)),
    ceiling(max(weight_series$day_since_anchor, na.rm = TRUE)),
    by = 1
  )

  fitted_curve <- data.frame(
    day_since_anchor = fitted_days,
    predicted_weight = baseline_weight + pmax(fitted_days - best_offset, 0) * average_gain,
    stringsAsFactors = FALSE
  )

  anchor_label <- if (abs(best_offset) < 0.25) {
    as.character(anchor$date)
  } else {
    paste0(
      as.character(anchor$date),
      if (best_offset > 0) " + " else " - ",
      format(abs(best_offset), trim = TRUE, scientific = FALSE),
      " day"
    )
  }

  list(
    offset_days = best_offset,
    fitted_curve = fitted_curve,
    aligned_score = unname(shift_scores[best_index]),
    average_gain = average_gain,
    anchor_label = anchor_label
  )
}

build_prediction_conclusion <- function(likelihood, confidence, offset_days = 0) {
  if (identical(likelihood, "Unavailable")) {
    return("Not enough matched history to judge pregnancy.")
  }

  if (likelihood %in% c("High", "Moderate")) {
    if (!is.na(offset_days) && abs(offset_days) >= 0.5) {
      return(paste0(
        "Pregnancy is supported by the current weight trend. The fitted pregnancy timing is shifted by ",
        format(offset_days, trim = TRUE, scientific = FALSE),
        " day relative to the observed plug date."
      ))
    }

    return(paste0("Pregnancy is supported by the current weight trend with ", tolower(confidence), " confidence."))
  }

  if (identical(likelihood, "Low")) {
    return("Current weight trend is not yet clearly supportive of pregnancy.")
  }

  "Current weight trend does not yet support pregnancy."
}

is_prediction_unlikely <- function(prediction) {
  if (is.null(prediction) || !identical(prediction$likelihood, "Low") || is.null(prediction$current_features)) {
    return(FALSE)
  }

  current_features <- prediction$current_features
  recent_day_offset <- suppressWarnings(as.numeric(current_features$recent_day_offset))
  recent_weight_gain <- suppressWarnings(as.numeric(current_features$recent_weight_gain))
  recent_relative_gain <- suppressWarnings(as.numeric(current_features$recent_relative_gain))
  post_measurement_count <- suppressWarnings(as.integer(current_features$post_measurement_count))

  is.finite(recent_day_offset) && recent_day_offset >= 7 &&
    is.finite(post_measurement_count) && post_measurement_count >= 1 &&
    is.finite(recent_weight_gain) && recent_weight_gain < 2 &&
    is.finite(recent_relative_gain) && recent_relative_gain < 0.08
}

format_prediction_today_age <- function(prediction, reference_date = Sys.Date()) {
  if (is.null(prediction) || is.null(prediction$anchor) || is.na(prediction$anchor$date)) {
    return(NA_character_)
  }

  fitted_offset <- 0
  if (!is.null(prediction$fitted_anchor) && !is.null(prediction$fitted_anchor$offset_days) &&
      is.finite(prediction$fitted_anchor$offset_days)) {
    fitted_offset <- prediction$fitted_anchor$offset_days
  }

  fitted_date <- prediction$anchor$date + fitted_offset
  age_days <- as.numeric(reference_date - fitted_date)
  if (is.na(age_days) || age_days < 0) {
    return(NA_character_)
  }

  age_value <- if (abs(age_days - round(age_days)) < 0.15) {
    round(age_days)
  } else {
    round(age_days, 1)
  }

  paste0("E", format(age_value, trim = TRUE, scientific = FALSE), " today")
}

build_prediction_plot_label <- function(prediction, reference_date = Sys.Date()) {
  status_label <- if (is.null(prediction) || is.null(prediction$likelihood)) {
    "Prediction Unavailable"
  } else if (identical(prediction$likelihood, "High")) {
    "Likely Pregnant"
  } else if (identical(prediction$likelihood, "Moderate")) {
    "Possibly Pregnant"
  } else if (identical(prediction$likelihood, "Low")) {
    if (is_prediction_unlikely(prediction)) "Pregnancy Unlikely" else "Pregnancy Unclear"
  } else {
    "Prediction Unavailable"
  }

  if (is.null(prediction) || is.null(prediction$show_today_age) || !isTRUE(prediction$show_today_age)) {
    return(status_label)
  }

  age_label <- format_prediction_today_age(prediction, reference_date)
  if (!is.na(age_label) && age_label != "") {
    paste(status_label, age_label, sep = ", ")
  } else {
    status_label
  }
}

build_prediction_plot_annotation <- function(prediction, reference_date = Sys.Date()) {
  label_text <- build_prediction_plot_label(prediction, reference_date)

  fill_color <- if (!is.null(prediction) && identical(prediction$likelihood, "High")) {
    "rgba(22, 163, 74, 0.14)"
  } else if (!is.null(prediction) && identical(prediction$likelihood, "Moderate")) {
    "rgba(245, 158, 11, 0.14)"
  } else {
    "rgba(148, 163, 184, 0.16)"
  }

  border_color <- if (!is.null(prediction) && prediction$likelihood %in% c("High", "Moderate")) {
    "rgba(180, 83, 9, 0.5)"
  } else {
    "rgba(71, 85, 105, 0.45)"
  }

  list(
    x = 0.02,
    y = 0.98,
    xref = "paper",
    yref = "paper",
    xanchor = "left",
    yanchor = "top",
    text = label_text,
    showarrow = FALSE,
    align = "left",
    font = list(size = 13, color = "#334155"),
    bgcolor = fill_color,
    bordercolor = border_color,
    borderwidth = 1,
    borderpad = 6
  )
}

should_show_prediction_timing <- function(prediction) {
  if (is.null(prediction) || is.null(prediction$anchor) || is.na(prediction$anchor$date)) {
    return(FALSE)
  }

  likelihood <- if (!is.null(prediction$likelihood)) prediction$likelihood else NA_character_
  if (!likelihood %in% c("High", "Moderate")) {
    return(FALSE)
  }

  current_features <- prediction$current_features
  post_measurement_count <- if (!is.null(current_features) && !is.null(current_features$post_measurement_count)) {
    suppressWarnings(as.integer(current_features$post_measurement_count))
  } else {
    NA_integer_
  }

  recent_day_offset <- if (!is.null(current_features) && !is.null(current_features$recent_day_offset)) {
    suppressWarnings(as.numeric(current_features$recent_day_offset))
  } else {
    NA_real_
  }

  is.finite(post_measurement_count) && post_measurement_count >= 1 &&
    is.finite(recent_day_offset) && recent_day_offset >= 5
}

build_prediction_timing_lines <- function(prediction) {
  if (!should_show_prediction_timing(prediction)) {
    return(character(0))
  }

  offset_days <- 0
  if (!is.null(prediction$fitted_anchor) && !is.null(prediction$fitted_anchor$offset_days) && is.finite(prediction$fitted_anchor$offset_days)) {
    offset_days <- prediction$fitted_anchor$offset_days
  }

  if (!is.finite(offset_days) || abs(offset_days) < 0.5) {
    return(character(0))
  }

  estimated_date <- prediction$anchor$date + offset_days
  paste0("Estimated pregnancy timing: ", as.character(estimated_date))
}

build_prediction_banner_lines <- function(prediction) {
  if (is.null(prediction)) {
    return(character(0))
  }

  c(build_prediction_summary_lines(prediction), build_prediction_timing_lines(prediction))
}

build_prediction_summary_lines <- function(prediction) {
  if (is.null(prediction)) {
    return(character(0))
  }

  lines <- character(0)
  likelihood <- if (!is.null(prediction$likelihood)) prediction$likelihood else NA_character_

  if (likelihood %in% c("High", "Moderate")) {
    estimated_age_range <- if (!is.null(prediction$estimated_age_range)) prediction$estimated_age_range else "Unknown"
    if (!is.na(estimated_age_range) && estimated_age_range != "" && estimated_age_range != "Unknown") {
      lines <- c(lines, paste0("Estimated embryo age: ", estimated_age_range))
    }

    estimated_litter_size_band <- if (!is.null(prediction$estimated_litter_size_band)) prediction$estimated_litter_size_band else "Unknown"
    if (!is.na(estimated_litter_size_band) && estimated_litter_size_band != "" && estimated_litter_size_band != "Unknown") {
      lines <- c(lines, paste0("Estimated embryos: ", estimated_litter_size_band))
    }
  }

  lines
}

build_prediction_metadata_lines <- function(prediction) {
  if (is.null(prediction)) {
    return(character(0))
  }

  lines <- character(0)

  if (!is.null(prediction$model_mode_label) && !is.na(prediction$model_mode_label) && prediction$model_mode_label != "") {
    lines <- c(lines, paste0("Model: ", prediction$model_mode_label))
  }

  if (!is.null(prediction$prediction_source_label) && !is.na(prediction$prediction_source_label) && prediction$prediction_source_label != "") {
    lines <- c(lines, paste0("Source: ", prediction$prediction_source_label))
  }

  lines
}

build_body_weight_plot_y_range <- function(actual_weights, fitted_weights = numeric(0), padding_fraction = 0.12,
                                           min_padding = 0.5, clamp_to_zero = TRUE) {
  all_weights <- suppressWarnings(as.numeric(c(actual_weights, fitted_weights)))
  all_weights <- all_weights[is.finite(all_weights)]

  if (length(all_weights) == 0) {
    return(NULL)
  }

  weight_range <- range(all_weights)
  spread <- diff(weight_range)
  if (!is.finite(spread) || spread <= 0) {
    spread <- max(abs(weight_range[1]) * 0.08, min_padding)
  }

  padding <- max(spread * padding_fraction, min_padding)
  lower_bound <- weight_range[1] - padding
  upper_bound <- weight_range[2] + padding

  if (clamp_to_zero) {
    lower_bound <- max(0, lower_bound)
  }

  c(lower_bound, upper_bound)
}

build_prediction_gain_plot_data <- function(prediction) {
  empty_series <- data.frame(
    pregnancy_age = numeric(0),
    weight_gain = numeric(0),
    measurement_date = as.Date(character(0)),
    stringsAsFactors = FALSE
  )

  if (is.null(prediction)) {
    return(list(actual = empty_series, fitted = empty_series))
  }

  baseline_weight <- NA_real_
  if (!is.null(prediction$current_features) && !is.null(prediction$current_features$baseline_weight)) {
    baseline_weight <- suppressWarnings(as.numeric(prediction$current_features$baseline_weight))
  }

  fitted_offset <- 0
  if (!is.null(prediction$fitted_anchor) && !is.null(prediction$fitted_anchor$offset_days) &&
      is.finite(prediction$fitted_anchor$offset_days)) {
    fitted_offset <- prediction$fitted_anchor$offset_days
  }

  actual_series <- empty_series
  if (!is.null(prediction$weight_series) && nrow(prediction$weight_series) > 0) {
    actual_series <- prediction$weight_series[prediction$weight_series$day_since_anchor >= 0, c("measurement_date", "day_since_anchor", "weight_grams"), drop = FALSE]
    if (nrow(actual_series) > 0) {
      if (is.na(baseline_weight)) {
        baseline_weight <- actual_series$weight_grams[1]
      }
      actual_series$pregnancy_age <- pmax(actual_series$day_since_anchor - fitted_offset, 0)
      actual_series$weight_gain <- actual_series$weight_grams - baseline_weight
      actual_series <- actual_series[order(actual_series$pregnancy_age, actual_series$measurement_date), c("pregnancy_age", "weight_gain", "measurement_date"), drop = FALSE]
    }
  }

  fitted_series <- empty_series
  if (!is.null(prediction$fitted_curve) && nrow(prediction$fitted_curve) > 0) {
    fitted_series <- prediction$fitted_curve[prediction$fitted_curve$day_since_anchor >= 0, c("day_since_anchor", "predicted_weight"), drop = FALSE]
    if (nrow(fitted_series) > 0) {
      if (is.na(baseline_weight)) {
        baseline_weight <- min(fitted_series$predicted_weight, na.rm = TRUE)
      }
      fitted_series$pregnancy_age <- pmax(fitted_series$day_since_anchor - fitted_offset, 0)
      fitted_series$weight_gain <- fitted_series$predicted_weight - baseline_weight
      fitted_series$measurement_date <- as.Date(NA)
      fitted_series <- fitted_series[order(fitted_series$pregnancy_age), c("pregnancy_age", "weight_gain", "measurement_date"), drop = FALSE]
    }
  }

  list(actual = actual_series, fitted = fitted_series)
}

build_training_model_gain_curve <- function(training_dataset, max_days = 21) {
  empty_series <- data.frame(
    pregnancy_age = numeric(0),
    weight_gain = numeric(0),
    stringsAsFactors = FALSE
  )

  if (is.null(training_dataset) || nrow(training_dataset) == 0) {
    return(empty_series)
  }

  positive_cases <- training_dataset[
    training_dataset$outcome_label == "pregnant" & !is.na(training_dataset$average_daily_gain),
    ,
    drop = FALSE
  ]

  if (nrow(positive_cases) == 0) {
    return(empty_series)
  }

  average_gain <- suppressWarnings(stats::median(positive_cases$average_daily_gain, na.rm = TRUE))
  if (is.na(average_gain) || !is.finite(average_gain) || average_gain <= 0) {
    average_gain <- 0.15
  }

  curve_days <- seq.int(0, max(0, min(21, round(max_days))))
  data.frame(
    pregnancy_age = curve_days,
    weight_gain = curve_days * average_gain,
    stringsAsFactors = FALSE
  )
}

build_prediction_ml_formula <- function(response_name, dataset, numeric_candidates, factor_candidates = character(0)) {
  terms <- character(0)

  for (column_name in numeric_candidates) {
    if (!(column_name %in% names(dataset))) {
      next
    }

    column_values <- suppressWarnings(as.numeric(dataset[[column_name]]))
    unique_values <- unique(stats::na.omit(column_values))
    if (length(unique_values) > 1) {
      terms <- c(terms, column_name)
    }
  }

  for (column_name in factor_candidates) {
    if (!(column_name %in% names(dataset))) {
      next
    }

    column_values <- stats::na.omit(dataset[[column_name]])
    if (length(unique(column_values)) > 1) {
      terms <- c(terms, column_name)
    }
  }

  if (length(terms) == 0) {
    stats::as.formula(paste(response_name, "~ 1"))
  } else {
    stats::as.formula(paste(response_name, "~", paste(terms, collapse = " + ")))
  }
}

build_pregnancy_ml_models <- function(training_dataset, current_breeding_line = NA_character_, breeding_line_mode = "feature") {
  result <- list(
    classifier = NULL,
    embryo_model = NULL,
    classifier_formula = NULL,
    embryo_formula = NULL,
    training_rows = 0L,
    positive_rows = 0L,
    breeding_line_used = FALSE,
    breeding_line_mode = "feature"
  )

  if (is.null(training_dataset) || nrow(training_dataset) == 0) {
    return(result)
  }

  normalized_mode <- normalize_prediction_breeding_line_mode(breeding_line_mode)
  result$breeding_line_mode <- normalized_mode

  candidate_numeric <- c(
    "female_age_weeks",
    "baseline_weight",
    "latest_post_plug_weight",
    "recent_weight_gain",
    "recent_relative_gain",
    "recent_day_offset",
    "estimated_pregnancy_age_value"
  )

  classifier_columns <- unique(c("outcome_label", "female_breeding_line", candidate_numeric))
  classifier_columns <- classifier_columns[classifier_columns %in% names(training_dataset)]

  classifier_data <- training_dataset[
    training_dataset$outcome_label %in% c("pregnant", "not_pregnant"),
    classifier_columns,
    drop = FALSE
  ]
  classifier_data$pregnant_flag <- ifelse(classifier_data$outcome_label == "pregnant", 1, 0)

  breeding_line_candidates <- character(0)
  if (identical(normalized_mode, "feature") && !is.na(current_breeding_line) && current_breeding_line != "") {
    available_lines <- unique(stats::na.omit(classifier_data$female_breeding_line))
    if (current_breeding_line %in% available_lines && length(available_lines) > 1) {
      breeding_line_candidates <- "female_breeding_line"
      classifier_data$female_breeding_line <- factor(classifier_data$female_breeding_line)
      result$breeding_line_used <- TRUE
    }
  }

  classifier_formula <- build_prediction_ml_formula(
    "pregnant_flag",
    classifier_data,
    numeric_candidates = candidate_numeric,
    factor_candidates = breeding_line_candidates
  )

  classifier_terms <- all.vars(classifier_formula)[-1]
  if (length(classifier_terms) == 0) {
    return(result)
  }

  classifier_complete <- stats::complete.cases(classifier_data[, c("pregnant_flag", classifier_terms), drop = FALSE])
  classifier_data <- classifier_data[classifier_complete, , drop = FALSE]
  result$training_rows <- nrow(classifier_data)
  result$positive_rows <- sum(classifier_data$pregnant_flag == 1, na.rm = TRUE)

  if (nrow(classifier_data) < 12 || length(unique(classifier_data$pregnant_flag)) < 2) {
    return(result)
  }

  result$classifier_formula <- classifier_formula
  result$classifier <- tryCatch(
    suppressWarnings(stats::glm(classifier_formula, data = classifier_data, family = stats::binomial())),
    error = function(e) NULL
  )

  embryo_columns <- unique(c(
    "final_report_total_embryos", "female_breeding_line", candidate_numeric,
    "final_report_age_mean_value", "final_report_age_span_value", "final_report_mixed_age"
  ))
  embryo_columns <- embryo_columns[embryo_columns %in% names(training_dataset)]
  positive_cases <- training_dataset[
    training_dataset$outcome_label == "pregnant" & !is.na(training_dataset$final_report_total_embryos),
    embryo_columns,
    drop = FALSE
  ]

  if (nrow(positive_cases) >= 8) {
    if (result$breeding_line_used) {
      positive_cases$female_breeding_line <- factor(
        positive_cases$female_breeding_line,
        levels = levels(classifier_data$female_breeding_line)
      )
    }

    embryo_formula <- build_prediction_ml_formula(
      "final_report_total_embryos",
      positive_cases,
      numeric_candidates = c(candidate_numeric, "final_report_age_mean_value", "final_report_age_span_value"),
      factor_candidates = if (result$breeding_line_used) "female_breeding_line" else character(0)
    )
    embryo_terms <- all.vars(embryo_formula)[-1]

    if (length(embryo_terms) > 0) {
      embryo_complete <- stats::complete.cases(positive_cases[, c("final_report_total_embryos", embryo_terms), drop = FALSE])
      positive_cases <- positive_cases[embryo_complete, , drop = FALSE]

      if (nrow(positive_cases) >= 8) {
        result$embryo_formula <- embryo_formula
        result$embryo_model <- tryCatch(
          suppressWarnings(stats::lm(embryo_formula, data = positive_cases)),
          error = function(e) NULL
        )
      }
    }
  }

  result
}

predict_pregnancy_ml_outputs <- function(model_bundle, current_features, current_age_weeks = NA_real_, current_breeding_line = NA_character_) {
  result <- list(
    probability = NA_real_,
    predicted_embryos = NA_real_,
    details = character(0)
  )

  if (is.null(model_bundle$classifier) || is.null(model_bundle$classifier_formula)) {
    return(result)
  }

  current_row <- data.frame(
    female_age_weeks = current_age_weeks,
    baseline_weight = current_features$baseline_weight,
    latest_post_plug_weight = current_features$latest_post_plug_weight,
    recent_weight_gain = current_features$recent_weight_gain,
    recent_relative_gain = current_features$recent_relative_gain,
    recent_day_offset = current_features$recent_day_offset,
    estimated_pregnancy_age_value = if (!is.null(current_features$estimated_pregnancy_age_value)) current_features$estimated_pregnancy_age_value else NA_real_,
    final_report_age_mean_value = if (!is.null(current_features$estimated_pregnancy_age_value)) current_features$estimated_pregnancy_age_value else NA_real_,
    final_report_age_span_value = 0,
    final_report_mixed_age = FALSE,
    female_breeding_line = if (is.na(current_breeding_line) || current_breeding_line == "") NA_character_ else current_breeding_line,
    stringsAsFactors = FALSE
  )

  classifier_terms <- all.vars(model_bundle$classifier_formula)[-1]
  if ("female_breeding_line" %in% classifier_terms && model_bundle$breeding_line_used) {
    current_row$female_breeding_line <- factor(
      current_row$female_breeding_line,
      levels = levels(model_bundle$classifier$model$female_breeding_line)
    )
  }

  if (any(!stats::complete.cases(current_row[, classifier_terms, drop = FALSE]))) {
    return(result)
  }

  result$probability <- tryCatch(
    suppressWarnings(as.numeric(stats::predict(model_bundle$classifier, newdata = current_row, type = "response"))),
    error = function(e) NA_real_
  )

  if (!is.null(model_bundle$embryo_model) && !is.null(model_bundle$embryo_formula)) {
    embryo_terms <- all.vars(model_bundle$embryo_formula)[-1]
    if ("female_breeding_line" %in% embryo_terms && model_bundle$breeding_line_used) {
      current_row$female_breeding_line <- factor(
        current_row$female_breeding_line,
        levels = levels(model_bundle$embryo_model$model$female_breeding_line)
      )
    }

    if (all(stats::complete.cases(current_row[, embryo_terms, drop = FALSE]))) {
      result$predicted_embryos <- tryCatch(
        suppressWarnings(as.numeric(stats::predict(model_bundle$embryo_model, newdata = current_row))),
        error = function(e) NA_real_
      )
    }
  }

  result
}

predict_plugging_event_outcome <- function(plugging_row, current_weight_history, training_dataset,
                                           breeding_lines = NULL, min_age_weeks = NA_real_, max_age_weeks = NA_real_,
                                           breeding_line_mode = "feature") {
  report_details <- extract_plugging_final_report(plugging_row)
  current_features <- build_body_weight_features_for_event(current_weight_history, plugging_row, report_details)
  current_age_weeks <- NA_real_
  current_breeding_line <- if ("female_breeding_line" %in% names(plugging_row)) plugging_row$female_breeding_line[1] else NA_character_
  normalized_mode <- normalize_prediction_breeding_line_mode(breeding_line_mode)
  model_mode_label <- describe_prediction_breeding_line_mode(normalized_mode, current_breeding_line)
  filtered_training <- filter_prediction_training_dataset(
    training_dataset,
    breeding_lines,
    min_age_weeks,
    max_age_weeks,
    breeding_line_mode = normalized_mode,
    current_breeding_line = current_breeding_line
  )
  current_female_dob <- if ("female_dob" %in% names(plugging_row)) safe_analysis_date(plugging_row$female_dob[1]) else as.Date(NA)
  current_pairing_start <- safe_analysis_date(plugging_row$pairing_start_date[1])
  if (!is.na(current_female_dob) && !is.na(current_pairing_start)) {
    current_age_weeks <- round(as.numeric(current_pairing_start - current_female_dob) / 7, 1)
  }

  anchor <- calculate_plugging_anchor(plugging_row)
  weight_series <- prepare_anchor_weight_series(current_weight_history, anchor$date)
  trend_line <- build_actual_trend_line(weight_series)
  presumable_curve <- build_presumable_weight_curve(current_features$baseline_weight, filtered_training)
  fitted_anchor <- estimate_potential_pregnancy_anchor(anchor, weight_series, current_features$baseline_weight, filtered_training)
  resolved_models <- resolve_prediction_model_bundle(
    filtered_training,
    current_breeding_line = current_breeding_line,
    breeding_line_mode = normalized_mode
  )
  model_bundle <- resolved_models$model_bundle
  prediction_source <- resolved_models$source
  prediction_source_label <- resolved_models$source_label
  ml_outputs <- predict_pregnancy_ml_outputs(model_bundle, current_features, current_age_weeks, current_breeding_line)

  if (nrow(filtered_training) < 10 || is.na(ml_outputs$probability)) {
    return(list(
      status = "insufficient_data",
      likelihood = "Unavailable",
      confidence = "Low",
      estimated_age_range = if (!is.na(anchor$date)) {
        latest_day <- min(as.numeric(Sys.Date() - anchor$date), 21)
        paste0("E", round(latest_day, 1), "-E", round(latest_day + 1, 1))
      } else {
        "Unknown"
      },
      estimated_litter_size_band = "Unknown",
      evidence = c("Not enough filtered historical events with usable body-weight features to support a prediction."),
      anchor = anchor,
      fitted_anchor = fitted_anchor,
      conclusion = build_prediction_conclusion("Unavailable", "Low", fitted_anchor$offset_days),
      current_features = current_features,
      weight_series = weight_series,
      trend_line = trend_line,
      presumable_curve = presumable_curve,
      fitted_curve = fitted_anchor$fitted_curve,
      filtered_training = filtered_training,
      report_details = report_details,
      model_mode = normalized_mode,
      model_mode_label = model_mode_label,
      prediction_source = prediction_source,
      prediction_source_label = prediction_source_label
    ))
  }

  positive_cases <- filtered_training[filtered_training$outcome_label == "pregnant", , drop = FALSE]
  evidence <- character(0)

  evidence <- c(
    evidence,
    sprintf(
      if (isTRUE(model_bundle$breeding_line_used)) {
        "ML pregnancy probability %.0f%% from age, breeding line, start weight, current weight, recent gain, and days since plug."
      } else {
        "ML pregnancy probability %.0f%% from age, start weight, current weight, recent gain, and days since plug."
      },
      ml_outputs$probability * 100
    )
  )

  if (!is.na(current_features$recent_weight_gain) && !is.na(current_features$recent_relative_gain)) {
    evidence <- c(
      evidence,
      sprintf(
        "Current event gain is %.2fg (%.2f%% of starting weight).",
        current_features$recent_weight_gain,
        current_features$recent_relative_gain * 100
      )
    )
  }

  if (current_features$post_measurement_count >= 2) {
    evidence <- c(evidence, "At least two post-plug body-weight measurements are available for this event.")
  } else {
    evidence <- c(evidence, "Fewer than two post-plug body-weight measurements reduce confidence.")
  }

  strong_gain_signal <- !is.na(current_features$recent_day_offset) && current_features$recent_day_offset >= 7 &&
    !is.na(current_features$recent_weight_gain) && current_features$recent_weight_gain >= 4.5 &&
    !is.na(current_features$recent_relative_gain) && current_features$recent_relative_gain >= 0.15 &&
    current_features$post_measurement_count >= 1

  moderate_gain_signal <- !is.na(current_features$recent_day_offset) && current_features$recent_day_offset >= 7 &&
    !is.na(current_features$recent_weight_gain) && current_features$recent_weight_gain >= 3.5 &&
    !is.na(current_features$recent_relative_gain) && current_features$recent_relative_gain >= 0.12 &&
    current_features$post_measurement_count >= 1

  if (strong_gain_signal) {
    evidence <- c(evidence, "Recent body-weight gain is strongly supportive of pregnancy for this event.")
  } else if (moderate_gain_signal) {
    evidence <- c(evidence, "Recent body-weight gain is supportive of pregnancy, but with limited follow-up.")
  }

  likelihood <- if (ml_outputs$probability >= 0.7 || strong_gain_signal) {
    "High"
  } else if (ml_outputs$probability >= 0.55 || moderate_gain_signal) {
    "Moderate"
  } else {
    "Low"
  }

  confidence <- if (model_bundle$training_rows >= 25 && current_features$post_measurement_count >= 2) {
    "High"
  } else if (model_bundle$training_rows >= 15) {
    "Medium"
  } else {
    "Low"
  }

  estimated_age_range <- "Unknown"
  estimated_pregnancy_age_value <- NA_real_
  if (!is.na(anchor$date)) {
    reference_date <- Sys.Date()
    age_days <- as.numeric(reference_date - anchor$date) - fitted_anchor$offset_days
    if (!is.na(age_days) && age_days >= 0) {
      age_days <- min(age_days, 21)
      estimated_pregnancy_age_value <- age_days
      estimated_age_range <- paste0("E", round(age_days, 1), "-E", round(min(age_days + 1, 21), 1))
    }
  }

  current_features$estimated_pregnancy_age_value <- estimated_pregnancy_age_value
  ml_outputs <- predict_pregnancy_ml_outputs(model_bundle, current_features, current_age_weeks, current_breeding_line)

  litter_band <- "Unknown"
  if (ml_outputs$probability >= 0.55 && !is.na(ml_outputs$predicted_embryos) && is.finite(ml_outputs$predicted_embryos)) {
    embryo_guess <- max(1, round(ml_outputs$predicted_embryos))
    litter_band <- paste0("Around ", embryo_guess, " embryos")
    evidence <- c(evidence, sprintf("Estimated embryo count from pregnant historical cases: about %d.", embryo_guess))
  } else if (ml_outputs$probability >= 0.55 && nrow(positive_cases) >= 5) {
    median_litter <- suppressWarnings(stats::median(positive_cases$final_report_total_embryos, na.rm = TRUE))
    if (!is.na(median_litter)) {
      litter_band <- paste0("Around ", round(median_litter), " embryos")
    }
  }

  list(
    status = "ok",
    likelihood = likelihood,
    confidence = confidence,
    estimated_age_range = estimated_age_range,
    estimated_litter_size_band = litter_band,
    evidence = evidence,
    anchor = anchor,
    fitted_anchor = fitted_anchor,
    conclusion = build_prediction_conclusion(likelihood, confidence, fitted_anchor$offset_days),
    current_features = current_features,
    weight_series = weight_series,
    trend_line = trend_line,
    presumable_curve = presumable_curve,
    fitted_curve = fitted_anchor$fitted_curve,
    filtered_training = filtered_training,
    report_details = report_details,
    model_mode = normalized_mode,
    model_mode_label = model_mode_label,
    prediction_source = prediction_source,
    prediction_source_label = prediction_source_label,
    pregnancy_probability = ml_outputs$probability,
    predicted_embryos = ml_outputs$predicted_embryos
  )
}