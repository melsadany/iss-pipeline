#!/usr/bin/env Rscript
# run_03_transcription_cleanup.R
# ISS Pipeline — Stage 3: Transcription Cleanup with Multi-Reviewer Consensus

required_packages <- c("optparse", "yaml", "logger", "tidyverse", "logging", "stringdist")
for (pkg in required_packages) {
  if (!suppressWarnings(suppressPackageStartupMessages(
        require(pkg, character.only = TRUE, quietly = TRUE)
      ))) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
    suppressWarnings(suppressPackageStartupMessages(
      library(pkg, character.only = TRUE)
    ))
  }
}

get_script_path <- function() {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  needle  <- "--file="
  match   <- grep(needle, cmdArgs)
  if (length(match) > 0) dirname(normalizePath(sub(needle, "", cmdArgs[match])))
  else getwd()
}
script_dir <- get_script_path()

option_list <- list(
  make_option(c("--id"),                 type = "character", default = NULL,
              help = "Participant ID"),
  make_option(c("--transcription_file"), type = "character", default = NULL,
              help = "Path to the raw stage-2 transcription TSV"),
  make_option(c("--review_dir"),         type = "character", default = NULL,
              help = paste(
                "Directory containing per-rater review TSV files.",
                "Expected filename pattern:",
                "<id>_review_<INITIALS>_<YYYYMMDDTHHMM>.tsv.",
                "When the directory contains one or more such files the script",
                "computes consensus across all raters (latest file per rater)",
                "before applying the standard cleaning rules.",
                "When no review files are found the script falls back to the",
                "automatic cleanup path."
              )),
  make_option(c("--reviewed_tsv"),       type = "character", default = NULL,
              help = "[DEPRECATED] Legacy single-reviewer TSV. Use --review_dir instead."),
  make_option(c("--mode"),               type = "character", default = "auto",
              help = "Processing mode: auto, review, or strict"),
  make_option(c("--config"),             type = "character", default = NULL,
              help = "Path to config YAML"),
  make_option(c("--reference"),          type = "character", default = NULL,
              help = "Reference data directory"),
  make_option(c("--output"),             type = "character", default = NULL,
              help = "Output directory")
)

opt_parser <- OptionParser(option_list = option_list)
opt        <- parse_args(opt_parser)

log_threshold(INFO)
log_appender(appender_console)

source(file.path(script_dir, "00_initialize.R"))
source(file.path(script_dir, "03_transcription_cleanup.R"))

# ---------------------------------------------------------------------------
# Helper: normalize word <-> response on any freshly read TSV so all
# downstream code always has both columns available.
# ---------------------------------------------------------------------------
norm_word_response <- function(df) {
  if (!"word" %in% names(df) && "response" %in% names(df)) {
    df$word <- df$response
  }
  if (!"response" %in% names(df) && "word" %in% names(df)) {
    df$response <- df$word
  }
  df
}

# ---------------------------------------------------------------------------
# Helper: convert a logical or character drop/comment column to character
# in a way that preserves NA as NA_character_ (not the string "NA").
# as.character(NA_logical_) produces "NA" in base R which then leaks into
# string-based filtering logic. This helper avoids that.
# ---------------------------------------------------------------------------
lgl_to_char <- function(x) {
  if (is.logical(x)) {
    # TRUE -> "TRUE", FALSE -> "FALSE", NA -> NA_character_
    ifelse(is.na(x), NA_character_, ifelse(x, "TRUE", "FALSE"))
  } else {
    as.character(x)
  }
}

# ---------------------------------------------------------------------------
# Helper functions for multi-rater support
# ---------------------------------------------------------------------------

# Given a vector of review file paths like
#   /.../SUB0001_review_MES_20260508T1542.tsv
# return a named list with the latest file per rater initials.
get_latest_per_rater <- function(files, id) {
  if (!length(files)) return(list())
  basenames <- basename(files)
  # expected pattern: <id>_review_<INITIALS>_<YYYYMMDDTHHMM>.tsv
  # use a permissive split and keep the token after 'review'
  parts <- strsplit(basenames, "_")
  initials <- vapply(parts, function(p) {
    idx <- which(p == "review")
    if (length(idx) && length(p) >= idx + 1) p[idx + 1] else NA_character_
  }, character(1))
  timestamps <- sub("\\.tsv$", "", vapply(parts, function(p) tail(p, 1), character(1)))

  df <- tibble::tibble(file = files, initials = initials, ts = timestamps)
  df <- df[!is.na(df$initials) & nzchar(df$initials), , drop = FALSE]
  if (!nrow(df)) return(list())

  df <- df %>% dplyr::group_by(initials) %>%
    dplyr::slice_max(order_by = ts, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()

  out <- as.list(df$file)
  names(out) <- df$initials
  out
}

# Compute a consensus data frame aligned to raw_tsv.
#
# Reviewer TSVs may have FEWER rows than raw_tsv when a reviewer deleted rows
# outright (rather than flagging drop = TRUE). We therefore join reviewer
# decisions back onto raw_tsv by key (audio_file + start) instead of assuming
# positional alignment. Rows absent from a reviewer file receive NA drop
# (treated as keep) and NA comment.
#
# Consensus rules:
#   drop    — majority vote (>50 % TRUE => TRUE); ties => FALSE (keep)
#   comment — latest non-NA, non-empty comment across raters
compute_consensus <- function(raw_tsv, rater_files) {
  if (!length(rater_files)) return(raw_tsv)

  n_raw <- nrow(raw_tsv)

  # Build per-rater drop/comment vectors aligned to raw_tsv rows via key join.
  # Key: audio_file + start (both present in every stage-2 TSV).
  # If a rater file is missing these columns we fall back to positional fill.
  # NOTE: use lgl_to_char() instead of as.character() so logical NA stays
  # NA_character_ rather than becoming the string "NA".
  drop_mat    <- matrix(NA_character_, nrow = n_raw, ncol = length(rater_files))
  comment_mat <- matrix(NA_character_, nrow = n_raw, ncol = length(rater_files))
  colnames(drop_mat)    <- names(rater_files)
  colnames(comment_mat) <- names(rater_files)

  key_cols <- c("audio_file", "start")

  for (i in seq_along(rater_files)) {
    nm  <- names(rater_files)[i]
    df  <- tryCatch({
      d <- readr::read_tsv(rater_files[[i]], show_col_types = FALSE)
      norm_word_response(d)
    }, error = function(e) {
      log_warn("  Could not read rater file [{nm}]: {conditionMessage(e)}")
      NULL
    })
    if (is.null(df)) next

    has_keys <- all(key_cols %in% names(df)) && all(key_cols %in% names(raw_tsv))

    if (has_keys) {
      # Join by (audio_file, start) — safe when row counts differ
      raw_keys  <- paste(raw_tsv$audio_file, raw_tsv$start, sep = "\x00")
      rat_keys  <- paste(df$audio_file,      df$start,      sep = "\x00")
      idx       <- match(raw_keys, rat_keys)   # NA where rater row is missing

      if ("drop" %in% names(df)) {
        drop_vals     <- lgl_to_char(df$drop)
        drop_mat[, i] <- ifelse(is.na(idx), NA_character_, drop_vals[idx])
      }
      if ("comment" %in% names(df)) {
        comment_vals     <- lgl_to_char(df$comment)
        comment_mat[, i] <- ifelse(is.na(idx), NA_character_, comment_vals[idx])
      }
    } else {
      # Fallback: positional fill up to min(nrow(df), n_raw)
      log_warn("  Rater [{nm}]: key columns missing — using positional alignment (row counts may differ).")
      n_use <- min(nrow(df), n_raw)
      if ("drop"    %in% names(df)) drop_mat[seq_len(n_use), i]    <- lgl_to_char(df$drop[seq_len(n_use)])
      if ("comment" %in% names(df)) comment_mat[seq_len(n_use), i] <- lgl_to_char(df$comment[seq_len(n_use)])
    }
  }

  # Majority vote for drop (TRUE wins if > 50 % of non-NA votes are TRUE)
  majority_drop <- vapply(seq_len(n_raw), function(r) {
    x <- toupper(trimws(drop_mat[r, ]))
    x <- x[!is.na(x) & x != "NA" & nzchar(x)]
    x <- x[x %in% c("TRUE", "FALSE", "1", "0", "YES", "NO")]
    if (!length(x)) return(NA_character_)
    bool <- x %in% c("TRUE", "1", "YES")
    if (sum(bool) > length(bool) / 2) "TRUE" else "FALSE"
  }, character(1))

  # Latest non-NA, non-empty comment
  latest_comment <- vapply(seq_len(n_raw), function(r) {
    x <- comment_mat[r, ]
    x <- x[!is.na(x) & nzchar(trimws(x)) & toupper(trimws(x)) != "NA"]
    if (!length(x)) NA_character_ else tail(x, 1)
  }, character(1))

  base         <- raw_tsv
  base$drop    <- majority_drop
  base$comment <- latest_comment
  base
}

config <- yaml::read_yaml(opt$config)

log_info("STAGE 3: Transcription Cleanup")
log_info(strrep("-", 80))
log_info("  Participant       : {opt$id}")
log_info("  Transcription file: {opt$transcription_file}")
log_info("  Mode              : {opt$mode}")
log_info("  Output dir        : {opt$output}")

# Validate input
if (!file.exists(opt$transcription_file))
  stop(sprintf("Transcription file not found: %s", opt$transcription_file), call. = FALSE)

tx_raw <- norm_word_response(
  read_tsv(opt$transcription_file, show_col_types = FALSE)
)
log_info("  Input rows        : {nrow(tx_raw)} ({ncol(tx_raw)} columns)")
task_row_counts <- table(tx_raw$task)
for (t in names(task_row_counts)) log_info("    - {t}: {task_row_counts[[t]]} rows")

system(paste0("mkdir -p ", file.path(opt$output, "review_files")))
system(paste0("mkdir -p ", file.path(opt$output, "features")))

auto_stats_path <- file.path(
  opt$output, "features",
  paste0(opt$id, "_auto_cleaning_stats.rds")
)

apply_review_tsv <- function(reviewed, config, n_dropped_extra = 0L) {
  # norm_bool: robustly coerce any representation of TRUE/FALSE to logical.
  # Handles: actual logical TRUE/NA, character "TRUE"/"FALSE", "1"/"0",
  # "YES"/"NO", NA_character_, and the string "NA" (readr artefact).
  # Uses element-wise isTRUE() for logical input so TRUE/NA/FALSE are each
  # handled unambiguously without relying on ifelse NA-propagation behaviour.
  norm_bool <- function(x) {
    if (is.logical(x)) {
      # Element-wise: TRUE -> TRUE, FALSE -> FALSE, NA -> FALSE (keep row)
      return(vapply(x, isTRUE, logical(1)))
    }
    s <- toupper(trimws(as.character(x)))
    # Treat NA, empty string, or the literal string "NA" as FALSE (keep row)
    s[is.na(s) | s == "" | s == "NA"] <- "FALSE"
    s %in% c("TRUE", "1", "YES")
  }

  if (!"drop"    %in% names(reviewed)) reviewed$drop    <- FALSE
  if (!"comment" %in% names(reviewed)) reviewed$comment <- NA_character_

  # Diagnostic: log drop column type and value distribution before coercion
  drop_raw  <- reviewed$drop
  drop_type <- class(drop_raw)[1]
  n_true  <- sum(isTRUE(drop_raw) | (!is.na(drop_raw) & toupper(trimws(as.character(drop_raw))) %in% c("TRUE","1","YES")), na.rm = TRUE)
  n_false <- sum(!is.na(drop_raw) & toupper(trimws(as.character(drop_raw))) %in% c("FALSE","0","NO"), na.rm = TRUE)
  n_na    <- sum(is.na(drop_raw))
  log_info("  drop column: type=<{drop_type}> TRUE={n_true} FALSE={n_false} NA={n_na} (NA treated as keep)")

  reviewed <- reviewed %>%
    mutate(
      drop    = norm_bool(drop),
      comment = {
        # Guard: if comment is logical (all NA), convert cleanly without
        # producing the string "NA"
        cm <- if (is.logical(comment)) {
          rep(NA_character_, length(comment))
        } else {
          as.character(comment)
        }
        ifelse(toupper(trimws(cm)) %in% c("FALSE", "NA", ""), NA_character_, cm)
      }
    )

  # Diagnostic: log drop distribution after coercion
  n_drop_after <- sum(reviewed$drop, na.rm = TRUE)
  log_info("  drop after coercion: TRUE={n_drop_after} FALSE/keep={nrow(reviewed) - n_drop_after}")

  # Warn if an unusually high fraction of rows are flagged for dropping.
  pct_drop <- mean(reviewed$drop, na.rm = TRUE)
  if (pct_drop > 0.8) {
    log_warn(
      "  WARNING: {round(pct_drop * 100, 1)}% of rows are flagged drop=TRUE.",
      " Check review file encoding — may indicate string/logical coercion issue."
    )
  }

  comments_per_prompt <- reviewed %>%
    dplyr::filter(!is.na(comment)) %>%
    group_by(participant_id, task, audio_file, prompt) %>%
    summarise(comment_count = n(), .groups = "drop")
  n_before  <- nrow(reviewed)
  tx_clean  <- reviewed %>% dplyr::filter(!drop)
  n_dropped <- (n_before - nrow(tx_clean)) + n_dropped_extra
  log_info("  Dropped {n_before - nrow(tx_clean)} row(s) flagged for removal ({nrow(tx_clean)} remaining)")
  # Ensure 'response' column is populated. Review-path TSVs from the desktop
  # app use column name 'response'; auto-cleanup TSVs use 'word'. Copy word
  # -> response when response is absent or entirely NA so stage 4 always finds
  # a populated 'response' column regardless of which path produced the TSV.
  if (!"response" %in% names(tx_clean) || all(is.na(tx_clean[["response"]]))) {
    if ("word" %in% names(tx_clean)) {
      tx_clean <- tx_clean %>% mutate(response = word)
    }
  }
  list(
    tx_clean            = tx_clean,
    comments_per_prompt = comments_per_prompt,
    n_dropped           = n_dropped
  )
}

load_or_build_baseline <- function(auto_stats_path, tx_clean,
                                   comments_per_prompt, n_dropped, config) {
  if (file.exists(auto_stats_path)) {
    log_info("  Loading frozen auto-cleanup baseline stats: {auto_stats_path}")
    auto_stats               <- read_rds(auto_stats_path)
    baseline_removed_stats   <- auto_stats$removed_stats
    baseline_rule_violations <- auto_stats$rule_violations
  } else {
    log_warn("  No auto-cleanup baseline stats found — building fallback from reviewed TSV.")
    fillers_to_remove <- config$transcription$filters$remove_fillers
    ums_fb <- tx_clean %>%
      dplyr::filter(task %in% c("SENT-REP", "WMEMORY", "WORD-ASSOC")) %>%
      mutate(is_um = response %in% fillers_to_remove) %>%
      group_by(participant_id, task, audio_file, prompt) %>%
      summarise(ums_count = sum(is_um, na.rm = TRUE), .groups = "drop")
    rep_prompt_fb <- tx_clean %>%
      dplyr::filter(task == "WORD-ASSOC") %>%
      mutate(is_rep_prompt = (prompt == response)) %>%
      group_by(participant_id, task, audio_file, prompt) %>%
      summarise(rep_prompt = sum(is_rep_prompt, na.rm = TRUE), .groups = "drop")
    rep_words_fb <- tx_clean %>%
      dplyr::filter(task == "WORD-ASSOC") %>%
      group_by(participant_id, task, audio_file, prompt) %>%
      mutate(is_repeat = duplicated(response)) %>%
      summarise(rep_words_per_prompt = sum(is_repeat, na.rm = TRUE), .groups = "drop")
    baseline_removed_stats   <- list(
      ums            = sum(ums_fb$ums_count),
      repetitions    = sum(rep_prompt_fb$rep_prompt) + sum(rep_words_fb$rep_words_per_prompt),
      low_confidence = 0L,
      comments       = 0L,
      total_removed  = n_dropped
    )
    baseline_rule_violations <- list(
      ums                       = ums_fb,
      comments                  = tibble::tibble(),
      repeated_prompts          = rep_prompt_fb,
      repeated_words_per_prompt = rep_words_fb
    )
  }
  list(removed_stats = baseline_removed_stats, rule_violations = baseline_rule_violations)
}

run_task_cleaners <- function(tx_clean, config) {
  cleaned_tasks <- list()
  tx_checkbox <- tx_clean %>% dplyr::filter(stringr::str_detect(audio_file, "CHECKBOX"))
  if (nrow(tx_checkbox) > 0) {
    cleaned_tasks$checkbox <- clean_checkbox(tx_checkbox, config)
    log_info("  CHECKBOX: {nrow(cleaned_tasks$checkbox)} trials processed")
  }
  tx_hi <- tx_clean %>% dplyr::filter(stringr::str_detect(audio_file, "HI"))
  if (nrow(tx_hi) > 0) {
    cleaned_tasks$hi <- clean_hi(tx_hi, config)
    log_info("  HI: {nrow(cleaned_tasks$hi)} trials processed")
  }
  tx_word_assoc <- tx_clean %>% dplyr::filter(stringr::str_detect(audio_file, "WORD-ASSOC"))
  if (nrow(tx_word_assoc) > 0) {
    cleaned_tasks$word_assoc <- clean_word_assoc(tx_word_assoc, config)
    log_info("  WORD-ASSOC: {nrow(cleaned_tasks$word_assoc)} words across {dplyr::n_distinct(cleaned_tasks$word_assoc$prompt)} prompts")
  }
  tx_wmemory <- tx_clean %>% dplyr::filter(stringr::str_detect(audio_file, "WMEMORY"))
  if (nrow(tx_wmemory) > 0 && !is.null(cleaned_tasks$word_assoc)) {
    cleaned_tasks$wmemory <- clean_wmemory(tx_wmemory, cleaned_tasks$word_assoc, config)
    log_info("  WMEMORY: {nrow(cleaned_tasks$wmemory)} recall trials")
  }
  tx_sent_rep <- tx_clean %>% dplyr::filter(stringr::str_detect(audio_file, "SENT-REP"))
  if (nrow(tx_sent_rep) > 0) {
    cleaned_tasks$sent_rep <- clean_sent_rep(tx_sent_rep, config)
    log_info("  SENT-REP: {nrow(cleaned_tasks$sent_rep)} sentences")
  }
  tx_reading <- tx_clean %>% dplyr::filter(stringr::str_detect(audio_file, "READING"))
  if (nrow(tx_reading) > 0) {
    cleaned_tasks$reading <- clean_reading(tx_reading, config)
    log_info("  READING: {nrow(cleaned_tasks$reading)} words")
  }
  cleaned_tasks
}

# ---------------------------------------------------------------------------
# Resolve the review-file directory
# ---------------------------------------------------------------------------
effective_review_dir <- if (!is.null(opt$review_dir) && nzchar(opt$review_dir)) {
  opt$review_dir
} else {
  file.path(opt$output, "review_files")
}

all_review_files <- character(0)
if (dir.exists(effective_review_dir)) {
  pattern          <- paste0("^", opt$id, "_review_[A-Za-z0-9]+_\\d{8}T\\d{4}\\.tsv$")
  all_review_files <- list.files(effective_review_dir, pattern = pattern, full.names = TRUE)
}
all_review_files <- all_review_files[
  !grepl("_consensus", basename(all_review_files))
]

stage_start <- proc.time()[[3]]

if (length(all_review_files) > 0) {

  # REVIEW PATH
  rater_files <- get_latest_per_rater(all_review_files, opt$id)
  n_raters    <- length(rater_files)
  log_info("  Path          : REVIEW ({n_raters} rater file(s) found)")
  for (nm in names(rater_files)) log_info("    [{nm}] {basename(rater_files[[nm]])}")

  raw_tsv <- tx_raw   # already normalized above

  if (n_raters > 1) {
    log_info("  Multiple raters — computing consensus.")
    reviewed <- compute_consensus(raw_tsv, rater_files)
    consensus_out <- file.path(
      effective_review_dir,
      paste0(opt$id, "_consensus_", format(Sys.time(), "%Y%m%dT%H%M"), ".tsv")
    )
    write_tsv(reviewed, consensus_out)
    log_info("  Consensus written: {consensus_out}")
  } else {
    log_info("  Single rater — applying review directly.")
    reviewed <- norm_word_response(
      read_tsv(rater_files[[1]], show_col_types = FALSE)
    )
    # Single-rater file may also have fewer rows than raw_tsv if the reviewer
    # deleted rows outright. Re-align to raw_tsv so downstream steps see all rows.
    if (nrow(reviewed) != nrow(raw_tsv) &&
        all(c("audio_file", "start") %in% names(reviewed)) &&
        all(c("audio_file", "start") %in% names(raw_tsv))) {
      log_warn("  Single-rater TSV has {nrow(reviewed)} rows vs {nrow(raw_tsv)} in raw — re-aligning by key.")
      reviewed <- compute_consensus(raw_tsv, rater_files)
    }
  }

  applied              <- apply_review_tsv(reviewed, config)
  tx_clean             <- applied$tx_clean
  comments_per_prompt  <- applied$comments_per_prompt
  n_dropped            <- applied$n_dropped

  stats_out       <- load_or_build_baseline(auto_stats_path, tx_clean, comments_per_prompt, n_dropped, config)
  removed_stats   <- stats_out$removed_stats
  rule_violations <- stats_out$rule_violations

  log_info("  Cleanup stats — fillers: {removed_stats$ums}, repetitions: {removed_stats$repetitions}, drops this round: {n_dropped}, total removed: {removed_stats$total_removed}")
  log_info("  Valid responses remaining: {nrow(tx_clean)}")

  cleaned_tasks <- run_task_cleaners(tx_clean, config)

  cleanup_results <- list(
    requires_review = FALSE,
    clean_tx        = tx_clean,
    removed_stats   = removed_stats,
    review_file     = NULL,
    cleaned_by_task = cleaned_tasks,
    rule_violations = rule_violations
  )

} else if (!is.null(opt$reviewed_tsv) && file.exists(opt$reviewed_tsv)) {

  # LEGACY PATH
  log_info("  Path: LEGACY (--reviewed_tsv supplied; no review_dir files found)")
  log_info("  File: {opt$reviewed_tsv}")
  log_warn("  Consider migrating to --review_dir for multi-reviewer support.")

  reviewed            <- norm_word_response(
    read_tsv(opt$reviewed_tsv, show_col_types = FALSE)
  )
  log_info("  Legacy TSV rows: {nrow(reviewed)}")
  applied             <- apply_review_tsv(reviewed, config)
  tx_clean            <- applied$tx_clean
  comments_per_prompt <- applied$comments_per_prompt
  n_dropped           <- applied$n_dropped

  stats_out       <- load_or_build_baseline(auto_stats_path, tx_clean, comments_per_prompt, n_dropped, config)
  removed_stats   <- stats_out$removed_stats
  rule_violations <- stats_out$rule_violations

  log_info("  Cleanup stats — fillers: {removed_stats$ums}, repetitions: {removed_stats$repetitions}, drops this round: {n_dropped}, total removed: {removed_stats$total_removed}")
  log_info("  Valid responses remaining: {nrow(tx_clean)}")

  cleaned_tasks <- run_task_cleaners(tx_clean, config)

  cleanup_results <- list(
    requires_review = FALSE,
    clean_tx        = tx_clean,
    removed_stats   = removed_stats,
    review_file     = NULL,
    cleaned_by_task = cleaned_tasks,
    rule_violations = rule_violations
  )

} else {

  # AUTOMATIC PATH
  log_info("  Path: AUTO (no review files found — running automatic cleanup)")
  log_info("  Mode: {opt$mode}")

  transcription   <- tx_raw
  cleanup_results <- cleanup_transcription(
    transcription  = transcription,
    participant_id = opt$id,
    mode           = opt$mode,
    config         = config,
    output_dir     = file.path(opt$output, "review_files")
  )

  if (!is.null(cleanup_results$removed_stats)) {
    rs <- cleanup_results$removed_stats
    log_info("  Cleanup stats — fillers: {rs$ums}, repetitions: {rs$repetitions}, low-conf: {rs$low_confidence}, comments: {rs$comments}, total removed: {rs$total_removed}")
    log_info("  Valid responses remaining: {nrow(cleanup_results$clean_tx)}")
  }
}

elapsed <- round(proc.time()[[3]] - stage_start, 1)

# ===========================================================================
# NORMALISE SCHEMA — ensure review, legacy, and auto paths write identical columns
# ===========================================================================

CANONICAL_COLS <- c(
  "participant_id", "task", "audio_file", "prompt", "trial",
  "type", "subtype", "start", "end", "response", "confidence",
  "drop", "comment"
)
ctx <- cleanup_results$clean_tx

# FIX: The auto-cleanup path names the transcribed token column 'word', not
# 'response'. Stage 4 always expects 'response'. Copy word -> response before
# the canonical column loop adds a blank NA column in its place.
if (!"response" %in% names(ctx) || all(is.na(ctx[["response"]]))) {
  if ("word" %in% names(ctx)) {
    log_info("  Populating 'response' from 'word' column (auto-cleanup path).")
    ctx[["response"]] <- ctx[["word"]]
  }
}

for (.col in CANONICAL_COLS) {
  if (!.col %in% names(ctx)) ctx[[.col]] <- NA
}
extra_cols <- setdiff(names(ctx), CANONICAL_COLS)
cleanup_results$clean_tx <- ctx[, c(CANONICAL_COLS, extra_cols), drop = FALSE]

# ===========================================================================
# SAVE OUTPUTS
# ===========================================================================

cleaned_tsv_path <- file.path(opt$output, "review_files",
                               paste0(opt$id, "_cleaned_transcription.tsv"))
write_tsv(cleanup_results$clean_tx, cleaned_tsv_path)

# Abort early with a useful message if the cleaned TSV is empty. An empty
# cleaned transcription will crash stage 4 with unhelpful errors.
n_clean <- nrow(cleanup_results$clean_tx)
if (n_clean == 0) {
  log_error("  ✗ Cleaned transcription has 0 rows — aborting to prevent downstream failures.")
  log_error("    Raw input had {nrow(tx_raw)} rows.")
  log_error("    Review path used: {length(all_review_files)} file(s) found in review dir.")
  log_error("    If using reviewer files, check that the drop column is not all TRUE.")
  quit(status = 1)
}
log_info("  ✓ Cleaned transcription saved ({n_clean} rows): {cleaned_tsv_path}")

cleaning_stats_path <- file.path(opt$output, "features",
                                  paste0(opt$id, "_transcription_cleaning_stats.rds"))
write_rds(
  list(
    removed_stats   = cleanup_results$removed_stats,
    rule_violations = cleanup_results$rule_violations
  ),
  cleaning_stats_path,
  compress = "gz"
)
log_info("  ✓ Cleaning stats saved: {cleaning_stats_path}")

# FIX: was _cleaned_tasks.rds — renamed to _tasks-minimal-features.rds
# to match the filename run_04_feature_extraction.R expects.
task_features_path <- file.path(opt$output, "features",
                                 paste0(opt$id, "_tasks-minimal-features.rds"))
write_rds(cleanup_results$cleaned_by_task, task_features_path, compress = "gz")
log_info("  ✓ Task-level features saved: {task_features_path}")

log_info(strrep("-", 80))
log_info("STAGE 3 complete in {elapsed}s")
