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

# Compute a simple consensus file given the raw transcription and a set of
# per-rater TSV review files that share the same columns as the "raw" export
# plus reviewer-specific columns (e.g., drop/comment). For now we use the
# most common decision per row across raters when possible, otherwise fall
# back to the latest rater for ties.
compute_consensus <- function(raw_tsv, rater_files) {
  if (!length(rater_files)) return(raw_tsv)

  # read all rater files
  rater_dfs <- lapply(rater_files, readr::read_tsv, show_col_types = FALSE)

  # assume they all have the same number/order of rows as raw_tsv
  # and at least the columns: participant_id, task, audio_file, prompt, word
  # plus optional drop/comment columns.
  base <- raw_tsv

  # Collect drop/comment decisions if present
  drop_mat    <- list()
  comment_mat <- list()
  for (nm in names(rater_dfs)) {
    df <- rater_dfs[[nm]]
    if ("drop" %in% names(df))    drop_mat[[nm]]    <- df$drop    else drop_mat[[nm]]    <- rep(NA, nrow(df))
    if ("comment" %in% names(df)) comment_mat[[nm]] <- df$comment else comment_mat[[nm]] <- rep(NA, nrow(df))
  }

  drop_mat    <- as.data.frame(drop_mat, stringsAsFactors = FALSE)
  comment_mat <- as.data.frame(comment_mat, stringsAsFactors = FALSE)

  # Majority vote for drop, latest non-NA comment
  majority_drop <- apply(drop_mat, 1, function(x) {
    x <- toupper(trimws(as.character(x)))
    x <- x[x %in% c("TRUE", "FALSE", "1", "0", "YES", "NO")]
    if (!length(x)) return(NA_character_)
    # treat TRUE/1/YES as TRUE, others as FALSE
    bool <- x %in% c("TRUE", "1", "YES")
    if (sum(bool) > length(bool) / 2) "TRUE" else "FALSE"
  })

  latest_comment <- apply(comment_mat, 1, function(x) {
    x <- as.character(x)
    x <- x[!is.na(x) & nzchar(trimws(x))]
    if (!length(x)) NA_character_ else tail(x, 1)
  })

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

tx_raw <- read_tsv(opt$transcription_file, show_col_types = FALSE)
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
  norm_bool <- function(x) {
    if (is.logical(x)) return(x)
    toupper(trimws(as.character(x))) %in% c("TRUE", "1", "YES")
  }
  if (!"drop"    %in% names(reviewed)) reviewed$drop    <- FALSE
  if (!"comment" %in% names(reviewed)) reviewed$comment <- NA_character_
  reviewed <- reviewed %>%
    mutate(
      drop    = norm_bool(drop),
      comment = ifelse(toupper(trimws(as.character(comment))) %in%
                         c("FALSE", "NA", ""), NA_character_, as.character(comment))
    )
  comments_per_prompt <- reviewed %>%
    dplyr::filter(!is.na(comment)) %>%
    group_by(participant_id, task, audio_file, prompt) %>%
    summarise(comment_count = n(), .groups = "drop")
  n_before  <- nrow(reviewed)
  tx_clean  <- reviewed %>% dplyr::filter(!drop)
  n_dropped <- (n_before - nrow(tx_clean)) + n_dropped_extra
  log_info("  Dropped {n_before - nrow(tx_clean)} row(s) flagged for removal ({nrow(tx_clean)} remaining)")
  # Preserve drop/comment so they survive into the cleaned TSV
  if (!"response" %in% names(tx_clean)) tx_clean <- tx_clean %>% mutate(response = word)
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

  raw_tsv <- read_tsv(opt$transcription_file, show_col_types = FALSE)

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
    reviewed <- read_tsv(rater_files[[1]], show_col_types = FALSE)
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

  reviewed            <- read_tsv(opt$reviewed_tsv, show_col_types = FALSE)
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
log_info("  ✓ Cleaned transcription saved: {cleaned_tsv_path}")

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
