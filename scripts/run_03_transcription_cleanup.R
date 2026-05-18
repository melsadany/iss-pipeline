#!/usr/bin/env Rscript
# run_03_transcription_cleanup.R
# ISS Pipeline — Stage 3: Transcription Cleanup
# NOTE: Reviewer-file paths are temporarily commented out.
#       The script always runs in AUTO mode until reviewer support is re-enabled.

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
  # REVIEWER DISABLED: --review_dir, --no_review, --reviewed_tsv commented out
  # make_option(c("--review_dir"), type = "character", default = NULL,
  #             help = "Directory containing per-rater review TSV files."),
  # make_option(c("--no_review"), type = "logical", default = FALSE,
  #             action = "store_true",
  #             help = "Force automatic cleanup — skip all reviewer file detection."),
  # make_option(c("--reviewed_tsv"), type = "character", default = NULL,
  #             help = "[DEPRECATED] Legacy single-reviewer TSV."),
  make_option(c("--no_review"),          type = "logical",   default = FALSE,
              action = "store_true",
              help = "(Accepted and ignored — reviewer paths disabled. Always runs auto.)"),
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
# ---------------------------------------------------------------------------
lgl_to_char <- function(x) {
  if (is.logical(x)) {
    ifelse(is.na(x), NA_character_, ifelse(x, "TRUE", "FALSE"))
  } else {
    as.character(x)
  }
}

# ---------------------------------------------------------------------------
# REVIEWER DISABLED: get_latest_per_rater, compute_consensus, apply_review_tsv,
# load_or_build_baseline helper functions commented out.
# ---------------------------------------------------------------------------
# get_latest_per_rater <- function(files, id) { ... }
# compute_consensus    <- function(raw_tsv, rater_files) { ... }
# apply_review_tsv     <- function(reviewed, config, n_dropped_extra = 0L) { ... }
# load_or_build_baseline <- function(auto_stats_path, tx_clean,
#                                    comments_per_prompt, n_dropped, config) { ... }

config <- yaml::read_yaml(opt$config)

log_info("STAGE 3: Transcription Cleanup")
log_info(strrep("-", 80))
log_info("  Participant       : {opt$id}")
log_info("  Transcription file: {opt$transcription_file}")
log_info("  Mode              : auto (reviewer paths disabled)")
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
# REVIEWER DISABLED: review-file detection block commented out.
# When re-enabling, restore the if/else if/else chain that checks
# force_auto -> all_review_files -> opt$reviewed_tsv -> AUTO PATH.
# ---------------------------------------------------------------------------
# force_auto <- isTRUE(opt$no_review)
# if (force_auto) {
#   log_info("  Path : AUTO (--no-review flag set)")
#   all_review_files <- character(0)
# } else {
#   effective_review_dir <- if (!is.null(opt$review_dir) && nzchar(opt$review_dir)) {
#     opt$review_dir
#   } else {
#     file.path(opt$output, "review_files")
#   }
#   all_review_files <- character(0)
#   if (dir.exists(effective_review_dir)) {
#     pattern <- paste0("^", opt$id, "_review_[A-Za-z0-9]+_\\d{8}T\\d{4}\\.tsv$")
#     all_review_files <- list.files(effective_review_dir, pattern = pattern, full.names = TRUE)
#   }
#   all_review_files <- all_review_files[!grepl("_consensus", basename(all_review_files))]
# }

stage_start <- proc.time()[[3]]

# REVIEWER DISABLED: REVIEW PATH and LEGACY PATH blocks commented out.
# if (length(all_review_files) > 0) {
#   # ... REVIEW PATH ...
# } else if (!is.null(opt$reviewed_tsv) && file.exists(opt$reviewed_tsv)) {
#   # ... LEGACY PATH ...
# } else {

# AUTOMATIC PATH — always taken while reviewer support is disabled
log_info("  Path: AUTO (reviewer paths disabled — running automatic cleanup)")
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

# } # end of reviewer else block

elapsed <- round(proc.time()[[3]] - stage_start, 1)

# ===========================================================================
# NORMALISE SCHEMA — ensure auto path writes canonical columns
# ===========================================================================

CANONICAL_COLS <- c(
  "participant_id", "task", "audio_file", "prompt", "trial",
  "type", "subtype", "start", "end", "response", "confidence",
  "drop", "comment"
)
ctx <- cleanup_results$clean_tx

# The auto-cleanup path names the transcribed token column 'word', not 'response'.
# Stage 4 always expects 'response'. Copy word -> response before the canonical
# column loop would otherwise add a blank NA column in its place.
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

n_clean <- nrow(cleanup_results$clean_tx)
if (n_clean == 0) {
  log_error("  ✗ Cleaned transcription has 0 rows — aborting to prevent downstream failures.")
  log_error("    Raw input had {nrow(tx_raw)} rows.")
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

task_features_path <- file.path(opt$output, "features",
                                 paste0(opt$id, "_tasks-minimal-features.rds"))
write_rds(cleanup_results$cleaned_by_task, task_features_path, compress = "gz")
log_info("  ✓ Task-level features saved: {task_features_path}")

log_info(strrep("-", 80))
log_info("STAGE 3 complete in {elapsed}s")
