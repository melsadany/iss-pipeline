#!/usr/bin/env Rscript

required_packages <- c("optparse", "yaml", "logger", "tidyverse", "logging", "stringdist")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    log_warn("Package {pkg} not installed. Installing...")
    install.packages(pkg, repos = "https://cloud.r-project.org")
    suppressWarnings(suppressPackageStartupMessages({library(pkg, character.only = TRUE)}))
  }
}

# Get script directory
get_script_path <- function() {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  needle <- "--file="
  match <- grep(needle, cmdArgs)
  if (length(match) > 0) {
    return(dirname(normalizePath(sub(needle, "", cmdArgs[match]))))
  } else {
    return(getwd())
  }
}

script_dir <- get_script_path()

# Parse arguments
option_list <- list(
  make_option(c("--id"), type = "character", default = NULL,
              help = "Participant ID"),
  make_option(c("--transcription_file"), type = "character", default = NULL,
              help = "Path to transcription TSV file"),
  make_option(c("--reviewed_tsv"), type = "character", default = NULL,
              help = "Path to reviewed TSV (review_files/<id>_cleaned_transcription.tsv). \
When supplied the script uses drop/comment columns set during manual review \
instead of running automatic cleanup from scratch."),
  make_option(c("--mode"), type = "character", default = "auto",
              help = "Processing mode: auto, review, or strict"),
  make_option(c("--config"), type = "character", default = NULL,
              help = "Path to config YAML"),
  make_option(c("--reference"), type = "character", default = NULL,
              help = "Reference data directory"),
  make_option(c("--output"), type = "character", default = NULL,
              help = "Output directory")
)

opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# Setup logging
log_threshold(INFO)
log_appender(appender_console)

# Source dependencies
source(file.path(script_dir, "00_initialize.R"))
source(file.path(script_dir, "03_transcription_cleanup.R"))

# Load config
config <- yaml::read_yaml(opt$config)

log_info("STAGE 3: Transcription Cleanup")
log_info(strrep("-", 80))

system(paste0("mkdir -p ", file.path(opt$output, "review_files")))
system(paste0("mkdir -p ", file.path(opt$output, "features")))

# ─────────────────────────────────────────────────────────────────────────────
# REVIEWED PATH: user has already edited the TSV in the desktop app.
# We read it, apply the drop/comment columns they set, then derive the same
# outputs that the automatic path produces so stage 4 is unaffected.
# ─────────────────────────────────────────────────────────────────────────────
if (!is.null(opt$reviewed_tsv) && file.exists(opt$reviewed_tsv)) {

  log_info("  Reviewed TSV supplied — using manual review instead of auto-cleanup")
  log_info("  File: {opt$reviewed_tsv}")

  reviewed <- read_tsv(opt$reviewed_tsv, show_col_types = FALSE)

  # Normalise the boolean columns (accept TRUE/FALSE as strings or logicals)
  norm_bool <- function(x) {
    if (is.logical(x)) return(x)
    toupper(trimws(as.character(x))) == "TRUE"
  }

  if ("drop" %in% names(reviewed)) {
    reviewed <- reviewed %>% mutate(drop = norm_bool(drop))
  } else {
    reviewed <- reviewed %>% mutate(drop = FALSE)
  }

  if ("comment" %in% names(reviewed)) {
    reviewed <- reviewed %>% mutate(comment = norm_bool(comment))
  } else {
    reviewed <- reviewed %>% mutate(comment = FALSE)
  }

  # Count comment rows as a feature before dropping anything
  comments_per_prompt <- reviewed %>%
    dplyr::filter(comment) %>%
    group_by(participant_id, task, audio_file, prompt) %>%
    summarise(comment_count = n(), .groups = "drop")

  # Drop rows the reviewer flagged
  n_before <- nrow(reviewed)
  tx_clean <- reviewed %>% dplyr::filter(!drop)
  n_dropped <- n_before - nrow(tx_clean)
  log_info("  Dropped {n_dropped} rows flagged for removal ({nrow(tx_clean)} remaining)")

  # Strip the review-only columns before passing to task-specific cleaners
  tx_clean <- tx_clean %>% select(-any_of(c("drop", "comment")))

  # Ensure response column exists (reviewed TSV already has it)
  if (!"response" %in% names(tx_clean)) {
    tx_clean <- tx_clean %>% mutate(response = word)
  }

  # Compute remaining rule violations from the kept rows
  fillers_to_remove <- config$transcription$filters$remove_fillers

  ums_per_prompt <- tx_clean %>%
    dplyr::filter(task %in% c("SENT-REP", "WMEMORY", "WORD-ASSOC")) %>%
    mutate(is_um = response %in% fillers_to_remove) %>%
    group_by(participant_id, task, audio_file, prompt) %>%
    summarise(ums_count = sum(is_um, na.rm = TRUE), .groups = "drop")

  rep_prompts_per_prompt <- tx_clean %>%
    dplyr::filter(task == "WORD-ASSOC") %>%
    mutate(is_rep_prompt = (prompt == response)) %>%
    group_by(participant_id, task, audio_file, prompt) %>%
    summarise(rep_prompt = sum(is_rep_prompt, na.rm = TRUE), .groups = "drop")

  rep_words_per_prompt <- tx_clean %>%
    dplyr::filter(task == "WORD-ASSOC") %>%
    group_by(participant_id, task, audio_file, prompt) %>%
    mutate(is_repeat = duplicated(response)) %>%
    summarise(rep_words_per_prompt = sum(is_repeat, na.rm = TRUE), .groups = "drop")

  rule_violations <- list(
    ums              = ums_per_prompt,
    comments         = comments_per_prompt,
    repeated_prompts = rep_prompts_per_prompt,
    repeated_words_per_prompt = rep_words_per_prompt
  )

  removed_stats <- list(
    ums            = sum(ums_per_prompt$ums_count),
    repetitions    = sum(rep_prompts_per_prompt$rep_prompt) +
                     sum(rep_words_per_prompt$rep_words_per_prompt),
    low_confidence = 0,
    comments       = sum(comments_per_prompt$comment_count),
    total_removed  = n_dropped
  )

  # Run task-specific cleaners on the kept rows
  cleaned_tasks <- list()

  tx_checkbox <- tx_clean %>% dplyr::filter(str_detect(audio_file, "CHECKBOX"))
  if (nrow(tx_checkbox) > 0) cleaned_tasks$checkbox <- clean_checkbox(tx_checkbox, config)

  tx_hi <- tx_clean %>% dplyr::filter(str_detect(audio_file, "HI"))
  if (nrow(tx_hi) > 0) cleaned_tasks$hi <- clean_hi(tx_hi, config)

  tx_word_assoc <- tx_clean %>% dplyr::filter(str_detect(audio_file, "WORD-ASSOC"))
  if (nrow(tx_word_assoc) > 0) cleaned_tasks$word_assoc <- clean_word_assoc(tx_word_assoc, config)

  tx_wmemory <- tx_clean %>% dplyr::filter(str_detect(audio_file, "WMEMORY"))
  if (nrow(tx_wmemory) > 0 && !is.null(cleaned_tasks$word_assoc)) {
    cleaned_tasks$wmemory <- clean_wmemory(tx_wmemory, cleaned_tasks$word_assoc, config)
  }

  tx_sent_rep <- tx_clean %>% dplyr::filter(str_detect(audio_file, "SENT-REP"))
  if (nrow(tx_sent_rep) > 0) cleaned_tasks$sent_rep <- clean_sent_rep(tx_sent_rep, config)

  tx_reading <- tx_clean %>% dplyr::filter(str_detect(audio_file, "READING"))
  if (nrow(tx_reading) > 0) cleaned_tasks$reading <- clean_reading(tx_reading, config)

  cleanup_results <- list(
    requires_review = FALSE,
    clean_tx        = tx_clean,
    removed_stats   = removed_stats,
    review_file     = NULL,
    cleaned_by_task = cleaned_tasks,
    rule_violations = rule_violations
  )

} else {
  # ─────────────────────────────────────────────────────────────────────────
  # AUTOMATIC PATH (first run, no reviewed TSV yet)
  # ─────────────────────────────────────────────────────────────────────────
  transcription <- read_tsv(opt$transcription_file, show_col_types = FALSE)

  cleanup_results <- cleanup_transcription(
    transcription  = transcription,
    participant_id = opt$id,
    mode           = opt$mode,
    config         = config,
    output_dir     = file.path(opt$output, "review_files")
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# SAVE OUTPUTS (same for both paths)
# ─────────────────────────────────────────────────────────────────────────────

# Cleaned transcription (this is what the review tab edits and stage 4 reads)
write_tsv(
  cleanup_results$clean_tx,
  file.path(opt$output, "review_files", paste0(opt$id, "_cleaned_transcription.tsv"))
)

write_rds(
  list(
    removed_stats   = cleanup_results$removed_stats,
    rule_violations = cleanup_results$rule_violations
  ),
  file.path(opt$output, "features", paste0(opt$id, "_transcription_cleaning_stats.rds")),
  compress = "gz"
)

write_rds(
  cleanup_results$cleaned_by_task,
  file.path(opt$output, "features", paste0(opt$id, "_tasks-minimal-features.rds")),
  compress = "gz"
)

log_info("  ✓ Transcription cleaned")
log_info("  ✓ {nrow(cleanup_results$clean_tx)} valid responses")
log_info("✓ Stage 3 Complete")
