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

# ---------------------------------------------------------------------------
# Script-path helper
# ---------------------------------------------------------------------------
get_script_path <- function() {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  needle  <- "--file="
  match   <- grep(needle, cmdArgs)
  if (length(match) > 0) dirname(normalizePath(sub(needle, "", cmdArgs[match])))
  else getwd()
}
script_dir <- get_script_path()

# ---------------------------------------------------------------------------
# CLI arguments
# ---------------------------------------------------------------------------
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
  # Legacy alias kept for backwards-compatibility with older run scripts.
  # Ignored when --review_dir is supplied.
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

# ---------------------------------------------------------------------------
# Logging
# ---------------------------------------------------------------------------
log_threshold(INFO)
log_appender(appender_console)

# ---------------------------------------------------------------------------
# Dependencies
# ---------------------------------------------------------------------------
source(file.path(script_dir, "00_initialize.R"))
source(file.path(script_dir, "03_transcription_cleanup.R"))

config <- yaml::read_yaml(opt$config)

log_info("STAGE 3: Transcription Cleanup")
log_info(strrep("-", 80))

system(paste0("mkdir -p ", file.path(opt$output, "review_files")))
system(paste0("mkdir -p ", file.path(opt$output, "features")))

# Path to the frozen auto-cleanup baseline (written once; never overwritten)
auto_stats_path <- file.path(
  opt$output, "features",
  paste0(opt$id, "_auto_cleaning_stats.rds")
)

# ---------------------------------------------------------------------------
# Helper: apply drop/comment columns produced by review (single TSV path)
# ---------------------------------------------------------------------------
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

  # Comment-flagged rows counted as a feature before any dropping
  comments_per_prompt <- reviewed %>%
    dplyr::filter(!is.na(comment)) %>%
    group_by(participant_id, task, audio_file, prompt) %>%
    summarise(comment_count = n(), .groups = "drop")

  n_before  <- nrow(reviewed)
  tx_clean  <- reviewed %>% dplyr::filter(!drop)
  n_dropped <- (n_before - nrow(tx_clean)) + n_dropped_extra
  log_info("  Dropped {n_before - nrow(tx_clean)} row(s) flagged for removal ({nrow(tx_clean)} remaining)")

  tx_clean <- tx_clean %>% select(-any_of(c("drop", "comment")))
  if (!"response" %in% names(tx_clean)) tx_clean <- tx_clean %>% mutate(response = word)

  list(
    tx_clean            = tx_clean,
    comments_per_prompt = comments_per_prompt,
    n_dropped           = n_dropped
  )
}

# ---------------------------------------------------------------------------
# Helper: load frozen baseline stats (or build a fallback)
# ---------------------------------------------------------------------------
load_or_build_baseline <- function(auto_stats_path, tx_clean,
                                   comments_per_prompt, n_dropped, config) {
  if (file.exists(auto_stats_path)) {
    log_info("  Loading frozen auto-cleanup baseline stats: {auto_stats_path}")
    auto_stats              <- read_rds(auto_stats_path)
    baseline_removed_stats  <- auto_stats$removed_stats
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
      comments                  = comments_per_prompt,
      repeated_prompts          = rep_prompt_fb,
      repeated_words_per_prompt = rep_words_fb
    )
  }

  # Overlay current-round comment / drop counts
  removed_stats               <- baseline_removed_stats
  removed_stats$comments      <- sum(comments_per_prompt$comment_count)
  removed_stats$total_removed <- baseline_removed_stats$total_removed + n_dropped

  rule_violations          <- baseline_rule_violations
  rule_violations$comments <- comments_per_prompt

  list(removed_stats = removed_stats, rule_violations = rule_violations)
}

# ---------------------------------------------------------------------------
# Helper: run task-specific cleaners on a tx_clean data frame
# ---------------------------------------------------------------------------
run_task_cleaners <- function(tx_clean, config) {
  cleaned_tasks <- list()

  tx_checkbox <- tx_clean %>% dplyr::filter(str_detect(audio_file, "CHECKBOX"))
  if (nrow(tx_checkbox) > 0) cleaned_tasks$checkbox <- clean_checkbox(tx_checkbox, config)

  tx_hi <- tx_clean %>% dplyr::filter(str_detect(audio_file, "HI"))
  if (nrow(tx_hi) > 0) cleaned_tasks$hi <- clean_hi(tx_hi, config)

  tx_word_assoc <- tx_clean %>% dplyr::filter(str_detect(audio_file, "WORD-ASSOC"))
  if (nrow(tx_word_assoc) > 0) cleaned_tasks$word_assoc <- clean_word_assoc(tx_word_assoc, config)

  tx_wmemory <- tx_clean %>% dplyr::filter(str_detect(audio_file, "WMEMORY"))
  if (nrow(tx_wmemory) > 0 && !is.null(cleaned_tasks$word_assoc))
    cleaned_tasks$wmemory <- clean_wmemory(tx_wmemory, cleaned_tasks$word_assoc, config)

  tx_sent_rep <- tx_clean %>% dplyr::filter(str_detect(audio_file, "SENT-REP"))
  if (nrow(tx_sent_rep) > 0) cleaned_tasks$sent_rep <- clean_sent_rep(tx_sent_rep, config)

  tx_reading <- tx_clean %>% dplyr::filter(str_detect(audio_file, "READING"))
  if (nrow(tx_reading) > 0) cleaned_tasks$reading <- clean_reading(tx_reading, config)

  cleaned_tasks
}

# ===========================================================================
# DECISION TREE
#
#   1. --review_dir exists AND contains <id>_review_*.tsv files
#         ├─ multiple raters → compute_consensus() → apply_review_tsv()
#         └─ single rater   → apply_review_tsv() directly
#   2. --review_dir is empty or absent AND --reviewed_tsv supplied (legacy)
#         └─ apply_review_tsv() directly
#   3. No review at all → automatic cleanup_transcription()
# ===========================================================================

# Resolve effective review directory (prefer --review_dir over legacy)
effective_review_dir <- if (!is.null(opt$review_dir) && nchar(opt$review_dir) > 0)
  opt$review_dir
else
  file.path(opt$output, "review_files")  # default search location

# Glob per-rater files
all_review_files <- list.files(
  effective_review_dir,
  pattern = paste0("^", opt$id, "_review_[A-Za-z0-9]+_\\d{8}T\\d{4}\\.tsv$"),
  full.names = TRUE
)

# Remove consensus output from the file list if it somehow ended up there
all_review_files <- all_review_files[
  !grepl("_consensus", basename(all_review_files))
]

if (length(all_review_files) > 0) {

  # -------------------------------------------------------------------------
  # REVIEW PATH (one or more rater files found)
  # -------------------------------------------------------------------------
  rater_files <- get_latest_per_rater(all_review_files, opt$id)
  n_raters    <- length(rater_files)

  log_info("  Found {n_raters} rater file(s) in {effective_review_dir}:")
  for (nm in names(rater_files)) log_info("    [{nm}] {basename(rater_files[[nm]])}")

  # Load raw stage-2 TSV as the reference (needed by consensus and by
  # the single-reviewer path for row-key alignment)
  raw_tsv <- read_tsv(opt$transcription_file, show_col_types = FALSE)

  if (n_raters > 1) {
    # ── Multi-reviewer: compute consensus ───────────────────────────────────
    log_info("  Multiple raters detected — computing consensus.")
    reviewed <- compute_consensus(raw_tsv, rater_files)

    # Persist the consensus TSV for auditability
    consensus_out <- file.path(
      effective_review_dir,
      paste0(opt$id, "_consensus_",
             format(Sys.time(), "%Y%m%dT%H%M"), ".tsv")
    )
    write_tsv(reviewed, consensus_out)
    log_info("  Consensus written to: {consensus_out}")

  } else {
    # ── Single reviewer: use their file directly ────────────────────────────
    log_info("  Single rater — applying review directly without consensus voting.")
    reviewed <- read_tsv(rater_files[[1]], show_col_types = FALSE)
  }

  # Apply drop/comment columns
  applied              <- apply_review_tsv(reviewed, config)
  tx_clean             <- applied$tx_clean
  comments_per_prompt  <- applied$comments_per_prompt
  n_dropped            <- applied$n_dropped

  # Build / overlay stats
  stats_out  <- load_or_build_baseline(
    auto_stats_path, tx_clean, comments_per_prompt, n_dropped, config
  )
  removed_stats   <- stats_out$removed_stats
  rule_violations <- stats_out$rule_violations

  log_info("  Stats — ums: {removed_stats$ums} (baseline), repetitions: {removed_stats$repetitions} (baseline), drops this round: {n_dropped}, total removed: {removed_stats$total_removed}")

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

  # -------------------------------------------------------------------------
  # LEGACY PATH: --reviewed_tsv (single-reviewer old-style TSV)
  # -------------------------------------------------------------------------
  log_info("  [LEGACY] --reviewed_tsv supplied — no review_dir files found.")
  log_info("  File: {opt$reviewed_tsv}")
  log_info("  Consider migrating to --review_dir for multi-reviewer support.")

  reviewed            <- read_tsv(opt$reviewed_tsv, show_col_types = FALSE)
  applied             <- apply_review_tsv(reviewed, config)
  tx_clean            <- applied$tx_clean
  comments_per_prompt <- applied$comments_per_prompt
  n_dropped           <- applied$n_dropped

  stats_out       <- load_or_build_baseline(
    auto_stats_path, tx_clean, comments_per_prompt, n_dropped, config
  )
  removed_stats   <- stats_out$removed_stats
  rule_violations <- stats_out$rule_violations

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

  # -------------------------------------------------------------------------
  # AUTOMATIC PATH (no review files of any kind)
  # -------------------------------------------------------------------------
  log_info("  No review files found — running automatic cleanup.")

  transcription  <- read_tsv(opt$transcription_file, show_col_types = FALSE)
  cleanup_results <- cleanup_transcription(
    transcription  = transcription,
    participant_id = opt$id,
    mode           = opt$mode,
    config         = config,
    output_dir     = file.path(opt$output, "review_files")
  )
}

# ===========================================================================
# SAVE OUTPUTS (same for all paths)
# ===========================================================================

# Cleaned transcription (the file the Review tab loads; Stage 4 reads this)
write_tsv(
  cleanup_results$clean_tx,
  file.path(opt$output, "review_files",
            paste0(opt$id, "_cleaned_transcription.tsv"))
)

# Transcription cleaning stats (updated each run; consumed by stage 4)
write_rds(
  list(
    removed_stats   = cleanup_results$removed_stats,
    rule_violations = cleanup_results$rule_violations
  ),
  file.path(opt$output, "features",
            paste0(opt$id, "_transcription_cleaning_stats.rds")),
  compress = "gz"
)

# Frozen auto-cleanup baseline: written ONLY on the first automatic run so
# that ums / repetitions / low_confidence remain stable across review rounds.
if (!file.exists(auto_stats_path)) {
  log_info("  Writing frozen auto-cleanup baseline: {auto_stats_path}")
  write_rds(
    list(
      removed_stats   = cleanup_results$removed_stats,
      rule_violations = cleanup_results$rule_violations
    ),
    auto_stats_path,
    compress = "gz"
  )
} else {
  log_info("  Frozen auto-cleanup baseline already exists — not overwriting.")
}

write_rds(
  cleanup_results$cleaned_by_task,
  file.path(opt$output, "features",
            paste0(opt$id, "_tasks-minimal-features.rds")),
  compress = "gz"
)

log_info("  ✓ Transcription cleaned")
log_info("  ✓ {nrow(cleanup_results$clean_tx)} valid responses retained")
log_info("✓ Stage 3 Complete")
