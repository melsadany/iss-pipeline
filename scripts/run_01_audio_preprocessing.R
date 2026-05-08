#!/usr/bin/env Rscript

required_packages <- c("optparse", "yaml", "logger", "tidyverse", "tuneR", "logging")
for (pkg in required_packages) {
  if (!suppressWarnings(suppressPackageStartupMessages(
        require(pkg, character.only = TRUE, quietly = TRUE)
      ))) {
    log_warn("Package {pkg} not installed. Installing...")
    install.packages(pkg, repos = "https://cloud.r-project.org")
    suppressWarnings(suppressPackageStartupMessages(
      library(pkg, character.only = TRUE)
    ))
  }
}

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

option_list <- list(
  make_option(c("--audio"), type = "character", default = NULL,
              help = "Path to audio file", metavar = "FILE"),
  make_option(c("--id"), type = "character", default = NULL,
              help = "Participant ID", metavar = "ID"),
  make_option(c("--config"), type = "character", default = NULL,
              help = "Path to task configuration YAML"),
  make_option(c("--output"), type = "character", default = NULL,
              help = "Output directory"),
  make_option(c("--verbose"), action = "store_true", default = FALSE,
              help = "Verbose logging")
)

opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

if (is.null(opt$audio) || is.null(opt$id) || is.null(opt$config) || is.null(opt$output)) {
  print_help(opt_parser)
  stop("All arguments are required: --audio, --id, --config, --output", call. = FALSE)
}

log_threshold(if (opt$verbose) DEBUG else INFO)
log_appender(appender_console)

log_info("STAGE 1: Audio Preprocessing")
log_info(strrep("-", 80))
log_info("  Participant : {opt$id}")
log_info("  Audio file  : {opt$audio}")
log_info("  Config      : {opt$config}")
log_info("  Output dir  : {opt$output}")

# Validate inputs
if (!file.exists(opt$audio)) stop(sprintf("Audio file not found: %s", opt$audio), call. = FALSE)
if (!file.exists(opt$config)) stop(sprintf("Config file not found: %s", opt$config), call. = FALSE)
audio_size_mb <- round(file.info(opt$audio)$size / 1024^2, 1)
log_info("  Audio size  : {audio_size_mb} MB")

# Source + config
source(file.path(script_dir, "01_audio_preprocessing.R"))
log_info("  Sourced 01_audio_preprocessing.R")

config <- yaml::read_yaml(opt$config)
log_info("  Config loaded ({length(config)} top-level keys)")

# Run
stage_start <- proc.time()[[3]]
audio_results <- preprocess_audio(
  audio_file     = opt$audio,
  participant_id = opt$id,
  config         = config,
  output_dir     = opt$output
)
elapsed <- round(proc.time()[[3]] - stage_start, 1)

log_info("  ✓ Audio cropped to {length(audio_results$segments)} segments in {elapsed}s")

# Count segments by task type
task_counts <- table(sapply(audio_results$segments, `[[`, "task"))
for (task in names(task_counts)) {
  log_info("    - {task}: {task_counts[[task]]} segment(s)")
}

# Save
out_rds <- file.path(opt$output, opt$id, paste0(opt$id, "_audio_segments.rds"))
saveRDS(audio_results, out_rds)
log_info("  ✓ Segment metadata saved: {out_rds}")

log_info("✓ Stage 1 Complete [{elapsed}s]")
