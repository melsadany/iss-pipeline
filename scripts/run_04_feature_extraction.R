#!/usr/bin/env Rscript

required_packages <- c("optparse", "yaml", "logger", "tidyverse", "logging", "phonics")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    log_warn("Package {pkg} not installed. Installing...")
    install.packages(pkg, repos = "https://cloud.r-project.org")
    suppressWarnings(suppressPackageStartupMessages({library(pkg, character.only = TRUE)}))
  }
}

# opt = list(id="TEST0001",
#            config="config/task_template.yaml",
#            output="output/features",
#            transcription_file="output/review_files/TEST0001_cleaned_transcription.tsv",
#            reference="reference_data")
# script_dir = "scripts"


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
  make_option(c("--id"), type = "character", default = NULL),
  make_option(c("--transcription_file"), type = "character", default = NULL),
  make_option(c("--config"), type = "character", default = NULL),
  make_option(c("--reference"), type = "character", default = NULL),
  make_option(c("--output"), type = "character", default = NULL)
)

opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# Setup
log_threshold(INFO)
log_appender(appender_console)

# Source dependencies
source(file.path(script_dir, "00_initialize.R"))
source(file.path(script_dir, "04_feature_extraction.R"))

# Load and initialize
config <- yaml::read_yaml(opt$config)
pipeline_env <- initialize_pipeline(
  config = config,
  reference_dir = opt$reference,
  mode = "auto",
  where = dirname(script_dir)
)

log_info("STAGE 4: Feature Extraction")
log_info(strrep("-", 80))

# Read cleaned transcription
clean_tx <- read_tsv(opt$transcription_file, show_col_types = FALSE)

# Read transcription cleaning stats
clean_tx_stats <- read_rds(file.path(opt$output, paste0(opt$id, "_transcription_cleaning_stats.rds")))

# Read pre-computed features
clean_tx_by_task <- read_rds(file.path(opt$output, paste0(opt$id, "_tasks-minimal-features.rds")))


# Extract features
features <- extract_all_features(
  cleaned_transcription = clean_tx_by_task,
  participant_id = opt$id,
  embeddings = pipeline_env$embeddings,
  archetype_refs = pipeline_env$archetype_refs,
  reference_data = pipeline_env$reference_data,
  config = config,
  rule_violations = clean_tx_stats$rule_violations,
  cleaned_full = clean_tx
)

# Save results
dir.create(opt$output, showWarnings = FALSE, recursive = TRUE)
write_csv(features$per_prompt, file.path(opt$output, paste0(opt$id, "_per_prompt.csv")))
write_csv(features$per_task, file.path(opt$output, paste0(opt$id, "_per_task.csv")))
write_csv(features$per_participant, file.path(opt$output, paste0(opt$id, "_per_participant.csv")))

log_info("  ✓ Features extracted")
log_info("✓ Stage 4 Complete")
