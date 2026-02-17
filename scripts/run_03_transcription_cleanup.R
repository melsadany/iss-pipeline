#!/usr/bin/env Rscript

required_packages <- c("optparse", "yaml", "logger", "tidyverse", "logging", "stringdist")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    log_warn("Package {pkg} not installed. Installing...")
    install.packages(pkg, repos = "https://cloud.r-project.org")
    suppressWarnings(suppressPackageStartupMessages({library(pkg, character.only = TRUE)}))
  }
}

# opt = list(id="TEST0001", 
#            config="config/task_template.yaml", 
#            output="output", 
#            transcription_file="output/transcriptions/TEST0001/TEST0001_all_transcriptions.tsv",
#            mode="auto",
#            reference="reference_data")
# script_dir = "scripts"


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

# Load config and initialize
config <- yaml::read_yaml(opt$config)
# pipeline_env <- initialize_pipeline(
#   config = config,
#   reference_dir = opt$reference,
#   mode = opt$mode,
#   where = dirname(script_dir)
# )

# Read transcription
log_info("STAGE 3: Transcription Cleanup")
log_info(strrep("-", 80))

transcription <- read_tsv(opt$transcription_file, show_col_types = FALSE)

# Execute cleanup
cleanup_results <- cleanup_transcription(
  transcription = transcription,
  participant_id = opt$id,
  mode = opt$mode,
  config = config,
  output_dir = file.path(opt$output, "review_files")
)

# Save cleaned transcription
write_tsv(
  cleanup_results$clean_tx,
  file.path(opt$output, "review_files", paste0(opt$id, "_cleaned_transcription.tsv"))
)

write_rds(list(
  removed_stats = cleanup_results$removed_stats,
  rule_violations = cleanup_results$rule_violations), 
  file.path(opt$output, "features", paste0(opt$id, "_transcription_cleaning_stats.rds")), compress = "gz")

write_rds(cleanup_results$cleaned_by_task, 
  file.path(opt$output, "features", paste0(opt$id, "_tasks-minimal-features.rds")), compress = "gz")


log_info("  ✓ Transcription cleaned")
log_info("  ✓ {nrow(cleanup_results$clean_tx)} valid responses")
log_info("✓ Stage 3 Complete")
