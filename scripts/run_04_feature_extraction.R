#!/usr/bin/env Rscript

required_packages <- c("optparse", "yaml", "logger", "tidyverse", "logging", "phonics")
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
  make_option(c("--id"),                 type = "character", default = NULL),
  make_option(c("--transcription_file"), type = "character", default = NULL),
  make_option(c("--config"),             type = "character", default = NULL),
  make_option(c("--reference"),          type = "character", default = NULL),
  make_option(c("--output"),             type = "character", default = NULL)
)

opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

log_threshold(INFO)
log_appender(appender_console)
system(paste0("mkdir -p ", file.path(opt$output, "features")))

source(file.path(script_dir, "00_initialize.R"))
source(file.path(script_dir, "04_feature_extraction.R"))

config <- yaml::read_yaml(opt$config)

log_info("STAGE 4: Feature Extraction")
log_info(strrep("-", 80))
log_info("  Participant       : {opt$id}")
log_info("  Transcription file: {opt$transcription_file}")
log_info("  Reference dir     : {opt$reference}")
log_info("  Output dir        : {opt$output}")

# Validate inputs
if (!file.exists(opt$transcription_file))
  stop(sprintf("Cleaned transcription not found: %s", opt$transcription_file), call. = FALSE)

clean_tx_stats_path <- file.path(opt$output, "features",
                                   paste0(opt$id, "_transcription_cleaning_stats.rds"))
task_feats_path     <- file.path(opt$output, "features",
                                   paste0(opt$id, "_tasks-minimal-features.rds"))

if (!file.exists(clean_tx_stats_path))
  stop(sprintf("Cleaning stats not found: %s", clean_tx_stats_path), call. = FALSE)
if (!file.exists(task_feats_path))
  stop(sprintf("Task features not found: %s", task_feats_path), call. = FALSE)

# Initialize pipeline
log_info("  Initializing pipeline environment (embeddings + archetypes)...")
init_start <- proc.time()[[3]]
pipeline_env <- initialize_pipeline(
  config        = config,
  reference_dir = opt$reference,
  mode          = "auto",
  where         = dirname(script_dir)
)
log_info("  ✓ Pipeline environment ready [{round(proc.time()[[3]] - init_start, 1)}s]")
log_info("    - Semantic embeddings : {nrow(pipeline_env$embeddings$semantic)} words")
log_info("    - Phonetic embeddings : {nrow(pipeline_env$embeddings$phonetic)} words")
log_info("    - Parallel cores      : {pipeline_env$n_cores}")

# Load data
clean_tx <- read_tsv(opt$transcription_file, show_col_types = FALSE)
log_info("  Cleaned transcription : {nrow(clean_tx)} rows, {ncol(clean_tx)} columns")

clean_tx_stats   <- read_rds(clean_tx_stats_path)
clean_tx_by_task <- read_rds(task_feats_path)
log_info("  Task features loaded  : {length(clean_tx_by_task)} task(s): {paste(names(clean_tx_by_task), collapse=', ')}")
log_info("  Cleaning stats loaded : {clean_tx_stats_path}")

# Extract features
log_info("  Running feature extraction...")
stage_start <- proc.time()[[3]]

features <- extract_all_features_iss(
  cleaned_transcription = clean_tx_by_task,
  participant_id        = opt$id,
  embeddings            = pipeline_env$embeddings,
  archetype_refs        = pipeline_env$archetype_refs,
  reference_data        = pipeline_env$reference_data,
  config                = config,
  cleaned_full          = clean_tx
)

elapsed <- round(proc.time()[[3]] - stage_start, 1)
log_info("  ✓ Feature extraction complete [{elapsed}s]")

# Report feature dimensions
if (!is.null(features$per_prompt)) {
  n_prompts <- if (is.data.frame(features$per_prompt)) nrow(features$per_prompt) else
    sum(sapply(features$per_prompt, function(x) if (is.data.frame(x)) nrow(x) else 0))
  log_info("    - per_prompt rows    : {n_prompts}")
}
if (!is.null(features$per_task) && is.data.frame(features$per_task))
  log_info("    - per_task rows      : {nrow(features$per_task)}")
if (!is.null(features$per_participant) && is.data.frame(features$per_participant))
  log_info("    - per_participant rows: {nrow(features$per_participant)}")

# Save
dir.create(opt$output, showWarnings = FALSE, recursive = TRUE)

out_pp  <- file.path(opt$output, paste0(opt$id, "_per_prompt.rds"))
out_pt  <- file.path(opt$output, paste0(opt$id, "_per_task.rds"))
out_par <- file.path(opt$output, paste0(opt$id, "_per_participant.rds"))
out_all <- file.path(opt$output, paste0(opt$id, "_all_features.rds"))

write_rds(features$per_prompt,      out_pp,  compress = "gz")
write_rds(features$per_task,        out_pt,  compress = "gz")
write_rds(features$per_participant, out_par, compress = "gz")
write_rds(features$all_features,    out_all, compress = "gz")

log_info("  ✓ per_prompt saved      : {out_pp}")
log_info("  ✓ per_task saved        : {out_pt}")
log_info("  ✓ per_participant saved : {out_par}")
log_info("  ✓ all_features saved    : {out_all}")

log_info("✓ Stage 4 Complete [{elapsed}s]")
