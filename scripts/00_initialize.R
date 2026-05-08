################################################################################
# Initialize Pipeline
# Load dependencies, reference data, pre-computed embeddings, and archetypes
################################################################################


initialize_pipeline <- function(config, reference_dir, mode = "auto", where) {
  log_info("Initializing pipeline environment...")
  
  required_packages <- c(
    "tidyverse", "data.table", "yaml", "logger","logging",
    "tuneR", "seewave", "signal",
    "foreach", "doMC",
    "lingmatch", "syuzhet", "sentimentr",
    "reticulate",
    "TSP",
    "igraph",
    "geometry",
    "ineq",
    "archetypes",
    "phonics",
    "stringdist"
  )
  
  log_info("  Loading {length(required_packages)} required packages...")
  n_loaded <- 0L
  for (pkg in required_packages) {
    ok <- suppressWarnings(suppressPackageStartupMessages(
      require(pkg, character.only = TRUE, quietly = TRUE)
    ))
    if (!ok) {
      log_warn("  Package '{pkg}' not installed — installing from CRAN...")
      install.packages(pkg, repos = "https://cloud.r-project.org")
      suppressWarnings(suppressPackageStartupMessages(
        library(pkg, character.only = TRUE)
      ))
    }
    n_loaded <- n_loaded + 1L
  }
  log_info("  ✓ {n_loaded}/{length(required_packages)} packages loaded")
  
  # Load utility functions
  log_debug("Loading utility functions...")
  util_dir <- file.path("scripts/utils")
  if (dir.exists(util_dir)) {
    util_files <- list.files(util_dir, pattern = "\\.R$", full.names = TRUE)
    for (util_file in util_files) source(util_file)
    if (length(util_files) > 0)
      log_debug("  ✓ {length(util_files)} utility file(s) sourced from {util_dir}")
  }
  
  # Initialize Python for phonetic embeddings
  log_info("  Initializing Python environment (reticulate/PWE)...")
  py_ok <- tryCatch({
    Sys.setenv(RETICULATE_PYTHON = "/opt/conda/envs/r_pipeline_env/bin/python")
    reticulate::use_python("/opt/conda/envs/r_pipeline_env/bin/python", required = TRUE)
    TRUE
  }, error = function(e) {
    log_warn("  Could not initialize Python for reticulate: {e$message}")
    FALSE
  })
  if (py_ok) log_info("  ✓ Python environment ready")
  
  # Load reference data
  log_info("  Loading reference data...")
  reference_data <- load_reference_data(reference_dir)
  log_info("  ✓ Reference data loaded ({length(reference_data)} dataset(s))")
  
  # Load pre-computed embeddings
  log_info("  Loading pre-computed embeddings...")
  embeddings <- load_precomputed_embeddings(reference_dir)
  log_info("  ✓ Embeddings loaded — semantic: {nrow(embeddings$semantic)} words, phonetic: {nrow(embeddings$phonetic)} words")
  
  # Load pre-computed archetypes
  log_info("  Loading pre-computed archetypes...")
  archetype_refs <- load_precomputed_archetypes(reference_dir)
  log_info("  ✓ Archetypes loaded")
  
  # Set up parallel processing
  n_cores <- min(parallel::detectCores() - 1, 6)
  registerDoMC(cores = n_cores)
  log_info("  ✓ Parallel processing: {n_cores} core(s) registered")
  
  return(list(
    config         = config,
    reference_data = reference_data,
    embeddings     = embeddings,
    archetype_refs = archetype_refs,
    mode           = mode,
    n_cores        = n_cores
  ))
}

################################################################################
# Load Reference Data
################################################################################

load_reference_data <- function(reference_dir) {
  ref_data <- list()
  
  task_meta_file <- file.path(reference_dir, "task_metadata", "ISS_v_1.10_B_task_metadata.csv")
  if (file.exists(task_meta_file)) {
    ref_data$task_metadata <- read_csv(task_meta_file, show_col_types = FALSE)
    log_debug("  ✓ Task metadata loaded: {nrow(ref_data$task_metadata)} tasks")
  }
  
  aoa_file <- file.path(reference_dir, "linguistic", "AoA_51715_words.xlsx")
  if (file.exists(aoa_file)) {
    ref_data$aoa <- readxl::read_xlsx(aoa_file) %>%
      select(Word, `Alternative.spelling`, AoA_Kup_lem) %>%
      pivot_longer(cols = c(1,2), values_to = "word") %>%
      mutate(word = tolower(word),
             AoA_Kup_lem = as.numeric(AoA_Kup_lem)) %>%
      distinct(word, .keep_all = TRUE) %>%
      select(word, AoA_Kup_lem)
    log_debug("  ✓ AoA data loaded: {nrow(ref_data$aoa)} words")
  }
  
  gpt_fam_file <- file.path(reference_dir, "linguistic", "GPT_familiarity.xlsx")
  if (file.exists(gpt_fam_file)) {
    ref_data$gpt_familiarity <- readxl::read_xlsx(gpt_fam_file) %>%
      mutate(Word = tolower(Word),
             GPT_fam = as.numeric(GPT_Fam_dominant)) %>%
      select(word = Word, GPT_fam) %>%
      distinct(word, .keep_all = TRUE)
    log_debug("  ✓ GPT familiarity loaded: {nrow(ref_data$gpt_familiarity)} words")
  }
  
  mrc_file <- file.path(reference_dir, "linguistic", "MRC_database.csv")
  if (file.exists(mrc_file)) {
    ref_data$mrc <- read_csv(mrc_file, show_col_types = FALSE) %>%
      mutate(Word = tolower(Word)) %>%
      select(word = Word,
             number_of_phonemes = `Number of Phonemes`,
             imageability = Imageability) %>%
      distinct(word, .keep_all = TRUE)
    log_debug("  ✓ MRC data loaded: {nrow(ref_data$mrc)} words")
  }
  
  conc_file <- file.path(reference_dir, "linguistic", "concreteness_ratings.xlsx")
  if (file.exists(conc_file)) {
    ref_data$concreteness <- readxl::read_xlsx(conc_file) %>%
      select(word = Word, concreteness = `Conc.M`) %>%
      mutate(word = tolower(word)) %>%
      distinct(word, .keep_all = TRUE)
    log_debug("  ✓ Concreteness data loaded: {nrow(ref_data$concreteness)} words")
  }
  
  return(ref_data)
}

################################################################################
# Load Pre-computed Embeddings
################################################################################

load_precomputed_embeddings <- function(reference_dir) {
  embeddings <- list()
  
  sem_file <- file.path(reference_dir, "embeddings", "semantic_common_50k.rds")
  if (file.exists(sem_file)) {
    embeddings$semantic <- readRDS(sem_file)
    log_debug("  ✓ Semantic embeddings loaded: {nrow(embeddings$semantic)} words")
  } else {
    log_warn("  ! Semantic embeddings file not found. Will compute on-the-fly.")
    embeddings$semantic <- data.frame(text = character(), stringsAsFactors = FALSE)
  }
  
  pwe_file <- file.path(reference_dir, "embeddings", "phonetic_common_50k.rds")
  if (file.exists(pwe_file)) {
    embeddings$phonetic <- readRDS(pwe_file)
    log_debug("  ✓ Phonetic embeddings loaded: {nrow(embeddings$phonetic)} words")
  } else {
    log_warn("  ! Phonetic embeddings file not found. Will compute on-the-fly.")
    embeddings$phonetic <- data.frame(text = character(), stringsAsFactors = FALSE)
  }
  
  return(embeddings)
}

################################################################################
# Load Pre-computed Archetypes
################################################################################

load_precomputed_archetypes <- function(reference_dir) {
  archetype_refs <- list()
  
  sem_arch_file <- file.path(reference_dir, "archetypes", "semantic_common_50k.rds")
  if (file.exists(sem_arch_file)) {
    archetype_refs$semantic <- readRDS(sem_arch_file)
    log_debug("  ✓ Semantic archetypes loaded")
    log_debug("    - Archetype model: {length(archetype_refs$semantic$model$archetypes)} archetypes")
    log_debug("    - Simplex data: {nrow(archetype_refs$semantic$simplex)} words")
    log_debug("    - Word assignments: {nrow(archetype_refs$semantic$assignments)} words")
  } else {
    log_error("  ✗ Semantic archetypes file not found: {sem_arch_file}")
    log_error("    Please run 03_precompute_archetypes.R first")
    stop("Missing semantic archetypes reference file")
  }
  
  pho_arch_file <- file.path(reference_dir, "archetypes", "phonetic_common_50k.rds")
  if (file.exists(pho_arch_file)) {
    archetype_refs$phonetic <- readRDS(pho_arch_file)
    log_debug("  ✓ Phonetic archetypes loaded")
    log_debug("    - Archetype model: {length(archetype_refs$phonetic$model$archetypes)} archetypes")
    log_debug("    - High-variance dimensions: {length(archetype_refs$phonetic$high_var_dims)} dims")
    log_debug("    - Simplex data: {nrow(archetype_refs$phonetic$simplex)} words")
    log_debug("    - Word assignments: {nrow(archetype_refs$phonetic$assignments)} words")
  } else {
    log_error("  ✗ Phonetic archetypes file not found: {pho_arch_file}")
    log_error("    Please run 03_precompute_archetypes.R first")
    stop("Missing phonetic archetypes reference file")
  }
  
  return(archetype_refs)
}
