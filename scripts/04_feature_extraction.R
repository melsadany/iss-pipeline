#!/usr/bin/env Rscript
# 04_feature_extraction.R
# ISS Task Feature Extraction
# Adapted from PS-VC pipeline

suppressPackageStartupMessages({
  library(tidyverse)
  library(yaml)
  library(lingmatch)
  library(syuzhet)
  library(sentimentr)
  library(geometry)
  library(TSP)
  library(doMC)
  library(ineq)
  library(phonics)
  library(logging)
})

################################################################################
# Main Feature Extraction Function
################################################################################

extract_all_features_iss <- function(cleaned_transcription,
                                     participant_id,
                                     cleaned_full,
                                     embeddings,
                                     archetype_refs,
                                     reference_data,
                                     config) {
  
  loginfo("Extracting ISS features for %s", participant_id)
  
  # Extract features for WORD_ASSOC (main fluency task)
  if (!is.null(cleaned_transcription$word_assoc)) {
    tx_word_assoc <- cleaned_transcription$word_assoc %>%
      dplyr::select(-c(audio_file, trial, prompt_text,contains("constraint")))%>%
      mutate(task_type = "WORD_ASSOC")
    
    loginfo("  Extracting WORD_ASSOC features...")
    word_assoc_features <- extract_word_assoc_features(
      tx_word_assoc, embeddings, archetype_refs, reference_data, config
    )
  } else {
    word_assoc_features <- NULL
  }
  
  # Extract simple performance features for other tasks
  loginfo("  Extracting attention task features...")
  attention_features <- extract_attention_features(
    cleaned_transcription$checkbox,
    cleaned_transcription$hi,
    participant_id
  )
  
  loginfo("  Extracting memory features...")
  memory_features <- extract_memory_features(
    cleaned_transcription$wmemory,
    participant_id
  )
  
  loginfo("  Extracting sentence repetition features...")
  sent_rep_features <- extract_sent_rep_features(
    cleaned_transcription$sent_rep,
    participant_id
  )
  
  loginfo("  Extracting reading features...")
  reading_features <- extract_reading_features(
    cleaned_transcription$reading,
    participant_id
  )
  
  # Combine all features
  all_features <- list(
    word_assoc = word_assoc_features,
    attention = list(per_prompt = list(checkbox = cleaned_transcription$checkbox,
                                       hi = cleaned_transcription$hi),
                     summarized = attention_features),
    memory = list(per_prompt = cleaned_transcription$wmemory, 
                  summarized = memory_features),
    sent_rep = list(per_prompt = cleaned_transcription$sent_rep,
                    summarized = sent_rep_features),
    reading = list(per_prompt = cleaned_transcription$reading,
                   summarized = reading_features)
  )
  
  per_prompt <- list(checkbox=cleaned_transcription$checkbox, 
                     hi=cleaned_transcription$hi, 
                     wmemory=cleaned_transcription$wmemory,
                     reading=cleaned_transcription$reading,
                     sent_rep=cleaned_transcription$sent_rep,
                     word_assoc=word_assoc_features$per_prompt)
  per_task <- list(checkbox_and_hi=attention_features, 
                   wmemory = memory_features,
                   reading = reading_features,
                   sent_rep = sent_rep_features,
                   word_assoc = word_assoc_features$per_task)
  per_participant <- list(checkbox_and_hi=attention_features, 
                          wmemory = memory_features,
                          reading = reading_features,
                          sent_rep = sent_rep_features,
                          word_assoc = word_assoc_features$per_participant)
  
  
  loginfo("✓ Feature extraction complete")
  
  return(list(input = cleaned_transcription,
              all_features_start = all_features,
              per_prompt = per_prompt,
              per_task = per_task,
              per_participant = per_participant))
}

################################################################################
# Extract WORD_ASSOC Features (comprehensive - like PS-VC)
################################################################################

extract_word_assoc_features <- function(tx, embeddings, archetype_refs, 
                                        reference_data, config) {
  
  # 1. GENERAL FEATURES
  general_features <- extract_general_features(tx, reference_data)
  
  # 2. TEMPORAL FEATURES
  temporal_features <- extract_temporal_features(tx, reference_data)
  
  # 3. SEMANTIC EMBEDDING FEATURES
  # Check for novel words
  unique_words <- unique(c(tx$prompt, tx$response))
  novel_semantic_words <- setdiff(unique_words, embeddings$semantic$word)
  embeddings$semantic <- embeddings$semantic %>% dplyr::rename(text=word)
  
  if (length(novel_semantic_words) > 0) {
    logdebug("  Extracting semantic embeddings for %d novel words", 
             length(novel_semantic_words))
    new_sem_emb <- extract_semantic_embeddings(novel_semantic_words) %>%
      rename_at(.vars = vars(-c(text)), .funs = function(x)paste0("X",x))
    embeddings$semantic <- rbind(embeddings$semantic, new_sem_emb)
  }
  
  semantic_features <- extract_embedding_features(
    tx, embeddings$semantic, archetype_refs$semantic,
    prefix = "sem", embedding_type = "semantic", config
  )
  
  # Anchor similarity
  anchors_features <- calculate_anchor_set_similarity(
    tx, 
    embeddings$semantic %>% as.data.frame() %>% column_to_rownames("text"),
    prefix = "sem", 
    config
  )
  
  # 4. PHONETIC EMBEDDING FEATURES
  novel_phonetic_words <- setdiff(unique_words, embeddings$phonetic$text)
  
  if (length(novel_phonetic_words) > 0) {
    logdebug("  Extracting phonetic embeddings for %d novel words",
             length(novel_phonetic_words))
    new_pwe_emb <- extract_phonetic_embeddings(novel_phonetic_words)
    embeddings$phonetic <- rbind(embeddings$phonetic, 
                                 new_pwe_emb %>% rename_at(.vars = vars(-text),.funs = function(x) paste0("Dim",as.numeric(x)+1)))%>%
      distinct(text,.keep_all = T)
  }
  
  phonetic_features <- extract_embedding_features(
    tx, embeddings$phonetic, archetype_refs$phonetic,
    prefix = "pho", embedding_type = "phonetic", config
  )
  
  # COMBINE PER-PROMPT
  per_prompt <- general_features$per_prompt %>%
    full_join(temporal_features$per_prompt) %>%
    full_join(semantic_features$per_prompt) %>%
    full_join(anchors_features) %>%
    full_join(phonetic_features$per_prompt)
  
  # COMBINE PER-TASK
  per_task <- general_features$per_task %>%
    full_join(temporal_features$per_task) %>%
    full_join(semantic_features$per_task) %>%
    full_join(phonetic_features$per_task)
  
  # COMBINE PER-PARTICIPANT
  per_participant <- general_features$per_participant %>%
    full_join(temporal_features$per_participant) %>%
    full_join(semantic_features$per_participant) %>%
    full_join(phonetic_features$per_participant)
  
  return(list(
    per_prompt = per_prompt,
    per_task = per_task,
    per_participant = per_participant
  ))
}

################################################################################
# Extract General Features
################################################################################

extract_general_features <- function(tx_split, reference_data) {
  
  # Lingmatch features
  lingmatch_feat <- cbind(
    response = unique(tx_split$response),
    lingmatch::lma_meta(unique(tx_split$response)),
    lingmatch::lma_termcat(unique(tx_split$response))
  ) %>%
    mutate_at(vars(25:33), ~ ifelse(. > 0, 1, 0)) %>%
    mutate(sentence_response = ifelse(words > 1, 1, 0)) %>%
    select(response, characters_per_word, syllables_per_word,
           sentence_response, reading_grade, 25:33)
  
  # Sentiment features
  sent_feat <- cbind(
    response = unique(tx_split$response),
    syuzhet::get_nrc_sentiment(unique(tx_split$response)),
    sentimentr::profanity(unique(tx_split$response)) %>%
      select(count_profanity = profanity_count)
  ) %>%
    mutate_at(vars(-response), ~ as.numeric(. > 0))
  
  # Join with reference data
  tx_with_refs <- tx_split %>%
    left_join(reference_data$aoa, by = c("response" = "word")) %>%
    left_join(reference_data$gpt_familiarity, by = c("response" = "word")) %>%
    left_join(reference_data$mrc, by = c("response" = "word")) %>%
    left_join(reference_data$concreteness, by = c("response" = "word")) %>%
    left_join(lingmatch_feat, by = "response") %>%
    left_join(sent_feat, by = "response")
  
  # Aggregate per prompt
  per_prompt <- tx_with_refs %>%
    group_by(participant_id, prompt) %>%
    summarise(
      unique_word_count = n_distinct(response),
      mean_AoA = mean(AoA_Kup_lem, na.rm = TRUE),
      sd_AoA = sd(AoA_Kup_lem, na.rm = TRUE),
      mean_GPT_fam = mean(GPT_fam, na.rm = TRUE),
      min_GPT_fam = min(GPT_fam, na.rm = TRUE),
      sd_GPT_fam = sd(GPT_fam, na.rm = TRUE),
      mean_concreteness = mean(concreteness, na.rm = TRUE),
      mean_imageability = mean(imageability, na.rm = TRUE),
      mean_phonemes = mean(number_of_phonemes, na.rm = TRUE),
      mean_characters_per_word = mean(characters_per_word, na.rm = TRUE),
      max_characters_per_word = max(characters_per_word, na.rm = TRUE),
      mean_syllables_per_word = mean(syllables_per_word, na.rm = TRUE),
      max_syllables_per_word = max(syllables_per_word, na.rm = TRUE),
      mean_reading_grade = mean(reading_grade, na.rm = TRUE),
      max_reading_grade = max(reading_grade, na.rm = TRUE),
      sentence_response = mean(sentence_response, na.rm = TRUE),
      across(starts_with("count_"), sum, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Aggregate per task
  per_task <- per_prompt %>% 
    left_join(tx_split %>% select(prompt,type) %>% distinct()) %>%
    dplyr::select(-prompt) %>% ungroup() %>% 
    group_by(participant_id,type) %>%
    summarise_all(.funs = function(x) mean(x, na.rm=T))
  
  per_participant <- per_task %>% dplyr::select(-type) %>% ungroup() %>%
    group_by(participant_id) %>%
    summarise_all(.funs = function(x) mean(x, na.rm=T))
  
  
  return(list(
    per_prompt = per_prompt,
    per_task = per_task,
    per_participant=per_participant
  ))
}

################################################################################
# NEW FUNCTION: Extract Bins Statistics
################################################################################

extractBinsStatistics <- function(tx) {
  require(doMC)
  require(ineq)
  
  registerDoMC(6)
  
  bins_stats <- foreach(id = 1:length(unique(tx$participant_id)), .combine = rbind) %dopar% {
    id_n <- unique(tx$participant_id)[id]
    
    p_res <- foreach(p = 1:length(unique(tx$prompt[tx$participant_id == id_n])), .combine = rbind) %dopar% {
      p_n <- unique(tx$prompt[tx$participant_id == id_n])[p]
      df <- tx %>% dplyr::filter(participant_id == id_n, prompt == p_n)
      
      task_duration <- 15
      
      if (nrow(df) > 1) {
        # First/last half ratio
        first_last_ratio <- sum(df$start < task_duration/2) / 
          max(sum(df$start >= task_duration/2), 1)
        
        # K-S statistic for distribution
        ks <- tryCatch({
          as.numeric(ks.test(df$start, "punif", 0, task_duration, 
                             alternative = "two.sided")$statistic)
        }, error = function(e) NA_real_)
        
        # Gini index
        gini_index <- tryCatch({
          ineq::Gini(df$start)
        }, error = function(e) NA_real_)
        
        # Bins slope
        if (nrow(df) > 2) {
          bins <- seq(0, task_duration, length.out = 7)  # 6 bins
          df_binned <- df %>%
            mutate(bin = cut(start, breaks = bins, labels = FALSE, include.lowest = TRUE)) %>%
            mutate(bin = case_when(
              start >= task_duration ~ 6,
              is.na(bin) ~ 1,
              TRUE ~ bin
            ))
          
          bin_counts <- df_binned %>%
            group_by(bin) %>%
            summarise(counts = n(), .groups = "drop")
          
          bins_slope <- tryCatch({
            summary(lm(counts ~ bin, data = bin_counts))$coef[2, 1]
          }, error = function(e) NA_real_)
        } else {
          bins_slope <- NA_real_
        }
        
        data.frame(
          participant_id = id_n,
          prompt = p_n,
          halfs_ratio = first_last_ratio,
          ks_statistic = ks,
          Gini_index = gini_index,
          bins_slope = bins_slope
        )
      } else {
        return(NULL)
      }
    }
    
    return(p_res)
  }
  
  return(bins_stats)
}

################################################################################
# NEW FUNCTION: Extract Word Productivity Slope
################################################################################

extractProductivitySlope <- function(tx) {
  
  # Count words per prompt
  word_count <- tx %>%
    distinct(participant_id, prompt, response) %>%
    group_by(participant_id, prompt) %>%
    summarise(count = n(), .groups = "drop")
  
  # Calculate productivity slopes
  productivity <- word_count %>%
    group_by(participant_id) %>%
    arrange(prompt) %>%
    mutate(prompt_num = row_number()) %>%
    summarise(
      productivity_slope = {
        if (n() > 1) {
          tryCatch({
            summary(lm(count ~ prompt_num))$coef[2, 1]
          }, error = function(e) NA_real_)
        } else {
          NA_real_
        }
      },
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = task_type,
      values_from = productivity_slope,
      names_prefix = "productivity_slope_"
    )
  
  return(productivity)
}

################################################################################
# NEW FUNCTION: Extract Last Response Gap
################################################################################

extractLastResponseGap <- function(tx) {
  
  last_response <- tx %>%
    dplyr::filter(task_type == "WORD_ASSOC") %>%
    group_by(participant_id, prompt) %>%
    slice_max(order_by = end, n = 1, with_ties = FALSE) %>%
    mutate(
      last_response_gap = end - 15  # Can be negative
    ) %>%
    select(participant_id, prompt, last_response_gap)
  
  return(last_response)
}

################################################################################
# Extract Temporal Features - UPDATED WITH NEW FUNCTIONS
################################################################################

extract_temporal_features <- function(tx, reference_data) {
  
  tx_temporal <- tx
  
  # Onset (response latency)
  onset <- tx_temporal %>%
    group_by(participant_id, prompt) %>%
    slice_min(order_by = start, n = 1, with_ties = FALSE) %>%
    select(participant_id, prompt, onset = start)
  
  # Thinking time (inter-word intervals)
  thinking_time <- tx_temporal %>%
    group_by(participant_id, prompt) %>%
    dplyr::filter(n() > 1) %>%
    arrange(participant_id, prompt, start) %>%
    mutate(
      thinking_time_e = start - lag(end),
      thinking_time_s = start - lag(start),
      thinking_time = ifelse(thinking_time_e < 0, thinking_time_s, thinking_time_e)
    ) %>%
    dplyr::filter(!is.na(thinking_time)) %>%
    summarise(
      thinking_time_mean = mean(thinking_time, na.rm = TRUE),
      thinking_time_sd = sd(thinking_time, na.rm = TRUE),
      thinking_time_min = min(thinking_time, na.rm = TRUE),
      thinking_time_max = max(thinking_time, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Burst coefficient
  burst <- tx_temporal %>%
    group_by(participant_id, prompt) %>%
    dplyr::filter(n() > 2) %>%
    arrange(start) %>%
    mutate(thinking_time = start - lag(start)) %>%
    dplyr::filter(!is.na(thinking_time)) %>%
    summarise(
      burst_coefficient = (sd(thinking_time, na.rm = TRUE) - mean(thinking_time, na.rm = TRUE))/
        (sd(thinking_time, na.rm = TRUE) + mean(thinking_time, na.rm = TRUE)),
      .groups = "drop"
    )
  
  # Insight patterns
  insight <- tx_temporal %>%
    group_by(participant_id, prompt) %>%
    dplyr::filter(n() > 3) %>%
    arrange(start) %>%
    summarise(
      insight_pattern_results = list(analyze_insight_patterns(start, response)),
      .groups = "drop"
    ) %>%
    mutate(
      insight_pattern_density = map_dbl(insight_pattern_results, ~ .[[1]]),
      insight_pattern_count = map_dbl(insight_pattern_results, ~ .[[2]]),
      pattern_regularity = map_dbl(insight_pattern_results, ~ .[[3]])
    ) %>%
    select(-insight_pattern_results)
  
  # NEW: Bins statistics
  bins_stats <- extractBinsStatistics(tx_temporal)
  
  # NEW: Last response gap
  last_response_gap <- extractLastResponseGap(tx_temporal)
  
  # Combine per-prompt
  per_prompt <- onset %>%
    full_join(thinking_time, by = c("participant_id", "prompt")) %>%
    full_join(burst, by = c("participant_id", "prompt")) %>%
    full_join(insight, by = c("participant_id", "prompt")) %>%
    full_join(bins_stats, by = c("participant_id", "prompt")) %>%
    full_join(last_response_gap, by = c("participant_id", "prompt"))
  
  # Aggregate per task
  per_task <- per_prompt %>% 
    left_join(tx_temporal %>% distinct(prompt,type)) %>%
    dplyr::select(-prompt) %>% ungroup() %>%
    group_by(participant_id, type) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")
  
  # Aggregate per participant
  per_participant <- per_task %>% select(-type) %>% ungroup() %>%
    group_by(participant_id) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")
  
  return(list(
    per_prompt = per_prompt,
    per_task = per_task,
    per_participant = per_participant
  ))
}

################################################################################
# Extract Semantic Embeddings using sentence-transformers
################################################################################
extract_semantic_embeddings <- function(words) {
  log_debug("Extracting semantic embeddings for {length(words)} words...")
  
  # Create temp files
  temp_words_file <- tempfile(fileext = ".csv")
  temp_output_file <- tempfile(fileext = ".csv")
  
  # Write words to CSV
  write_csv(data.frame(word = words), temp_words_file)
  
  # Call semantic embeddings wrapper (runs in r_pipeline_env)
  cmd <- paste(
    "/opt/conda/envs/r_pipeline_env/bin/python",
    "/app/scripts/semantic_embeddings_wrapper.py",
    "--input", temp_words_file,
    "--output", temp_output_file,
    "--model", "all-MiniLM-L6-v2",  # 384-dim embeddings, fast
    "--verbose"
  )
  
  result <- system(cmd)
  
  if (result != 0 || !file.exists(temp_output_file)) {
    log_error("Semantic embedding extraction failed")
    return(NULL)
  }
  
  # Read embeddings
  embeddings <- read_csv(temp_output_file, show_col_types = FALSE)
  
  # Cleanup
  unlink(c(temp_words_file, temp_output_file))
  
  # Rename 'word' to 'text' for consistency with pre-computed embeddings
  embeddings <- embeddings %>% rename(text = word)
  
  return(embeddings)
}

################################################################################
# Extract Phonetic Embeddings using PWESuite
################################################################################
extract_phonetic_embeddings <- function(words) {
  log_debug("Extracting phonetic embeddings for {length(words)} words...")
  
  # Create temp files
  temp_words_file <- tempfile(fileext = ".csv")
  temp_output_file <- tempfile(fileext = ".csv")
  
  # Write words to CSV
  write_csv(data.frame(word = words), temp_words_file)
  
  # Call PWE wrapper (runs in pwesuite_env)
  cmd <- paste(
    "/opt/conda/bin/conda run --no-capture-output -n pwesuite_env",
    "python /app/scripts/pwe_wrapper.py",
    "--input", temp_words_file,
    "--output", temp_output_file,
    "--model", "/app/reference_data/models/rnn_metric_learning_token_ort_all.pt",
    "--verbose"
  )
  
  result <- system(cmd)
  
  if (result != 0 || !file.exists(temp_output_file)) {
    log_error("PWE extraction failed")
    return(NULL)
  }
  
  # Read embeddings
  embeddings <- read_csv(temp_output_file, show_col_types = FALSE)
  
  # Cleanup
  unlink(c(temp_words_file, temp_output_file))
  
  # Rename 'word' to 'text' for consistency
  embeddings <- embeddings %>% rename(text = word)
  
  return(embeddings)
}

################################################################################
# Extract Embedding-Based Features (works for BOTH semantic and phonetic)
################################################################################

extract_embedding_features <- function(tx_split, embeddings, archetype_ref,
                                       prefix = "sem", embedding_type = "semantic",
                                       config = NULL) {
  
  # Prepare embedding matrix for calculations
  emb_matrix <- embeddings %>% as.data.frame() %>%
    dplyr::filter(text %in% c(tx_split$prompt, tx_split$response)) %>%
    column_to_rownames("text") %>%
    as.matrix()
  
  # Similarity to prompts
  sim_to_prompt <- tx_split %>%
    distinct(participant_id, prompt, response) %>%
    rowwise() %>%
    mutate(
      cosine_sim = cosine_similarity(
        as.numeric(emb_matrix[prompt, ]),
        as.numeric(emb_matrix[response, ])
      )
    ) %>%
    group_by(participant_id, prompt) %>%
    summarise(!!paste0(prefix, "_sim_to_prompt") := mean(cosine_sim, na.rm = TRUE),
              .groups = "drop")
  
  # Pairwise similarity (consecutive and all pairs)
  pairwise_sim <- calculate_pairwise_similarity(tx_split, emb_matrix, prefix)
  
  # Trajectory features (divergence, path length)
  trajectory_features <- calculate_trajectory_features(tx_split, emb_matrix, prefix)
  
  # Vocabulary volume (convex hull in embedding space)
  # vocab_volume <- calculate_vocabulary_volume(tx_split, emb_matrix, prefix)
  
  # Archetypal features (with novel word handling)
  archetypal_result <- calculate_archetypal_features(
    tx_split, embeddings, archetype_ref, prefix, embedding_type
  )
  
  archetypal_features <- archetypal_result$per_prompt
  arch_data <- archetypal_result$arch_data  # Get arch_data for other functions
  
  # Archetypal area (using arch_data from previous step)
  archetypal_area <- calculate_archetypal_area(tx_split, arch_data, prefix)
  
  # Archetypal richness (using arch_data from previous step)
  archetypal_richness <- calculate_archetypal_richness(tx_split, arch_data, prefix)
  
  # Norms (distance from origin)
  norms <- calculate_norms(tx_split, embeddings %>%
                             dplyr::filter(text %in% c("a", "the", tx_split$response)) %>%
                             column_to_rownames("text") %>%
                             as.matrix(), prefix)
  
  # Prompt similarity trajectory
  prompt_sim_trajectory <- calculate_prompt_similarity_trajectory(tx_split, emb_matrix, prefix)
  
  
  # Combine per-prompt
  per_prompt <- sim_to_prompt %>%
    full_join(pairwise_sim$per_prompt, by = c("participant_id", "prompt")) %>%
    full_join(trajectory_features$per_prompt, by = c("participant_id", "prompt")) %>%
    # full_join(vocab_volume$per_prompt, by = c("participant_id", "prompt")) %>%
    full_join(archetypal_features, by = c("participant_id", "prompt")) %>%
    full_join(archetypal_area$per_prompt, by = c("participant_id", "prompt")) %>%
    full_join(archetypal_richness$per_prompt, by = c("participant_id", "prompt")) %>%
    full_join(norms$per_prompt, by = c("participant_id", "prompt")) %>%
    full_join(prompt_sim_trajectory, by = c("participant_id", "prompt"))
  
  # Aggregate per task
  per_task <- per_prompt %>% left_join(tx_split %>% distinct(prompt, type)) %>%
    select(-prompt) %>%
    group_by(participant_id, type) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")
  
  # Aggregate per participant
  per_participant <- per_prompt %>% dplyr::select(-colnames(norms$per_prompt)[-c(1:2)],-contains(c("archetypal_area", "archetypal_richness"))) %>%
    group_by(participant_id) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>%
    # full_join(vocab_volume$per_participant, by = "participant_id") %>%
    full_join(archetypal_area$per_participant, by = "participant_id") %>%
    full_join(archetypal_richness$per_participant, by = "participant_id") %>%
    full_join(norms$per_participant, by = "participant_id")
  
  return(list(
    per_prompt = per_prompt,
    per_task = per_task,
    per_participant = per_participant
  ))
}

################################################################################
# Get Archetype Assignments for Words (with novel word handling)
################################################################################

get_archetype_assignments <- function(words, embeddings, archetype_ref,
                                      embedding_type = "semantic") {
  
  # Check which words are already in archetype references
  known_words <- words[words %in% archetype_ref$assignments$text]
  novel_words <- words[!words %in% archetype_ref$assignments$text]
  
  # Get assignments for known words
  known_assignments <- archetype_ref$assignments %>%
    dplyr::filter(text %in% known_words)
  
  # Predict assignments for novel words if any
  if (length(novel_words) > 0) {
    log_debug("      Predicting archetypes for {length(novel_words)} novel words...")
    
    # Get embeddings for novel words
    novel_emb <- embeddings %>%
      dplyr::filter(text %in% novel_words)
    
    if (nrow(novel_emb) > 0) {
      # Extract embedding matrix
      novel_emb_matrix <- novel_emb %>%
        select(-text) %>%
        as.matrix()
      
      # For phonetic: filter to same dimensions used in archetype model
      if (embedding_type == "phonetic" && !is.null(archetype_ref$high_var_dims)) {
        high_var_dims <- archetype_ref$high_var_dims
        novel_emb_matrix <- novel_emb_matrix[, high_var_dims, drop = FALSE]
      }
      
      # Load archetype model
      arch <- archetype_ref$model
      
      # Predict archetype coefficients for novel words
      novel_arch_coef <- tryCatch({
        predict(arch, novel_emb_matrix)
      }, error = function(e) {
        log_warn("      Failed to predict archetypes: {e$message}")
        return(NULL)
      })
      
      if (!is.null(novel_arch_coef)) {
        # Convert to weights data frame
        novel_arch_weights <- as.data.frame(novel_arch_coef)
        colnames(novel_arch_weights) <- paste0("A", 1:ncol(novel_arch_weights))
        novel_arch_weights$text <- novel_emb$text
        
        # Assign to dominant archetype
        novel_assignments <- novel_arch_weights %>%
          pivot_longer(cols = starts_with("A"), names_to = "archetype", values_to = "weight") %>%
          group_by(text) %>%
          slice_max(order_by = weight, n = 1, with_ties = FALSE) %>%
          select(text, archetype, weight)
        
        # Combine known and novel assignments
        all_assignments <- bind_rows(known_assignments, novel_assignments)
        
        # Also return weights for richness calculation
        all_weights <- bind_rows(
          archetype_ref$weights %>% dplyr::filter(text %in% known_words),
          novel_arch_weights
        )
        
        # Predict simplex coordinates for novel words (for area calculation)
        novel_simplex <- predict_simplex_coords(novel_arch_coef, arch, novel_emb$text)
        all_simplex <- bind_rows(
          archetype_ref$simplex %>% rename(text=word) %>% dplyr::filter(text %in% known_words),
          novel_simplex
        )
      } else {
        # Prediction failed
        all_assignments <- known_assignments
        all_weights <- archetype_ref$weights %>% dplyr::filter(text %in% known_words)
        all_simplex <- archetype_ref$simplex %>% dplyr::filter(text %in% known_words)
      }
    } else {
      # Novel words not in embeddings either
      all_assignments <- known_assignments
      all_weights <- archetype_ref$weights %>% dplyr::filter(text %in% known_words)
      all_simplex <- archetype_ref$simplex %>% dplyr::filter(text %in% known_words)
    }
  } else {
    # All words are known
    all_assignments <- known_assignments
    all_weights <- archetype_ref$weights %>% dplyr::filter(text %in% known_words)
    all_simplex <- archetype_ref$simplex %>% dplyr::filter(text %in% known_words)
  }
  
  return(list(
    assignments = all_assignments,
    weights = all_weights,
    simplex = all_simplex
  ))
}

################################################################################
# Helper: Predict Simplex Coordinates for Novel Words
################################################################################

predict_simplex_coords <- function(arch_coef, arch_model, word_labels) {
  # Get the simplex projection (2D coordinates from archetype weights)
  # Use first 2 archetype dimensions for 2D projection
  k <- ncol(arch_coef)
  
  if (k >= 2) {
    # Simple approach: use first 2 dimensions
    simplex_coords <- data.frame(
      text = word_labels,
      x = arch_coef[, 1],
      y = arch_coef[, 2]
    )
  } else {
    # Only 1 archetype dimension
    simplex_coords <- data.frame(
      text = word_labels,
      x = arch_coef[, 1],
      y = 0
    )
  }
  
  return(simplex_coords)
}

################################################################################
# Calculate Archetypal Features - UPDATED with novel word handling
################################################################################

calculate_archetypal_features <- function(tx_split, embeddings, archetype_ref,
                                          prefix = "sem", embedding_type = "semantic") {
  
  # Get all words (prompts + responses)
  all_words <- unique(c(tx_split$prompt, tx_split$response))
  
  # Get archetype assignments (handling novel words)
  arch_data <- get_archetype_assignments(
    words = all_words,
    embeddings = embeddings,
    archetype_ref = archetype_ref,
    embedding_type = embedding_type
  )
  
  # Join with transcription data
  tx_with_arch <- tx_split %>%
    left_join(arch_data$assignments, by = c("response" = "text"))
  
  # Calculate metrics per prompt
  archetypal_valid_data <- tx_with_arch %>%
    dplyr::filter(!is.na(archetype))
  archetypal_lifetimes <- archetypal_valid_data %>%
    group_by(participant_id,prompt,archetype) %>%
    summarise(lifetime = sum(end - start, na.rm = TRUE), .groups = "drop") %>%
    group_by(participant_id,prompt) %>% summarise(!!paste0(prefix, "_community_lifetime") := mean(lifetime), .groups = "drop")
  archetypal_switches <- archetypal_valid_data %>%
    group_by(participant_id,prompt) %>%
    summarise(!!paste0(prefix, "_community_switches") := sum(archetype != lag(archetype), na.rm = TRUE))
  
  archetypal_count <- archetypal_valid_data %>%
    group_by(participant_id, prompt) %>%
    summarise(
      # Number of unique archetypes visited
      !!paste0(prefix, "_communities") := length(unique(archetype[!is.na(archetype)])),
      .groups = "drop"
    )
  archetypal <- full_join(archetypal_count, archetypal_lifetimes) %>% full_join(archetypal_switches)
  
  return(list(
    per_prompt = archetypal,
    arch_data = arch_data  # Return for use in other functions
  ))
}

################################################################################
# Calculate Archetypal Richness - UPDATED with novel word handling
################################################################################

calculate_archetypal_richness <- function(tx_split, arch_data, prefix = "sem") {
  
  # Per prompt richness
  per_prompt_rich <- tx_split %>%
    distinct(participant_id, prompt, response) %>%
    left_join(arch_data$weights, by = c("response" = "text")) %>%
    group_by(participant_id, prompt) %>%
    summarise(
      !!paste0(prefix, "_archetypal_richness") := {
        # Sum of max weights across all archetypes for words in this prompt
        weights_subset <- cur_data() %>%
          select(starts_with("A"))
        if (ncol(weights_subset) > 0 && nrow(weights_subset) > 0) {
          sum(apply(weights_subset, 2, max, na.rm = TRUE), na.rm = TRUE)
        } else {
          NA_real_
        }
      },
      .groups = "drop"
    )
  
  # Per participant richness
  per_participant_rich <- tx_split %>%
    distinct(participant_id, response) %>%
    left_join(arch_data$weights, by = c("response" = "text")) %>%
    group_by(participant_id) %>%
    summarise(
      !!paste0(prefix, "_archetypal_richness") := {
        weights_subset <- cur_data() %>%
          select(starts_with("A"))
        if (ncol(weights_subset) > 0 && nrow(weights_subset) > 0) {
          sum(apply(weights_subset, 2, max, na.rm = TRUE), na.rm = TRUE)
        } else {
          NA_real_
        }
      },
      .groups = "drop"
    )
  
  return(list(
    per_prompt = per_prompt_rich,
    per_participant = per_participant_rich
  ))
}

################################################################################
# Calculate Archetypal Area - UPDATED with novel word handling
################################################################################

calculate_archetypal_area <- function(tx_split, arch_data, prefix = "sem") {
  require(geometry)
  
  # Per prompt area
  per_prompt_area <- tx_split %>%
    distinct(participant_id, prompt, response) %>%
    group_by(participant_id, prompt) %>%
    summarise(
      !!paste0(prefix, "_archetypal_area") := {
        words_in_prompt <- unique(response)
        if (length(words_in_prompt) >= 3) {
          # Get simplex coordinates for these words
          coords <- arch_data$simplex %>%
            dplyr::filter(text %in% words_in_prompt) %>%
            select(x, y)
          
          if (nrow(coords) >= 3) {
            tryCatch({
              convhulln(as.matrix(coords), output.options = TRUE)$vol
            }, error = function(e) NA_real_)
          } else {
            NA_real_
          }
        } else {
          NA_real_
        }
      },
      .groups = "drop"
    )
  
  # Per participant area
  per_participant_area <- tx_split %>%
    distinct(participant_id, response) %>%
    group_by(participant_id) %>%
    summarise(
      !!paste0(prefix, "_archetypal_area") := {
        all_words <- unique(response)
        if (length(all_words) >= 3) {
          coords <- arch_data$simplex %>%
            dplyr::filter(text %in% all_words) %>%
            select(x, y)
          
          if (nrow(coords) >= 3) {
            tryCatch({
              convhulln(as.matrix(coords), output.options = TRUE)$vol
            }, error = function(e) NA_real_)
          } else {
            NA_real_
          }
        } else {
          NA_real_
        }
      },
      .groups = "drop"
    )
  
  return(list(
    per_prompt = per_prompt_area,
    per_participant = per_participant_area
  ))
}

################################################################################
# Calculate Pairwise Similarity
################################################################################

calculate_pairwise_similarity <- function(tx_split, emb_matrix, prefix = "sem") {
  
  # Consecutive pairs
  consecutive_sim <- tx_split %>%
    arrange(participant_id, prompt, start) %>%
    group_by(participant_id, prompt) %>%
    mutate(
      cosine_sim = {
        if (n() > 1) {
          sims <- sapply(2:n(), function(i) {
            cosine_similarity(
              as.numeric(emb_matrix[response[i-1], ]),
              as.numeric(emb_matrix[response[i], ])
            )
          })
          c(NA_real_, sims)
        } else {
          NA_real_
        }
      }
    ) %>%
    summarise(
      !!paste0(prefix, "_consecutive_sim_mean") := mean(cosine_sim, na.rm = TRUE),
      !!paste0(prefix, "_consecutive_sim_sd") := sd(cosine_sim, na.rm = TRUE),
      .groups = "drop"
    )
  
  # All pairs
  all_pairs_sim <- tx_split %>%
    group_by(participant_id, prompt) %>%
    summarise(
      !!paste0(prefix, "_all_pairs_sim_mean") := {
        words <- unique(response)
        if (length(words) > 1) {
          pairs <- combn(words, 2)
          sims <- apply(pairs, 2, function(pair) {
            cosine_similarity(
              as.numeric(emb_matrix[pair[1], ]),
              as.numeric(emb_matrix[pair[2], ])
            )
          })
          mean(sims, na.rm = TRUE)
        } else {
          NA_real_
        }
      },
      !!paste0(prefix, "_all_pairs_sim_sd") := {
        words <- unique(response)
        if (length(words) > 1) {
          pairs <- combn(words, 2)
          sims <- apply(pairs, 2, function(pair) {
            cosine_similarity(
              as.numeric(emb_matrix[pair[1], ]),
              as.numeric(emb_matrix[pair[2], ])
            )
          })
          sd(sims, na.rm = TRUE)
        } else {
          NA_real_
        }
      },.groups = "drop"
    )
  
  per_prompt <- consecutive_sim %>%
    full_join(all_pairs_sim, by = c("participant_id", "prompt"))
  
  return(list(per_prompt = per_prompt))
}

################################################################################
# Calculate Trajectory Features
################################################################################

calculate_trajectory_features <- function(tx_split, emb_matrix, prefix = "sem") {
  require(TSP)
  
  trajectory <- tx_split %>%
    group_by(participant_id, prompt) %>%
    summarise(
      # Cumulative Euclidean distance
      !!paste0(prefix, "_path_length") := {
        words <- unique(response)
        if (length(words) > 1) {
          dists <- sapply(2:length(words), function(i) {
            sqrt(sum((emb_matrix[words[i], ] - emb_matrix[words[i-1], ])^2))
          })
          sum(dists, na.rm = TRUE)
        } else {
          0
        }
      },
      
      # TSP divergence (ordered vs optimal path)
      !!paste0(prefix, "_divergence") := {
        words <- unique(response)
        if (length(words) > 3) {
          # Calculate distance matrix
          dist_matrix <- dist(emb_matrix[words, ])
          
          # Ordered path length (chronological)
          ordered_path <- sum(sapply(2:length(words), function(i) {
            as.matrix(dist_matrix)[i, i-1]
          }))
          
          # Optimal path (TSP solution)
          tryCatch({
            tsp_obj <- TSP(dist_matrix)
            tour <- solve_TSP(tsp_obj)
            optimal_path <- tour_length(tour)
            
            # Divergence = ordered - optimal
            ordered_path - optimal_path
          }, error = function(e) {
            NA_real_
          })
        } else {
          NA_real_
        }
      },
      
      .groups = "drop"
    )
  
  return(list(per_prompt = trajectory))
}

################################################################################
# Calculate Vocabulary Volume
################################################################################

calculate_vocabulary_volume <- function(tx_split, emb_matrix, prefix = "sem") {
  require(geometry)
  
  # Per prompt volume
  per_prompt_vol <- tx_split %>%
    group_by(participant_id, prompt) %>%
    summarise(
      !!paste0(prefix, "_vocab_volume") := {
        words <- unique(response)
        if (length(words) >= 3) {
          tryCatch({
            convhulln(emb_matrix[words, ], output.options = TRUE)$vol
          }, error = function(e) NA_real_)
        } else {
          NA_real_
        }
      },
      .groups = "drop"
    )
  
  # Per participant volume (all responses across all prompts)
  per_participant_vol <- tx_split %>%
    group_by(participant_id) %>%
    summarise(
      !!paste0(prefix, "_vocab_volume") := {
        all_words <- unique(response)
        if (length(all_words) >= 3) {
          tryCatch({
            convhulln(emb_matrix[all_words, ], output.options = TRUE)$vol
          }, error = function(e) NA_real_)
        } else {
          NA_real_
        }
      },
      .groups = "drop"
    )
  
  return(list(
    per_prompt = per_prompt_vol,
    per_participant = per_participant_vol
  ))
}

################################################################################
# Calculate Norms (Distance from origin)
################################################################################

calculate_norms <- function(tx_split, emb_matrix, prefix = "sem") {
  
  # per-word norms (raw and origin-centered)
  l1_raw <- rowSums(emb_matrix)
  l2_raw <- sqrt(rowSums(emb_matrix^2))
  
  # origin = average of all emb_matrix
  mean_all <- colMeans(emb_matrix)
  emb_center_all <- sweep(emb_matrix, 2, mean_all, "-")
  l1_origin_all <- rowSums(emb_center_all)
  l2_origin_all <- sqrt(rowSums(emb_center_all^2))
  
  # origin = average of a small set (e.g., "a", "the")
  origin_set_words <- c("a", "the")
  has_origin_set <- all(origin_set_words %in% rownames(emb_matrix))
  
  if (has_origin_set) {
    mean_set <- colMeans(emb_matrix[origin_set_words,])
    emb_center_set <- sweep(emb_matrix, 2, mean_set, "-")
    l1_origin_set <- rowSums(emb_center_set)
    l2_origin_set <- sqrt(rowSums(emb_center_set^2))
  } else {
    # if origin is not avaialble, fall bacj to NA
    l1_origin_set <- rep(NA_real_, nrow(emb_matrix))
    l2_origin_set <- rep(NA_real_, nrow(emb_matrix))
  }
  
  ## collect norms in a data.frame keyed by word
  norms_df <- data.frame(
    response = rownames(emb_matrix),
    l1_norm = l1_raw,
    l2_norm = l2_raw,
    l1_norm_origin_avg_all = l1_origin_all,
    l2_norm_origin_avg_all = l2_origin_all,
    l1_norm_origin_avg_set = l1_origin_set,
    l2_norm_origin_avg_set = l2_origin_set,
    stringsAsFactors = FALSE
  )
  
  
  # --- Per-prompt summaries (mean/sd for each norm type) ---
  
  per_prompt_norms <- tx_split %>%
    dplyr::left_join(norms_df, by = "response") %>%
    dplyr::group_by(participant_id, prompt) %>%
    dplyr::summarise(
      !!paste0(prefix, "_l1_norm_mean") :=
        mean(l1_norm, na.rm = TRUE),
      !!paste0(prefix, "_l1_norm_sd") :=
        sd(l1_norm, na.rm = TRUE),
      
      !!paste0(prefix, "_l2_norm_mean") :=
        mean(l2_norm, na.rm = TRUE),
      !!paste0(prefix, "_l2_norm_sd") :=
        sd(l2_norm, na.rm = TRUE),
      
      !!paste0(prefix, "_l1_norm_origin_avg_all_mean") :=
        mean(l1_norm_origin_avg_all, na.rm = TRUE),
      !!paste0(prefix, "_l1_norm_origin_avg_all_sd") :=
        sd(l1_norm_origin_avg_all, na.rm = TRUE),
      
      !!paste0(prefix, "_l2_norm_origin_avg_all_mean") :=
        mean(l2_norm_origin_avg_all, na.rm = TRUE),
      !!paste0(prefix, "_l2_norm_origin_avg_all_sd") :=
        sd(l2_norm_origin_avg_all, na.rm = TRUE),
      
      !!paste0(prefix, "_l1_norm_origin_avg_set_mean") :=
        mean(l1_norm_origin_avg_set, na.rm = TRUE),
      !!paste0(prefix, "_l1_norm_origin_avg_set_sd") :=
        sd(l1_norm_origin_avg_set, na.rm = TRUE),
      
      !!paste0(prefix, "_l2_norm_origin_avg_set_mean") :=
        mean(l2_norm_origin_avg_set, na.rm = TRUE),
      !!paste0(prefix, "_l2_norm_origin_avg_set_sd") :=
        sd(l2_norm_origin_avg_set, na.rm = TRUE),
      
      .groups = "drop"
    )
  
  # --- Per-participant summaries (same stats collapsed over prompts) ---
  
  per_participant_norms <- tx_split %>%
    dplyr::left_join(norms_df, by = "response") %>%
    dplyr::group_by(participant_id) %>%
    dplyr::summarise(
      !!paste0(prefix, "_l1_norm_mean") :=
        mean(l1_norm, na.rm = TRUE),
      !!paste0(prefix, "_l1_norm_sd") :=
        sd(l1_norm, na.rm = TRUE),
      
      !!paste0(prefix, "_l2_norm_mean") :=
        mean(l2_norm, na.rm = TRUE),
      !!paste0(prefix, "_l2_norm_sd") :=
        sd(l2_norm, na.rm = TRUE),
      
      !!paste0(prefix, "_l1_norm_origin_avg_all_mean") :=
        mean(l1_norm_origin_avg_all, na.rm = TRUE),
      !!paste0(prefix, "_l1_norm_origin_avg_all_sd") :=
        sd(l1_norm_origin_avg_all, na.rm = TRUE),
      
      !!paste0(prefix, "_l2_norm_origin_avg_all_mean") :=
        mean(l2_norm_origin_avg_all, na.rm = TRUE),
      !!paste0(prefix, "_l2_norm_origin_avg_all_sd") :=
        sd(l2_norm_origin_avg_all, na.rm = TRUE),
      
      !!paste0(prefix, "_l1_norm_origin_avg_set_mean") :=
        mean(l1_norm_origin_avg_set, na.rm = TRUE),
      !!paste0(prefix, "_l1_norm_origin_avg_set_sd") :=
        sd(l1_norm_origin_avg_set, na.rm = TRUE),
      
      !!paste0(prefix, "_l2_norm_origin_avg_set_mean") :=
        mean(l2_norm_origin_avg_set, na.rm = TRUE),
      !!paste0(prefix, "_l2_norm_origin_avg_set_sd") :=
        sd(l2_norm_origin_avg_set, na.rm = TRUE),
      
      .groups = "drop"
    )
  
  return(list(
    per_prompt = per_prompt_norms,
    per_participant = per_participant_norms
  ))
}

################################################################################
# Calculate Prompt Similarity Trajectory
################################################################################

calculate_prompt_similarity_trajectory <- function(tx_split, emb_matrix, prefix = "sem") {
  
  in_data <- tx_split %>%
    arrange(participant_id, prompt, start) %>%
    rowwise() %>%
    mutate(
      sim_to_prompt_current = cosine_similarity(
        as.numeric(emb_matrix[prompt, ]),
        as.numeric(emb_matrix[response, ])
      )
    ) %>% ungroup()
  
  prompt_sim_traj_slope <- in_data %>% group_by(participant_id,prompt) %>%
    summarise(!!paste0(prefix, "_sim_to_prompt_slope") := {
      if (n() > 1) {
        time_seq <- start - min(start)
        lm_result <- lm(sim_to_prompt_current ~ time_seq)
        coef(lm_result)[2]
      } else {
        NA_real_
      }
    }) %>% ungroup()
    
  prompt_sim_traj <- in_data %>% group_by(participant_id,prompt) %>%
    summarise(
      # Initial similarity (first response to prompt)
      !!paste0(prefix, "_sim_to_prompt_initial") := first(sim_to_prompt_current),
      
      # Final similarity (last response to prompt)
      !!paste0(prefix, "_sim_to_prompt_final") := last(sim_to_prompt_current),
      
      # Change in similarity (final - initial)
      !!paste0(prefix, "_sim_to_prompt_delta") := last(sim_to_prompt_current) - first(sim_to_prompt_current)
    )
  
  prompt_sim_traj_all <- full_join(prompt_sim_traj,prompt_sim_traj_slope)
  
  return(prompt_sim_traj_all)
}

################################################################################
# Utility: Cosine Similarity
################################################################################

cosine_similarity <- function(vec1, vec2) {
  if (all(is.na(vec1)) || all(is.na(vec2))) return(NA_real_)
  if (length(vec1) != length(vec2)) return(NA_real_)
  
  dot_product <- sum(vec1 * vec2, na.rm = TRUE)
  norm1 <- sqrt(sum(vec1^2, na.rm = TRUE))
  norm2 <- sqrt(sum(vec2^2, na.rm = TRUE))
  
  if (norm1 == 0 || norm2 == 0) return(0)
  
  return(dot_product / (norm1 * norm2))
}


################################################################################
# Calculate similarity to semantic anchor sets (config-driven)
################################################################################

calculate_anchor_set_similarity <- function(tx_split, emb_matrix, prefix = "sem", config) {
  # Expect anchors under config$semantic$anchor_sets
  if (is.null(config$features$spatial_anchors)) {
    return(NULL)
  }
  
  anchor_sets <- config$features$spatial_anchors
  
  # Convert to long tibble: anchor_category, text
  anchors_meta <- purrr::imap_dfr(anchor_sets, function(words, name) {
    tibble::tibble(
      anchor_category = name,
      text = tolower(unlist(words))
    )
  })
  
  # Keep only anchors present in the embedding matrix
  anchors_meta <- anchors_meta %>%
    dplyr::filter(text %in% rownames(emb_matrix))
  
  if (nrow(anchors_meta) == 0) {
    return(NULL)
  }
  
  # Responses that have embeddings
  valid_responses <- unique(tx_split$response)
  valid_responses <- valid_responses[valid_responses %in% rownames(emb_matrix)]
  
  if (length(valid_responses) == 0) {
    return(NULL)
  }
  
  # All anchor–response pairs
  pairs <- expand.grid(
    w1 = anchors_meta$text,
    w2 = valid_responses,
    stringsAsFactors = FALSE
  )
  
  pairs$cosine_similarity <- vapply(
    seq_len(nrow(pairs)),
    function(i) {
      w1 <- pairs$w1[i]
      w2 <- pairs$w2[i]
      cosine_similarity(
        as.numeric(emb_matrix[w1, ]),
        as.numeric(emb_matrix[w2, ])
      )
    },
    numeric(1)
  )
  
  # Join to trial-level data
  anchor_sim <- tx_split %>%
    dplyr::filter(response %in% valid_responses) %>%
    dplyr::select(participant_id, prompt, response) %>%
    dplyr::left_join(
      pairs %>% dplyr::rename(response = w2, text=w1)
    ) %>%
    dplyr::left_join(
      anchors_meta,
      by = dplyr::join_by(text),
      relationship = "many-to-many"
    )
  
  # Aggregate: max similarity per participant × prompt × category
  per_prompt <- anchor_sim %>%
    dplyr::group_by(participant_id, prompt, anchor_category) %>%
    dplyr::summarise(
      val = max(cosine_similarity, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      anchor_category = paste0(prefix, "_", anchor_category, "_similarity")
    ) %>%
    tidyr::pivot_wider(
      names_from = anchor_category,
      values_from = val
    )
  
  return(per_prompt)
}

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# Simple task scoring functions
extract_attention_features <- function(checkbox, hi, participant_id) {
  # Combine CHECKBOX and HI performance
  checkbox_summary <- checkbox %>%
    summarise(
      checkbox_hit_rate = mean(has_correct_response),
      checkbox_mean_rt = mean(first_response_time, na.rm = TRUE),
      checkbox_rt_sd = sd(first_response_time, na.rm = TRUE)
    )
  
  hi_summary <- hi %>%
    summarise(
      hi_hit_rate = mean(has_correct_response),
      hi_mean_rt = mean(first_response_time, na.rm = TRUE),
      hi_sd_rt = sd(first_response_time, na.rm = TRUE)
    )
  
  bind_cols(participant_id = participant_id, checkbox_summary, hi_summary)
}

extract_memory_features <- function(wmemory, participant_id) {
  wmemory %>% mutate(exact_match = as.numeric(exact_match)) %>% group_by(participant_id) %>%
    summarise_at(.vars = vars(c(recall_time, exact_match, levenshtein_dist, phonological_similarity, recall_score)), 
                 .funs = function(x) mean(x, na.rm=T))
}

extract_sent_rep_features <- function(sent_rep, participant_id) {
  sent_rep %>% group_by(participant_id) %>%
    summarise_at(.vars = vars(c(response_duration, levenshtein_dist, word_accuracy, 
                                bigram_accuracy, word_count_diff, lcs_length,
                                word_order_score)), 
                 .funs = function(x) mean(x, na.rm=T))
}

extract_reading_features <- function(reading, participant_id) {
  reading %>% group_by(participant_id, valence) %>%
    summarise(reading_latency = mean(reading_latency, na.rm=T),
              is_correct = sum(as.numeric(is_correct)),
              substitution = sum(as.numeric(!is.na(substitution))))
}


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# END OF PART 3
################################################################################
