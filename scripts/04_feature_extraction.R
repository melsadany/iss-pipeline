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
      dplyr::select(
        -dplyr::any_of(c("audio_file", "trial", "prompt_text")),
        -dplyr::contains("constraint")
      ) %>%
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
    if (!is.null(new_pwe_emb)) {
      embeddings$phonetic <- rbind(
        embeddings$phonetic,
        new_pwe_emb %>% rename_at(.vars = vars(-text), .funs = function(x) paste0("Dim", as.numeric(x) + 1))
      ) %>% distinct(text, .keep_all = TRUE)
    } else {
      logwarn("  PWE extraction returned NULL for novel words; skipping — using pre-computed phonetic embeddings only")
    }
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

  # Guard: drop rows with a missing or blank response before passing to
  # lingmatch. lma_meta() and lma_termcat() return data frames with differing
  # row counts when given NA or empty-string inputs, which causes data.frame()
  # to error with "arguments imply differing number of rows".
  tx_split <- tx_split %>%
    dplyr::filter(!is.na(response) & nzchar(trimws(as.character(response))))

  if (nrow(tx_split) == 0) {
    empty <- tibble::tibble(participant_id = character(), prompt = character())
    return(list(per_prompt = empty, per_task = empty, per_participant = empty))
  }

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
    "/opt/conda/envs/pwesuite_env/bin/python",
    "/app/scripts/pwe_wrapper.py",
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
                             as.