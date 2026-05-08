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
      dplyr::select(-dplyr::any_of(c("audio_file", "trial", "prompt_text")),
                    -dplyr::contains("constraint")) %>%
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
  
  
  loginfo("\u2713 Feature extraction complete")
  
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

# ... rest of file unchanged ...
