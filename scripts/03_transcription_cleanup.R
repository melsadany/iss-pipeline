#!/usr/bin/env Rscript
# 03_transcription_cleanup.R
# ISS Task Transcription Cleanup and Validation

suppressPackageStartupMessages({
  library(tidyverse)
  library(yaml)
  library(stringdist)
  library(logging)
})

#' Main cleanup function
cleanup_transcription <- function(transcription, participant_id, config, mode = "auto", output_dir) {
  
  loginfo(strrep("=", 60))
  loginfo(sprintf("ISS Transcription Cleanup: %s", participant_id))
  loginfo(strrep("=", 60))
  cfg = config
  
  # Initialize cleanup stats
  removed_stats <- list(
    ums = 0,
    repetitions = 0,
    low_confidence = 0,
    comments = 0,
    total_removed = 0
  )
  
  # Prepare transcription
  tx <- transcription %>%
    mutate(
      word = tolower(word),
      prompt = str_replace_all(tolower(prompt),"-"," "),
      # Convert numbers to words if needed
      word_clean = ifelse(grepl("[0-9]", word),
                          as.character(english::english(as.numeric(gsub("[^0-9]", "", word)))),
                          word),
      word_clean = gsub("[[:punct:]]", "", word_clean)
    )
  
  ################################################################################
  # MODE-SPECIFIC FILTERING
  ################################################################################
  
  if (mode == "strict") {
    # STRICT MODE: Only keep high-confidence transcriptions
    threshold <- config$transcription$confidence_threshold$strict
    low_conf_rows <- sum(tx$confidence < threshold, na.rm = TRUE)
    tx <- tx %>% dply::filter(confidence >= threshold | is.na(confidence))
    removed_stats$low_confidence <- low_conf_rows
    log_debug("  Strict mode: Removed {low_conf_rows} low-confidence words (threshold: {threshold})")
    
  } else if (mode == "review") {
    # REVIEW MODE: Flag low-confidence words for manual review
    threshold <- config$transcription$confidence_threshold$review
    flagged <- tx %>%
      dply::filter(confidence < threshold | is.na(confidence)) %>%
      mutate(review_reason = "low_confidence")
    
    if (nrow(flagged) > 0) {
      # Create review file
      review_file <- file.path(output_dir, paste0(participant_id, "_REVIEW_REQUIRED.xlsx"))
      review_df <- flagged %>%
        select(participant_id, prompt, task_number, start, end,
               word, confidence, review_reason) %>%
        mutate(
          corrected_word = "",  # Empty column for manual correction
          action = "",          # keep/remove/correct
          notes = ""
        )
      
      writexl::write_xlsx(review_df, review_file)
      log_warn("  Review mode: {nrow(flagged)} words flagged for review")
      log_warn("  Review file: {review_file}")
      
      return(list(
        requires_review = TRUE,
        review_file = review_file,
        flagged_words = flagged,
        clean_tx = NULL,
        removed_stats = removed_stats,
        rule_violations = NULL
      ))
    }
  }
  # AUTO MODE: Accept all transcriptions, no filtering by confidence
  
  ################################################################################
  # EXTRACT RULE VIOLATIONS AS FEATURES (BEFORE REMOVAL)
  ################################################################################
  
  fillers_to_remove <- config$transcription$filters$remove_fillers
  
  # 1. UMs count (per prompt)
  ums_per_prompt <- tx %>% dplyr::filter(task %in% c("SENT-REP", "WMEMORY", "WORD-ASSOC")) %>%
    mutate(is_um = word_clean %in% fillers_to_remove) %>%
    group_by(participant_id, task, audio_file, prompt) %>%
    summarise(ums_count = sum(is_um, na.rm = TRUE), .groups = "drop")
  
  # 2. Comments count (per prompt)
  comments_per_prompt <- tx %>%dplyr::filter(task %in% c("SENT-REP", "WMEMORY", "WORD-ASSOC")) %>%
    mutate(is_comment = grepl("\\[|\\]|\\*", word)) %>%
    group_by(participant_id, task, audio_file, prompt) %>%
    summarise(comment_count = sum(is_comment, na.rm = TRUE), .groups = "drop")
  
  # 3. Repeated prompts (per prompt)
  rep_prompts_per_prompt <- tx %>%dplyr::filter(task %in% c("WORD-ASSOC")) %>%
    mutate(is_rep_prompt = (prompt == word_clean)) %>%
    group_by(participant_id, task, audio_file, prompt) %>%
    summarise(rep_prompt = sum(is_rep_prompt, na.rm = TRUE), .groups = "drop")
  
  # 4. Repeated words within each prompt (per prompt)
  rep_words_per_prompt <- tx %>%dplyr::filter(task %in% c("WORD-ASSOC")) %>%
    group_by(participant_id, task, audio_file, prompt) %>%
    mutate(is_repeat = duplicated(word_clean)) %>%
    summarise(rep_words_per_prompt = sum(is_repeat, na.rm = TRUE), .groups = "drop")
  
  # Combine rule violations
  rule_violations <- list(
    ums = ums_per_prompt,
    comments = comments_per_prompt,
    repeated_prompts = rep_prompts_per_prompt,
    repeated_words_per_prompt = rep_words_per_prompt
  )
  
  # Update removed stats
  removed_stats$ums <- sum(ums_per_prompt$ums_count)
  removed_stats$comments <- sum(comments_per_prompt$comment_count)
  removed_stats$repetitions <- sum(rep_prompts_per_prompt$rep_prompt) + 
    sum(rep_words_per_prompt$rep_words_per_prompt)
  
  
  
  ################################################################################
  # UNIVERSAL CLEANUP (all modes) - NOW REMOVE AFTER COUNTING
  ################################################################################
  
  # Remove fillers and acknowledgments
  tx_clean <- tx %>%
    dplyr::filter(
      !word_clean %in% fillers_to_remove,
      !word_clean %in% config$transcription$filters$remove_acknowledgments,
      !grepl("\\[|\\]|\\*", word)  # Remove bracketed content
    )
  
  # Remove repeated prompts
  tx_clean <- tx_clean %>% mutate(rep_pro = ifelse(task == "WORD-ASSOC", prompt == word_clean, F)) %>%
    dplyr::filter(!rep_pro)
  
  # Remove repeated words per prompt
  tx_clean <- tx_clean %>%
    group_by(participant_id, task, trial, audio_file, prompt) %>%
    mutate(is_repeat = ifelse(task=="WORD-ASSOC",duplicated(word_clean),F)) %>%
    ungroup() %>%
    dplyr::filter(!is_repeat) %>%
    select(-is_repeat)
  
  # Final cleanup: use corrected word
  tx_clean <- tx_clean %>%
    mutate(response = word_clean) %>%
    select(participant_id, task, audio_file, prompt, trial, type, subtype,
           start, end, response, confidence) %>% ungroup()
  
  
  removed_stats$total_removed <- nrow(tx) - nrow(tx_clean)
  
  ################################################################################
  # TASK - CLEANUPS
  ################################################################################
  # Split by task type and clean each
  cleaned_tasks <- list()
  
  # CHECKBOX
  tx_checkbox <- tx_clean %>% dplyr::filter(str_detect(audio_file, "CHECKBOX"))
  if (nrow(tx_checkbox) > 0) {
    cleaned_tasks$checkbox <- clean_checkbox(tx_checkbox, cfg)
    loginfo(sprintf("  CHECKBOX: %d trials processed", nrow(cleaned_tasks$checkbox)))
  }
  
  # HI
  tx_hi <- tx_clean %>% dplyr::filter(str_detect(audio_file, "HI"))
  if (nrow(tx_hi) > 0) {
    cleaned_tasks$hi <- clean_hi(tx_hi, cfg)
    loginfo(sprintf("  HI: %d trials processed", nrow(cleaned_tasks$hi)))
  }
  
  # WORD_ASSOC
  tx_word_assoc <- tx_clean %>% dplyr::filter(str_detect(audio_file, "WORD-ASSOC"))
  if (nrow(tx_word_assoc) > 0) {
    cleaned_tasks$word_assoc <- clean_word_assoc(tx_word_assoc, cfg)
    loginfo(sprintf("  WORD-ASSOC: %d words across %d prompts", 
                    nrow(cleaned_tasks$word_assoc), 
                    n_distinct(cleaned_tasks$word_assoc$prompt)))
  }
  
  # WMEMORY (needs word_assoc for validation)
  tx_wmemory <- tx_clean %>% dplyr::filter(str_detect(audio_file, "WMEMORY"))
  if (nrow(tx_wmemory) > 0 && !is.null(cleaned_tasks$word_assoc)) {
    cleaned_tasks$wmemory <- clean_wmemory(tx_wmemory, cleaned_tasks$word_assoc, cfg)
    loginfo(sprintf("  WMEMORY: %d recall trials", nrow(cleaned_tasks$wmemory)))
  }
  
  # SENT_REP
  tx_sent_rep <- tx_clean %>% dplyr::filter(str_detect(audio_file, "SENT-REP"))
  if (nrow(tx_sent_rep) > 0) {
    cleaned_tasks$sent_rep <- clean_sent_rep(tx_sent_rep, cfg)
    loginfo(sprintf("  SENT-REP: %d sentences", nrow(cleaned_tasks$sent_rep)))
  }
  
  # READING
  tx_reading <- tx_clean %>% dplyr::filter(str_detect(audio_file, "READING"))
  if (nrow(tx_reading) > 0) {
    cleaned_tasks$reading <- clean_reading(tx_reading, cfg)
    loginfo(sprintf("  READING: %d words", nrow(cleaned_tasks$reading)))
  }
  
  log_debug("  Cleanup summary:")
  log_debug("    - Fillers removed: {removed_stats$ums}")
  log_debug("    - Repetitions removed: {removed_stats$repetitions}")
  log_debug("    - Comments removed: {removed_stats$comments}")
  log_debug("    - Total removed: {removed_stats$total_removed}")
  log_debug("    - Remaining valid responses: {nrow(tx_clean)}")
  log_debug("  Rule violations saved as features for {length(unique(tx_clean$prompt))} prompts")
  
  return(list(
    requires_review = FALSE,
    clean_tx = tx_clean,
    removed_stats = removed_stats,
    review_file = NULL,
    cleaned_by_task = cleaned_tasks,
    rule_violations = rule_violations  # RETURN THESE FOR FEATURE EXTRACTION!
  ))
}


#' Clean CHECKBOX responses
clean_checkbox <- function(tx_checkbox, cfg) {
  valid_words <- cfg$task_settings$CHECKBOX$valid_words
  
  tx_ret <- tx_checkbox %>%
    mutate(
      trial = as.integer(str_extract(audio_file, "trial-(\\d+)", group = 1)),
      is_correct = str_detect(response, paste(valid_words, collapse = "|")),
      response_latency = start  # Time from trial start
    ) %>%
    group_by(participant_id, trial) %>%
    summarise(
      n_responses = n(),
      has_correct_response = any(is_correct),
      first_response_time = min(start),
      all_words = paste(response, collapse = " "),
      .groups = "drop"
    )
  
  return(tx_ret)
}

#' Clean HI responses
clean_hi <- function(tx_hi, cfg) {
  valid_words <- cfg$task_settings$HI$valid_words
  
  tx_ret <- tx_hi %>%
    mutate(
      trial = as.integer(str_extract(audio_file, "trial-(\\d+)", group = 1)),
      is_correct = str_detect(response, paste(valid_words, collapse = "|")),
      response_latency = start
    ) %>%
    group_by(participant_id, trial) %>%
    summarise(
      n_responses = n(),
      has_correct_response = any(is_correct),
      first_response_time = min(start),
      all_words = paste(response, collapse = " "),
      .groups = "drop"
    )
  
  return(tx_ret)
}

#' Clean WORD_ASSOC responses with constraint checking
clean_word_assoc <- function(tx_word_assoc, cfg) {
  
  # Extract metadata from audio filename
  tx_word_assoc2 <- tx_word_assoc %>%
    mutate(
      type = str_extract(audio_file, "type-([^_]+)", group = 1),
      subtype = as.integer(str_extract(audio_file, "subtype-(\\d+)", group = 1)),
      prompt_text = str_extract(audio_file, "prompt-([^\\.]+)", group = 1),
      prompt_text = str_replace_all(prompt_text, "_", " ")
    )
  
  # Join with prompt metadata from config
  prompts_df <- bind_rows(cfg$word_association_prompts)
  
  tx_word_assoc3 <- tx_word_assoc2 %>%
    left_join(prompts_df)
  
  # Check constraints
  tx_word_assoc4 <- tx_word_assoc3 %>%
    rowwise() %>%
    mutate(
      meets_constraint = check_constraint(response, type, prompt)
    ) %>%
    ungroup()
  
  return(tx_word_assoc4)
}

#' Check if word meets constraint
check_constraint <- function(word, type, prompt_info) {
  if (is.na(type) || is.null(prompt_info)) return(NA)
  
  tryCatch({
    if (type == "letter" && !is.null(prompt_info$constraint_letter)) {
      # Letter fluency
      return(str_starts(word, tolower(prompt_info$constraint_letter)))
      
    } else if (type == "morph") {
      if (!is.null(prompt_info$constraint_length)) {
        # Three letter words
        return(nchar(word) == prompt_info$constraint_length)
      } else if (!is.null(prompt_info$constraint_syllables)) {
        # Two syllable words (approximate using vowel clusters)
        n_syllables <- str_count(word, "[aeiouy]+")
        return(n_syllables == prompt_info$constraint_syllables)
      }
      
    } else if (type == "sound" && !is.null(prompt_info$constraint_rhyme)) {
      # Rhyming (using soundex as proxy)
      target_soundex <- phonics::soundex(prompt_info$constraint_rhyme)
      word_soundex <- phonics::soundex(word)
      # Also check ending similarity
      target_end <- str_sub(prompt_info$constraint_rhyme, -2, -1)
      word_end <- str_sub(word, -2, -1)
      return(word_soundex == target_soundex || word_end == target_end)
    }
    
    # For lexical and abstract, no automatic constraint checking
    return(TRUE)
    
  }, error = function(e) {
    return(NA)
  })
}

#' Clean WMEMORY responses and check accuracy
clean_wmemory <- function(tx_wmemory, tx_word_assoc, cfg) {
  
  tx_wmemory2 <- tx_wmemory %>%
    rowwise() %>% 
    mutate(
      trial = as.integer(str_extract(audio_file, "trial-(\\d+)", group = 1)),
      recalled_word = response[which.min(start)]  # First word is the recall
    ) %>% ungroup() %>%
    group_by(participant_id, trial) %>%
    summarise(
      recalled_word = first(response),
      recall_time = min(start),
      .groups = "drop"
    )
  
  # Map WMEMORY trials to corresponding WORD_ASSOC trials
  # Trial 1 -> after WORD_ASSOC letter S (prompt 1)
  # Trial 2 -> after WORD_ASSOC letter F (prompt 9)
  # Trial 3 -> after WORD_ASSOC letter A (prompt 10)
  # Trial 4 -> after WORD_ASSOC letter P (prompt 18)
  
  wmemory_mapping <- data.frame(
    trial = c(1, 2, 3, 4),
    prompt = c("begins with the letter s", 
               "begins with the letter f",
               "begins with the letter a",
               "begins with the letter p")
  )
  
  # Get first word from each mapped WORD_ASSOC trial
  first_words <- tx_word_assoc %>%
    group_by(participant_id, prompt) %>%
    arrange(start) %>%
    slice(1) %>%
    select(participant_id, prompt, target_word = response)
  
  # Get full response set from each mapped WORD_ASSOC prompt
  full_words <- tx_word_assoc %>% dplyr::filter(prompt %in% wmemory_mapping$prompt) %>%
    group_by(participant_id, prompt) %>%
    arrange(start) %>% mutate(order = c(1:n())) %>% ungroup() %>%
    select(participant_id, prompt, response, order)
  
  
  # Join and calculate accuracy
  tx_wmemory3 <- tx_wmemory2 %>%
    left_join(wmemory_mapping) %>%
    left_join(first_words) %>%
    left_join(full_words %>% dplyr::rename(recalled_word=response)) %>%
    rowwise() %>%
    mutate(
      exact_match = tolower(recalled_word) == tolower(target_word),
      levenshtein_dist = stringdist(recalled_word, target_word, method = "lv"),
      phonological_similarity = 1 - (levenshtein_dist / pmax(nchar(recalled_word), nchar(target_word))),
      recall_score = 1/order
    ) %>% ungroup()
  
  return(tx_wmemory3)
}

#' Clean SENT_REP responses and calculate accuracy (including bigram)
clean_sent_rep <- function(tx_sent_rep, cfg) {
  
  # Extract difficulty from filename
  tx_sent_rep2 <- tx_sent_rep %>%
    mutate(
      difficulty = str_extract(audio_file, "difficulty-([^_]+)", group = 1),
      trial = as.integer(str_extract(audio_file, "trial-(\\d+)", group = 1))
    )
  
  # Get target sentences from config
  target_sentences <- bind_rows(cfg$ground_truth$sent_rep_sentences)
  target_sentences$sentence=str_remove_all(target_sentences$sentence,"\\?|,|'")
  
  # Aggregate words into full response per trial
  tx_sent_rep_agg <- tx_sent_rep2 %>%
    group_by(participant_id, trial, difficulty) %>%
    arrange(start) %>%
    summarise(
      response_text = paste(response, collapse = " "),
      n_words = n(),
      response_duration = max(end) - min(start),
      .groups = "drop"
    ) %>%
    left_join(target_sentences, by = "difficulty") %>%
    mutate(
      target_text = sentence,
      target_words = str_count(sentence, "\\S+")
    )
  
  # Calculate accuracy metrics
  tx_sent_rep_agg <- tx_sent_rep_agg %>%
    rowwise() %>%
    mutate(
      # Levenshtein distance (word-level)
      levenshtein_dist = stringdist(response_text, target_text, method = "lv"),
      word_accuracy = 1 - (levenshtein_dist / nchar(target_text)),
      
      # Bigram accuracy
      bigram_accuracy = calculate_bigram_accuracy(response_text, target_text),
      
      # Word count difference
      word_count_diff = abs(n_words - target_words),
      
      # Word order errors (using longest common subsequence)
      lcs_length = stringdist(response_text, target_text, method = "lcs"),
      word_order_score = lcs_length / target_words
    ) %>%
    ungroup()
  
  return(tx_sent_rep_agg)
}

#' Calculate bigram accuracy between response and target
calculate_bigram_accuracy <- function(response, target) {
  # Tokenize into words
  response_words <- str_split(tolower(response), "\\s+")[[1]]
  target_words <- str_split(tolower(target), "\\s+")[[1]]
  
  if (length(response_words) < 2 || length(target_words) < 2) {
    return(0)
  }
  
  # Create bigrams
  response_bigrams <- sapply(1:(length(response_words)-1), function(i) {
    paste(response_words[i], response_words[i+1])
  })
  
  target_bigrams <- sapply(1:(length(target_words)-1), function(i) {
    paste(target_words[i], target_words[i+1])
  })
  
  # Calculate overlap
  matching_bigrams <- sum(response_bigrams %in% target_bigrams)
  bigram_accuracy <- matching_bigrams / length(target_bigrams)
  
  return(bigram_accuracy)
}

#' Clean READING responses and check accuracy
clean_reading <- function(tx_reading, cfg) {
  
  # Get expected words from config
  reading_words <- bind_rows(cfg$ground_truth$reading_words)
  
  # Assign order based on timing (words should appear sequentially)
  tx_reading2 <- tx_reading %>%
    arrange(start) %>%
    mutate(
      word_index = row_number()
    )
  
  # realign timing
  task_temp01 <- read_csv(cfg$template_csv)
  task_temp02 <- task_temp01 %>% dplyr::filter(task=="READING") %>% 
    dplyr::select(template_start=seconds, prompt) %>% 
    mutate(template_start = template_start - task_temp01$seconds[(which(task_temp01$task=="READING"))[1]])
  
  
  # Match to expected words
  tx_reading3 <- tx_reading2 %>%
    left_join(reading_words %>% dplyr::rename("word_index" = "order")) %>%
    mutate(
      expected_word = word,
      actual_word = response,
      is_correct = tolower(actual_word) == tolower(expected_word),
      substitution = if_else(!is_correct, actual_word, NA_character_),
      valence = valence
    ) %>%
    left_join(task_temp02 %>% rename(word=prompt)) %>% mutate(reading_latency = start - template_start) %>%
    select(-c(response, word, trial, prompt, type, subtype))
  
  return(tx_reading3)
}