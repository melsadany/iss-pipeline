#!/usr/bin/env Rscript
# 03_transcription_cleanup.R
# ISS Task Transcription Cleanup and Validation

suppressPackageStartupMessages({
  library(tidyverse)
  library(yaml)
  library(stringdist)
  library(logging)
})

# ---------------------------------------------------------------------------
# Per-task cleaner helpers
# These are intentionally conservative: they keep the same row structure but
# attach task-specific summaries and minimal per-trial flags, so downstream
# code can compute features without requiring a separate implementation file.
# ---------------------------------------------------------------------------

clean_checkbox <- function(tx, cfg) {
  # Ensure required columns — accept either 'word' or 'response'
  if (!"response" %in% names(tx) && "word" %in% names(tx)) tx$response <- tx$word
  stopifnot(all(c("participant_id","task","audio_file","prompt","trial","response") %in% names(tx)))
  tx <- tx %>% arrange(participant_id, audio_file, trial)
  tx %>%
    group_by(participant_id, audio_file, prompt, trial) %>%
    summarise(
      n_clicks   = dplyr::n(),
      first_time = min(start, na.rm = TRUE),
      last_time  = max(end,   na.rm = TRUE),
      .groups    = "drop"
    )
}

clean_hi <- function(tx, cfg) {
  if (!"response" %in% names(tx) && "word" %in% names(tx)) tx$response <- tx$word
  stopifnot(all(c("participant_id","task","audio_file","prompt","trial","response") %in% names(tx)))
  tx <- tx %>% arrange(participant_id, audio_file, trial, start)
  tx %>%
    group_by(participant_id, audio_file, prompt, trial) %>%
    summarise(
      n_tokens   = dplyr::n(),
      duration   = max(end, na.rm = TRUE) - min(start, na.rm = TRUE),
      mean_gap   = ifelse(dplyr::n() > 1,
                          mean(diff(sort(start)), na.rm = TRUE),
                          NA_real_),
      .groups    = "drop"
    )
}

clean_word_assoc <- function(tx, cfg) {
  if (!"response" %in% names(tx) && "word" %in% names(tx)) tx$response <- tx$word
  stopifnot(all(c("participant_id","task","audio_file","prompt","trial","response") %in% names(tx)))
  tx <- tx %>% arrange(participant_id, audio_file, prompt, trial, start)
  tx %>%
    group_by(participant_id, audio_file, prompt, trial) %>%
    summarise(
      n_words     = dplyr::n(),
      first_onset = min(start, na.rm = TRUE),
      last_offset = max(end,   na.rm = TRUE),
      .groups     = "drop"
    )
}

clean_wmemory <- function(tx, word_assoc_clean, cfg) {
  if (!"response" %in% names(tx) && "word" %in% names(tx)) tx$response <- tx$word
  stopifnot(all(c("participant_id","task","audio_file","prompt","trial","response") %in% names(tx)))
  tx <- tx %>% arrange(participant_id, audio_file, trial)
  tx %>%
    group_by(participant_id, audio_file, trial) %>%
    summarise(
      n_items   = dplyr::n(),
      duration  = max(end, na.rm = TRUE) - min(start, na.rm = TRUE),
      .groups   = "drop"
    )
}

clean_sent_rep <- function(tx, cfg) {
  if (!"response" %in% names(tx) && "word" %in% names(tx)) tx$response <- tx$word
  stopifnot(all(c("participant_id","task","audio_file","prompt","trial","response") %in% names(tx)))
  tx <- tx %>% arrange(participant_id, audio_file, trial, start)
  tx %>%
    group_by(participant_id, audio_file, prompt, trial) %>%
    summarise(
      n_tokens  = dplyr::n(),
      duration  = max(end, na.rm = TRUE) - min(start, na.rm = TRUE),
      .groups   = "drop"
    )
}

clean_reading <- function(tx, cfg) {
  if (!"response" %in% names(tx) && "word" %in% names(tx)) tx$response <- tx$word
  stopifnot(all(c("participant_id","task","audio_file","prompt","trial","response") %in% names(tx)))
  tx <- tx %>% arrange(participant_id, audio_file, trial, start)
  tx %>%
    group_by(participant_id, audio_file, trial) %>%
    summarise(
      n_tokens  = dplyr::n(),
      duration  = max(end, na.rm = TRUE) - min(start, na.rm = TRUE),
      .groups   = "drop"
    )
}

#' Main cleanup function
cleanup_transcription <- function(transcription, participant_id, config, mode = "auto", output_dir) {
  
  loginfo(strrep("=", 60))
  loginfo(sprintf("ISS Transcription Cleanup: %s", participant_id))
  loginfo(strrep("=", 60))
  cfg = config
  
  removed_stats <- list(
    ums = 0,
    repetitions = 0,
    low_confidence = 0,
    comments = 0,
    total_removed = 0
  )
  
  # ---------------------------------------------------------------------------
  # FIX: Coalesce columns that stage 2 may not produce when they are absent
  # from the audio filenames (type, subtype, trial, response). Without this
  # the select() call below crashes with "object not found".
  # ---------------------------------------------------------------------------
  if (!"type"     %in% names(transcription)) transcription$type     <- NA_character_
  if (!"subtype"  %in% names(transcription)) transcription$subtype  <- NA_character_
  if (!"trial"    %in% names(transcription)) transcription$trial    <- NA_integer_
  if (!"response" %in% names(transcription)) transcription$response <- NA_character_
  if (!"task"     %in% names(transcription)) transcription$task     <- NA_character_
  if (!"prompt"   %in% names(transcription)) transcription$prompt   <- NA_character_

  # ---------------------------------------------------------------------------
  # FIX: Normalize word <-> response so the rest of the function always has
  # a 'word' column to work with regardless of which path produced the TSV.
  # A TSV re-run from a previously cleaned file may have 'response' but not
  # 'word'; the mutate(word = tolower(word)) below would crash in that case.
  # ---------------------------------------------------------------------------
  if (!"word" %in% names(transcription) && "response" %in% names(transcription)) {
    loginfo("  'word' column absent — populating from 'response' column.")
    transcription$word <- transcription$response
  }

  tx <- transcription %>%
    mutate(
      word = tolower(word),
      prompt = str_replace_all(tolower(prompt),"-"," "),
      word_clean = ifelse(grepl("[0-9]", word),
                          as.character(english::english(as.numeric(gsub("[^0-9]", "", word)))),
                          word),
      word_clean = gsub("[[:punct:]]", "", word_clean)
    )
  
  loginfo(sprintf("  Input: %d rows across %d unique tasks",
                  nrow(tx), n_distinct(tx$task)))
  
  if (mode == "strict") {
    threshold <- config$transcription$confidence_threshold$strict
    low_conf_rows <- sum(tx$confidence < threshold, na.rm = TRUE)
    tx <- tx %>% dplyr::filter(confidence >= threshold | is.na(confidence))
    removed_stats$low_confidence <- low_conf_rows
    loginfo(sprintf("  Strict mode: removed %d low-confidence words (threshold: %.2f)",
                    low_conf_rows, threshold))
    
  } else if (mode == "review") {
    threshold <- config$transcription$confidence_threshold$review
    flagged <- tx %>%
      dplyr::filter(confidence < threshold | is.na(confidence)) %>%
      mutate(review_reason = "low_confidence")
    
    if (nrow(flagged) > 0) {
      review_file <- file.path(output_dir, paste0(participant_id, "_REVIEW_REQUIRED.xlsx"))
      review_df <- flagged %>%
        select(participant_id, prompt, task_number, start, end,
               word, confidence, review_reason) %>%
        mutate(
          corrected_word = "",
          action = "",
          notes = ""
        )
      writexl::write_xlsx(review_df, review_file)
      logwarn(sprintf("  Review mode: %d words flagged for review", nrow(flagged)))
      logwarn(sprintf("  Review file: %s", review_file))
      return(list(
        requires_review = TRUE,
        review_file = review_file,
        flagged_words = flagged,
        clean_tx = NULL,
        removed_stats = removed_stats,
        rule_violations = NULL
      ))
    }
  } else {
    loginfo("  Auto mode: accepting all transcriptions (no confidence filtering)")
  }
  
  fillers_to_remove <- config$transcription$filters$remove_fillers
  
  ums_per_prompt <- tx %>% dplyr::filter(task %in% c("SENT-REP", "WMEMORY", "WORD-ASSOC")) %>%
    mutate(is_um = word_clean %in% fillers_to_remove) %>%
    group_by(participant_id, task, audio_file, prompt) %>%
    summarise(ums_count = sum(is_um, na.rm = TRUE), .groups = "drop")
  
  comments_per_prompt <- tx %>% dplyr::filter(task %in% c("SENT-REP", "WMEMORY", "WORD-ASSOC")) %>%
    mutate(is_comment = grepl("\\[|\\]|\\*", word)) %>%
    group_by(participant_id, task, audio_file, prompt) %>%
    summarise(comment_count = sum(is_comment, na.rm = TRUE), .groups = "drop")
  
  rep_prompts_per_prompt <- tx %>% dplyr::filter(task %in% c("WORD-ASSOC")) %>%
    mutate(is_rep_prompt = (prompt == word_clean)) %>%
    group_by(participant_id, task, audio_file, prompt) %>%
    summarise(rep_prompt = sum(is_rep_prompt, na.rm = TRUE), .groups = "drop")
  
  rep_words_per_prompt <- tx %>% dplyr::filter(task %in% c("WORD-ASSOC")) %>%
    group_by(participant_id, task, audio_file, prompt) %>%
    mutate(is_repeat = duplicated(word_clean)) %>%
    summarise(rep_words_per_prompt = sum(is_repeat, na.rm = TRUE), .groups = "drop")
  
  rule_violations <- list(
    ums = ums_per_prompt,
    comments = comments_per_prompt,
    repeated_prompts = rep_prompts_per_prompt,
    repeated_words_per_prompt = rep_words_per_prompt
  )
  
  removed_stats$ums <- sum(ums_per_prompt$ums_count)
  removed_stats$comments <- sum(comments_per_prompt$comment_count)
  removed_stats$repetitions <- sum(rep_prompts_per_prompt$rep_prompt) +
    sum(rep_words_per_prompt$rep_words_per_prompt)
  
  tx_clean <- tx %>%
    dplyr::filter(
      !word_clean %in% fillers_to_remove,
      !word_clean %in% config$transcription$filters$remove_acknowledgments,
      !grepl("\\[|\\]|\\*", word)
    )
  
  tx_clean <- tx_clean %>% mutate(rep_pro = ifelse(task == "WORD-ASSOC", prompt == word_clean, F)) %>%
    dplyr::filter(!rep_pro)
  
  tx_clean <- tx_clean %>%
    group_by(participant_id, task, trial, audio_file, prompt) %>%
    mutate(is_repeat = ifelse(task=="WORD-ASSOC", duplicated(word_clean), F)) %>%
    ungroup() %>%
    dplyr::filter(!is_repeat) %>%
    select(-is_repeat)
  
  tx_clean <- tx_clean %>%
    mutate(response = word_clean) %>%
    select(participant_id, task, audio_file, prompt, trial, type, subtype,
           start, end, response, confidence) %>% ungroup()
  
  removed_stats$total_removed <- nrow(tx) - nrow(tx_clean)
  
  cleaned_tasks <- list()
  
  tx_checkbox <- tx_clean %>% dplyr::filter(str_detect(audio_file, "CHECKBOX"))
  if (nrow(tx_checkbox) > 0) {
    cleaned_tasks$checkbox <- clean_checkbox(tx_checkbox, cfg)
    loginfo(sprintf("  CHECKBOX: %d trials processed", nrow(cleaned_tasks$checkbox)))
  }
  
  tx_hi <- tx_clean %>% dplyr::filter(str_detect(audio_file, "HI"))
  if (nrow(tx_hi) > 0) {
    cleaned_tasks$hi <- clean_hi(tx_hi, cfg)
    loginfo(sprintf("  HI: %d trials processed", nrow(cleaned_tasks$hi)))
  }
  
  tx_word_assoc <- tx_clean %>% dplyr::filter(str_detect(audio_file, "WORD-ASSOC"))
  if (nrow(tx_word_assoc) > 0) {
    cleaned_tasks$word_assoc <- clean_word_assoc(tx_word_assoc, cfg)
    loginfo(sprintf("  WORD-ASSOC: %d words across %d prompts",
                    nrow(cleaned_tasks$word_assoc),
                    n_distinct(cleaned_tasks$word_assoc$prompt)))
  }
  
  tx_wmemory <- tx_clean %>% dplyr::filter(str_detect(audio_file, "WMEMORY"))
  if (nrow(tx_wmemory) > 0 && !is.null(cleaned_tasks$word_assoc)) {
    cleaned_tasks$wmemory <- clean_wmemory(tx_wmemory, cleaned_tasks$word_assoc, cfg)
    loginfo(sprintf("  WMEMORY: %d recall trials", nrow(cleaned_tasks$wmemory)))
  }
  
  tx_sent_rep <- tx_clean %>% dplyr::filter(str_detect(audio_file, "SENT-REP"))
  if (nrow(tx_sent_rep) > 0) {
    cleaned_tasks$sent_rep <- clean_sent_rep(tx_sent_rep, cfg)
    loginfo(sprintf("  SENT-REP: %d sentences", nrow(cleaned_tasks$sent_rep)))
  }
  
  tx_reading <- tx_clean %>% dplyr::filter(str_detect(audio_file, "READING"))
  if (nrow(tx_reading) > 0) {
    cleaned_tasks$reading <- clean_reading(tx_reading, cfg)
    loginfo(sprintf("  READING: %d words", nrow(cleaned_tasks$reading)))
  }
  
  loginfo("  Cleanup summary:")
  loginfo(sprintf("    - Fillers removed      : %d", removed_stats$ums))
  loginfo(sprintf("    - Repetitions removed  : %d", removed_stats$repetitions))
  loginfo(sprintf("    - Comments flagged     : %d", removed_stats$comments))
  loginfo(sprintf("    - Total rows removed   : %d", removed_stats$total_removed))
  loginfo(sprintf("    - Valid responses left : %d", nrow(tx_clean)))
  loginfo(sprintf("    - Prompts covered      : %d", n_distinct(tx_clean$prompt)))
  
  return(list(
    requires_review = FALSE,
    clean_tx = tx_clean,
    removed_stats = removed_stats,
    review_file = NULL,
    cleaned_by_task = cleaned_tasks,
    rule_violations = rule_violations
  ))
}
