#!/usr/bin/env Rscript
# 01_audio_preprocessing.R
# ISS Task Audio Preprocessing
# Segments audio based on ISS template CSV

suppressPackageStartupMessages({
  library(tuneR)
  library(tidyverse)
  library(yaml)
  library(logging)
})

#' Main preprocessing function
#' @param audio_file Path to audio file (mp3 or wav)
#' @param participant_id Participant ID
#' @param config Path to config YAML file
#' @param output_dir Output directory for cropped segments
preprocess_audio <- function(audio_file, participant_id, config, output_dir) {
  
  loginfo(strrep("=", 80))
  loginfo(sprintf("ISS Audio Preprocessing: %s", participant_id))
  loginfo(strrep("=", 80))
  
  # Load config
  cfg <- (config)
  template_path <- cfg$template_csv
  
  # Read audio file
  loginfo(sprintf("Reading audio file: %s", audio_file))
  if (grepl("\\.mp3$", audio_file, ignore.case = TRUE)) {
    audio <- readMP3(audio_file)
  } else if (grepl("\\.wav$", audio_file, ignore.case = TRUE)) {
    audio <- readWave(audio_file)
  } else {
    stop("Unsupported audio format. Use .mp3 or .wav")
  }
  
  audio_duration <- round(length(audio@left) / audio@samp.rate, 2)
  loginfo(sprintf("Audio duration: %s seconds", audio_duration))
  
  # Load template
  loginfo(sprintf("Loading template: %s", template_path))
  template <- read_csv(template_path, show_col_types = FALSE)
  
  # # align audio to template
  # offset <- align_audio(audio@left, audio@samp.rate, "/app/reference_data/task_metadata/ISS_v_1.10_B.mp3")
  # # Align all events
  # template$audio_time <- template$seconds + offset
  
  
  # Create output directory
  participant_dir <- file.path(output_dir, participant_id)
  dir.create(participant_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Crop segments
  loginfo("Cropping audio segments...")
  segments <- crop_audio_segments(audio, template, participant_dir, participant_id, cfg)
  
  loginfo(sprintf("✓ Cropped %d segments", length(segments)))
  
  # Save segment metadata
  metadata_file <- file.path(participant_dir, paste0(participant_id, "_segments_metadata.csv"))
  segments_df <- bind_rows(segments)
  write_csv(segments_df, metadata_file)
  loginfo(sprintf("✓ Saved metadata: %s", metadata_file))
  
  
  ## extract reaction time of these tasks using audio
  checkbox_responses <- detect_responses(audio@left, audio@samp.rate, 
                                         template$seconds[template$task == "CHECKBOX"], 
                                         "check") %>%mutate(task = "CHECKBOX")
  hi_responses <- detect_responses(audio@left, audio@samp.rate, 
                                   template$seconds[template$task == "HI"], 
                                   "hi") %>%mutate(task = "HI")
  word.reading.responses <- do.call(rbind,lapply(template$prompt[50:99], function(pp){
      detect_responses(audio@left, audio@samp.rate, 
                       template$seconds[which(template$prompt == pp)], 
                       pp)%>%mutate(prompt=pp)})) %>%mutate(task = "READING")
  
  write_rds(list(checkbox = checkbox_responses, hi = hi_responses, reading = word.reading.responses),
            file.path(participant_dir, paste0(participant_id, "_checkbox-hi-reading-timing-from-sound.rds")),
            compress = "gz")
  
  return(list(
    segments = segments,
    audio_duration = audio_duration
  ))
}

#' Crop audio segments based on ISS template
crop_audio_segments <- function(audio, template, output_dir, participant_id, cfg) {
  
  segments <- list()
  
  # ============================================================================
  # ORIENTATION - Transcribe but don't segment (just note timing)
  # ============================================================================
  orientation_rows <- template %>% filter(task == "ORIENTATION", !is.na(prompt))
  if (nrow(orientation_rows) > 0) {
    logdebug(sprintf("ORIENTATION: %d questions (timing only)", nrow(orientation_rows)))
    for (i in 1:nrow(orientation_rows)) {
      row <- orientation_rows[i, ]
      segments[[length(segments) + 1]] <- list(
        file = NA_character_,
        task = "ORIENTATION",
        question = row$prompt,
        start_time = row$seconds,
        end_time = if(i < nrow(orientation_rows)) orientation_rows$seconds[i+1] else row$seconds + 5
      )
    }
  }
  
  # ============================================================================
  # CHECKBOX - 5 trials
  # ============================================================================
  checkbox_rows <- template %>% filter(task == "CHECKBOX")
  if (nrow(checkbox_rows) > 0) {
    logdebug(sprintf("CHECKBOX: %d trials", nrow(checkbox_rows)))
    
    for (i in 1:nrow(checkbox_rows)) {
      row <- checkbox_rows[i, ]
      
      # Extract window around expected response (2 seconds after prompt)
      start_sec <- row$seconds
      end_sec <- start_sec + 2.0
      
      segment <- extractWave(audio, from = start_sec, to = end_sec, xunit = "time")
      
      segment_file <- file.path(output_dir, 
                                sprintf("%s_task-CHECKBOX_trial-%d.wav", 
                                        participant_id, i))
      
      writeWave(Wave(left = as.numeric(segment@left), 
                     samp.rate = audio@samp.rate, 
                     bit = 16, pcm = TRUE),
                filename = segment_file, extensible = TRUE)
      
      segments[[length(segments) + 1]] <- list(
        file = segment_file,
        task = "CHECKBOX",
        trial = i,
        start_time = start_sec,
        end_time = end_sec,
        duration = end_sec - start_sec
      )
    }
  }
  
  # ============================================================================
  # HI - 5 trials
  # ============================================================================
  hi_rows <- template %>% filter(task == "HI")
  if (nrow(hi_rows) > 0) {
    logdebug(sprintf("HI: %d trials", nrow(hi_rows)))
    
    for (i in 1:nrow(hi_rows)) {
      row <- hi_rows[i, ]
      
      # Extract window around expected response (2 seconds after hearing "hi")
      start_sec <- row$seconds
      end_sec <- start_sec + 2.0
      
      segment <- extractWave(audio, from = start_sec, to = end_sec, xunit = "time")
      
      segment_file <- file.path(output_dir, 
                                sprintf("%s_task-HI_trial-%d.wav", 
                                        participant_id, i))
      
      writeWave(Wave(left = as.numeric(segment@left), 
                     samp.rate = audio@samp.rate, 
                     bit = 16, pcm = TRUE),
                filename = segment_file, extensible = TRUE)
      
      segments[[length(segments) + 1]] <- list(
        file = segment_file,
        task = "HI",
        trial = i,
        start_time = start_sec,
        end_time = end_sec,
        duration = end_sec - start_sec
      )
    }
  }
  
  # ============================================================================
  # WORD_ASSOC - 18 prompts
  # ============================================================================
  word_assoc_rows <- template %>% filter(task == "WORD_ASSOC")
  if (nrow(word_assoc_rows) > 0) {
    logdebug(sprintf("WORD_ASSOC: %d prompts", nrow(word_assoc_rows)))
    
    for (i in 1:nrow(word_assoc_rows)) {
      row <- word_assoc_rows[i, ]
      
      # 15-second response window
      start_sec <- row$seconds
      end_sec <- start_sec + cfg$task_settings$WORD_ASSOC$response_window_sec
      
      segment <- extractWave(audio, from = start_sec, to = end_sec, xunit = "time")
      
      # Clean prompt for filename
      prompt_clean <- str_replace_all(row$prompt, "[^a-zA-Z0-9]", "-")
      prompt_clean <- str_trunc(prompt_clean, width = 30, ellipsis = "")
      
      segment_file <- file.path(output_dir, 
                                sprintf("%s_task-WORD-ASSOC_type-%s_subtype-%d_prompt-%s.wav", 
                                        participant_id, row$type, row$subtype, prompt_clean))
      
      writeWave(Wave(left = as.numeric(segment@left), 
                     samp.rate = audio@samp.rate, 
                     bit = 16, pcm = TRUE),
                filename = segment_file, extensible = TRUE)
      
      segments[[length(segments) + 1]] <- list(
        file = segment_file,
        task = "WORD_ASSOC",
        prompt = row$prompt,
        type = row$type,
        subtype = row$subtype,
        trial = i,
        start_time = start_sec,
        end_time = end_sec,
        duration = end_sec - start_sec
      )
    }
  }
  
  # ============================================================================
  # WMEMORY - 4 recall trials
  # ============================================================================
  wmemory_rows <- template %>% filter(task == "WMEMORY")
  if (nrow(wmemory_rows) > 0) {
    logdebug(sprintf("WMEMORY: %d recall trials", nrow(wmemory_rows)))
    
    for (i in 1:nrow(wmemory_rows)) {
      row <- wmemory_rows[i, ]
      
      # 5-second window for recall response
      start_sec <- row$seconds
      end_sec <- start_sec + 5.0
      
      segment <- extractWave(audio, from = start_sec, to = end_sec, xunit = "time")
      
      segment_file <- file.path(output_dir, 
                                sprintf("%s_task-WMEMORY_trial-%d.wav", 
                                        participant_id, i))
      
      writeWave(Wave(left = as.numeric(segment@left), 
                     samp.rate = audio@samp.rate, 
                     bit = 16, pcm = TRUE),
                filename = segment_file, extensible = TRUE)
      
      segments[[length(segments) + 1]] <- list(
        file = segment_file,
        task = "WMEMORY",
        trial = i,
        start_time = start_sec,
        end_time = end_sec,
        duration = end_sec - start_sec
      )
    }
  }
  
  # ============================================================================
  # SENT_REP - 3 sentences (easy, medium, hard)
  # ============================================================================
  sent_rep_sentences <- cfg$ground_truth$sent_rep_sentences
  
  for (i in seq_along(sent_rep_sentences)) {
    sent <- sent_rep_sentences[[i]]
    
    # Extract response window (participant repeating sentence)
    start_sec <- sent$response_window_start
    end_sec <- sent$response_window_end
    
    segment <- extractWave(audio, from = start_sec, to = end_sec, xunit = "time")
    
    segment_file <- file.path(output_dir, 
                              sprintf("%s_task-SENT-REP_difficulty-%s_trial-%d.wav", 
                                      participant_id, sent$difficulty, i))
    
    writeWave(Wave(left = as.numeric(segment@left), 
                   samp.rate = audio@samp.rate, 
                   bit = 16, pcm = TRUE),
              filename = segment_file, extensible = TRUE)
    
    segments[[length(segments) + 1]] <- list(
      file = segment_file,
      task = "SENT_REP",
      difficulty = sent$difficulty,
      target_sentence = sent$sentence,
      trial = i,
      start_time = start_sec,
      end_time = end_sec,
      duration = end_sec - start_sec
    )
  }
  
  logdebug(sprintf("SENT_REP: %d sentences", length(sent_rep_sentences)))
  
  # ============================================================================
  # READING - Entire 60-second block (50 words)
  # ============================================================================
  reading_words <- cfg$ground_truth$reading_words
  start_sec <- (template[template$task=="READING"&!is.na(template$task),]$seconds)[1]
  end_sec <- (template[template$task=="READING"&!is.na(template$task),]$seconds)[length(reading_words)] + 2  # Last word + buffer
  
  segment <- extractWave(audio, from = start_sec, to = end_sec, xunit = "time")
  
  segment_file <- file.path(output_dir, 
                            sprintf("%s_task-READING_full.wav", participant_id))
  
  writeWave(Wave(left = as.numeric(segment@left), 
                 samp.rate = audio@samp.rate, 
                 bit = 16, pcm = TRUE),
            filename = segment_file, extensible = TRUE)
  
  segments[[length(segments) + 1]] <- list(
    file = segment_file,
    task = "READING",
    trial = 1,
    start_time = start_sec,
    end_time = end_sec,
    duration = end_sec - start_sec
  )
  
  logdebug(sprintf("READING: 1 segment (%d words expected)", length(reading_words)))
  
  return(segments)
}



## alignment to task audio
align_audio <- function(audio_signal, sample_rate, template_file) {
  # Read template 
  template <- readMP3(template_file)
  template_signal <- template@left
  
  # Cross-correlation to find template in participant audio
  correlation <- ccf(audio_signal, template_signal, plot = T)
  max_corr_idx <- which.max(correlation$acf)
  offset <- correlation$lag[max_corr_idx] / sample_rate
  return(offset)
}

## detect responses
# Detect responses
detect_responses <- function(audio_signal, sample_rate, event_times, response_type = "check") {
  responses <- data.frame()
  
  for (i in 1:length(event_times)) {
    event_time <- event_times[i]
    
    # Extract audio segment after event (e.g., 0-3 seconds after)
    start_sample <- round(event_time * sample_rate)
    end_sample <- round((event_time + 5) * sample_rate)
    segment <- audio_signal[start_sample:min(end_sample, length(audio_signal))]
    
    if (response_type == "check") {
      # Detect "check" responses (look for specific phonetic patterns)
      response_detected <- detect_word(segment, sample_rate, "check")
    } else {
      # Detect "hi" responses
      response_detected <- detect_word(segment, sample_rate, response_type)
    }
    
    if (!is.na(response_detected)) {
      response_time <- event_time + response_detected
      responses <- rbind(responses, data.frame(
        event_index = i,
        stimulus_time = event_time,
        response_time = response_time,
        reaction_time = response_detected
      ))
    }
  }
  return(responses)
}

# Simple word detection using energy peaks
detect_word <- function(segment, sample_rate, word) {
  # Calculate short-term energy
  frame_length <- 0.005 * sample_rate  # 25ms frames
  energy <- sapply(seq(1, length(segment) - frame_length, by = frame_length/2),
                   function(i) mean(abs(segment[i:min(i+frame_length, length(segment))])))
  
  # Find energy peaks (simplified approach)
  peaks <- which(diff(sign(diff(energy))) < 0) + 1
  if (length(peaks) > 0) {
    first_peak <- peaks[1] * (frame_length/2) / sample_rate
    return(first_peak)
  }
  return(NA)
}
