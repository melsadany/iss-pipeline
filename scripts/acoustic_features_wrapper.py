#!/usr/bin/env python
"""
Acoustic Features Extraction using Parselmouth + openSMILE
Extracts prosodic features for each word-level segment
"""

import argparse
import pandas as pd
import parselmouth
from parselmouth.praat import call
import numpy as np

def extract_word_acoustics(audio_file, word_segments):
    """Extract acoustic features for each word segment"""
    sound = parselmouth.Sound(audio_file)
    
    features = []
    for idx, row in word_segments.iterrows():
        start = row['start']
        end = row['end']
        word = row['word']
        
        # Extract segment
        segment = sound.extract_part(from_time=start, to_time=end)
        
        # Pitch features
        pitch = segment.to_pitch()
        pitch_values = pitch.selected_array['frequency']
        pitch_values = pitch_values[pitch_values != 0]  # Remove unvoiced
        
        # Intensity
        intensity = segment.to_intensity()
        
        # Formants
        formants = segment.to_formant_burg()
        
        features.append({
            'word': word,
            'start': start,
            'end': end,
            'duration': end - start,
            'pitch_mean': np.mean(pitch_values) if len(pitch_values) > 0 else np.nan,
            'pitch_std': np.std(pitch_values) if len(pitch_values) > 0 else np.nan,
            'pitch_min': np.min(pitch_values) if len(pitch_values) > 0 else np.nan,
            'pitch_max': np.max(pitch_values) if len(pitch_values) > 0 else np.nan,
            'intensity_mean': call(intensity, "Get mean", 0, 0, "dB"),
            'intensity_std': call(intensity, "Get standard deviation", 0, 0),
            'f1_mean': call(formants, "Get mean", 1, start, end, "hertz"),
            'f2_mean': call(formants, "Get mean", 2, start, end, "hertz"),
            # Add jitter, shimmer, HNR...
        })
    
    return pd.DataFrame(features)
