#!/usr/bin/env python
"""
02_transcription.py
ISS Task Transcription using WhisperX
"""

import argparse
import os
import glob
import yaml
import pandas as pd
from pathlib import Path
import sys

# Setup for WhisperX
os.environ['TORCH_FORCE_NO_WEIGHTS_ONLY_LOAD'] = '1'

try:
    import torch.serialization
    from omegaconf.listconfig import ListConfig
    from omegaconf.dictconfig import DictConfig
    from omegaconf.base import ContainerMetadata
    torch.serialization.add_safe_globals([ListConfig, DictConfig, ContainerMetadata])
except ImportError as e:
    print(f"Warning: {e}", file=sys.stderr)

import whisperx

def load_config(config_path):
    with open(config_path) as f:
        return yaml.safe_load(f)

def parse_iss_filename(filename):
    """Parse ISS filename to extract metadata"""
    parts = Path(filename).stem.split("_")
    metadata = {'participant_id': parts[0]}
    
    for part in parts[1:]:
        if "-" in part:
            key, value = part.split("-", 1)
            metadata[key] = value
    
    return metadata

def transcribe_segment(audio_file, model, align_model, metadata, device, language):
    """Transcribe a single audio segment"""
    audio = whisperx.load_audio(audio_file)
    result = model.transcribe(audio, language=language)
    
    # Align
    result_aligned = whisperx.align(
        result["segments"],
        align_model,
        metadata,
        audio,
        device
    )
    
    # Extract words
    words = []
    for segment in result_aligned["segments"]:
        for word in segment.get("words", []):
            words.append({
                "start": word.get("start", ""),
                "end": word.get("end", ""),
                "word": word.get("word", "").strip(),
                "confidence": word.get("score", None)
            })
    
    return pd.DataFrame(words)

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--participant_id", required=True)
    parser.add_argument("--audio_dir", required=True)
    parser.add_argument("--config", required=True)
    parser.add_argument("--output_dir", required=True)
    args = parser.parse_args()
    
    config = load_config(args.config)
    os.makedirs(args.output_dir, exist_ok=True)
    
    # Load models
    device = config["transcription"].get("device", "cpu")
    model_name = config["transcription"]["whisper_model"]
    language = config["transcription"]["language"]
    
    print(f"Loading WhisperX model: {model_name} on {device}")
    model = whisperx.load_model(model_name, device, compute_type="float32")
    align_model, metadata = whisperx.load_align_model(language_code=language, device=device)
    
    # Find all audio segments
    audio_files = sorted(glob.glob(os.path.join(args.audio_dir, "*.wav")))
    print(f"Found {len(audio_files)} audio segments")
    
    all_transcriptions = []
    
    for audio_file in audio_files:
        filename = Path(audio_file).stem
        print(f"  Transcribing: {filename}")
        
        try:
            df = transcribe_segment(audio_file, model, align_model, metadata, device, language)
            
            # Parse metadata from filename
            file_meta = parse_iss_filename(filename)
            
            for key, value in file_meta.items():
                df[key] = value
            
            df["audio_file"] = filename
            
            all_transcriptions.append(df)
            
            # Save individual file
            output_file = os.path.join(args.output_dir, f"{filename}_whisperX.tsv")
            df.to_csv(output_file, sep="\t", index=False)
            
        except Exception as e:
            print(f"  ERROR: {e}")
            continue
    
    # Combine all
    if all_transcriptions:
        combined = pd.concat(all_transcriptions, ignore_index=True)
        output_combined = os.path.join(args.output_dir, f"{args.participant_id}_all_transcriptions.tsv")
        combined.to_csv(output_combined, sep="\t", index=False)
        print(f"✓ Saved: {output_combined}")
        print(f"✓ Total words: {len(combined)}")
    else:
        print("✗ No transcriptions generated")

if __name__ == "__main__":
    main()
