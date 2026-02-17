#!/usr/bin/env python
"""
WhisperX wrapper for PSVC pipeline
Runs in whisperx_env conda environment
"""

import os
import sys

# CORRECT: Use TORCH_FORCE_NO_WEIGHTS_ONLY_LOAD=1 to force weights_only=False
os.environ['TORCH_FORCE_NO_WEIGHTS_ONLY_LOAD'] = '1'

# Also try to add safe globals for omegaconf classes
try:
    import torch.serialization
    from omegaconf.listconfig import ListConfig
    from omegaconf.base import ContainerMetadata
    torch.serialization.add_safe_globals([ListConfig, ContainerMetadata])
    print("Added omegaconf classes to safe globals", file=sys.stderr)
except ImportError as e:
    print(f"Warning: Could not add safe globals: {e}", file=sys.stderr)

import whisperx
import csv
import argparse

def main():
    # Parse command line arguments
    parser = argparse.ArgumentParser(description="Transcribe and align audio with WhisperX.")
    parser.add_argument("audio", type=str, help="Path to the input audio file.")
    parser.add_argument("output", type=str, help="Path to the output TSV file.")
    parser.add_argument("model", type=str, help="Whisper model to use (e.g., 'base', 'medium').")
    parser.add_argument("language", type=str, help="Language of the audio (e.g., 'en').")
    parser.add_argument("device", type=str, help="Device: 'cpu' or 'cuda'.")
    
    args = parser.parse_args()
    
    # Load WhisperX model
    print(f"Loading model: {args.model} on {args.device}")
    model = whisperx.load_model(args.model, args.device, compute_type="float32")
    
    # Load and transcribe audio
    print(f"Loading audio: {args.audio}")
    audio = whisperx.load_audio(args.audio)
    
    print("Transcribing...")
    result = model.transcribe(audio, language=args.language)
    
    # Load alignment model and align
    print("Aligning...")
    align_model, metadata = whisperx.load_align_model(
        language_code=args.language, 
        device=args.device
    )
    result_aligned = whisperx.align(
        result["segments"], 
        align_model, 
        metadata, 
        audio, 
        args.device
    )
    
    # Save as TSV
    print(f"Saving to: {args.output}")
    with open(args.output, "w", newline='') as f:
        writer = csv.writer(f, delimiter='\t')
        writer.writerow(["start", "end", "word"])
        
        for segment in result_aligned["segments"]:
            for word in segment.get("words", []):
                writer.writerow([
                    word.get("start", ""),
                    word.get("end", ""),
                    word.get("word", "")
                ])
    
    print("Done!")

if __name__ == "__main__":
    main()
