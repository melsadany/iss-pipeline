#!/usr/bin/env python
"""
02_transcription.py
ISS Task Transcription using WhisperX

CLI flags:
  --whisper_model     overrides config["transcription"]["whisper_model"]
  --full_audio_file   path to the original (uncut) audio file; when supplied
                      the full recording is transcribed first and saved as
                      <participant_id>_full_audio_whisperX.tsv
"""

import argparse
import os
import glob
import yaml
import pandas as pd
from pathlib import Path
import sys

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
    """Transcribe a single audio file and return a word-level DataFrame."""
    audio = whisperx.load_audio(audio_file)
    result = model.transcribe(audio, language=language)
    result_aligned = whisperx.align(
        result["segments"],
        align_model,
        metadata,
        audio,
        device
    )
    words = []
    for segment in result_aligned["segments"]:
        for word in segment.get("words", []):
            words.append({
                "start":      word.get("start", ""),
                "end":        word.get("end", ""),
                "word":       word.get("word", "").strip(),
                "confidence": word.get("score", None)
            })
    return pd.DataFrame(words)

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--participant_id",  required=True)
    parser.add_argument("--audio_dir",       required=True)
    parser.add_argument("--config",          required=True)
    parser.add_argument("--output_dir",      required=True)
    parser.add_argument(
        "--whisper_model", default=None,
        help="Override the whisper model name from config (e.g. small, medium, large-v3)"
    )
    parser.add_argument(
        "--full_audio_file", default=None,
        help="Path to the original uncut audio file. When supplied the full "
             "recording is also transcribed and saved as "
             "<participant_id>_full_audio_whisperX.tsv."
    )
    args = parser.parse_args()

    config = load_config(args.config)
    os.makedirs(args.output_dir, exist_ok=True)

    device     = config["transcription"].get("device", "cpu")
    model_name = args.whisper_model or config["transcription"]["whisper_model"]
    language   = config["transcription"]["language"]

    print(f"Loading WhisperX model: {model_name} on {device}")
    model = whisperx.load_model(model_name, device, compute_type="float32")
    align_model, align_metadata = whisperx.load_align_model(
        language_code=language, device=device
    )

    # ── Full-audio transcription (optional) ──────────────────────────────────
    if args.full_audio_file and os.path.isfile(args.full_audio_file):
        print(f"\nTranscribing full audio: {args.full_audio_file}")
        try:
            df_full = transcribe_segment(
                args.full_audio_file, model, align_model,
                align_metadata, device, language
            )
            df_full["participant_id"] = args.participant_id
            df_full["audio_file"]     = Path(args.full_audio_file).stem
            out_full = os.path.join(
                args.output_dir,
                f"{args.participant_id}_full_audio_whisperX.tsv"
            )
            df_full.to_csv(out_full, sep="\t", index=False)
            print(f"\u2713 Full-audio transcription saved: {out_full}")
            print(f"  Words: {len(df_full)}")
        except Exception as e:
            print(
                f"  WARNING: full-audio transcription failed: {e}",
                file=sys.stderr
            )
    elif args.full_audio_file:
        print(
            f"WARNING: --full_audio_file '{args.full_audio_file}' not found; "
            "skipping full-audio transcription.",
            file=sys.stderr
        )

    # ── Per-segment transcription ─────────────────────────────────────────────
    audio_files = sorted(glob.glob(os.path.join(args.audio_dir, "*.wav")))
    print(f"\nFound {len(audio_files)} cropped audio segments")

    all_transcriptions = []

    for audio_file in audio_files:
        filename = Path(audio_file).stem
        print(f"  Transcribing: {filename}")
        try:
            df = transcribe_segment(
                audio_file, model, align_model,
                align_metadata, device, language
            )
            file_meta = parse_iss_filename(filename)
            for key, value in file_meta.items():
                df[key] = value
            df["audio_file"] = filename
            all_transcriptions.append(df)

            output_file = os.path.join(args.output_dir, f"{filename}_whisperX.tsv")
            df.to_csv(output_file, sep="\t", index=False)
        except Exception as e:
            print(f"  ERROR: {e}")
            continue

    if all_transcriptions:
        combined = pd.concat(all_transcriptions, ignore_index=True)
        output_combined = os.path.join(
            args.output_dir,
            f"{args.participant_id}_all_transcriptions.tsv"
        )
        combined.to_csv(output_combined, sep="\t", index=False)
        print(f"\u2713 Saved: {output_combined}")
        print(f"\u2713 Total words (segments): {len(combined)}")
    else:
        print("\u2717 No segment transcriptions generated")

if __name__ == "__main__":
    main()
