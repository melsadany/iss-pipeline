#!/bin/bash
set -euo pipefail

# Configuration
PARTICIPANT_ID=${1:-"TEST0001"}
AUDIO_FILE=${2:-"/input/test.mp3"}
CONFIG=${3:-"/app/config/task_template.yaml"}
OUTPUT_DIR="/app/output"
REFERENCE_DIR="/app/reference_data"
MODE="auto"

echo "=========================================="
echo "PSVC Pipeline - Participant: $PARTICIPANT_ID"
echo "=========================================="

# Stage 1: Audio Preprocessing (R)
echo "[Stage 1] Audio Preprocessing..."
/opt/conda/bin/conda run --no-capture-output -n r_pipeline_env \
  Rscript /app/scripts/run_01_audio_preprocessing.R \
    --audio "$AUDIO_FILE" \
    --id "$PARTICIPANT_ID" \
    --config "$CONFIG" \
    --output "$OUTPUT_DIR/cropped_audio"

# Stage 2: Transcription (Python with WhisperX)
echo "[Stage 2] Transcription..."
/opt/conda/bin/conda run --no-capture-output -n whisperx_env \
  python /app/scripts/02_transcription.py \
    --participant_id "$PARTICIPANT_ID" \
    --audio_dir "$OUTPUT_DIR/cropped_audio/$PARTICIPANT_ID" \
    --config "$CONFIG" \
    --output_dir "$OUTPUT_DIR/transcriptions/$PARTICIPANT_ID"

# Stage 3: Transcription Cleanup (R)
echo "[Stage 3] Transcription Cleanup..."
/opt/conda/bin/conda run --no-capture-output -n r_pipeline_env \
  Rscript /app/scripts/run_03_transcription_cleanup.R \
    --id "$PARTICIPANT_ID" \
    --transcription_file "$OUTPUT_DIR/transcriptions/$PARTICIPANT_ID/${PARTICIPANT_ID}_all_transcriptions.tsv" \
    --mode "$MODE" \
    --config "$CONFIG" \
    --reference "$REFERENCE_DIR" \
    --output "$OUTPUT_DIR"

# Stage 4: Feature Extraction (R + Python wrappers)
echo "[Stage 4] Feature Extraction..."
/opt/conda/bin/conda run --no-capture-output -n r_pipeline_env \
  Rscript /app/scripts/run_04_feature_extraction.R \
    --id "$PARTICIPANT_ID" \
    --transcription_file "$OUTPUT_DIR/review_files/${PARTICIPANT_ID}_cleaned_transcription.tsv" \
    --config "$CONFIG" \
    --reference "$REFERENCE_DIR" \
    --output "$OUTPUT_DIR/features"

echo "=========================================="
echo "Pipeline Complete!"
echo "Results: $OUTPUT_DIR/features/${PARTICIPANT_ID}_per_participant.csv"
echo "=========================================="
