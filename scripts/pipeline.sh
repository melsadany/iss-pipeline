#!/bin/bash
set -euo pipefail

# ============================================================
# ISS Pipeline — per-stage execution wrapper
#
# Usage:
#   pipeline.sh <PARTICIPANT_ID> <AUDIO_FILE> [CONFIG] [OPTIONS]
#
# Options:
#   --stage {1|2|3|4|all}   Which stage(s) to run (default: all)
#   --whisper-model <name>  Override whisper model in config
#                           (e.g. small, medium, large-v3)
#   --review-dir <path>     Directory of per-rater review TSV files to pass
#                           to stage 3 (multi-reviewer support). When omitted
#                           stage 3 looks in the default review_files/ folder.
#   --force-auto-cleanup    Force stage 3 to re-run automatic cleanup
#                           even if review files already exist
#
# Stage map:
#   1 = Audio Preprocessing (R)
#   2 = Transcription       (WhisperX / Python)
#   3 = Transcription Cleanup (R)
#   4 = Feature Extraction  (R + Python wrappers)
# ============================================================

PARTICIPANT_ID=${1:-"TEST0001"}
AUDIO_FILE=${2:-"/input/test.mp3"}
CONFIG=${3:-"/app/config/task_template.yaml"}
OUTPUT_DIR="/app/output"
REFERENCE_DIR="/app/reference_data"

# Defaults
STAGE="all"
WHISPER_MODEL_OVERRIDE=""
REVIEW_DIR_OVERRIDE=""
FORCE_AUTO_CLEANUP=0

# Parse extra options (starting from arg 4)
shift 3 2>/dev/null || true
while [[ $# -gt 0 ]]; do
  case "$1" in
    --stage)
      STAGE="$2"
      shift 2
      ;;
    --whisper-model)
      WHISPER_MODEL_OVERRIDE="$2"
      shift 2
      ;;
    --review-dir)
      REVIEW_DIR_OVERRIDE="$2"
      shift 2
      ;;
    --force-auto-cleanup)
      FORCE_AUTO_CLEANUP=1
      shift
      ;;
    *)
      shift
      ;;
  esac
done

echo "=========================================="
echo "ISS Pipeline"
echo "  Participant : $PARTICIPANT_ID"
echo "  Audio file  : $AUDIO_FILE"
echo "  Stage       : $STAGE"
if [[ -n "$WHISPER_MODEL_OVERRIDE" ]]; then
  echo "  Whisper     : $WHISPER_MODEL_OVERRIDE (override)"
fi
if [[ -n "$REVIEW_DIR_OVERRIDE" ]]; then
  echo "  Review dir  : $REVIEW_DIR_OVERRIDE"
fi
echo "=========================================="

# ---- helper: run_stage_1 ----
run_stage_1() {
  echo "[Stage 1] Audio Preprocessing..."
  /opt/conda/bin/conda run --no-capture-output -n r_pipeline_env \
    Rscript /app/scripts/run_01_audio_preprocessing.R \
      --audio "$AUDIO_FILE" \
      --id   "$PARTICIPANT_ID" \
      --config "$CONFIG" \
      --output "$OUTPUT_DIR/cropped_audio"

  RDS_SRC="$OUTPUT_DIR/cropped_audio/${PARTICIPANT_ID}/${PARTICIPANT_ID}_checkbox-hi-reading-timing-from-sound.rds"
  if [[ -f "$RDS_SRC" ]]; then
    mv "$RDS_SRC" "$OUTPUT_DIR/features/."
  fi
  echo "[Stage 1] Done."
}

# ---- helper: run_stage_2 ----
run_stage_2() {
  echo "[Stage 2] Transcription..."

  # Build whisper model arg — prefer CLI override, else read from config
  WHISPER_ARG=""
  if [[ -n "$WHISPER_MODEL_OVERRIDE" ]]; then
    WHISPER_ARG="--whisper_model $WHISPER_MODEL_OVERRIDE"
  fi

  /opt/conda/bin/conda run --no-capture-output -n whisperx_env \
    python /app/scripts/02_transcription.py \
      --participant_id "$PARTICIPANT_ID" \
      --audio_dir      "$OUTPUT_DIR/cropped_audio/$PARTICIPANT_ID" \
      --config         "$CONFIG" \
      --output_dir     "$OUTPUT_DIR/transcriptions/$PARTICIPANT_ID" \
      $WHISPER_ARG
  echo "[Stage 2] Done."
}

# ---- helper: run_stage_3 ----
run_stage_3() {
  echo "[Stage 3] Transcription Cleanup..."

  REVIEW_DIR_ARG=""

  if [[ "$FORCE_AUTO_CLEANUP" -eq 1 ]]; then
    # Explicit override: ignore all review files, run automatic cleanup
    echo "  [Stage 3] --force-auto-cleanup set — running automatic cleanup from scratch."
  elif [[ -n "$REVIEW_DIR_OVERRIDE" ]]; then
    # Caller supplied an explicit review directory (e.g. from desktop app staged dir)
    echo "  [Stage 3] Using supplied review directory: $REVIEW_DIR_OVERRIDE"
    REVIEW_DIR_ARG="--review_dir $REVIEW_DIR_OVERRIDE"
  else
    # Default: let run_03 look in the standard review_files/ output folder
    DEFAULT_REVIEW_DIR="$OUTPUT_DIR/review_files"
    REVIEW_FILES_COUNT=$(find "$DEFAULT_REVIEW_DIR" -maxdepth 1 \
      -name "${PARTICIPANT_ID}_review_*.tsv" \
      ! -name "*_consensus*" 2>/dev/null | wc -l)
    if [[ "$REVIEW_FILES_COUNT" -gt 0 ]]; then
      echo "  [Stage 3] Found $REVIEW_FILES_COUNT review file(s) in $DEFAULT_REVIEW_DIR — using reviewer consensus."
      REVIEW_DIR_ARG="--review_dir $DEFAULT_REVIEW_DIR"
    else
      echo "  [Stage 3] No review files found — running automatic cleanup."
    fi
  fi

  /opt/conda/bin/conda run --no-capture-output -n r_pipeline_env \
    Rscript /app/scripts/run_03_transcription_cleanup.R \
      --id   "$PARTICIPANT_ID" \
      --transcription_file "$OUTPUT_DIR/transcriptions/$PARTICIPANT_ID/${PARTICIPANT_ID}_all_transcriptions.tsv" \
      --mode   auto \
      --config "$CONFIG" \
      --reference "$REFERENCE_DIR" \
      --output    "$OUTPUT_DIR" \
      $REVIEW_DIR_ARG
  echo "[Stage 3] Done."
}

# ---- helper: run_stage_4 ----
run_stage_4() {
  echo "[Stage 4] Feature Extraction..."
  /opt/conda/bin/conda run --no-capture-output -n r_pipeline_env \
    Rscript /app/scripts/run_04_feature_extraction.R \
      --id   "$PARTICIPANT_ID" \
      --transcription_file "$OUTPUT_DIR/review_files/${PARTICIPANT_ID}_cleaned_transcription.tsv" \
      --config    "$CONFIG" \
      --reference "$REFERENCE_DIR" \
      --output    "$OUTPUT_DIR"
  echo "[Stage 4] Done."
}

# ---- dispatch ----
case "$STAGE" in
  1|"stage1") run_stage_1 ;;
  2|"stage2") run_stage_2 ;;
  3|"stage3") run_stage_3 ;;
  4|"stage4") run_stage_4 ;;
  "all")
    run_stage_1
    run_stage_2
    run_stage_3
    run_stage_4
    echo "=========================================="
    echo "Pipeline Complete!"
    echo "Results: $OUTPUT_DIR/features/${PARTICIPANT_ID}_per_participant.csv"
    echo "=========================================="
    ;;
  *)
    echo "ERROR: unknown --stage value '$STAGE'. Use 1, 2, 3, 4, or all." >&2
    exit 1
    ;;
esac
