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
#
# REVIEWER SUPPORT TEMPORARILY DISABLED — see commented blocks below.
# Re-enable by uncommenting the --review-dir / --no-review / --force-auto-cleanup
# option parsing and the corresponding run_stage_3 branches.
# ============================================================

PARTICIPANT_ID=${1:-"TEST0001"}
AUDIO_FILE=${2:-"/input/test.mp3"}
CONFIG=${3:-"/app/config/task_template.yaml"}
OUTPUT_DIR="/app/output"
REFERENCE_DIR="/app/reference_data"

# Defaults
STAGE="all"
WHISPER_MODEL_OVERRIDE=""
# REVIEWER DISABLED:
# REVIEW_DIR_OVERRIDE=""
# FORCE_AUTO_CLEANUP=0

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
    # REVIEWER DISABLED: option parsing commented out
    # --review-dir)
    #   REVIEW_DIR_OVERRIDE="$2"
    #   shift 2
    #   ;;
    # --no-review|--force-auto-cleanup)
    #   FORCE_AUTO_CLEANUP=1
    #   shift
    #   ;;
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
  echo "  [Stage 3] Auto cleanup mode — reviewer files disabled."

  # REVIEWER DISABLED: all reviewer-file detection / staging commented out.
  # To re-enable, uncomment the blocks below and restore FORCE_AUTO_CLEANUP
  # / REVIEW_DIR_OVERRIDE logic above.
  #
  # RSCRIPT_EXTRA_ARGS=()
  # if [[ "$FORCE_AUTO_CLEANUP" -eq 1 ]]; then
  #   echo "  [Stage 3] no-reviewer mode — automatic cleanup from stage 2 output"
  #   RSCRIPT_EXTRA_ARGS+=("--no_review")
  # elif [[ -n "$REVIEW_DIR_OVERRIDE" ]]; then
  #   echo "  [Stage 3] Using supplied review directory: $REVIEW_DIR_OVERRIDE"
  #   RSCRIPT_EXTRA_ARGS+=("--review_dir" "$REVIEW_DIR_OVERRIDE")
  # else
  #   DEFAULT_REVIEW_DIR="$OUTPUT_DIR/review_files"
  #   STAGED_REVIEW_DIR="$OUTPUT_DIR/review_files_staged"
  #   REVIEW_FILES_COUNT=$(find "$DEFAULT_REVIEW_DIR" -maxdepth 1 \
  #     -name "${PARTICIPANT_ID}_review_*.tsv" \
  #     ! -name "*_consensus*" 2>/dev/null | wc -l)
  #   if [[ "$REVIEW_FILES_COUNT" -gt 0 ]]; then
  #     echo "  [Stage 3] Found $REVIEW_FILES_COUNT review file(s) in review_files/ — using reviewer consensus."
  #     RSCRIPT_EXTRA_ARGS+=("--review_dir" "$DEFAULT_REVIEW_DIR")
  #   else
  #     STAGED_FILES_COUNT=$(find "$STAGED_REVIEW_DIR" -maxdepth 1 \
  #       -name "${PARTICIPANT_ID}_review_*.tsv" \
  #       ! -name "*_consensus*" 2>/dev/null | wc -l)
  #     if [[ "$STAGED_FILES_COUNT" -gt 0 ]]; then
  #       echo "  [Stage 3] Found $STAGED_FILES_COUNT review file(s) in review_files_staged/ — using staged reviewer files."
  #       RSCRIPT_EXTRA_ARGS+=("--review_dir" "$STAGED_REVIEW_DIR")
  #     else
  #       echo "  [Stage 3] No review files found — running automatic cleanup."
  #     fi
  #   fi
  # fi

  /opt/conda/bin/conda run --no-capture-output -n r_pipeline_env \
    Rscript /app/scripts/run_03_transcription_cleanup.R \
      --id   "$PARTICIPANT_ID" \
      --transcription_file "$OUTPUT_DIR/transcriptions/$PARTICIPANT_ID/${PARTICIPANT_ID}_all_transcriptions.tsv" \
      --mode   auto \
      --config "$CONFIG" \
      --reference "$REFERENCE_DIR" \
      --output    "$OUTPUT_DIR" \
      --no_review
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
