#!/bin/bash
set -euo pipefail

# Example run script
PARTICIPANT_ID="EXAMPLE001"
AUDIO_FILE="test_data/example_audio.mp3"

# Check if audio file exists
if [ ! -f "$AUDIO_FILE" ]; then
    echo "❌ Audio file not found: $AUDIO_FILE"
    echo "   Please place an audio file in test_data/"
    exit 1
fi

echo "Running pipeline for participant: $PARTICIPANT_ID"
echo "Audio file: $AUDIO_FILE"
echo ""

# Run pipeline
docker run --rm \
  -v $(pwd)/test_data:/input \
  -v $(pwd)/output:/app/output \
  -v $(pwd)/reference_data:/app/reference_data \
  -v $(pwd)/config:/app/config \
  psvc-pipeline:latest \
  $PARTICIPANT_ID \
  /input/$(basename $AUDIO_FILE)

echo ""
echo "✓ Pipeline complete!"
echo "Results in: output/features/${PARTICIPANT_ID}_per_participant.csv"
