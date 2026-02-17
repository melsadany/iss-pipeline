#!/bin/bash

###  **Verification Script**

set -euo pipefail

echo "========================================"
echo "PSVC Pipeline - Installation Verification"
echo "========================================"

# Check Docker
echo "[1/5] Checking Docker..."
if ! command -v docker &> /dev/null; then
    echo "❌ Docker not found. Please install Docker first."
    exit 1
fi
echo "✓ Docker found: $(docker --version)"

# Check image
echo "[2/5] Checking Docker image..."
if ! docker image inspect psvc-pipeline:latest &> /dev/null; then
    echo "❌ Image psvc-pipeline:latest not found."
    echo "   Run: docker build -t psvc-pipeline:latest ."
    exit 1
fi
echo "✓ Image found"

# Check conda environments
echo "[3/5] Verifying conda environments..."
docker run --rm psvc-pipeline:latest \
  /bin/bash -c "/opt/conda/bin/conda env list" | grep -q "whisperx_env"
echo "✓ whisperx_env found"

docker run --rm psvc-pipeline:latest \
  /bin/bash -c "/opt/conda/bin/conda env list" | grep -q "pwesuite_env"
echo "✓ pwesuite_env found"

docker run --rm psvc-pipeline:latest \
  /bin/bash -c "/opt/conda/bin/conda env list" | grep -q "r_pipeline_env"
echo "✓ r_pipeline_env found"

# Check WhisperX import
echo "[4/5] Testing WhisperX import..."
docker run --rm psvc-pipeline:latest \
  /opt/conda/bin/conda run -n whisperx_env python -c "import whisperx; print('✓ WhisperX OK')"

# Check R packages
echo "[5/5] Testing R packages..."
docker run --rm psvc-pipeline:latest \
  /opt/conda/bin/conda run -n r_pipeline_env Rscript -e "library(tidyverse); library(logger); cat('✓ R packages OK\n')"

echo "========================================"
echo "✓ All checks passed!"
echo "========================================"
echo ""
echo "Next steps:"
echo "  1. Place audio files in test_data/"
echo "  2. Run: ./scripts/run_example.sh"
