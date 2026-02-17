# PSVC Audio Processing Pipeline

Docker-based automated pipeline for processing Phonemic and Semantic Verbal Cognition (PSVC) audio assessments, extracting cognitive and linguistic features from verbal fluency tasks.

## Table of Contents

- [Overview](#overview)
- [Features](#features)
- [System Requirements](#system-requirements)
- [Installation](#installation)
- [Quick Start](#quick-start)
- [Detailed Usage](#detailed-usage)
- [Configuration](#configuration)
- [Output Description](#output-description)
- [Pipeline Architecture](#pipeline-architecture)
- [Troubleshooting](#troubleshooting)
- [Development](#development)
- [Citation](#citation)
- [License](#license)

## Overview

The PSVC pipeline processes audio recordings of verbal cognitive assessments and extracts multi-dimensional features capturing semantic, phonetic, lexical, and temporal characteristics of speech. Designed for clinical and research applications in cognitive assessment.

### Supported Tasks

1. **WAT (Word Association Test)**: Semantic fluency with 20 prompts
2. **RAN (Rapid Automatic Naming)**: 4 rapid naming blocks
3. **COWAT (Controlled Oral Word Association Test)**: Phonemic fluency (letters S, A, C, F, L)
4. **Faces**: Face matching task (timing only, no transcription)

## Features

### Audio Processing
- Automatic audio segmentation based on precise task timings
- Support for MP3 and WAV formats
- Configurable task windows and timing adjustments

### Transcription
- State-of-the-art WhisperX for speech recognition
- Word-level time alignment
- Confidence scoring for quality control
- Multi-model support (base, small, medium, large-v3)

### Feature Extraction

**Semantic Features**
- BERT-based semantic embeddings
- Semantic similarity to spatial/reasoning anchor concepts
- Archetype-based semantic divergence patterns

**Phonetic Features**
- PWESuite phonetic word embeddings
- Phonetic similarity metrics
- Sound-based clustering analysis

**Lexical Features**
- Word frequency (SUBTLEX-US)
- Age of Acquisition (AoA) norms
- Lexical diversity (TTR, MTLD, vocd-D)
- Part-of-speech distributions
- Concreteness and imageability ratings

**Temporal Features**
- Inter-response times
- Response clustering patterns
- Semantic/phonetic switches
- Temporal dynamics and trajectory analysis

**Cognitive Patterns**
- Insight patterns (systematic exploration)
- Euclidean path length in semantic space
- Divergence from prototypical responses
- Task-specific violation detection

## System Requirements

### Minimum Requirements
- **OS**: Linux, macOS, or Windows with WSL2
- **CPU**: 4 cores
- **RAM**: 8 GB
- **Storage**: 15 GB free space
- **Docker**: Version 20.10 or higher

### Recommended Requirements
- **CPU**: 8+ cores
- **RAM**: 16 GB (for large-v3 Whisper model)
- **GPU**: NVIDIA GPU with CUDA support (optional, for faster transcription)
- **Storage**: 20+ GB SSD

### Software Dependencies
All dependencies are containerized. You only need:
- Docker (or Docker Desktop on Windows/Mac)

## Installation

### 1. Clone the Repository

```bash
git clone https://github.com/melsadany/psvc-pipeline
cd psvc-pipeline
```

### 2. Build the Docker Image

```bash
docker build -t psvc-pipeline:latest .
```

**Build time**: ~15-30 minutes (depending on internet speed and hardware)

### 3. Verify Installation

```bash
bash scripts/verify_installation.sh
```

This will check:
- Docker installation
- Conda environments (whisperx_env, pwesuite_env, r_pipeline_env)
- Python packages (whisperx, torch, transformers)
- R packages (tidyverse, logger, archetypes)

## Quick Start

### Basic Usage

```bash
docker run --rm \
  -v $(pwd)/test_data:/input \
  -v $(pwd)/output:/app/output \
  psvc-pipeline:latest \
  PARTICIPANT_001 \
  /input/my_audio.mp3
```

### With Custom Configuration

```bash
docker run --rm \
  -v $(pwd)/test_data:/input \
  -v $(pwd)/output:/app/output \
  -v $(pwd)/config:/app/config \
  psvc-pipeline:latest \
  PARTICIPANT_001 \
  /input/my_audio.mp3 \
  /app/config/custom_task_template.yaml
```

### Using GPU (if available)

```bash
docker run --rm --gpus all \
  -v $(pwd)/test_data:/input \
  -v $(pwd)/output:/app/output \
  psvc-pipeline:latest \
  PARTICIPANT_001 \
  /input/my_audio.mp3
```

**Note**: Edit `config/task_template.yaml` and change `device: "cpu"` to `device: "cuda"` before running.

## Detailed Usage

### Step-by-Step Pipeline Execution

#### 1. Prepare Your Data

Place audio files in `test_data/`:
```
test_data/
├── participant_001.mp3
├── participant_002.mp3
└── participant_003.wav
```

**Audio Requirements**:
- Format: MP3 or WAV
- Duration: ~12 minutes (full PSVC battery)
- Sample rate: 44.1 kHz or 48 kHz recommended
- Recording quality: Clear speech, minimal background noise

#### 2. Configure Task Timing

Edit `config/task_template.yaml` if your audio has different task timings:

```yaml
tasks:
  WAT:
    prompts:
      - {word: "fly", start_sec: 40, end_sec: 52}
      # ... adjust timing for each prompt
```

#### 3. Run Pipeline

```bash
# Process single participant
docker run --rm \
  -v $(pwd)/test_data:/input \
  -v $(pwd)/output:/app/output \
  psvc-pipeline:latest \
  PARTICIPANT_001 \
  /input/participant_001.mp3
```

#### 4. View Results

```bash
# View main output
cat output/features/PARTICIPANT_001_per_participant.csv

# View per-task features
cat output/features/PARTICIPANT_001_per_task.csv

# View per-prompt features
cat output/features/PARTICIPANT_001_per_prompt.csv
```

### Batch Processing

```bash
#!/bin/bash
for audio_file in test_data/*.mp3; do
  participant_id=$(basename "$audio_file" .mp3)
  docker run --rm \
    -v $(pwd)/test_data:/input \
    -v $(pwd)/output:/app/output \
    psvc-pipeline:latest \
    "$participant_id" \
    "/input/$(basename $audio_file)"
done
```

## Configuration

### Task Template (`config/task_template.yaml`)

```yaml
# Whisper model selection
transcription:
  whisper_model: "large-v3"  # Options: base, small, medium, large-v3
  language: "en"
  device: "cpu"              # Options: cpu, cuda
  confidence_threshold:
    strict: 0.9              # High confidence only
    review: 0.8              # Flag low-confidence for review
    auto: 0.0                # Accept all

# Feature extraction
features:
  semantic_embeddings:
    model: "text-embedding-bert"
  phonetic_embeddings:
    model: "PWE"
  spatial_anchors:
    visual: ["rotate", "pattern", "space", "shape", ...]
    spatial: ["above", "below", "left", "right", ...]
    reasoning: ["logic", "rule", "pattern", ...]
```

### Environment Variables

```bash
# Set processing mode
export PSVC_MODE="auto"  # Options: auto, review, strict

# Set device
export PSVC_DEVICE="cpu"  # Options: cpu, cuda

# Enable verbose logging
export PSVC_VERBOSE="true"
```

## Output Description

### Directory Structure

```
output/
├── cropped_audio/
│   └── PARTICIPANT_001/
│       ├── PARTICIPANT_001_task-1_fly_1.wav
│       ├── PARTICIPANT_001_task-1_face_2.wav
│       └── ...
├── transcriptions/
│   └── PARTICIPANT_001/
│       ├── PARTICIPANT_001_all_transcriptions.tsv
│       └── individual segment files...
├── review_files/
│   ├── PARTICIPANT_001_cleaned_transcription.tsv
│   └── PARTICIPANT_001_transcription_cleaning_stats.rds
└── features/
    ├── PARTICIPANT_001_per_prompt.csv        # Word-level features
    ├── PARTICIPANT_001_per_task.csv          # Task-level aggregates
    └── PARTICIPANT_001_per_participant.csv   # Summary features
```

### Feature Files

#### Per-Participant Features (`*_per_participant.csv`)
Summary statistics across all tasks:
- `n_words_total`: Total word count across all tasks
- `lexical_diversity_*`: Vocabulary richness measures
- `semantic_coherence_mean`: Average semantic relatedness
- `temporal_clustering_*`: Response timing patterns
- `cognitive_style_*`: Strategic approach indicators

#### Per-Task Features (`*_per_task.csv`)
Task-specific features:
- `task_type`: WAT, RAN, COWAT
- `n_words`: Word count for this task
- `mean_confidence`: Transcription quality
- `lexical_diversity`: Task-specific vocabulary richness
- `semantic_similarity_*`: Semantic feature aggregates
- `phonetic_similarity_*`: Phonetic feature aggregates

#### Per-Prompt Features (`*_per_prompt.csv`)
Individual prompt/trial responses:
- `prompt`: Stimulus word/letter
- `word`: Response word
- `start`, `end`: Word timing (seconds)
- `confidence`: Transcription confidence (0-1)
- `word_freq_log`: Log word frequency
- `aoa_kup_lem`: Age of acquisition
- `semantic_sim_*`: Similarity to anchor concepts
- `phonetic_sim_*`: Phonetic similarity scores

### Quality Metrics

Each run generates quality indicators:
- Mean transcription confidence
- Low-confidence word count
- Removed filler words
- Rule violation counts

## Pipeline Architecture

### Stage 1: Audio Preprocessing
- **Input**: Raw audio file (MP3/WAV)
- **Process**: Segment audio based on task timings from config
- **Output**: Individual WAV files per prompt/trial
- **Duration**: ~30 seconds

### Stage 2: Transcription (WhisperX)
- **Input**: Segmented audio files
- **Process**: 
  1. Speech-to-text transcription
  2. Word-level forced alignment
  3. Confidence scoring
- **Output**: Word-level transcriptions with timestamps
- **Duration**: ~5-10 minutes (CPU), ~1-2 minutes (GPU)

### Stage 3: Transcription Cleanup
- **Input**: Raw transcriptions
- **Process**:
  1. Remove filler words (um, uh, etc.)
  2. Filter repetitions
  3. Validate against lexical databases
  4. Flag low-confidence words
- **Output**: Cleaned transcription TSV
- **Duration**: ~30 seconds

### Stage 4: Feature Extraction
- **Input**: Cleaned transcriptions
- **Process**:
  1. Compute semantic embeddings (BERT)
  2. Compute phonetic embeddings (PWE)
  3. Calculate lexical features (frequency, AoA, diversity)
  4. Extract temporal patterns
  5. Identify cognitive strategies
- **Output**: Multi-level feature CSVs
- **Duration**: ~2-5 minutes

**Total Pipeline Time**: ~10-20 minutes per participant (CPU)

## Troubleshooting

### Common Issues

#### 1. Docker build fails with "connection timeout"

```bash
# Use a more stable mirror
docker build --network=host -t psvc-pipeline:latest .
```

#### 2. WhisperX import error: "No module named 'torch'"

This indicates conda environment activation failed. Verify:
```bash
docker run --rm psvc-pipeline:latest \
  /opt/conda/bin/conda env list
```

#### 3. Out of memory during transcription

Reduce model size in `config/task_template.yaml`:
```yaml
transcription:
  whisper_model: "base"  # Instead of large-v3
```

#### 4. Transcription has low accuracy

Check audio quality:
- Ensure clear speech recording
- Minimize background noise
- Use lossless or high-bitrate audio (128+ kbps)

Try increasing model size:
```yaml
transcription:
  whisper_model: "large-v3"
```

#### 5. Feature extraction fails: "Reference data not found"

Ensure reference data is mounted:
```bash
docker run --rm \
  -v $(pwd)/reference_data:/app/reference_data \
  ...
```

### Debug Mode

Enable verbose logging:
```bash
docker run --rm \
  -v $(pwd)/test_data:/input \
  -v $(pwd)/output:/app/output \
  -e PSVC_VERBOSE=true \
  psvc-pipeline:latest \
  PARTICIPANT_001 \
  /input/audio.mp3
```

### Access Container Shell

```bash
docker run --rm -it \
  -v $(pwd)/test_data:/input \
  -v $(pwd)/output:/app/output \
  --entrypoint /bin/bash \
  psvc-pipeline:latest
```

## Development

### Local Development Setup

```bash
# Clone repository
git clone https://github.com/melsadany/psvc-pipeline
cd psvc-pipeline

# Build with development tag
docker build -t psvc-pipeline:dev .

# Mount scripts for live editing
docker run --rm -it \
  -v $(pwd)/scripts:/app/scripts \
  -v $(pwd)/test_data:/input \
  -v $(pwd)/output:/app/output \
  --entrypoint /bin/bash \
  psvc-pipeline:dev
```

### Running Individual Stages

```bash
# Stage 1 only
docker run --rm \
  -v $(pwd)/scripts:/app/scripts \
  -v $(pwd)/test_data:/input \
  -v $(pwd)/output:/app/output \
  psvc-pipeline:dev \
  /opt/conda/bin/conda run -n r_pipeline_env \
  Rscript /app/scripts/run_01_audio_preprocessing.R \
  --audio /input/test.mp3 \
  --id TEST001 \
  --config /app/config/task_template.yaml \
  --output /app/output/cropped_audio
```

### Testing

```bash
# Run verification tests
bash scripts/verify_installation.sh

# Run example pipeline
bash scripts/run_example.sh

# Unit tests (if implemented)
docker run --rm psvc-pipeline:dev \
  /opt/conda/bin/conda run -n r_pipeline_env \
  Rscript tests/test_all.R
```

### Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## Citation

If you use this pipeline in your research, please cite:

```bibtex
@software{psvc_pipeline_2026,
  author = {Muhammad Elsadany},
  title = {PSVC Audio Processing Pipeline},
  year = {2026},
  url = {https://github.com/melsadany/psvc-pipeline},
  version = {1.0}
}
```

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- WhisperX: m-bain/whisperX on GitHub
- PWESuite: zouharvi/pwesuite on GitHub
- Sentence Transformers: UKPLab/sentence-transformers on GitHub
- Reference lexical databases: SUBTLEX-US, AoA norms

## Contact

- **Maintainer**: Muhammad Elsadany (melsadany24@gmail.com)
- **Institution**: University of Iowa, Department of Psychiatry
- **Issues**: GitHub Issues page for your repository

## Changelog

### Version 1.0 (February 2026)
- Initial release
- Support for WAT, RAN, COWAT, and Faces tasks
- WhisperX integration for transcription
- Multi-dimensional feature extraction
- Docker containerization

---

**Note**: This pipeline is intended for research purposes. For clinical applications, please ensure appropriate validation and compliance with relevant regulations.