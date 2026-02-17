# Pipeline for Iowa Speech Sample (ISS) v.1.10.B

[![Docker](https://img.shields.io/badge/docker-ready-blue)](https://www.docker.com/)
[![License](https://img.shields.io/badge/license-MIT-green)](LICENSE)

A fully containerized audio‑to‑feature pipeline designed for the **Iowa Speech Sample (ISS)**, version 1.10.B.  
The pipeline processes recorded task audio, performs transcription with **WhisperX**, cleans the transcripts, and extracts a rich set of **linguistic, temporal, semantic, phonetic, and acoustic features** – all orchestrated via Docker.

For a detailed description of the ISS task design, please refer to the provided PDF:  
📄 **[ISS_task_design.pdf](config/ISS_task_design.pdf)**

---

## Table of Contents

- [Overview](#overview)
- [Features Extracted](#features-extracted)
- [Requirements](#requirements)
- [Quick Start](#quick-start)
- [Configuration](#configuration)
- [Reference Data](#reference-data)
- [Outputs](#outputs)
- [License & Citation](#license--citation)

---

## Overview

The **ISS (Iowa Speech Sample) Pipeline** automates the analysis of audio recordings from the ISS cognitive‑linguistic assessment. It:

- Segments audio using a precise task template (CSV timestamps).
- Transcribes each segment with **WhisperX** (word‑level alignment).
- Cleans transcripts by removing fillers, repetitions, and low‑confidence words.
- Extracts **per‑word** and **per‑prompt** features:
  - **General linguistic** (AoA, familiarity, concreteness, sentiment)
  - **Temporal** (response latencies, inter‑word intervals, burstiness)
  - **Semantic embedding** (sentence‑transformers, archetypes, spatial anchors)
  - **Phonetic embedding** (PWE, archetypes, norms)
  - **Task‑specific** (accuracy scores for memory, sentence repetition, reading)

All computations run inside isolated **conda environments** (`whisperx_env`, `pwesuite_env`, `r_pipeline_env`) within a single Docker image – no manual installation needed.

---

## Features Extracted

The pipeline outputs three levels of aggregated data:

| Level            | Description                                                                 |
|------------------|-----------------------------------------------------------------------------|
| **Per‑prompt**   | Features for each prompt.                                                   |
| **Per‑task**     | Averages across prompts, grouped by task type (e.g., letter, lexical).     |
| **Per‑participant** | Single‑row summary for the entire session.                                 |

### Task‑Specific Metrics

- **CHECKBOX & HI** – hit rate, response latency.
- **WORD_ASSOC** – constraint adherence, semantic/phonetic diversity, archetype switches, productivity slope, bins statistics, etc.
- **WMEMORY** – recall accuracy (exact match, Levenshtein, phonological similarity, recall order).
- **SENT_REP** – word‑level and bigram accuracy, word order errors.
- **READING** – correctness per word, reading latency by valence.

### Embedding‑Based Features

- **Semantic** (384‑d from `all‑MiniLM‑L6‑v2`):  
  - Similarity to prompt  
  - Pairwise similarities (consecutive / all pairs)  
  - Path length, TSP divergence  
  - Archetype assignment (k=10) and area  
  - Norms (distance from origin / anchors)  
  - Similarity to config‑defined **spatial anchors** (visual, spatial, reasoning)

- **Phonetic** (300‑d from PWESuite RNN metric learning model):  
  - Same family of metrics (similarities, archetypes, norms)  
  - High‑variance dimensions pre‑selected


---

## Requirements

- **Docker** (version 20.10+ recommended)
- At least **8 GB RAM** (16 GB preferred for WhisperX large‑v3)
- **Storage**: ~10 GB for the Docker image + reference data

No local installation of Python, R, or CUDA is required – everything runs inside the container.

---

## Quick Start

1. **Clone the repository** (or download the code and reference data):
   ```bash
   git clone https://github.com/melsadany/iss-pipeline.git
   cd psvc-pipeline
   ```

2. **Place your ISS audio file** (MP3 or WAV) in `test_data/` (create the folder if needed):
   ```bash
   mkdir -p test_data
   cp /path/to/your/recording.mp3 test_data/
   ```

3. **Build the Docker image**:
   ```bash
   docker build -t iss-pipeline:latest .
   ```

4. **Run the pipeline** (replace with participant files and information):
   ```bash
   ./scripts/run_example.sh
   ```

By default, this uses `test_data/test.mp3`. Edit the script to point to your file.

5. **Find the results** in `output/features/.` The main per‑participant file is:
   ```bash
   output/features/TEST0001_per_participant.csv
   ```

---
   
## Configuration

All pipeline parameters are controlled by a single YAML file: `config/task_template.yaml`.
Key sections:

  - task_settings: response windows, valid words for CHECKBOX/HI, constraint rules for WORD_ASSOC.

  - transcription: WhisperX model name (`large‑v3`), confidence thresholds, filler lists.

  - ground_truth: target sentences for SENT_REP and the reading word list with valence.

  - features: switches to enable/disable acoustic, semantic, phonetic, linguistic, and temporal feature groups.

  - spatial_anchors: user‑defined word lists for similarity calculations (visual, spatial, reasoning).

  - output: what to save (per‑trial, per‑task, per‑participant).

Edit this file to adapt the pipeline to different task versions or to enable/disable specific feature sets.   

---

## Reference Data

The pipeline relies on pre‑computed resources located in `reference_data/.` You must provide:

  - `task_metadata/ISS_v_1.10_B_task_metadata.csv` – the ISS template with timestamps for each task prompt.

  - Linguistic norms (AoA, GPT familiarity, MRC, concreteness) – see `00_initialize.R` for expected filenames.

  - Pre‑computed embeddings (e`mbeddings/semantic_common_50k.rds`, `phonetic_common_50k.rds`) – or they will be computed on‑the‑fly (slower).

  - Pre‑computed archetypes (`archetypes/semantic_common_50k.rds`, `phonetic_common_50k.rds`) – generated by 03_precompute_archetypes.R.

If these files are missing, the pipeline will attempt to create them, but this requires significant time and resources. It is strongly recommended to run `03_precompute_archetypes.R` once on your reference word list (e.g., the 50k most common English words) before processing participants.

---

## Outputs

After a successful run, the `output/` directory contains:

```text
output/
├── cropped_audio/
│   └── <participant_id>/          # WAV segments
├── transcriptions/
│   └── <participant_id>/           # Raw WhisperX TSV files
├── review_files/
│   ├── <participant_id>_cleaned_transcription.tsv
│   └── (optionally) ..._REVIEW_REQUIRED.xlsx
└── features/
    ├── <participant_id>_transcription_cleaning_stats.rds
    ├── <participant_id>_tasks-minimal-features.rds
    ├── <participant_id>_per_prompt.rds
    ├── <participant_id>_per_task.rds
    ├── <participant_id>_per_participant.rds
    └── <participant_id>_all_features.rds
```

The most useful for downstream analysis are:

  - `*_per_participant.rds` – one row per participant with all aggregated scores.

  - `*_per_prompt.rds` – detailed data for each fluency prompt, useful for item‑level analysis.
  
  - `*_per_task.rds` – one row per task type per participant with all aggregated scores.

You can convert these RDS files to CSV using `readRDS()` in R or `pyreadr` in Python.

---

## License & Citation

This pipeline is released under the MIT License.
If you use it in your research, please cite:

    [hj]


Maintainer: Melsadany melsadany24@gmail.com
Repository: https://github.com/melsadany/iss-pipeline