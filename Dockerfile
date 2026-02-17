# Use Ubuntu 22.04 LTS as base - more stable than Debian testing
FROM ubuntu:22.04

LABEL maintainer="melsadany24@gmail.com"
LABEL description="PSVC Audio Processing Pipeline with WhisperX, PWESuite, and R"
LABEL version="1.0"
LABEL org.opencontainers.image.source="https://github.com/melsadany/psvc-pipeline"


# Set environment to avoid interactive prompts
ENV DEBIAN_FRONTEND=noninteractive

# Use a reliable mirror and force IPv4
RUN sed -i 's/archive.ubuntu.com/mirrors.ustc.edu.cn/g' /etc/apt/sources.list && \
    echo 'Acquire::ForceIPv4 "true";' > /etc/apt/apt.conf.d/99force-ipv4 && \
    echo 'Acquire::Retries "5";' > /etc/apt/apt.conf.d/80-retries

# Install basic packages first
RUN apt-get update && apt-get install -y \
    wget \
    curl \
    ca-certificates \
    gnupg \
    lsb-release

# Install system dependencies
RUN apt-get update && apt-get install -y \
    ffmpeg \
    libsndfile1 \
    git \
    build-essential \
    # R package system dependencies
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libgit2-dev \
    # For audio packages (tuneR, seewave)
    libasound2-dev \
    libfftw3-dev \
    # For igraph
    libglpk-dev \
    libgmp3-dev \
    # For geometry
    libqhull-dev \
    cmake \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install Miniconda manually
RUN wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O /tmp/miniconda.sh && \
    bash /tmp/miniconda.sh -b -p /opt/conda && \
    rm /tmp/miniconda.sh

# Add conda to PATH
ENV PATH="/opt/conda/bin:$PATH"


WORKDIR /app

# Copy environment files
COPY envs/whisperx.yml /tmp/whisperx.yml
COPY envs/pwesuite.yml /tmp/pwesuite.yml
COPY envs/r_pipeline.yml /tmp/r_pipeline.yml

# Accept conda Terms of Service
ENV CONDA_TOS_ACCEPT=yes

# Increase pip timeout and retries
RUN pip config set global.timeout 600 && \
    pip config set global.retries 10 && \
    pip config set global.default-timeout 600

# Create all conda environments
RUN conda tos accept --override-channels --channel https://repo.anaconda.com/pkgs/main && \
    conda tos accept --override-channels --channel https://repo.anaconda.com/pkgs/r && \
    conda env create -f /tmp/whisperx.yml && \
    conda env create -f /tmp/pwesuite.yml && \
    conda env create -f /tmp/r_pipeline.yml && \
    conda clean -afy

# verify whisperx installation
RUN /opt/conda/bin/conda run -n whisperx_env python -c "import torch; import whisperx; print('whisperx_env verified')" || echo "WARNING: whisperx_env verification failed"
    
# Clone PWESuite to /app/pwesuite and install
RUN cd /app && \
    git clone https://github.com/zouharvi/pwesuite.git && \
    /opt/conda/envs/pwesuite_env/bin/pip install -e /app/pwesuite

# Update pwe_wrapper.py to reference the correct path
ENV PYTHONPATH="/app/pwesuite"


# Install sentence-transformers in r_pipeline_env for semantic embeddings
RUN /opt/conda/envs/r_pipeline_env/bin/pip install \
    sentence-transformers \
    transformers \
    torch \
    vaderSentiment \
    textstat \
    pandas

# Install R packages via conda (pre-compiled)
RUN conda install -n r_pipeline_env -c conda-forge -y \
    r-devtools \
    && conda clean -afy

# Install R packages NOT in conda (but safe to install from CRAN)
# MUST activate environment first to use conda's compilers
RUN /bin/bash -c "source /opt/conda/etc/profile.d/conda.sh && \
    conda activate r_pipeline_env && \
    Rscript -e \"options(repos = c(CRAN = 'https://cloud.r-project.org')); \
    packages <- c('ineq', 'archetypes', 'syuzhet', 'lingmatch', 'sentimentr', 'logging', 'stringdist', 'phonics'); \
    for (pkg in packages) { \
      tryCatch({ \
        install.packages(pkg, dependencies = TRUE); \
        cat(paste('Successfully installed', pkg, '\\\\n')); \
      }, error = function(e) { \
        cat(paste('FAILED to install', pkg, ':', e\$message, '\\\\n')); \
      }); \
    }\""


# Copy scripts and reference data
COPY scripts/ /app/scripts/
COPY config/ /app/config/
COPY reference_data/ /app/reference_data/

# Create wrapper scripts that activate correct environments
RUN echo '#!/bin/bash\n\
exec /opt/conda/bin/conda run --no-capture-output -n whisperx_env python "$@"' > /usr/local/bin/run-whisperx && chmod +x /usr/local/bin/run-whisperx

RUN echo '#!/bin/bash\n\
exec /opt/conda/bin/conda run --no-capture-output -n pwesuite_env python "$@"' > /usr/local/bin/run-pwesuite && chmod +x /usr/local/bin/run-pwesuite

RUN echo '#!/bin/bash\n\
exec /opt/conda/bin/conda run --no-capture-output -n r_pipeline_env python "$@"' > /usr/local/bin/run-pipeline && chmod +x /usr/local/bin/run-pipeline


# Create output directories
RUN mkdir -p /app/output/{logs,features,transcriptions,cropped_audio,review_files}

# Default to R pipeline environment
COPY scripts/pipeline.sh /app/pipeline.sh
RUN chmod +x /app/pipeline.sh

ENTRYPOINT ["/app/pipeline.sh"]
CMD ["test_participant", "/app/data/example_audio.wav"]
