#!/usr/bin/env python
"""
PWE (Phonetic Word Embeddings) Wrapper Script
Runs in pwesuite_env conda environment
"""

import argparse
import os
import sys
import pandas as pd
import torch
import tqdm
import math
from pathlib import Path

# Add pwesuite to path
PWESUITE_DIR = "/app/pwesuite"
if os.path.exists(PWESUITE_DIR):
    sys.path.insert(0, PWESUITE_DIR)

try:
    from models.metric_learning.preprocessor import preprocess_dataset_foreign
    from models.metric_learning.model import RNNMetricLearner
except ImportError as e:
    print(f"ERROR: Failed to import PWESuite modules: {e}", file=sys.stderr)
    print(f"Make sure PWESuite is installed at {PWESUITE_DIR}", file=sys.stderr)
    sys.exit(1)


def extract_pwe_embeddings(words_file, output_file, model_path, batch_size=32, verbose=False):
    """
    Extract PWE embeddings for words in input CSV file
    
    Args:
        words_file: Path to CSV with 'word' column
        output_file: Path to save embeddings CSV
        model_path: Path to pretrained PWE model
        batch_size: Batch size for model inference
        verbose: Print progress
    
    Returns:
        Path to output file if successful, None otherwise
    """
    
    # === LOAD CSV ===
    if verbose:
        print(f"Loading words from: {words_file}")
    
    try:
        df = pd.read_csv(words_file)
        # Ensure 'word' column exists
        if df.columns[0] != "word":
            df.columns = ["word"]
        words = df["word"].tolist()
        
        if verbose:
            print(f"  Loaded {len(words)} words")
        
        # Remove duplicates while preserving order
        words = list(dict.fromkeys(words))
        if verbose and len(words) < len(df):
            print(f"  Removed {len(df) - len(words)} duplicates")
            
    except Exception as e:
        print(f"ERROR: Failed to load input file: {e}", file=sys.stderr)
        return None
    
    # === PREPROCESS ===
    if verbose:
        print("Preprocessing words...")
    
    try:
        data = preprocess_dataset_foreign(
            [{"token_ort": word, "token_ipa": None} for word in words],
            features="token_ort"
        )
    except Exception as e:
        print(f"ERROR: Preprocessing failed: {e}", file=sys.stderr)
        return None
    
    # === LOAD MODEL ===
    if verbose:
        print("Loading PWE model...")
    
    if not os.path.exists(model_path):
        print(f"ERROR: Model file not found: {model_path}", file=sys.stderr)
        return None
    
    try:
        model = RNNMetricLearner(dimension=300, feature_size=data[0][0].shape[1])
        model.load_state_dict(torch.load(model_path, map_location='cpu'))
        model.eval()
        
        if verbose:
            print(f"  Model loaded: 300-dimensional embeddings")
    except Exception as e:
        print(f"ERROR: Failed to load model: {e}", file=sys.stderr)
        return None
    
    # === GET EMBEDDINGS ===
    if verbose:
        print(f"Extracting embeddings (batch_size={batch_size})...")
    
    embeddings = []
    try:
        iterator = range(math.ceil(len(data) / batch_size))
        if verbose:
            iterator = tqdm.tqdm(iterator, desc="Processing batches")
        
        for i in iterator:
            batch = [f for f, _ in data[i * batch_size:(i + 1) * batch_size]]
            with torch.no_grad():
                batch_out = model.forward(batch).detach().cpu().numpy()
            embeddings.extend(batch_out)
    except Exception as e:
        print(f"ERROR: Embedding extraction failed: {e}", file=sys.stderr)
        return None
    
    # === SAVE TO CSV ===
    if verbose:
        print(f"Saving embeddings to: {output_file}")
    
    try:
        embedding_df = pd.DataFrame(embeddings)
        embedding_df["word"] = words
        
        # Move 'word' column to first position
        cols = ["word"] + [col for col in embedding_df.columns if col != "word"]
        embedding_df = embedding_df[cols]
        
        embedding_df.to_csv(output_file, index=False)
        
        if verbose:
            print(f"  ✓ Saved {len(words)} word embeddings ({embedding_df.shape[1]-1} dimensions)")
        
        return output_file
        
    except Exception as e:
        print(f"ERROR: Failed to save output: {e}", file=sys.stderr)
        return None

def main():
    parser = argparse.ArgumentParser(
        description="Extract PWE embeddings for words using RNN metric learning model"
    )
    
    parser.add_argument(
        "--input",
        type=str,
        required=True,
        help="Input CSV file with 'word' column"
    )
    
    parser.add_argument(
        "--output",
        type=str,
        required=True,
        help="Output CSV file for embeddings"
    )
    
    parser.add_argument(
        "--model",
        type=str,
        default="/app/reference_data/models/rnn_metric_learning_token_ort_all.pt",
        help="Path to PWE model file"
    )
    
    parser.add_argument(
        "--batch-size",
        type=int,
        default=32,
        help="Batch size for model inference (default: 32)"
    )
    
    parser.add_argument(
        "--verbose",
        action="store_true",
        help="Print progress information"
    )
    
    args = parser.parse_args()
    
    # Validate input file exists
    if not os.path.exists(args.input):
        print(f"ERROR: Input file not found: {args.input}", file=sys.stderr)
        sys.exit(1)
    
    # Create output directory if needed
    output_dir = os.path.dirname(args.output)
    if output_dir and not os.path.exists(output_dir):
        os.makedirs(output_dir, exist_ok=True)
    
    # Extract embeddings
    result = extract_pwe_embeddings(
        words_file=args.input,
        output_file=args.output,
        model_path=args.model,
        batch_size=args.batch_size,
        verbose=args.verbose
    )
    
    if result is None:
        sys.exit(1)
    else:
        sys.exit(0)

if __name__ == "__main__":
    main()
