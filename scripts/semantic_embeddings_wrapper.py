#!/usr/bin/env python
"""
Semantic Embeddings Wrapper Script
Extracts semantic embeddings for words using sentence-transformers
Mirrors the PWE wrapper structure

Usage:
    python semantic_embeddings_wrapper.py --input words.csv --output embeddings.csv

Requirements:
    - Must run in r_pipeline_env conda environment
    - Input CSV must have 'word' column
"""

import argparse
import os
import sys
import pandas as pd
import torch
from sentence_transformers import SentenceTransformer

def extract_semantic_embeddings(words_file, output_file, model_name="all-MiniLM-L6-v2", batch_size=32, verbose=False):
    """
    Extract semantic embeddings for words in input CSV file
    
    Args:
        words_file: Path to CSV with 'word' column
        output_file: Path to save embeddings CSV
        model_name: Sentence-transformers model name
        batch_size: Batch size for model inference
        verbose: Print progress information
    
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
    
    # === LOAD MODEL ===
    if verbose:
        print(f"Loading sentence-transformers model: {model_name}...")
    
    try:
        model = SentenceTransformer(model_name)
        
        # Use GPU if available
        device = 'cuda' if torch.cuda.is_available() else 'cpu'
        model = model.to(device)
        
        if verbose:
            print(f"  Model loaded on {device}")
            print(f"  Embedding dimension: {model.get_sentence_embedding_dimension()}")
            
    except Exception as e:
        print(f"ERROR: Failed to load model: {e}", file=sys.stderr)
        return None
    
    # === GET EMBEDDINGS ===
    if verbose:
        print(f"Extracting embeddings (batch_size={batch_size})...")
    
    try:
        embeddings = model.encode(
            words,
            batch_size=batch_size,
            show_progress_bar=verbose,
            convert_to_numpy=True
        )
        
        if verbose:
            print(f"  ✓ Extracted {len(embeddings)} embeddings")
            
    except Exception as e:
        print(f"ERROR: Embedding extraction failed: {e}", file=sys.stderr)
        return None
    
    # === SAVE TO CSV ===
    if verbose:
        print(f"Saving embeddings to: {output_file}")
    
    try:
        embedding_df = pd.DataFrame(embeddings)
        embedding_df.insert(0, "word", words)
        
        # Move 'word' column to first position (consistent with PWE)
        embedding_df.to_csv(output_file, index=False)
        
        if verbose:
            print(f"  ✓ Saved {len(words)} word embeddings ({embedding_df.shape[1]-1} dimensions)")
        
        return output_file
        
    except Exception as e:
        print(f"ERROR: Failed to save output: {e}", file=sys.stderr)
        return None

def main():
    parser = argparse.ArgumentParser(
        description="Extract semantic embeddings for words using sentence-transformers"
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
        default="all-MiniLM-L6-v2",
        help="Sentence-transformers model name (default: all-MiniLM-L6-v2)"
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
    result = extract_semantic_embeddings(
        words_file=args.input,
        output_file=args.output,
        model_name=args.model,
        batch_size=args.batch_size,
        verbose=args.verbose
    )
    
    if result is None:
        sys.exit(1)
    else:
        sys.exit(0)

if __name__ == "__main__":
    main()
