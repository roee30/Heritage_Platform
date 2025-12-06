#!/usr/bin/env python3
"""
Segmentation using transducers with lexicon lookup.

Demonstrates:
1. Loading lexicon (word -> code mappings)
2. Using transducers for word matching
3. Complete segmentation pipeline
"""

import sys
import os
import time
from pathlib import Path
from typing import List, Dict, Tuple

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parent))

from heritage.modules.transducers import load_transducers
from heritage.modules.canon import decode
from heritage.modules.word import Word
import pickle


def text_to_codes(text: str) -> List[int]:
    """Convert Sanskrit text (VH) to internal code representation."""
    # Create a Word from the text and get its internal codes
    # For now, use a simple approach: split and look up in lexicon
    return []  # Placeholder - will use lexicon lookup instead


def load_lexicon(pkl_path: str) -> Dict[str, List[int]]:
    """Load lexicon from pickle file."""
    with open(pkl_path, 'rb') as f:
        lexicon = pickle.load(f)
    return lexicon


class SimpleSegmenter:
    """Simple greedy segmenter using lexicon and transducers."""
    
    def __init__(self, transducers, lexicon):
        self.transducers = transducers
        self.lexicon = lexicon
        # Create reverse mapping: code sequence -> word
        self.codes_to_word = {}
        for word, codes in lexicon.items():
            self.codes_to_word[tuple(codes)] = word
    
    def segment_greedy(self, text: str) -> Tuple[List[str], bool]:
        """Segment text greedily, left to right.
        
        This uses lexicon lookup directly - in a real system,
        you would convert text to codes and match against transducers.
        
        Returns (segmentation, complete) where:
        - segmentation: list of words
        - complete: whether entire text was segmented
        """
        # For demo: just look for exact word matches in lexicon
        words = []
        offset = 0
        text_len = len(text)
        
        while offset < text_len:
            found = False
            # Try longest matches first (greedy)
            for length in range(text_len - offset, 0, -1):
                substring = text[offset:offset+length]
                if substring in self.lexicon:
                    words.append(substring)
                    offset += length
                    found = True
                    break
            
            if not found:
                # No match found - skip this character
                words.append(f"[?:{text[offset]}]")
                offset += 1
        
        complete = offset == text_len
        return words, complete
    
    def traverse_transducer(self, codes: List[int], root_node_id: int) -> bool:
        """Check if codes can be traversed through a transducer."""
        node = self.transducers.nodes[root_node_id]
        return self._traverse_rec(node, codes, 0)
    
    def _traverse_rec(self, node, codes: List[int], pos: int) -> bool:
        """Recursive traversal."""
        if pos == len(codes):
            return node.accept
        
        code = codes[pos]
        for (letter, target_id) in node.arcs:
            if letter == code:
                target_node = self.transducers.nodes[target_id]
                if self._traverse_rec(target_node, codes, pos + 1):
                    return True
        
        return False
    
    def validate_with_transducers(self, word: str) -> Dict[str, bool]:
        """Check which transducers can validate this word.
        
        Returns dict: transducer_name -> can_traverse_and_accept
        """
        # For now, just check if word is in lexicon
        # In a full implementation, would use transducer validation
        if word in self.lexicon:
            word_codes = self.lexicon[word]
            results = {}
            
            for transducer_name, root_id in self.transducers.roots.items():
                can_traverse = self.traverse_transducer(word_codes, root_id)
                results[transducer_name] = can_traverse
            
            return results
        return {}


def demo_segmentation():
    """Demonstrate segmentation."""
    print("\n" + "="*70)
    print("SEGMENTATION DEMO")
    print("="*70 + "\n")
    
    # Paths
    python_dir = Path(__file__).parent
    transducers_path = python_dir / "heritage" / "data" / "transducers.msgpack"
    lexicon_path = python_dir / "heritage" / "modules" / "lexicon.pkl"
    
    # Load resources
    print("📦 Loading resources...")
    start = time.time()
    transducers = load_transducers(str(transducers_path))
    load_trans_time = time.time() - start
    
    print(f"   Transducers: {load_trans_time:.2f}s ({len(transducers.nodes)} nodes)")
    
    start = time.time()
    lexicon = load_lexicon(str(lexicon_path))
    load_lex_time = time.time() - start
    print(f"   Lexicon: {load_lex_time:.2f}s ({len(lexicon)} entries)")
    
    # Create segmenter
    segmenter = SimpleSegmenter(transducers, lexicon)
    
    # Test segmentations
    test_cases = [
        "namaskar",
        "namaskara",
        "yoga",
        "yogapractice",
    ]
    
    print(f"\n🔍 Testing segmentation:\n")
    for text in test_cases:
        try:
            segmentation, complete = segmenter.segment_greedy(text)
            status = "✅" if complete else "⚠️ "
            print(f"{status} {text:20} → {' + '.join(segmentation)}")
            
            # Check against transducers
            if len(segmentation) == 1:
                word = segmentation[0]
                validators = segmenter.validate_with_transducers(word)
                valid_in = [name for name, valid in validators.items() if valid]
                if valid_in:
                    print(f"   (valid in: {', '.join(valid_in[:3])})")
        except Exception as e:
            print(f"❌ {text:20} → Error: {e}")
    
    print()


def demo_word_validation():
    """Demonstrate word validation with transducers."""
    print("\n" + "="*70)
    print("WORD VALIDATION DEMO")
    print("="*70 + "\n")
    
    # Paths
    python_dir = Path(__file__).parent
    transducers_path = python_dir / "heritage" / "data" / "transducers.msgpack"
    lexicon_path = python_dir / "heritage" / "modules" / "lexicon.pkl"
    
    # Load
    transducers = load_transducers(str(transducers_path))
    lexicon = load_lexicon(str(lexicon_path))
    
    segmenter = SimpleSegmenter(transducers, lexicon)
    
    # Test words from lexicon
    test_words = list(lexicon.keys())[:10]
    
    print(f"🔍 Validating {len(test_words)} sample words:\n")
    for word in test_words:
        validators = segmenter.validate_with_transducers(word)
        valid_count = sum(1 for v in validators.values() if v)
        valid_in = [name for name, valid in validators.items() if valid]
        print(f"   {word:20} → valid in {valid_count:2} transducers: {', '.join(valid_in[:3])}")
    
    print()


def main():
    """Main."""
    print("\n")
    print("╔" + "="*68 + "╗")
    print("║" + " " * 12 + "TRANSDUCER-BASED SEGMENTATION DEMO" + " " * 22 + "║")
    print("╚" + "="*68 + "╝")
    
    demo_segmentation()
    demo_word_validation()
    
    print("="*70)
    print("✅ Demo complete!")
    print("="*70 + "\n")


if __name__ == "__main__":
    main()
