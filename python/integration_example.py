#!/usr/bin/env python3
"""
Integration example: Using transducers in the main Heritage interface.

This shows how to integrate transducer-based validation and segmentation
into the existing Heritage Platform interface.
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))

from heritage.modules.transducers import load_transducers
from heritage.modules.transducer_segmenter import TransducerSegmenter
from heritage.modules.interface import Interface
from heritage.modules.word import Word
import pickle
import time


class EnhancedInterface(Interface):
    """Heritage interface enhanced with transducer validation."""
    
    def __init__(self, transducers_path=None, lexicon_path=None):
        """Initialize with optional transducers."""
        super().__init__()
        self.transducers = None
        self.segmenter = None
        
        if transducers_path and lexicon_path:
            self._load_transducers(transducers_path, lexicon_path)
    
    def _load_transducers(self, transducers_path, lexicon_path):
        """Load transducers and create segmenter."""
        try:
            print(f"Loading transducers from {transducers_path}...")
            self.transducers = load_transducers(transducers_path)
            
            print(f"Loading lexicon from {lexicon_path}...")
            with open(lexicon_path, 'rb') as f:
                lexicon = pickle.load(f)
            
            self.segmenter = TransducerSegmenter(self.transducers, lexicon)
            print(f"✅ Ready: {len(self.transducers.nodes)} nodes, "
                  f"{len(lexicon)} lexicon entries")
        except Exception as e:
            print(f"⚠️  Could not load transducers: {e}")
    
    def validate_word(self, word: str) -> dict:
        """Validate a word against transducers.
        
        Returns:
            {
                'valid': bool (found in at least one transducer),
                'transducers': {name: bool, ...},
                'in_lexicon': bool
            }
        """
        if not self.segmenter:
            return None
        
        # Check lexicon
        in_lexicon = word in self.segmenter.lexicon
        
        # Check transducers
        validators = self.segmenter.get_possible_targets(word)
        valid = any(validators.values()) if validators else False
        
        return {
            'word': word,
            'valid': valid,
            'in_lexicon': in_lexicon,
            'transducers': validators or {}
        }
    
    def segment_text(self, text: str) -> dict:
        """Segment text using transducers and lexicon.
        
        Returns:
            {
                'input': original text,
                'segments': [word, ...],
                'complete': bool (entire text covered),
                'segmenter': 'greedy' | None
            }
        """
        if not self.segmenter:
            return None
        
        segments, complete = self.segmenter.segment_greedy(text)
        
        return {
            'input': text,
            'segments': segments,
            'complete': complete,
            'segmenter': 'greedy',
            'validated': [self.validate_word(w) for w in segments if not w.startswith('[?')]
        }
    
    def analyze_word(self, word: str) -> dict:
        """Comprehensive word analysis."""
        result = {
            'word': word,
            'length': len(word),
        }
        
        if self.segmenter:
            # Validation
            validation = self.validate_word(word)
            result['validation'] = validation
            
            # Check which transducers accept it
            accepting = [name for name, valid in validation['transducers'].items() if valid]
            result['accepted_by'] = accepting
        
        return result


def demo_enhanced_interface():
    """Demonstrate the enhanced interface."""
    print("\n" + "="*70)
    print("ENHANCED INTERFACE WITH TRANSDUCERS")
    print("="*70 + "\n")
    
    # Paths
    base_dir = Path(__file__).parent
    transducers_path = base_dir / "heritage" / "data" / "transducers.msgpack"
    lexicon_path = base_dir / "heritage" / "modules" / "lexicon.pkl"
    
    if not transducers_path.exists():
        print(f"❌ Transducers not found: {transducers_path}")
        return
    
    if not lexicon_path.exists():
        print(f"❌ Lexicon not found: {lexicon_path}")
        return
    
    # Create enhanced interface
    print("📦 Initializing enhanced interface...")
    start = time.time()
    interface = EnhancedInterface(str(transducers_path), str(lexicon_path))
    init_time = time.time() - start
    print(f"   Initialization time: {init_time:.2f}s\n")
    
    # Test word analysis
    print("🔍 Word Analysis:")
    test_words = ["yoga", "pranayama", "asana", "meditation", "xyz123"]
    
    for word in test_words:
        analysis = interface.analyze_word(word)
        print(f"\n   {word}:")
        print(f"      Length: {analysis['length']}")
        if 'accepted_by' in analysis:
            print(f"      Accepted by: {analysis['accepted_by'][:3] or 'none'}")
    
    # Test segmentation
    print("\n\n📝 Segmentation:")
    test_texts = [
        "yoga",
        "yogapractice",
        "pranayama",
    ]
    
    for text in test_texts:
        segmentation = interface.segment_text(text)
        print(f"\n   {text}:")
        print(f"      Segments: {' + '.join(segmentation['segments'])}")
        print(f"      Complete: {segmentation['complete']}")
    
    print("\n" + "="*70)
    print("✅ Enhanced interface ready for use!")
    print("="*70 + "\n")


def usage_example():
    """Show how to use the enhanced interface."""
    print("""
USAGE EXAMPLE
=============

1. Initialize with transducers:
   
   from integration_example import EnhancedInterface
   
   interface = EnhancedInterface(
       'python/heritage/data/transducers.msgpack',
       'python/heritage/modules/lexicon.pkl'
   )

2. Validate words:
   
   result = interface.validate_word('yoga')
   # Returns: {'valid': True, 'in_lexicon': True, 'transducers': {...}}

3. Segment text:
   
   result = interface.segment_text('yogapractice')
   # Returns: {'segments': ['yoga', 'practice'], 'complete': True, ...}

4. Comprehensive analysis:
   
   analysis = interface.analyze_word('pranayama')
   # Returns: {'word': '...', 'accepted_by': [...], ...}

5. Integrate with existing interface:
   
   # The enhanced interface inherits all existing methods from Interface
   # and adds transducer functionality on top
""")


if __name__ == "__main__":
    demo_enhanced_interface()
    usage_example()
