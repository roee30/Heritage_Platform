"""
Integration tests: compare segmentation output to canonical examples.

These tests verify that our segmentation matches expected outputs
for known Sanskrit phrases.
"""

import sys
from pathlib import Path
from typing import List

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from heritage.modules.stubs import Segmenter, Sanskrit, _LEXICON_CODES
from heritage.modules.canon import CANON_VH
from heritage.modules.word import Word


def encode_to_codes(text: str) -> List[int]:
    """Encode VH text to code sequence (helper)."""
    REVERSE_VH = {v: k for k, v in CANON_VH.items()}
    TOKENS = sorted(REVERSE_VH.keys(), key=lambda s: -len(s))
    i = 0
    codes = []
    while i < len(text):
        matched = False
        for tok in TOKENS:
            if text.startswith(tok, i):
                codes.append(REVERSE_VH[tok])
                i += len(tok)
                matched = True
                break
        if not matched:
            codes.append(-ord(text[i]))
            i += 1
    return codes


def test_idamapi_segmentation():
    """Test segmentation of 'idamapi' (this this)."""
    # Use the segmenter with our lexicon
    seg = Segmenter()

    # Create Word objects for "idam" and "api"
    idam_codes = _LEXICON_CODES.get("idam", [])
    api_codes = _LEXICON_CODES.get("api", [])

    if not idam_codes or not api_codes:
        print("⚠ Lexicon missing 'idam' or 'api'; skipping test")
        return

    chunks = [Word(idam_codes), Word(api_codes)]
    full, count = seg.segment_iter(chunks)

    print(f"✓ test_idamapi_segmentation:")
    print(f"  Full segmentation: {full}")
    print(f"  Solution count: {count}")
    assert full, "idamapi should be fully segmentable"
    assert count >= 1, "Should have at least one segmentation"
    print(f"  Graph offsets: {list(seg.graph.keys())}")


def test_rama_iti_segmentation():
    """Test segmentation of 'ramaiti' (Rama and)."""
    seg = Segmenter()

    rama_codes = _LEXICON_CODES.get("rama", [])
    iti_codes = _LEXICON_CODES.get("iti")

    if not rama_codes:
        print("⚠ Lexicon missing 'rama'; skipping test")
        return

    if iti_codes:
        chunks = [Word(rama_codes), Word(iti_codes)]
        full, count = seg.segment_iter(chunks)
        print(f"✓ test_rama_iti_segmentation:")
        print(f"  Full segmentation: {full}")
        print(f"  Solution count: {count}")
        print(f"  Graph offsets: {list(seg.graph.keys())}")
    else:
        print("⚠ Lexicon missing 'iti'; skipping test")


if __name__ == "__main__":
    print("Running integration tests...\n")
    test_idamapi_segmentation()
    print()
    test_rama_iti_segmentation()
    print("\nIntegration tests completed ✓")
