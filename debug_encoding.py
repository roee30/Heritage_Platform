#!/usr/bin/env python3
"""Debug VH encoding."""
import sys
from pathlib import Path
package_dir = Path(__file__).parent / "python"
sys.path.insert(0, str(package_dir))

from heritage.modules.stubs import _encode_string_to_codes
from heritage.modules.canon import CANON_VH

# Check what _encode_string_to_codes does
test_words = ["a", "bhi", ".seka", "raamas", "tu", "siitaa"]

print("Testing _encode_string_to_codes:")
for word in test_words:
    codes = _encode_string_to_codes(word)
    print(f"  '{word}' -> {codes}")

# Check what tokens are in CANON_VH
print("\nTokens in CANON_VH (first 30):")
tokens = {v: k for k, v in CANON_VH.items() if isinstance(v, str)}
sorted_tokens = sorted(tokens.keys(), key=len, reverse=True)
for tok in sorted_tokens[:30]:
    print(f"  '{tok}' -> {tokens[tok]}")

# Try to manually encode 'abhi.seka.h'
print("\nManual breakdown of 'abhi.seka.h':")
s = "abhi.seka.h"
i = 0
result = []
while i < len(s):
    matched = False
    for tok in sorted_tokens:
        if s.startswith(tok, i):
            result.append((tok, tokens[tok]))
            i += len(tok)
            matched = True
            break
    if not matched:
        result.append((s[i], -ord(s[i])))
        i += 1

print(f"  Breakdown: {result}")
print(f"  Codes: {[code for _, code in result]}")
