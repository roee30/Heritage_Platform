#!/usr/bin/env python3
"""Debug segmentation matching."""
import sys
from pathlib import Path
package_dir = Path(__file__).parent / "python"
sys.path.insert(0, str(package_dir))

from heritage.modules.stubs import _LEXICON_CODES, Sanskrit, Encode
from heritage.modules.canon import decode, uniromcode
from heritage.modules.word import Word

# Input text in VH form
text = "abhi.seka.h raamas tu siitaa saumitri.h eva ca"

# Get encoding function
encode_func = Encode.switch_code("VH")

# Chunk the text
chunks = Sanskrit.read_raw_sanskrit(encode_func, text)

print(f"Input: {text}")
print(f"\nNumber of chunks: {len(chunks)}")
for i, chunk in enumerate(chunks):
    codes = chunk.elements if isinstance(chunk, Word) else chunk
    roman = uniromcode(codes)
    print(f"  Chunk {i}: codes={codes[:20]}... roman='{roman}'")

# Check what's in the lexicon
print(f"\nLexicon size: {len(_LEXICON_CODES)}")
print(f"Lexicon sample (first 10): {list(_LEXICON_CODES.keys())[:10]}")

# Look for matching lexicon entries
print("\nLooking for matching lexicon entries...")
for i, chunk in enumerate(chunks):
    codes = chunk.elements if isinstance(chunk, Word) else chunk
    codes_tuple = tuple(codes)
    roman = uniromcode(codes)
    
    # Check if this exact sequence is in lexicon
    for lex_token, lex_codes in list(_LEXICON_CODES.items())[:10]:
        if tuple(lex_codes) == codes_tuple:
            print(f"  Chunk {i} ('{roman}'): MATCHES lexicon entry '{lex_token}'")
            break
    else:
        print(f"  Chunk {i} ('{roman}'): code sequence {codes[:10]}... NOT in lexicon")
        # Try to find partial matches
        print(f"    Looking for lexicon entries starting with these codes...")
        for lex_token, lex_codes in list(_LEXICON_CODES.items()):
            if lex_codes[0] == codes[0]:
                print(f"      '{lex_token}' starts with code {codes[0]}")
                break
