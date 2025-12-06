#!/usr/bin/env python3
"""Extract Sanskrit lexicon headwords from DICO HTML files."""
import re
from pathlib import Path

dico_dir = Path(__file__).parent / "Heritage_Resources" / "DICO"
output_file = Path(__file__).parent / "python" / "heritage" / "data" / "lexicon_from_dico.txt"

# Regex to extract headwords in trans12 spans that are dictionary entries
# Looking for pattern: <a class="navy" name="..."><i><span class="trans12">WORD</span></i></a>
headword_pattern = re.compile(
    r'<a class="navy"[^>]*name="[^"]*"[^>]*><i><span class="trans12">([^<]+)</span></i></a>'
)

lexicon = set()

# Process all numbered HTML files
for html_file in sorted(dico_dir.glob("*.html")):
    if html_file.name.endswith(".html") and html_file.name[:-5].isdigit():
        try:
            with open(html_file, 'r', encoding='utf-8') as f:
                content = f.read()
            
            matches = headword_pattern.findall(content)
            for match in matches:
                # Clean up the entry: remove subscripts, underscores, etc.
                # Keep as-is for now to preserve exact lexicon form
                lexicon.add(match)
        except Exception as e:
            print(f"Error reading {html_file.name}: {e}")

# Write lexicon to file (one word per line, sorted)
sorted_lex = sorted(lexicon)
with open(output_file, 'w', encoding='utf-8') as f:
    for word in sorted_lex:
        f.write(word + "\n")

print(f"Extracted {len(sorted_lex)} unique headwords from {dico_dir}")
print(f"Wrote to {output_file}")
print(f"First 20 entries: {sorted_lex[:20]}")
print(f"Last 20 entries: {sorted_lex[-20:]}")
