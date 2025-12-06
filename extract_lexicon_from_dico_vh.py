#!/usr/bin/env python3
"""Extract Sanskrit lexicon headwords from DICO HTML files and convert to VH."""
import re
import html
from pathlib import Path

dico_dir = Path(__file__).parent / "Heritage_Resources" / "DICO"
output_file = Path(__file__).parent / "python" / "heritage" / "data" / "lexicon_from_dico_vh.txt"

# Regex to extract headwords in trans12 spans that are dictionary entries
headword_pattern = re.compile(
    r'<a class="navy"[^>]*name="[^"]*"[^>]*><i><span class="trans12">([^<]+)</span></i></a>'
)

# Helper function to normalize IAST/diacritic to VH
def normalize_iast_to_vh(token: str) -> str:
    """Normalize an IAST/diacritic token into a VH (Velthuis-Harvard) string."""
    if not token:
        return ""

    t = token.strip()
    # Decode HTML entities (&#257; -> ā, etc.)
    t = html.unescape(t)
    # Remove leading/trailing markers
    t = t.strip(" '()[]+")
    # Lowercase for stability
    t = t.lower()

    # Replacement map for common IAST characters -> VH
    repl = {
        'ā': 'aa',
        'ī': 'ii',
        'ū': 'uu',
        'ṛ': '.r',
        'ṝ': '.rr',
        'ḷ': '.l',
        'ṅ': '~n',
        'ñ': 'ny',
        'ś': 'z',
        'ṣ': '.s',
        'ṭ': '.t',
        'ḍ': '.d',
        'ṇ': '.n',
        'ṃ': '.m',
        'ṁ': '.m',
        'ḥ': '.h',
        # Accents to base vowels
        'á': 'a',
        'à': 'a',
        'é': 'e',
        'í': 'i',
        'ó': 'o',
        'ú': 'u',
    }

    out_chars = []
    for ch in t:
        if ch in repl:
            out_chars.append(repl[ch])
        else:
            out_chars.append(ch)

    normalized = "".join(out_chars)
    normalized = normalized.replace("--", "-")
    normalized = normalized.replace(" ", "")
    normalized = normalized.strip("-_")
    return normalized

lexicon = set()
failed = []

# Process all numbered HTML files
for html_file in sorted(dico_dir.glob("*.html")):
    if html_file.name.endswith(".html") and html_file.name[:-5].isdigit():
        try:
            with open(html_file, 'r', encoding='utf-8') as f:
                content = f.read()
            
            matches = headword_pattern.findall(content)
            for match in matches:
                # Normalize to VH
                vh = normalize_iast_to_vh(match)
                if vh:
                    lexicon.add(vh)
                else:
                    failed.append(match)
        except Exception as e:
            print(f"Error reading {html_file.name}: {e}")

# Write lexicon to file (one word per line, sorted)
sorted_lex = sorted(lexicon)
with open(output_file, 'w', encoding='utf-8') as f:
    for word in sorted_lex:
        f.write(word + "\n")

print(f"Extracted {len(sorted_lex)} unique headwords from {dico_dir}")
print(f"Wrote to {output_file}")
print(f"Failed to normalize: {len(failed)} entries")
if failed:
    print(f"Sample failures: {failed[:10]}")
print(f"First 20 entries: {sorted_lex[:20]}")
print(f"Last 20 entries: {sorted_lex[-20:]}")
