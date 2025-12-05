"""
Build a VH-token lexicon from the MW HTML files in Heritage_Resources/MW.

Usage:
    python -m heritage.tools.build_lexicon

This script looks for `<a class="Blue">...</a>` anchors in the MW HTML files,
extracts their inner text, strips any nested tags, unescapes HTML entities,
and writes one token per line to `python/heritage/data/lexicon.txt`.

The resulting lexicon will be used by the simplified segmenter if present.
"""

from pathlib import Path
import re
import html

ROOT = Path(__file__).resolve().parents[3]
MW_DIR = ROOT / "Heritage_Resources" / "MW"
OUT_DIR = Path(__file__).resolve().parents[1] / "data"
OUT_DIR.mkdir(parents=True, exist_ok=True)
OUT_FILE = OUT_DIR / "lexicon.txt"

BLUE_RE = re.compile(
    r"<a[^>]*class=[\"']Blue[\"'][^>]*>(.*?)</a>", re.DOTALL | re.IGNORECASE
)
TAG_RE = re.compile(r"<[^>]+>")

tokens = set()

if not MW_DIR.exists():
    print(f"MW directory not found at {MW_DIR}. Nothing to do.")
    raise SystemExit(1)

for html_file in sorted(MW_DIR.glob("*.html")):
    try:
        txt = html_file.read_text(encoding="utf-8", errors="replace")
    except Exception as e:
        print(f"Skipping {html_file}: {e}")
        continue
    for m in BLUE_RE.findall(txt):
        # remove any nested tags
        inner = TAG_RE.sub("", m)
        inner = html.unescape(inner).strip()
        if not inner:
            continue
        # Blue entries may contain spaced phrases; split on whitespace
        # but also keep hyphenated tokens intact
        parts = re.split(r"\s+", inner)
        for p in parts:
            p = p.strip()
            if not p:
                continue
            # Skip purely punctuation entries
            if all(ch in "-_,;()[]{}\/" for ch in p):
                continue
            tokens.add(p)

# Normalize tokens: collapse duplicates while preserving case
cleaned = sorted(tokens)
OUT_FILE.write_text("\n".join(cleaned), encoding="utf-8")
print(f"Wrote {len(cleaned)} lexicon entries to {OUT_FILE}")
