"""Stub modules for morphology, dispatch, and transducers.

These are complex modules that would require deep translation of the entire
OCaml automata and linguistic system. For now, providing interfaces.
"""

from typing import Callable, Optional, Tuple, Dict, List
from dataclasses import dataclass
from .graph_segmenter import GraphSegmenter
from .phases import Phase
from .word import Word
from .canon import CANON_VH
from pathlib import Path
import pickle

"""
Lexicon loading and helper utilities.

If a lexicon file exists at `python/heritage/data/lexicon.txt`, it will be
loaded at import time and used by the simplified segmenter. Otherwise a small
fallback lexicon is used to keep behavior stable.
"""

# Default small lexicon (fallback)
_DEFAULT_LEXICON = {"idam", "api", "rama", "namaskar", "namas", "kara"}


def _load_lexicon_from_file() -> list[str]:
    """Load lexicon tokens from extracted DICO VH lexicon or fall back to lexicon.txt.

    Returns list of tokens (strings). Each line in file is treated as a token.
    """
    # Prefer the extracted DICO-based VH lexicon (34K+ entries)
    candidate = Path(__file__).resolve().parents[1] / "data" / "lexicon_from_dico_vh.txt"
    if candidate.exists():
        try:
            text = candidate.read_text(encoding="utf-8")
            toks = [line.strip() for line in text.splitlines() if line.strip()]
            if toks:
                return toks
        except Exception:
            pass
    
    # Fall back to the original lexicon.txt if extracted lexicon not available
    candidate = Path(__file__).resolve().parents[1] / "data" / "lexicon.txt"
    if candidate.exists():
        try:
            text = candidate.read_text(encoding="utf-8")
            toks = [line.strip() for line in text.splitlines() if line.strip()]
            if toks:
                return toks
        except Exception:
            pass
    
    return list(_DEFAULT_LEXICON)


def _normalize_iast_to_vh(token: str) -> str:
    """Normalize an IAST/diacritic token into a VH (Velthuis-Harvard) string.

    This is a heuristic mapping intended to convert the common diacritics
    found in the shipped lexicon into VH tokens used by the rest of the
    pipeline. It strips a few leading/trailing punctuation characters and
    applies character-by-character replacements for common diacritics.
    """
    if not token:
        return ""

    t = token.strip()
    # Remove obvious leading/trailing markers (parentheses, quotes, plusses)
    t = t.strip(" '()[]+")
    # Lowercase for stability
    t = t.lower()

    # Simple replacement map for common IAST characters -> VH
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
        # common accent characters -> base vowel
        'á': 'a',
        'à': 'a',
        'é': 'e',
        'í': 'i',
        'ó': 'o',
        'ú': 'u',
    }

    # Apply replacements
    out_chars: list[str] = []
    for ch in t:
        if ch in repl:
            out_chars.append(repl[ch])
        else:
            out_chars.append(ch)

    # Join and collapse accidental double spaces/hyphens
    normalized = "".join(out_chars)
    normalized = normalized.replace("--", "-")
    normalized = normalized.replace(" ", "")
    # Remove remaining punctuation unlikely to be part of a VH token
    normalized = normalized.strip("-_")
    return normalized


# Build reverse VH map (string -> code) from canon mapping
_REVERSE_VH: dict[str, int] = {v: k for k, v in CANON_VH.items()}
# Precompute VH tokens ordered by length (longest-first) for greedy matching
_TOKENS = sorted(_REVERSE_VH.keys(), key=lambda s: -len(s))


def _encode_string_to_codes(s: str) -> list[int]:
    """Encode a VH-transliterated string to internal numeric codes.

    Greedy longest-token matching. Unknown single characters become negative
    ordinals to avoid collisions with real codes.
    """
    i = 0
    codes: list[int] = []
    while i < len(s):
        matched = False
        for tok in _TOKENS:
            if s.startswith(tok, i):
                codes.append(_REVERSE_VH[tok])
                i += len(tok)
                matched = True
                break
        if not matched:
            codes.append(-ord(s[i]))
            i += 1
    return codes


def _ascii_encode(s: str) -> list[int]:
    """OCaml-like Ascii.encode: map each character to its ordinal.

    This mirrors the OCaml `Ascii.encode` which maps chars to int_of_char.
    We use Python's `ord` on each Unicode character; for UTF-8 multi-byte
    characters this yields the Unicode code point, which preserves the
    token identity compared to raw byte reads in many environments.
    """
    return [ord(ch) for ch in s]


def cache[**K, R](func: Callable[K, R]) -> Callable[K, R]:
    """Decorator to cache function output to a pickle file."""
    cache_file = (
        Path(__file__)
        .with_name(f"{func.__module__}@{func.__qualname__}")
        .with_suffix(".pkl")
    )

    def wrapper(*args, **kwargs):
        if cache_file.exists():
            with cache_file.open("rb") as f:
                return pickle.load(f)
        result = func(*args, **kwargs)
        with cache_file.open("wb") as f:
            pickle.dump(result, f)
        return result

    return wrapper


def get_lexicon() -> dict[str, list[int]]:
    # Load raw tokens from file (do not normalize) and encode each using the
    # greedy VH-token encoder. This mirrors the OCaml loader which simply
    # converts lexicon strings into sequences of character codes; here we
    # attempt to preserve the original token text and encode it into the
    # internal numeric representation used by the segmenter.
    raw = _load_lexicon_from_file()
    lexicon: dict[str, list[int]] = {}
    for tok in raw:
        if not tok:
            continue
        try:
            # First try: assume the token is already in VH/ASCII form and
            # attempt to encode it directly to CANON codes.
            codes = _encode_string_to_codes(tok)
            # If encoding produced negative ordinals (unknown fragments), try
            # to normalize from IAST/diacritics to VH and re-encode.
            if any(c < 0 for c in codes):
                vh = _normalize_iast_to_vh(tok)
                if vh:
                    codes_vh = _encode_string_to_codes(vh)
                    if not any(c < 0 for c in codes_vh):
                        lexicon[vh] = codes_vh
                        continue
            # Otherwise accept the original encoding (may contain negatives)
            lexicon[tok] = codes
        except Exception:
            # Skip problematic tokens but continue building the lexicon
            continue

    if not lexicon:
        return {w: _encode_string_to_codes(w) for w in _DEFAULT_LEXICON}
    return lexicon


# Load lexicon tokens (from file if available) and precompute code sequences
_LEXICON_CODES: dict[str, list[int]] = get_lexicon()


@dataclass
class Morphology:
    """Morphology database and lookup."""

    def tags_of(self, phase: Phase, word: Word) -> list:
        """Get all possible morphological tags for a word in a phase."""
        return []

    def is_cache(self, phase: Phase) -> bool:
        """Check if phase uses caching."""
        return True

    def generative(self, phase: Phase) -> bool:
        """Check if phase is generative."""
        return True


@dataclass
class Dispatcher:
    """Dispatcher - manages phase transitions in morphological analysis."""

    def dispatch(self, word: Word, phase: Phase) -> list[Phase]:
        """Get possible next phases for a word in current phase."""
        return []

    def initial_phases(self) -> list[Phase]:
        """Get initial phases for segmentation."""
        return [Phase.NOUN, Phase.ROOT, Phase.INDE]

    def accepting(self, phase: Phase) -> bool:
        """Check if phase is an accepting/terminal phase."""
        return True


@dataclass
class Segmenter:
    """Segmentation engine - segments input text."""

    def segment_iter(self, chunks: list[Word]) -> Tuple[bool, int]:
        """Segment chunks and return (all_segmented, num_solutions)."""
        # Returns: (full_segmentation, solution_count)
        # We implement a simplified graph-based segmentation:
        # - For each chunk (a Word with numeric codes) attempt to split it into
        #   lexicon entries (precomputed in _LEXICON_CODES).
        # - Count the number of segmentations per chunk and multiply across
        #   chunks (chunks are independent in this simplified model).
        if not chunks:
            self.graph = {}
            return (True, 1)

        full = True
        total_count = 1

        # Use the new GraphSegmenter to build a graph and compute counts.
        # We pass the precomputed `_LEXICON_CODES` mapping to the graph segmenter.
        seg = GraphSegmenter(_LEXICON_CODES)

        # Flatten chunks into a single code stream and remember offsets
        combined: list[int] = []
        chunk_offsets: list[int] = []
        for chunk in chunks:
            chunk_offsets.append(len(combined))
            combined.extend(list(chunk.elements))

        full, count, graph_candidates = seg.process(combined)

        # Convert GraphSegmenter output into the same shape used elsewhere:
        # mapping offset -> list of (Word, []) tuples
        graph: Dict[int, list] = {}
        for off, cands in graph_candidates.items():
            graph.setdefault(off, [])
            for cand in cands:
                graph[off].append((cand.word, cand.meta or []))

        self.graph = graph
        return (full, count)


@dataclass
class Graph:
    """Segmentation graph with segments at offsets."""

    def __init__(self):
        self.segments: Dict[int, list] = {}


class Lemmas:
    """Lemma module - handles morphological lookups."""

    @staticmethod
    def tags_of(phase: Phase, word: Word) -> Optional[list]:
        """Get tags for word in phase."""
        return None


class Transducers:
    """Transducers module - finite state transducers for morphology."""

    @staticmethod
    def get_transducer(phase: Phase):
        """Get transducer for a phase."""
        return None


class Load_morphs:
    """Load morphological data banks."""

    pass


class Load_transducers:
    """Load precompiled transducers."""

    pass


class Morpho_html:
    """HTML output for morphological analysis."""

    @staticmethod
    def print_graph_link(pvs, cached, form, loc, gen, tag):
        """Print HTML link for morphological entry."""
        return ""

    @staticmethod
    def print_final(word: Word):
        """Print final word analysis."""
        return ""


class Html:
    """HTML generation utilities."""

    @staticmethod
    def escape(text: str) -> str:
        """HTML escape text."""
        return text.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")

    @staticmethod
    def h3_begin(color="") -> str:
        return f"<h3>"

    @staticmethod
    def h3_end() -> str:
        return "</h3>"

    @staticmethod
    def h3_section(title: str) -> str:
        """Generate an H3 section."""
        return f"<h3>{Html.escape(title)}</h3>\n"

    @staticmethod
    def anchor(color, url: str, text: str) -> str:
        return f"<a href='{url}'>{text}</a>"

    @staticmethod
    def td_wrap(content: str) -> str:
        return f"<td>{content}</td>"

    @staticmethod
    def tr_begin() -> str:
        return "<tr>"

    @staticmethod
    def tr_end() -> str:
        return "</tr>"

    @staticmethod
    def table_begin(style: str = "") -> str:
        return f"<table {('class=' + style) if style else ''}>"

    @staticmethod
    def table_end() -> str:
        return "</table>"


class Web:
    """Web utilities."""

    output_channel = None

    @staticmethod
    def set_output(channel):
        """Set output channel for CGI."""
        Web.output_channel = channel

    @staticmethod
    def write(text: str):
        """Write to output."""
        if Web.output_channel:
            Web.output_channel.write(text)


type Env = dict[str, str]
type MaybeEnv = Optional[Env]


class Cgi:
    """CGI utilities for parameter handling."""

    @staticmethod
    def get(name: str, env: MaybeEnv, default: str = "") -> str:
        """Get CGI parameter."""
        return (env or {}).get(name, default)

    @staticmethod
    def url(path: str) -> str:
        """Generate URL."""
        return path


class Encode:
    """Encoding/decoding utilities."""

    @staticmethod
    def switch_code(translit: str) -> Callable[[str], Word]:
        """Get encoding function for transliteration scheme."""

        def encoder(text: str) -> Word:
            # Placeholder - would convert text to internal representation
            return Word([1, 2, 3])

        return encoder


class Sanskrit:
    """Sanskrit text processing."""

    @staticmethod
    def read_sanskrit(encode_func, text: str) -> list[Word]:
        """Chunk Sanskrit text (with sandhi processing)."""
        # Placeholder
        # For now, behave like read_raw_sanskrit (no sandhi splitting implemented)
        return Sanskrit.read_raw_sanskrit(encode_func, text)

    @staticmethod
    def read_raw_sanskrit(encode_func, text: str) -> list[Word]:
        """Read raw Sanskrit text (without sandhi processing)."""
        if not text:
            return []

        # Simple built-in lexicon to support basic segmentation (extendable)
        LEXICON = {"idam", "api", "rama", "namaskar", "namas", "kara"}

        # Build reverse VH map (string -> code)
        REVERSE_VH: Dict[str, int] = {v: k for k, v in CANON_VH.items()}
        # Order tokens by length for greedy longest-match
        TOKENS = sorted(REVERSE_VH.keys(), key=lambda s: -len(s))

        def encode_string_to_codes(s: str) -> list[int]:
            """Encode a VH-transliterated string to internal numeric codes.

            Greedy longest-match using VH tokens. Unknown fragments are
            represented as their ord() value negated to avoid collisions.
            """
            i = 0
            codes: list[int] = []
            while i < len(s):
                matched = False
                for tok in TOKENS:
                    if s.startswith(tok, i):
                        codes.append(REVERSE_VH[tok])
                        i += len(tok)
                        matched = True
                        break
                if not matched:
                    # Unknown single char -- fall back to ordinal negative
                    codes.append(-ord(s[i]))
                    i += 1
            return codes

        def segment_by_lexicon(s: str) -> Optional[list[str]]:
            """Attempt to segment string `s` into lexicon items using recursion."""
            if not s:
                return []
            if s in LEXICON:
                return [s]
            # Try all possible prefixes
            for i in range(1, len(s)):
                prefix = s[:i]
                if prefix in LEXICON:
                    rest = segment_by_lexicon(s[i:])
                    if rest is not None:
                        return [prefix] + rest
            return None

        # Normalize input (assume VH/roman input)
        txt = text.strip()

        # If whitespace present, treat as already segmented
        if any(ch.isspace() for ch in txt):
            parts = [p for p in txt.split() if p]
            # Encode each part using VH token encoder (assume input is in VH form)
            words = [Word(encode_string_to_codes(p)) for p in parts]
            return words

        # Try lexicon-based segmentation first
        seg = segment_by_lexicon(txt)
        if seg:
            return [Word(encode_string_to_codes(p)) for p in seg]

        # Fallback: encode the whole token as a single Word
        return [Word(encode_string_to_codes(txt))]


class Checkpoints:
    """Checkpoint management for segmentation."""

    @staticmethod
    def string_points(checkpoints: list) -> str:
        """Serialize checkpoints to string."""
        return ""

    @staticmethod
    def parse_points(checkpoint_str: str) -> list:
        """Parse checkpoint string."""
        return []
