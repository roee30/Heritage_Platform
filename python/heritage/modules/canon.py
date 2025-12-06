"""Canon module - Sanskrit character encoding/decoding.

Translated from canon.ml - handles conversion between internal numeric representation
and transliteration schemes (VH, WX, SLP1, Devanagari).
"""

from typing import List
from .word import Word
from .sandhi import apply_all_sandhi_rules


# Internal numeric representation to VH (Velthuis-Harvard) transliteration
CANON_VH = {
    0: "-",  # notation for suffixes
    1: "a",
    2: "aa",
    3: "i",
    4: "ii",
    5: "u",
    6: "uu",
    7: ".r",
    8: ".rr",
    9: ".l",
    10: "e",
    11: "ai",
    12: "o",
    13: "au",
    14: ".m",  # anusvaara
    15: "~~",  # anusvara candrabindu
    16: ".h",
    17: "k",
    18: "kh",
    19: "g",
    20: "gh",
    21: "f",  # retroflex ṇ predecessor
    22: "c",
    23: "ch",
    24: "j",
    25: "jh",
    26: "~n",
    27: ".t",
    28: ".th",
    29: ".d",
    30: ".dh",
    31: ".n",
    32: "t",
    33: "th",
    34: "d",
    35: "dh",
    36: "n",
    37: "p",
    38: "ph",
    39: "b",
    40: "bh",
    41: "m",
    42: "y",
    43: "r",
    44: "l",
    45: "v",
    46: "z",  # ś (palatal sibilant)
    47: ".s",  # ṣ (retroflex sibilant)
    48: "s",
    49: "h",
    50: "_",  # hiatus
    -1: "'",  # avagraha
    -2: "[-]",  # amuissement (lopa of a/aa in preverb)
    -3: "aa|a",  # sandhi aa + a
    -4: "aa|i",  # sandhi aa + i
    -5: "aa|u",  # sandhi aa + u
    -6: "aa|r",  # sandhi aa + .r
    -7: "aa|I",  # sandhi aa + ii
    -8: "aa|U",  # sandhi aa + uu
    -9: "aa|A",  # sandhi aa + aa
    123: "aa|C",  # sandhi aa + ch (cch gemination)
    100: "+",  # segmentation hint
}

# WX encoding (University of Hyderabad)
CANON_WX = {
    0: "-",
    1: "a",
    2: "A",
    3: "i",
    4: "I",
    5: "u",
    6: "U",
    7: "q",
    8: "Q",
    9: "L",
    10: "e",
    11: "E",
    12: "o",
    13: "O",
    14: "M",
    15: "z",
    16: "H",
    17: "k",
    18: "K",
    19: "g",
    20: "G",
    21: "f",
    22: "c",
    23: "C",
    24: "j",
    25: "J",
    26: "F",
    27: "w",
    28: "W",
    29: "x",
    30: "X",
    31: "N",
    32: "t",
    33: "T",
    34: "d",
    35: "D",
    36: "n",
    37: "p",
    38: "P",
    39: "b",
    40: "B",
    41: "m",
    42: "y",
    43: "r",
    44: "l",
    45: "v",
    46: "S",
    47: "R",
    48: "s",
    49: "h",
    50: "_",
    -1: "'",
    -2: "[-]",
}

# SLP1 encoding (Sanskrit Library Phonetic v1)
CANON_SLP1 = {
    0: "-",
    1: "a",
    2: "A",
    3: "i",
    4: "I",
    5: "u",
    6: "U",
    7: "f",
    8: "F",
    9: "x",
    10: "e",
    11: "E",
    12: "o",
    13: "O",
    14: "M",
    15: "H",
    16: "~",
    17: "k",
    18: "K",
    19: "g",
    20: "G",
    21: "N",
    22: "c",
    23: "C",
    24: "j",
    25: "J",
    26: "Y",
    27: "w",
    28: "W",
    29: "q",
    30: "Q",
    31: "R",
    32: "t",
    33: "T",
    34: "d",
    35: "D",
    36: "n",
    37: "p",
    38: "P",
    39: "b",
    40: "B",
    41: "m",
    42: "y",
    43: "r",
    44: "l",
    45: "v",
    46: "S",
    47: "z",
    48: "s",
    49: "h",
    50: "_",
    -1: "'",
    -2: "[-]",
}


def iast_to_vh(text: str) -> str:
    """Convert IAST (International Alphabet of Sanskrit Transliteration) to VH (Velthuis-Harvard).
    
    Args:
        text: Text in IAST format
        
    Returns:
        Text in VH format
    """
    # Mapping from IAST diacriticals to VH equivalents
    iast_to_vh_map = {
        'ā': 'aa',    # U+0101 - long a
        'ī': 'ii',    # U+012B - long i
        'ū': 'uu',    # U+016B - long u
        'ṛ': '.r',    # U+1E5B - vocalic r
        'ḷ': '.l',    # U+1E37 - vocalic l
        'ñ': '~n',    # U+00F1 - tilde n
        'ṭ': '.t',    # U+1E6D - dot below t
        'ḍ': '.d',    # U+1E0D - dot below d
        'ṇ': '.n',    # U+1E47 - dot below n
        'ś': 'z',     # U+015B - acute s (palatal sibilant)
        'ṣ': '.s',    # U+1E63 - dot below s (retroflex sibilant)
        'ḥ': '.h',    # U+1E25 - dot below h (visarga marker)
        'ṁ': '.m',    # U+1E41 - dot above m (anusvara)
    }
    
    result = []
    i = 0
    while i < len(text):
        char = text[i]
        if char in iast_to_vh_map:
            result.append(iast_to_vh_map[char])
            i += 1
        else:
            result.append(char)
            i += 1
    
    return ''.join(result)


def is_vowel(code: int) -> bool:
    """Check if code represents a vowel (including diphthongs)."""
    return 1 <= code <= 13 or code == 50  # hiatus counts as vowel-like


def decode(word: Word, scheme: str = "VH") -> str:
    """Decode a word from internal representation to transliteration.

    Args:
        word: Word object with numeric elements
        scheme: Transliteration scheme ("VH", "WX", "SLP1")

    Returns:
        Transliterated string with hiatus handling
    """
    # Mapping from Unicode IAST characters to VH transliteration
    UTF8_TO_VH = {
        'ā': 'aa',    # U+0101 - long a
        'ī': 'ii',    # U+012B - long i
        'ū': 'uu',    # U+016B - long u
        'ṛ': '.r',    # U+1E5B - vocalic r
        'ḷ': '.l',    # U+1E37 - vocalic l
        'ñ': '~n',    # U+00F1 - tilde n
        'ṭ': '.t',    # U+1E6D - dot below t
        'ḍ': '.d',    # U+1E0D - dot below d
        'ṇ': '.n',    # U+1E47 - dot below n
        'ś': 'z',     # U+015B - acute s (palatal sibilant)
        'ṣ': '.s',    # U+1E63 - dot below s (retroflex sibilant)
        'ḥ': '.h',    # U+1E25 - dot below h (visarga marker)
        'ṁ': '.m',    # U+1E41 - dot above m (anusvara)
        'ṃ': '.m',    # U+1E43 - dot below m (anusvara variant)
        'ė': 'e',     # variants
        'ơ': 'o',
    }
    
    # VH tokens that represent vowels
    VH_VOWELS = {'a', 'aa', 'i', 'ii', 'u', 'uu', 'e', 'ai', 'o', 'au', '.r', '.l'}
    
    schemes = {
        "VH": CANON_VH,
        "WX": CANON_WX,
        "SLP1": CANON_SLP1,
    }
    canon_map = schemes.get(scheme, CANON_VH)

    result = []
    word_list = word.elements if isinstance(word, Word) else word

    prev_is_vowel = False
    for elem in word_list:
        elem_int = int(elem) if not isinstance(elem, int) else elem

        if elem_int not in canon_map:
            # Unknown character - handle special cases
            if elem_int < 0:
                # Negative codes encode UTF-8 characters by their Unicode code point
                try:
                    utf8_char = chr(-elem_int)
                    # Convert UTF-8 IAST to VH if available
                    if utf8_char in UTF8_TO_VH:
                        vh_text = UTF8_TO_VH[utf8_char]
                        result.append(vh_text)
                        prev_is_vowel = vh_text in VH_VOWELS
                    else:
                        result.append(utf8_char)
                        prev_is_vowel = False
                except (ValueError, OverflowError):
                    # Fallback for out-of-range codes
                    result.append(f"#{elem_int}")
                    prev_is_vowel = False
            elif -60 < elem_int < 0:
                result.append(f"#{chr(elem_int + 48)}")
                prev_is_vowel = False
            else:
                result.append(f"#{elem_int}")
                prev_is_vowel = False
        else:
            text = canon_map[elem_int]
            # Add hiatus marker if current is vowel and previous was vowel
            if prev_is_vowel and is_vowel(elem_int):
                result.append("_" + text)
            else:
                result.append(text)
            prev_is_vowel = is_vowel(elem_int)

    return "".join(result)


def rdecode(word: Word, scheme: str = "VH") -> str:
    """Decode a reversed word (mirror)."""
    if isinstance(word, Word):
        reversed_word = Word(word.elements[::-1])
    else:
        reversed_word = Word(word[::-1])
    return decode(reversed_word, scheme)


def robust_decode(word: Word, scheme: str = "VH") -> str:
    """Robust decoding that doesn't fail on special characters."""
    try:
        return decode(word, scheme)
    except:
        # Fallback for problematic characters
        return "".join(str(e) for e in word)


def switch_decode(translit: str, word: Word) -> str:
    """Decode with automatic scheme detection."""
    scheme_map = {
        "VH": "VH",
        "IAST": "VH",  # Map to closest
        "WX": "WX",
        "SLP1": "SLP1",
    }
    scheme = scheme_map.get(translit, "VH")
    return decode(word, scheme)


def decode_list(words: List[Word], scheme: str = "VH") -> List[str]:
    """Decode a list of words."""
    return [decode(w, scheme) for w in words]


def uniromcode(word_list: List) -> str:
    """Decode as Unicode Roman (VH scheme)."""
    if isinstance(word_list, Word):
        return decode(word_list, "VH")
    elif isinstance(word_list, list) and len(word_list) > 0:
        if isinstance(word_list[0], int):
            return decode(Word(word_list), "VH")
        else:
            return "".join(decode(w, "VH") for w in word_list)
    return ""


def unidevcode(word_list: List) -> str:
    """Convert to Devanagari Unicode (stub - would need full mapping)."""
    # Basic Devanagari transliteration for a limited set of tokens.
    # This is a heuristic stub and not a full orthographic implementation.
    VOWEL_INDEP = {
        "a": "अ",
        "aa": "आ",
        "i": "इ",
        "ii": "ई",
        "u": "उ",
        "uu": "ऊ",
        "e": "ए",
        "ai": "ऐ",
        "o": "ओ",
        "au": "औ",
        ".r": "ॠ",
    }
    VOWEL_MATRA = {
        "a": "",
        "aa": "ा",
        "i": "ि",
        "ii": "ी",
        "u": "ु",
        "uu": "ू",
        "e": "े",
        "ai": "ै",
        "o": "ो",
        "au": "ौ",
        ".r": "ृ",
    }
    CONSONANT = {
        "k": "क",
        "kh": "ख",
        "g": "ग",
        "gh": "घ",
        "c": "च",
        "ch": "छ",
        "j": "ज",
        "jh": "झ",
        "t": "त",
        "th": "थ",
        "d": "द",
        "dh": "ध",
        "n": "न",
        "p": "प",
        "ph": "फ",
        "b": "ब",
        "bh": "भ",
        "m": "म",
        "y": "य",
        "r": "र",
        "l": "ल",
        "v": "व",
        "s": "स",
        "h": "ह",
        ".t": "ट",
        ".th": "ठ",
        ".d": "ड",
        ".dh": "ढ",
        ".n": "ण",
        "z": "श",
        ".s": "ष",
        "~~": "ः",  # anusvara
    }

    # Helper to get VH token sequence from codes
    def codes_to_tokens(codes: List[int]) -> List[str]:
        tokens: List[str] = []
        for c in codes:
            if c in CANON_VH:
                tokens.append(CANON_VH[c])
            # Skip unknown codes instead of including them as error markers
        return tokens

    def render_tokens(tokens: List[str]) -> str:
        out = []
        pending_consonant = None
        for tok in tokens:
            # Treat underscore markers as vowel separators
            if tok.startswith("_"):
                tok = tok[1:]

            if tok in VOWEL_INDEP:
                if pending_consonant is None:
                    out.append(VOWEL_INDEP[tok])
                else:
                    # Attach matra to consonant
                    out.append(pending_consonant + VOWEL_MATRA.get(tok, ""))
                    pending_consonant = None
            elif tok in CONSONANT:
                # Special case: anusvara doesn't accumulate as pending
                if tok == "~~":
                    if pending_consonant is not None:
                        out.append(pending_consonant)
                        pending_consonant = None
                    out.append(CONSONANT[tok])
                else:
                    # If a consonant is pending with no explicit vowel, keep it (inherent 'a')
                    if pending_consonant is not None:
                        out.append(pending_consonant)
                    pending_consonant = CONSONANT[tok]
            else:
                # Unknown token: keep pending consonant if any, then append token as-is
                if pending_consonant is not None:
                    out.append(pending_consonant + "a")  # Add inherent 'a' to pending consonant
                    pending_consonant = None
                # For unknown tokens, try to find them in CONSONANT as fallback
                # or append as raw string
                if tok in CONSONANT:
                    pending_consonant = CONSONANT[tok]
                else:
                    out.append(tok)

        if pending_consonant is not None:
            # Append final pending consonant with virama (्) to suppress inherent vowel
            out.append(pending_consonant + "्")

        return "".join(out)

    # Handle Word instance and lists
    if isinstance(word_list, Word):
        tokens = codes_to_tokens(word_list.elements)
        return render_tokens(tokens)
    elif isinstance(word_list, list) and len(word_list) > 0:
        if isinstance(word_list[0], int):
            tokens = codes_to_tokens(word_list)
            return render_tokens(tokens)
        else:
            # When given a list of words, render each word separately with spaces
            # This preserves word boundaries
            rendered_words = []
            for w in word_list:
                # Render each word's VH to Devanagari
                if isinstance(w, Word):
                    vh_text = decode(w, "VH")
                else:
                    vh_text = decode(Word(w), "VH")
                
                # Parse VH text into tokens
                REVERSE_VH = {v: k for k, v in CANON_VH.items()}
                TOKENS = sorted(REVERSE_VH.keys(), key=lambda s: -len(s))
                
                i = 0
                word_tokens: List[str] = []
                while i < len(vh_text):
                    matched = False
                    for tok in TOKENS:
                        if vh_text.startswith(tok, i):
                            word_tokens.append(tok)
                            i += len(tok)
                            matched = True
                            break
                    if not matched:
                        word_tokens.append(vh_text[i])
                        i += 1
                
                # Render tokens to Devanagari
                rendered_words.append(render_tokens(word_tokens))
            
            return " ".join(rendered_words)
    return ""
