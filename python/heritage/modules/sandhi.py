"""
Sanskrit Sandhi (Euphony) Rules

Implements common sandhi transformations for combining adjacent words/segments.
Rules are organized by category: vowel, visarga, anusvāra, retroflex, etc.

Each rule function takes (token1, token2) and returns a list of possible
combined tokens (empty list if rule doesn't apply).
"""

from typing import List, Set


# Define character classes for Sanskrit phonetics
VOWELS = set("aāiīuūeoṛṝaiauai")
STOPS = set("kgcjṭḍtdpb")  # voiceless/voiced stops (visarga)
ANUSVARA = set("ṃm")
VISARGA = STOPS
RETROFLEX = set("ṭḍṇ")
DENTAL = set("tdn")
VELAR = set("kg")
PALATAL = set("cj")
LABIAL = set("pb")
FRICATIVES = set("śṣsh")  # sibilants


def _get_last_char(token: str) -> str:
    """Get the last character of a token."""
    return token[-1] if token else ""


def _get_first_char(token: str) -> str:
    """Get the first character of a token."""
    return token[0] if token else ""


def _get_last_vowel_idx(token: str) -> int:
    """Get index of last vowel in token."""
    for i in range(len(token) - 1, -1, -1):
        if token[i] in VOWELS:
            return i
    return -1


def vowel_sandhi(t1: str, t2: str) -> List[str]:
    """Vowel-sandhi rules (guṇa and vṛddhi expansions when vowels combine).

    Examples: a + i -> e (guṇa), a + e -> ai (vṛddhi)
    """
    results = []
    if not t1 or not t2:
        return results

    last_char = _get_last_char(t1)
    first_char = _get_first_char(t2)

    # Only apply if both are vowels
    if last_char not in VOWELS or first_char not in VOWELS:
        return results

    # Guṇa rules (a + i/u/ṛ -> e/o/ar)
    if last_char == "a":
        if first_char in ("i", "ī"):
            results.append(t1[:-1] + "e" + t2[1:])
        elif first_char in ("u", "ū"):
            results.append(t1[:-1] + "o" + t2[1:])
        elif first_char in ("ṛ", "ṝ"):
            results.append(t1[:-1] + "ar" + t2[1:])

    # Vṛddhi rules (ā + same vowel -> longer)
    if last_char in ("a", "ā") and first_char in ("a", "ā"):
        results.append(t1[:-1] + "ā" + t2[1:])

    # e/o before vowel -> ay/av
    if last_char == "e" and first_char in VOWELS:
        results.append(t1[:-1] + "ay" + t2)
    if last_char == "o" and first_char in VOWELS:
        results.append(t1[:-1] + "av" + t2)

    # Final vowel before vowel: insert glide
    if last_char == "i" and first_char in VOWELS:
        results.append(t1 + "y" + t2)
    if last_char == "u" and first_char in VOWELS:
        results.append(t1 + "v" + t2)

    return results


def visarga_sandhi(t1: str, t2: str) -> List[str]:
    """Visarga (stop) sandhi rules.

    Rules like: voiceless stop + voiced stop -> voiced stop
    voiceless stop before s/ś/ṣ -> becomes s/ś/ṣ
    """
    results = []
    if not t1 or not t2:
        return results

    last = _get_last_char(t1)
    first = _get_first_char(t2)

    # Only apply if t1 ends with a visarga
    if last not in VISARGA:
        return results

    # Before voiceless stop: stay voiceless
    # Before voiced stop or nasal: assimilate to voiced
    if first in ("g", "ġ", "d", "ḍ", "b") or first in ("ṅ", "ñ", "ṇ", "n", "m"):
        # Map to voiced version of same class
        voicing_map = {
            "k": "g",
            "kh": "gh",
            "c": "j",
            "ch": "jh",
            "ṭ": "ḍ",
            "ṭh": "ḍh",
            "t": "d",
            "th": "dh",
            "p": "b",
            "ph": "bh",
        }
        voiced = voicing_map.get(last, last)
        results.append(t1[: -len(last)] + voiced + t2)

    # Before sibilant (s, ś, ṣ): assimilate to sibilant
    if first in ("s", "ś", "ṣ"):
        results.append(t1[: -len(last)] + first + t2[1:])

    return results


def anusvara_sandhi(t1: str, t2: str) -> List[str]:
    """Anusvāra (nasal marker) sandhi rules.

    Anusvāra assimilates to the class of the following consonant.
    ṃ before velar -> ṅ
    ṃ before dental -> n
    ṃ before labial -> m
    etc.
    """
    results = []
    if not t1 or not t2:
        return results

    last = _get_last_char(t1)
    first = _get_first_char(t2)

    # Only apply if t1 ends with anusvāra
    if last not in ANUSVARA:
        return results

    # Determine nasal class based on first consonant of t2
    nasal_map = {}
    if first in VELAR:
        nasal = "ṅ"
    elif first in PALATAL:
        nasal = "ñ"
    elif first in RETROFLEX:
        nasal = "ṇ"
    elif first in DENTAL:
        nasal = "n"
    elif first in LABIAL:
        nasal = "m"
    else:
        return results  # Can't determine; skip

    results.append(t1[:-1] + nasal + t2)
    return results


def retroflex_sandhi(t1: str, t2: str) -> List[str]:
    """Retroflex assimilation rules.

    t/d + retroflex -> retroflex
    """
    results = []
    if not t1 or not t2:
        return results

    last = _get_last_char(t1)
    first = _get_first_char(t2)

    # If t1 ends with dental and t2 starts with retroflex
    if last in DENTAL and first in RETROFLEX:
        results.append(t1[:-1] + first + t2[1:])

    return results


def consonant_cluster_sandhi(t1: str, t2: str) -> List[str]:
    """Final consonant cluster rules.

    Rules for handling final consonants before the next word.
    """
    results = []
    if not t1 or not t2:
        return results

    last = _get_last_char(t1)
    first = _get_first_char(t2)

    # Voiced consonant + voiceless -> assimilate to voiceless
    if last in ("g", "j", "ḍ", "d", "b"):
        if first in STOPS or first in FRICATIVES:
            voiceless_map = {"g": "k", "j": "c", "ḍ": "ṭ", "d": "t", "b": "p"}
            voiceless = voiceless_map.get(last, last)
            results.append(t1[:-1] + voiceless + t2)

    return results


def apply_all_sandhi_rules(t1: str, t2: str) -> List[str]:
    """Apply all sandhi rules and return all possible combined tokens."""
    combined = set()

    # Plain concatenation (always possible)
    combined.add(t1 + t2)

    # Apply each rule category
    combined.update(vowel_sandhi(t1, t2))
    combined.update(visarga_sandhi(t1, t2))
    combined.update(anusvara_sandhi(t1, t2))
    combined.update(retroflex_sandhi(t1, t2))
    combined.update(consonant_cluster_sandhi(t1, t2))

    return list(combined)
