#!/usr/bin/env python3
"""
Simple morphological analysis module to strip common nominal inflections.

This is a stub that handles basic Sanskrit nominal endings to allow matching
of inflected forms against a lemma-based lexicon.
"""

# Common Sanskrit nominal endings to try stripping (in priority order)
# These are VH-encoded endings; try longest first
NOMINAL_ENDINGS = [
    # Masculine singular nominative
    ".h",  # ḥ
    # Feminine/neutral singular
    "ii",  # ī (feminine nominative singular)
    "aa",  # ā (feminine nominative singular / neuter nominative accusative)
    # Neuter singular nominative/accusative
    "am",  # m (neuter accusative)
    # Accusative singular
    "am",  # am (masculine/neuter accusative singular)
    # General oblique singular
    "a",  # a (masculine nominative, various other forms)
]


def strip_inflection(word: str) -> list[str]:
    """Strip common morphological endings from a word.

    Returns a list of candidate root forms, with the longest-match stripper first.
    Example: 'abhi.seka.h' -> ['abhi.seka', 'abhi.seka.', ...]
    """
    candidates = [word]  # include the word itself as fallback
    
    for ending in NOMINAL_ENDINGS:
        if word.endswith(ending) and len(word) > len(ending):
            root = word[:-len(ending)]
            candidates.append(root)
    
    return candidates
