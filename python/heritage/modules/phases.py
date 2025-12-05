"""Phases module - lexical sorts as states in the modular transducer."""

from enum import Enum
from dataclasses import dataclass


class Phase(Enum):
    """Lexical sorts/phases in the Sanskrit morphological analyzer."""

    # Core noun/pronoun phrases
    NOUN = "noun"
    PRON = "pronoun"

    # Root phase
    ROOT = "root"

    # Indeclinables
    INDE = "indeclinable"

    # Absolutives
    ABSV = "absolutive_vowel"
    ABSC = "absolutive_consonant"
    ABSO = "absolutive_ya"

    # Vocatives and invocations
    VOCA = "vocative"
    INV = "invocation"

    # Compound phases
    IIC = "compound_first_part"
    IIIF = "compound_ifc_from_iic"
    IIV = "inchoative"
    IIVV = "inchoative_middle"
    IIVC = "inchoative_consonant"

    # Auxiliary verbs
    AUXI = "auxiliary"
    AUXIINV = "auxiliary_invocation"
    AUXIK = "auxiliary_k"
    AUXIICK = "auxiliary_ick"

    # Second part of compounds
    IFC = "compound_second_part"
    INDIFC = "indeclinable_compound"

    # Periphrastic
    PERI = "periphrastic"

    # Lopa forms
    LOPA = "lopa"
    LOPAK = "lopak"

    # Unknown phase
    UNKNOWN = "unknown"


class Phases:
    """Phases module - constants and utilities."""

    # Core phase singleton
    UNKNOWN = Phase.UNKNOWN

    @staticmethod
    def unknown() -> Phase:
        """Get the unknown phase."""
        return Phase.UNKNOWN

    @staticmethod
    def aa_phase(phase: Phase) -> Phase:
        """Get AA variant of phase (for doubled vowels)."""
        return phase  # Placeholder

    @staticmethod
    def preverb_phase(phase: Phase) -> bool:
        """Check if phase is a preverb phase."""
        return False  # Placeholder

    @staticmethod
    def ii_phase(phase: Phase) -> bool:
        """Check if phase is an II (compound initial) phase."""
        return phase in (Phase.IIC, Phase.IIIF, Phase.IIV, Phase.IIVV, Phase.IIVC)

    @staticmethod
    def un_lopa(phase: Phase) -> Phase:
        """Remove lopa marker from phase."""
        if phase == Phase.LOPA:
            return Phase.NOUN  # Placeholder mapping
        elif phase == Phase.LOPAK:
            return Phase.NOUN
        return phase
