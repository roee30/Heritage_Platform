"""Sanskrit morphology type definitions translated from skt_morph.mli."""

from enum import Enum
from typing import List, Optional, Union, Tuple
from dataclasses import dataclass


class Deictic(Enum):
    """Deictic pronouns - gender determined from context."""

    SPEAKER = "speaker"
    LISTENER = "listener"
    SELF = "self"
    NUMERAL = "numeral"


class Gender(Enum):
    """Noun genders in Sanskrit."""

    MASCULINE = "masculine"
    NEUTER = "neuter"
    FEMININE = "feminine"
    DEICTIC = "deictic"


class Number(Enum):
    """Noun numbers in Sanskrit."""

    SINGULAR = "singular"
    DUAL = "dual"
    PLURAL = "plural"


class Case(Enum):
    """Noun cases in Sanskrit (8 cases)."""

    NOMINATIVE = "nominative"
    ACCUSATIVE = "accusative"
    INSTRUMENTAL = "instrumental"
    DATIVE = "dative"
    ABLATIVE = "ablative"
    GENITIVE = "genitive"
    LOCATIVE = "locative"
    VOCATIVE = "vocative"


class Gana(int):
    """Verb class: 1-10 for regular verbs, 11 for denominatives."""

    pass


class AorClass(int):
    """Aorist class: 1-7."""

    pass


class Person(Enum):
    """Grammatical person."""

    FIRST = "first"
    SECOND = "second"
    THIRD = "third"


class Conjugation(Enum):
    """Verb conjugation types."""

    PRIMARY = "primary"
    CAUSATIVE = "causative"
    DESIDERATIVE = "desiderative"
    INTENSIVE = "intensive"


class Voice(Enum):
    """Verb voice/pada."""

    ACTIVE = "active"
    MIDDLE = "middle"
    PASSIVE = "passive"


class PrMode(Enum):
    """Present tense modes."""

    PRESENT = "present"  # Indicative (lat)
    IMPERFECT = "imperfect"  # Preterit (laf)
    IMPERATIVE = "imperative"  # (lot)
    OPTATIVE = "optative"  # Potential (lif)


class Tense(Enum):
    """Tense types (other than present)."""

    FUTURE = "future"  # (lrt)
    PERFECT = "perfect"  # Remote past (lit)
    AORIST = "aorist"  # Immediate past (lut)
    INJUNCTIVE = "injunctive"  # (let)
    BENEDICTIVE = "benedictive"  # Precative (azirlif)
    CONDITIONAL = "conditional"  # Preterit of future (lrf)
    SUBJUNCTIVE = "subjunctive"  # (let) - rare


@dataclass
class Paradigm:
    """Finite verb paradigm."""

    pass  # Will be subclassed


@dataclass
class Presenta(Paradigm):
    """Parasmaipade present."""

    gana: int
    pr_mode: PrMode


@dataclass
class Presentm(Paradigm):
    """Aatmanepade present."""

    gana: int
    pr_mode: PrMode


@dataclass
class Presentp(Paradigm):
    """Passive present."""

    pr_mode: PrMode


@dataclass
class Conjug(Paradigm):
    """Other tenses/modes/aspects."""

    tense: Tense
    voice: Voice


@dataclass
class Perfut(Paradigm):
    """Periphrastic future (lut) - always active."""

    voice: Voice


@dataclass
class Finite:
    """Finite forms of verbs."""

    conjugation: Conjugation
    paradigm: Paradigm


# Kritya - shades of intention of passive future/potential participle
KRITYA_YA = 1  # -ya (obligation, necessity or possibility, potentiality)
KRITYA_ANIYA = 2  # -aniiya (fitness, desirability, effectivity)
KRITYA_TAVYA = 3  # -tavya (necessity, unavoidability)

Kritya = int


class Participle(Enum):
    """Participle types (kridanta stems)."""

    PPP = "ppp"  # passive past participle
    PPPA = "pppa"  # active past participle
    PPRA = "ppra"  # active present participle
    PPRM = "pprm"  # middle present participle
    PPRP = "pprp"  # passive present participle
    PPFTA = "ppfta"  # active perfect participle
    PPFTM = "ppftm"  # middle perfect participle
    PFUTA = "pfuta"  # active future participle
    PFUTM = "pfutm"  # middle future participle
    PFUTP = "pfutp"  # passive future/potential participle/gerundive
    ACTION_NOUN = "action_noun"


@dataclass
class Verbal:
    """Verbal adjectives."""

    conjugation: Conjugation
    participle: Union[Participle, Tuple[Participle, int]]  # Tuple for PFUTP with kritya


class Invar(Enum):
    """Invariable verbal forms."""

    INFI = "infinitive"
    ABSV = "absolutive_vowel"  # root absolutives in -tvaa
    ABSC = "absolutive_consonant"  # root absolutives in -tvaa
    ABSY = "absolutive_ya"  # absolutives in -ya with preverb


@dataclass
class Modal:
    """Modal forms (indeclinables)."""

    conjugation: Conjugation
    invar: Invar


class NanKind(Enum):
    """Nominal derivative kinds."""

    AGENT = "agent"
    INSTRUMENTAL = "instrumental"
    LOCATIVE = "locative"
    POSSESSIVE = "possessive"


class Sadhana(Enum):
    """Nominal derivative formation (sadhana)."""

    DENOM_NA = "denom_na"
    DENOM_ANI = "denom_ani"
    DENOM_ANE = "denom_ane"
    DERIVED = "derived"
    PRIMITIVE = "primitive"


@dataclass
class Nominal:
    """Nominal forms (nouns, adjectives)."""

    stem: str
    gender: Gender
    number: Number
    case: Case
    sadhana: Optional[Sadhana] = None
    nan_kind: Optional[NanKind] = None


class IndicKind(Enum):
    """Indeclinable forms."""

    ADVERB = "adverb"
    PREPOSITION = "preposition"
    CONJUNCTION = "conjunction"
    PARTICLE = "particle"


@dataclass
class InflectedForm:
    """A complete inflected form with all morphological info."""

    stem: str
    gender: Optional[Gender] = None
    number: Optional[Number] = None
    case: Optional[Case] = None
    person: Optional[Person] = None
    tense: Optional[Tense] = None
    mood: Optional[Union[PrMode, Invar]] = None
    voice: Optional[Voice] = None
    conjugation: Optional[Conjugation] = None
    paradigm: Optional[Paradigm] = None
