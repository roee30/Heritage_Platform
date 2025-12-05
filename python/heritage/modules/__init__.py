"""Heritage Platform Python Translation.

Core modules for Sanskrit morphological analysis and text processing.
"""

from .word import Word
from .config import Config, Params
from .canon import decode, rdecode, robust_decode, switch_decode

__all__ = [
    "Word",
    "Config",
    "Params",
    "decode",
    "rdecode",
    "robust_decode",
    "switch_decode",
]
