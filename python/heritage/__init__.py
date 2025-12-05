"""Sanskrit Heritage Platform - Python Translation

A Python port of the OCaml-based Sanskrit morphological analysis system.
This package provides core functionality for Sanskrit text analysis, morphological
parsing, and segmentation (sandhi splitting).

Main modules:
- heritage.modules: Core functionality
- heritage.types: Type definitions and enums
"""

__version__ = "2.0.0-python"
__author__ = "Gérard Huet, Pawan Goyal"

from .modules import interface

__all__ = ["interface"]
