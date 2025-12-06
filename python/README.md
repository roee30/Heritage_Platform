# Sanskrit Heritage Platform - Python Translation

This is a Python translation of the Sanskrit Heritage Platform, converting the OCaml-based morphological analysis system to pure Python.

## Project Structure

```
python/
├── main.py                 # Entry point (CGI handler, replaces arg.ps1)
├── requirements.txt        # Python dependencies
├── README.md              # This file
└── heritage/              # Main package
    ├── __init__.py
    ├── types/             # Type definitions
    │   ├── __init__.py
    │   └── morphology.py  # Sanskrit morphology types
    └── modules/           # Core modules
        ├── __init__.py
        ├── word.py        # Word representation
        ├── config.py      # Configuration and paths
        ├── canon.py       # Character encoding/decoding
        ├── phases.py      # Lexical phase definitions
        ├── stubs.py       # Complex module stubs
        └── interface.py   # Main CGI interface
```

## Overview

The translation preserves the core architecture of the original OCaml system:

1. **Word Representation** (`word.py`) - Sanskrit words as sequences of phonetic elements
2. **Character Encoding** (`canon.py`) - Conversion between VH, WX, SLP1 schemes and Devanagari
3. **Type System** (`morphology.py`) - Complete Sanskrit morphological types (genders, cases, tenses, etc.)
4. **Phases** (`phases.py`) - Lexical sort definitions for the modular transducer
5. **Interface** (`interface.py`) - CGI handler for text segmentation and analysis

## Translation Status

### Completed ✓
- Core type definitions (Gender, Number, Case, Person, Tense, Voice, Conjugation, etc.)
- Word module with operations (prefix, mirror, length)
- Character encoding/decoding (Canon module)
- Configuration and paths
- Phases/lexical sorts
- Main CGI interface structure
- Error handling

### Partially Implemented 
- Morphology lookups (stubs)
- Segmentation engine (stubs)
- HTML output generation (basic)
- CGI parameter parsing

### Not Yet Translated (Stubs Available)
- Full automata implementation (Zen package)
- Complete morphological database loading
- Transducer compilation and execution
- Complex sandhi resolution
- Corpus management
- Web utilities (full implementation)

## Usage

### As CGI Handler (GoldenDict)

From PowerShell:
```powershell
python main.py "namaskar"
```

Or configure in GoldenDict:
```
Command line: python "path\to\python\main.py" "%GDWORD%"
```

### As Python Module

```python
from heritage.modules.interface import Interface

interface = Interface()
output = interface.safe_engine({'text': 'namaskar'})
print(output)
```

## Architecture

### Phases
The system organizes morphological analysis into distinct phases (lexical sorts):
- NOUN, PRON: Nominal
- ROOT: Verb roots
- INDE: Indeclinables
- IIC, IFC: Compound parts
- AUXI: Auxiliary verbs
- VOCA: Vocatives
- And many more...

### Character Encoding
Supports multiple transliteration schemes:
- **VH**: Velthuis-Harvard (standard, used as internal representation)
- **WX**: University of Hyderabad scheme
- **SLP1**: Sanskrit Library Phonetic v1
- **Devanagari**: Unicode representation

### Segmentation
The sandhi splitter uses a modular transducer system to:
1. Read Sanskrit input text
2. Apply morphological rules
3. Split at morpheme boundaries
4. Generate segmentation graph
5. Display alternatives with checkpoints

## Implementation Notes

### Translation Strategy

This translation takes a pragmatic approach:

1. **Core Functionality First** - Focus on interface.ml entry point and its dependencies
2. **Stub Complex Modules** - Morphology database and automata provided as interfaces
3. **Preserve Type System** - All OCaml types faithfully translated to Python
4. **Modular Design** - Each OCaml module maps to a Python module

### Key Differences from OCaml

| OCaml | Python |
|-------|--------|
| Immutable records | Dataclasses |
| Pattern matching | If/elif chains |
| Module system | Package structure |
| Variants (sum types) | Enums + Union types |
| Functors | Factory functions/classes |
| Lazy evaluation | Explicit generators |

### Performance Considerations

The Python version is suitable for:
- Development and testing
- Educational purposes
- Integration with other Python tools
- Web services

For production Sanskrit analysis at scale, the OCaml version remains recommended due to:
- Compiled performance
- Strict type safety
- Mature linguistic algorithms

## Integration Points

### With GoldenDict
The `main.py` script acts as a GoldenDict plugin, taking a Sanskrit word and returning HTML analysis.

### With Other Tools
The `heritage` package can be imported into other Python projects:
```python
from heritage.modules import Word, decode, Interface
```

## Development

### Running Tests
```bash
cd python
python -m pytest tests/  # Once test suite is added
```

### Adding Type Hints
The codebase uses Python 3.8+ type hints for clarity and IDE support.

### Code Style
- Follow PEP 8
- Use type hints for function signatures
- Document functions with docstrings

## Future Work

To complete the translation, implement:

1. **Morphology Database** - Load and query morphological data
2. **Transducers** - Implement finite state transducers for phonetic rules
3. **Automata** - Complete automata simulation from Zen package
4. **Sandhi Rules** - Full Sanskrit phonetic combination (sandhi) rules
5. **Corpus Tools** - Corpus management and annotation
6. **Web Interface** - Full web-based UI beyond CGI

## References

### Original OCaml Project
- **Repository**: https://gitlab.inria.fr/huet/Heritage_Platform
- **Resources**: https://gitlab.inria.fr/huet/Heritage_Resources
- **Zen Package**: https://gitlab.inria.fr/huet/Zen

### Sanskrit Resources
- Panini's Grammar
- Sanskrit linguistic databases
- Devanagari encoding standards

## License

Inherits the LGPL license from the original Heritage Platform.

## Authors

**Original OCaml Version:**
- Gérard Huet (INRIA)
- Pawan Goyal

**Python Translation:**
- [Your Name]

## Notes

This translation prioritizes:
- **Clarity** - Python is more readable than OCaml for many developers
- **Accessibility** - Python tooling is broader
- **Maintainability** - Easier to extend and modify

However, it currently sacrifices some of the performance and type safety of the original.
