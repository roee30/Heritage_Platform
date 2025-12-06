# Quick Start Guide - Python Heritage Platform

## 5-Minute Setup

### Step 1: Navigate to Python Directory
```powershell
cd c:\Users\roee\code\clean\Heritage_Platform\python
```

### Step 2: Run Tests (Verify Installation)
```powershell
python test_basic.py
```

**Expected output:**
```
==================================================
✓ All tests passed!
==================================================
```

### Step 3: Test the Interface

#### Option A: Command Line
```powershell
python main.py "namaskar"
```

#### Option B: Python Import
```python
from heritage.modules.interface import Interface

interface = Interface()
env = {'text': 'namaskar', 't': 'VH'}
html = interface.safe_engine(env)
print(html)
```

---

## Project Structure at a Glance

```
python/
├── main.py                          # Run this file
├── test_basic.py                   # Run to verify
├── heritage/
│   ├── types/morphology.py         # Sanskrit types
│   └── modules/
│       ├── interface.py            # Main logic
│       ├── word.py                 # Word class
│       ├── canon.py               # Encoding
│       ├── phases.py              # Grammar
│       ├── config.py              # Settings
│       └── stubs.py               # Placeholders
├── README.md                        # User guide
├── TRANSLATION_GUIDE.md             # Technical details
└── IMPLEMENTATION_SUMMARY.md        # What was done
```

---

## Usage Examples

### 1. As GoldenDict Plugin

Configure in GoldenDict:
```
Programs:
  Type: HTML
  Command: python "c:\path\to\python\main.py" "%GDWORD%"
```

Then select a Sanskrit word in GoldenDict and press Ctrl+C twice.

### 2. As Web Service

```python
from heritage.modules.interface import process_request

# Simulate CGI request
env = {
    'text': 'namaskar',
    't': 'VH',           # Transliteration scheme
    'lex': 'MW',         # Lexicon
    'cpts': '',          # Checkpoints
}

html_output = process_request(env)

# Output is HTML that can be sent to browser or displayed
print(html_output)
```

### 3. Direct Module Usage

```python
from heritage.modules.word import Word
from heritage.modules.canon import decode

# Create a word (sequence of phonetic elements)
word = Word([1, 2, 3])  # represents: a, aa, i

# Decode to text
roman = decode(word, "VH")      # "aai"
wx = decode(word, "WX")         # "Ai"
slp1 = decode(word, "SLP1")     # "Ai"

print(f"VH: {roman}")
print(f"WX: {wx}")
print(f"SLP1: {slp1}")
```

### 4. Working with Types

```python
from heritage.types import Gender, Number, Case, Person

# Create morphological information
gender = Gender.MASCULINE
number = Number.SINGULAR
case = Case.NOMINATIVE
person = Person.THIRD

print(f"Form: {gender.value} {number.value} {case.value}")
# Output: Form: masculine singular nominative
```

---

## What Each Module Does

| Module | Purpose |
|--------|---------|
| `main.py` | Entry point - takes Sanskrit word, returns HTML |
| `interface.py` | Core processing engine |
| `word.py` | Represents Sanskrit phonetic sequences |
| `canon.py` | Converts between encoding schemes |
| `phases.py` | Grammar phase definitions |
| `config.py` | System configuration |
| `stubs.py` | Placeholder for complex linguistics (ready to implement) |
| `morphology.py` | Type definitions for Sanskrit grammar |

---

## Understanding the Code

### How Words Are Represented

```python
# Internal representation: numeric phonetic elements
word = Word([1, 2, 3])
# Element meanings (from canon.py):
# 1 = 'a'
# 2 = 'aa'
# 3 = 'i'

# Convert to text
text = decode(word, "VH")  # "aai"
```

### How Text Processing Works

1. **Input** - Raw Sanskrit text or word
2. **Encoding** - Convert to internal numeric representation
3. **Processing** - Apply morphological rules (currently stubbed)
4. **Analysis** - Extract morphological information
5. **Output** - Generate HTML with analysis

### How the Interface Works

```
User input (word)
    ↓
main.py processes request
    ↓
Interface.graph_engine()
    ↓
Parse CGI parameters
    ↓
Segment text (stub)
    ↓
Get morphological tags (stub)
    ↓
Generate HTML
    ↓
Return to GoldenDict/Browser
```

---

## Extending the System

### Adding a Feature: Print Word Operations

```python
# In your script:
from heritage.modules.word import Word

w = Word([1, 2, 3])

print(f"Length: {w.length()}")           # 3
print(f"Reversed: {w.mirror().elements}") # [3, 2, 1]
print(f"Prefix: {w[0:2]}")                # Word([1, 2])

# Concatenate words
w2 = Word([4, 5])
combined = w + w2  # Word([1, 2, 3, 4, 5])
print(f"Combined: {combined}")
```

### Adding a Feature: Custom Encoding

```python
# Create custom transliteration
def my_decode(word):
    mapping = {
        1: 'a', 2: 'A', 3: 'i',
        # ... etc
    }
    return "".join(mapping.get(e, f"?{e}") for e in word.elements)

word = Word([1, 2, 3])
print(my_decode(word))  # "aAi"
```

### Adding a Feature: Custom Phase Logic

```python
from heritage.modules.phases import Phase, Phases

# Check phase type
phase = Phase.NOUN

if Phases.ii_phase(phase):
    print("This is a compound phase")
else:
    print("This is a basic phase")
```

---

## Troubleshooting

### Issue: Import Error
```
ImportError: No module named 'heritage'
```

**Solution:** Make sure you're in the `python` directory:
```powershell
cd python
python test_basic.py
```

### Issue: Python Not Found
```
'python' is not recognized
```

**Solution:** Use full path or ensure Python is in PATH:
```powershell
C:\Python39\python.exe main.py "word"
# Or add Python to PATH
```

### Issue: Permission Denied
```
Permission denied: 'main.py'
```

**Solution:** Make sure main.py is executable:
```powershell
# On Windows, this is usually not needed
# On Linux: chmod +x main.py
```

---

## Next Steps

1. ✅ **Verify Installation** - Run `python test_basic.py`
2. ✅ **Try It Out** - Run `python main.py "word"`
3. 📖 **Read Documentation** - See `README.md` for details
4. 🔧 **Implement Features** - Follow `TRANSLATION_GUIDE.md`
5. 🧪 **Add Tests** - Expand `test_basic.py`

---

## Resources

| Resource | Location |
|----------|----------|
| User Guide | `README.md` |
| Technical Details | `TRANSLATION_GUIDE.md` |
| Implementation Status | `IMPLEMENTATION_SUMMARY.md` |
| Tests | `test_basic.py` |
| Source Code | `heritage/` directory |

---

## Quick Reference

### Common Commands

```powershell
# Navigate to project
cd python

# Run tests
python test_basic.py

# Process a word (command line)
python main.py "namaskar"

# Launch Python REPL
python

# Check Python version
python --version

# View help
python main.py --help  # Not yet implemented
```

### Common Code

```python
# Import commonly used items
from heritage.modules import Word, decode, Config, Params

# Create a word from phonetic codes
word = Word([1, 2, 3])

# Decode to multiple schemes
vh = decode(word, "VH")
wx = decode(word, "WX")

# Access configuration
translit_scheme = Config.DEFAULT_TRANSLITERATION
server_host = Config.SERVER_HOST

# Access CGI parameters
corpus_dir_param = Params.CORPUS_DIR
```

---

## Support

For issues:
1. Check documentation files
2. Review inline code comments
3. Look at test examples
4. Check original OCaml code for reference

Good luck! 🙏

---

**Last Updated:** December 5, 2025
