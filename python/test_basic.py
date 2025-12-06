"""Basic tests for Python Heritage Platform translation."""

import sys
from pathlib import Path

# Add package to path
sys.path.insert(0, str(Path(__file__).parent))

from heritage.types.morphology import (
    Gender,
    Number,
    Case,
    Person,
    Conjugation,
    Voice,
    PrMode,
    Tense,
    Paradigm,
)
from heritage.modules.word import Word
from heritage.modules.config import Config
from heritage.modules.canon import decode, uniromcode
from heritage.modules.phases import Phase, Phases


def test_types():
    """Test morphology type definitions."""
    print("Testing types...")

    assert Gender.MASCULINE.value == "masculine"
    assert Number.SINGULAR.value == "singular"
    assert Case.NOMINATIVE.value == "nominative"
    assert Person.FIRST.value == "first"
    assert Conjugation.PRIMARY.value == "primary"
    assert Voice.ACTIVE.value == "active"

    print("[OK] Types test passed")


def test_word():
    """Test Word class."""
    print("Testing Word...")

    w = Word([1, 2, 3])
    assert len(w) == 3
    assert w.length() == 3

    w2 = Word([1, 2])
    # w2 is a prefix of w (w=[1,2,3], w2=[1,2])
    assert w2.prefix(w) == True

    w3 = Word([1])
    # w3 is a prefix of w2 (w2=[1,2], w3=[1])
    assert w3.prefix(w2) == True

    mirror = w.mirror()
    assert mirror.elements == [3, 2, 1]

    print("[OK] Word test passed")


def test_canon():
    """Test character encoding/decoding."""
    print("Testing Canon...")

    # Test VH decoding
    w = Word([1, 2, 3])  # a, aa, i
    result = decode(w, "VH")
    assert "a" in result

    # Test uniromcode
    result2 = uniromcode([1, 2])
    assert isinstance(result2, str)

    print("[OK] Canon test passed")


def test_phases():
    """Test phase definitions."""
    print("Testing Phases...")

    assert Phases.UNKNOWN == Phase.UNKNOWN

    unknown_phase = Phases.unknown()
    assert unknown_phase == Phase.UNKNOWN

    assert Phases.ii_phase(Phase.IIC) == True
    assert Phases.ii_phase(Phase.NOUN) == False

    print("[OK] Phases test passed")


def test_config():
    """Test configuration."""
    print("Testing Config...")

    assert Config.PLATFORM == "Station"
    assert Config.DEFAULT_TRANSLITERATION == "VH"
    assert Config.DEFAULT_LEXICON == "MW"

    print("[OK] Config test passed")


def test_interface_basic():
    """Test basic Interface instantiation."""
    print("Testing Interface...")

    from heritage.modules.interface import Interface

    interface = Interface()
    assert interface is not None
    assert interface.output is not None

    # Test basic output
    interface.ps("test")
    output = interface.get_output()
    assert "test" in output

    print("[OK] Interface test passed")


def main():
    """Run all tests."""
    print("=" * 50)
    print("Sanskrit Heritage Platform - Python Translation Tests")
    print("=" * 50)
    print()

    try:
        test_types()
        test_word()
        test_canon()
        test_phases()
        test_config()
        test_interface_basic()

        print()
        print("=" * 50)
        print("[PASS] All tests passed!")
        print("=" * 50)
        return 0

    except Exception as e:
        print()
        print("=" * 50)
        print(f"[FAIL] Test failed: {e}")
        print("=" * 50)
        import traceback

        traceback.print_exc()
        return 1


if __name__ == "__main__":
    sys.exit(main())
