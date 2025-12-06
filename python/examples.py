"""Usage example - demonstrates the Python Heritage Platform interface."""

import sys

from heritage.modules.interface import Interface
from heritage.modules.word import Word
from heritage.modules.canon import decode


def example_1_basic_interface():
    """Example 1: Basic interface usage."""
    print("\n" + "=" * 60)
    print("EXAMPLE 1: Basic Interface Usage")
    print("=" * 60)

    # Create interface instance
    interface = Interface()

    # Simulate a CGI request
    env = {
        "text": "namaskar",
        "t": "VH",
        "lex": "MW",
        "cache": "1",
    }

    # Process the request
    output = interface.safe_engine(env)

    # Show first 500 characters of output
    print("Output (first 500 chars):")
    print(output[:500])
    print("...")


def example_2_word_operations():
    """Example 2: Working with Word class."""
    print("\n" + "=" * 60)
    print("EXAMPLE 2: Word Operations")
    print("=" * 60)

    # Create words from phonetic codes
    word1 = Word([1, 2, 3])  # a, aa, i
    word2 = Word([4, 5])  # ii, u

    print(f"Word 1: {word1.elements}")
    print(f"Word 1 length: {word1.length()}")

    print(f"\nWord 2: {word2.elements}")
    print(f"Word 2 length: {word2.length()}")

    # Concatenate
    combined = word1 + word2
    print(f"\nCombined: {combined.elements}")
    print(f"Combined length: {len(combined)}")

    # Mirror/reverse
    mirrored = word1.mirror()
    print(f"\nOriginal: {word1.elements}")
    print(f"Mirrored: {mirrored.elements}")

    # Prefix checking
    prefix_word = Word([1, 2])
    print(f"\nIs {prefix_word.elements} a prefix of {word1.elements}?")
    print(f"Answer: {prefix_word.prefix(word1)}")


def example_3_character_encoding():
    """Example 3: Character encoding and transliteration."""
    print("\n" + "=" * 60)
    print("EXAMPLE 3: Character Encoding")
    print("=" * 60)

    # Create a word
    word = Word([1, 2, 3, 4, 5])  # a, aa, i, ii, u

    # Decode to different schemes
    print(f"Original codes: {word.elements}")

    vh = decode(word, "VH")
    print(f"VH (Harvard-Kyoto):  {vh}")

    wx = decode(word, "WX")
    print(f"WX (University of Hyderabad): {wx}")

    slp1 = decode(word, "SLP1")
    print(f"SLP1 (Sanskrit Library): {slp1}")


def example_4_process_request():
    """Example 4: Process a full request."""
    print("\n" + "=" * 60)
    print("EXAMPLE 4: Process Full Request")
    print("=" * 60)

    from heritage.modules.interface import process_request

    # Create a CGI-like environment
    env = {
        "text": "rama",
        "t": "VH",
        "lex": "MW",
        "font": "roma",
        "cache": "1",
        "st": "",
        "us": "0",
        "topic": "",
        "abs": "0",
        "cpts": "",
        "revised": "",
        "rev_off": "-1",
        "rev_ind": "-1",
        "corpus_permission": "",
        "corpus_dir": "",
        "sentence_no": "",
    }

    # Process the request
    output = process_request(env)

    print("Generated HTML (first 1000 chars):")
    print(output[:1000])
    if len(output) > 1000:
        print("...")
        print(f"\nTotal output length: {len(output)} characters")


def example_5_types_usage():
    """Example 5: Working with morphological types."""
    print("\n" + "=" * 60)
    print("EXAMPLE 5: Morphological Types")
    print("=" * 60)

    from heritage.types.morphology import (
        Gender,
        Number,
        Case,
        Person,
        Conjugation,
        Voice,
        Tense,
        PrMode,
        Nominal,
        Sadhana,
    )

    # Show available genders
    print("Available genders:")
    for gender in Gender:
        print(f"  - {gender.name}: {gender.value}")

    # Show available numbers
    print("\nAvailable numbers:")
    for number in Number:
        print(f"  - {number.name}: {number.value}")

    # Show available cases
    print("\nAvailable cases (first 5):")
    for i, case in enumerate(Case):
        if i < 5:
            print(f"  - {case.name}: {case.value}")
        else:
            break

    # Create a nominal form
    print("\nCreating a nominal form:")
    nominal = Nominal(
        stem="rama",
        gender=Gender.MASCULINE,
        number=Number.SINGULAR,
        case=Case.NOMINATIVE,
    )
    print(f"  Stem: {nominal.stem}")
    print(f"  Gender: {nominal.gender.value}")
    print(f"  Number: {nominal.number.value}")
    print(f"  Case: {nominal.case.value}")


def main():
    """Run all examples."""
    print("\n" + "#" * 60)
    print("# SANSKRIT HERITAGE PLATFORM - PYTHON USAGE EXAMPLES")
    print("#" * 60)

    try:
        example_1_basic_interface()
        example_2_word_operations()
        example_3_character_encoding()
        example_4_process_request()
        example_5_types_usage()

        print("\n" + "#" * 60)
        print("# ALL EXAMPLES COMPLETED SUCCESSFULLY")
        print("#" * 60 + "\n")

    except Exception as e:
        print(f"\n[ERROR] {type(e).__name__}: {e}")
        import traceback

        traceback.print_exc()
        return 1

    return 0


if __name__ == "__main__":
    sys.exit(main())
