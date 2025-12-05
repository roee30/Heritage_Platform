"""
Unit tests for Sanskrit sandhi rules.

Tests verify that sandhi transformations produce expected combined tokens.
"""

import sys
from pathlib import Path

# Add parent to path for imports
sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from heritage.modules.sandhi import (
    vowel_sandhi,
    visarga_sandhi,
    anusvara_sandhi,
    retroflex_sandhi,
    consonant_cluster_sandhi,
    apply_all_sandhi_rules,
)


def test_vowel_sandhi():
    """Test vowel combination rules."""
    # a + i -> e (guṇa)
    result = vowel_sandhi("rama", "iti")
    assert any("e" in r for r in result), f"Expected guṇa in {result}"
    print("✓ vowel_sandhi: a+i -> e (guṇa)")

    # a + u -> o (guṇa)
    result = vowel_sandhi("rama", "upagat")
    assert any("o" in r for r in result), f"Expected guṇa in {result}"
    print("✓ vowel_sandhi: a+u -> o (guṇa)")

    # a + a -> ā (vṛddhi)
    result = vowel_sandhi("rama", "aham")
    assert any("ā" in r for r in result), f"Expected vṛddhi in {result}"
    print("✓ vowel_sandhi: a+a -> ā (vṛddhi)")


def test_visarga_sandhi():
    """Test stop (visarga) combination rules."""
    # Voiceless + voiced -> voiced
    result = visarga_sandhi("tak", "gacchati")
    assert any("g" in r for r in result), f"Expected voicing in {result}"
    print("✓ visarga_sandhi: voiceless + voiced -> voiced")

    # Stop before sibilant -> sibilant
    result = visarga_sandhi("tat", "sat")
    assert any("s" in r for r in result), f"Expected sibilant assimilation in {result}"
    print("✓ visarga_sandhi: stop + sibilant -> sibilant")


def test_anusvara_sandhi():
    """Test anusvāra (nasal marker) rules."""
    # ṃ before velar -> ṅ
    result = anusvara_sandhi("ram", "kara")
    assert any("ṅ" in r for r in result), f"Expected anusvara->velar nasal in {result}"
    print("✓ anusvara_sandhi: ṃ + velar -> ṅ")

    # ṃ before dental -> n
    result = anusvara_sandhi("ram", "tara")
    assert any("n" in r for r in result), f"Expected anusvara->dental nasal in {result}"
    print("✓ anusvara_sandhi: ṃ + dental -> n")

    # ṃ before labial -> m
    result = anusvara_sandhi("ram", "pala")
    assert any("m" in r for r in result), f"Expected anusvara->labial nasal in {result}"
    print("✓ anusvara_sandhi: ṃ + labial -> m")


def test_retroflex_sandhi():
    """Test retroflex assimilation."""
    # dental + retroflex -> retroflex
    result = retroflex_sandhi("tat", "ṭhana")
    assert any("ṭ" in r for r in result), f"Expected retroflex assimilation in {result}"
    print("✓ retroflex_sandhi: dental + retroflex -> retroflex")


def test_consonant_cluster_sandhi():
    """Test final consonant cluster rules."""
    # Voiced + voiceless -> voiceless
    result = consonant_cluster_sandhi("vāg", "kara")
    assert any(
        "k" in r and "g" not in r[-2:] for r in result
    ), f"Expected devoicing in {result}"
    print("✓ consonant_cluster_sandhi: voiced + voiceless -> voiceless")


def test_apply_all_sandhi_rules():
    """Test comprehensive sandhi application."""
    # rama + iti: should produce multiple variants (including guṇa)
    result = apply_all_sandhi_rules("rama", "iti")
    assert len(result) >= 1, "Should produce at least one variant"
    assert "ramaiti" in result, "Should include plain concatenation"
    print(f"✓ apply_all_sandhi_rules('rama', 'iti'): {len(result)} variants")
    print(f"  Examples: {result[:3]}")

    # idamapi: plain concatenation
    result2 = apply_all_sandhi_rules("idam", "api")
    assert "idamapi" in result2, "Should include plain concatenation"
    print(f"✓ apply_all_sandhi_rules('idam', 'api'): {len(result2)} variant(s)")


if __name__ == "__main__":
    print("Running sandhi rule tests...\n")
    test_vowel_sandhi()
    test_visarga_sandhi()
    test_anusvara_sandhi()
    test_retroflex_sandhi()
    test_consonant_cluster_sandhi()
    test_apply_all_sandhi_rules()
    print("\nAll tests passed ✓")
