"""
Lightweight port of the core graph-based segmentation control flow.

This module provides a `GraphSegmenter` class with a compact, testable
implementation of the graph-building and orchestration logic. It is an
incremental step towards a full port of `ML/graph_segmenter.ml`.

The implementation currently:
- Accepts a lexicon mapping (token -> code sequence).
- Builds a graph of candidate segments at offsets via greedy matching.
- Applies comprehensive sandhi rules via `apply_all_sandhi_rules`.
- Provides `process` which returns (full_segmented, count, graph).

Later steps will fill in `schedule`, `react`, and control flow logic.
"""

from typing import Dict, List, Tuple
from dataclasses import dataclass
from .word import Word
from .sandhi import apply_all_sandhi_rules


@dataclass
class Candidate:
    """A candidate segment at an offset.

    - `word`: a `Word` object (holds code list)
    - `meta`: optional metadata (placeholder)
    """

    word: Word
    meta: List = None


class GraphSegmenter:
    def __init__(self, lexicon_codes: Dict[str, List[int]]):
        # lexicon_codes: token -> list[int]
        self.lexicon_codes = lexicon_codes
        # prepare sequences as tuples for fast comparison
        self.lex_seqs = [tuple(v) for v in lexicon_codes.values()]
        # internal mappings for token <-> sequence tuple
        self.token_to_seq = {k: tuple(v) for k, v in lexicon_codes.items()}
        self.seq_to_token = {tuple(v): k for k, v in lexicon_codes.items()}
        # set of tokens for quick membership tests
        self.tokens = set(lexicon_codes.keys())

    def build_graph(self, codes: List[int]) -> Dict[int, List[Candidate]]:
        """Find lexicon matches at all offsets and return mapping offset -> candidates.

        This is the core of the original `access`/`register` functionality.
        """
        graph: Dict[int, List[Candidate]] = {}
        n = len(codes)
        if n == 0:
            return graph

        # For each offset, try to match any lexicon sequence
        for i in range(n):
            for seq in self.lex_seqs:
                m = len(seq)
                if m == 0:
                    continue
                if i + m <= n and tuple(codes[i : i + m]) == seq:
                    # include token string in meta if available
                    token = self.seq_to_token.get(seq)
                    cand = Candidate(
                        Word(list(seq)), meta=[{"token": token, "length": m}]
                    )
                    graph.setdefault(i, []).append(cand)
        return graph

    def simple_accrue(
        self, codes: List[int], graph: Dict[int, List[Candidate]]
    ) -> Dict[int, List[Candidate]]:
        """Apply sandhi (euphony) rules to generate combined candidates.

        For adjacent candidates at offsets i and i+len, apply all sandhi rules
        to create possible combined forms that exist in the lexicon.
        """
        out = {k: list(v) for k, v in graph.items()}

        offsets = sorted(out.keys())

        for off in offsets:
            for cand in list(out.get(off, [])):
                length = len(cand.word.elements)
                next_off = off + length
                if next_off not in out:
                    continue
                for cand2 in out[next_off]:
                    # Extract token strings from metadata
                    t1 = None
                    t2 = None
                    if (
                        cand.meta
                        and isinstance(cand.meta, list)
                        and cand.meta
                        and isinstance(cand.meta[0], dict)
                    ):
                        t1 = cand.meta[0].get("token")
                    if (
                        cand2.meta
                        and isinstance(cand2.meta, list)
                        and cand2.meta
                        and isinstance(cand2.meta[0], dict)
                    ):
                        t2 = cand2.meta[0].get("token")

                    if t1 and t2:
                        # Apply comprehensive sandhi rules and check lexicon membership
                        sandhi_results = apply_all_sandhi_rules(t1, t2)
                        for combined_token in sandhi_results:
                            seq = self.token_to_seq.get(combined_token)
                            if seq:
                                # Only add if not already present
                                combined = Candidate(
                                    Word(list(seq)),
                                    meta=[
                                        {"token": combined_token, "length": len(seq)}
                                    ],
                                )
                                if not any(
                                    tuple(c.word.elements)
                                    == tuple(combined.word.elements)
                                    for c in out.get(off, [])
                                ):
                                    out.setdefault(off, []).append(combined)

        return out

    def count_segmentations(self, codes: List[int]) -> int:
        """Count number of segmentations using DP over a graph of candidates.

        Rather than relying solely on lexicon sequences, use the candidate
        graph (built by `build_graph` and possibly extended by `accrue`) to
        enumerate segmentation possibilities. This mirrors the dynamic
        programming approach in the OCaml implementation, but is simpler.
        """
        n = len(codes)
        if n == 0:
            return 1

        # Build initial graph and apply accrual to include combined segments
        base_graph = self.build_graph(codes)
        graph = self.simple_accrue(codes, base_graph)

        memo: Dict[int, int] = {}

        def rec(i: int) -> int:
            if i == n:
                return 1
            if i in memo:
                return memo[i]
            cnt = 0
            # for each candidate starting at i, advance by candidate length
            for cand in graph.get(i, []):
                m = len(cand.word.elements)
                cnt += rec(i + m)
            memo[i] = cnt
            return cnt

        return rec(0)

    def process(self, codes: List[int]) -> Tuple[bool, int, Dict[int, List[Candidate]]]:
        """Perform segmentation graph build and counting.

        Returns (full_segmented, count, graph)
        - `full_segmented` is True if every position can be covered by lexicon matches
        - `count` is number of segmentation solutions
        - `graph` maps offsets to candidate `Candidate` objects
        """
        base = self.build_graph(codes)
        accrued = self.simple_accrue(codes, base)
        count = self.count_segmentations(codes)
        full = count > 0
        return (full, count, accrued)
