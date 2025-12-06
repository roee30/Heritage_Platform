"""Segmentation using automata transducers from OCaml.

This module provides transducer-based segmentation that uses the finite-state
automata exported from OCaml to generate candidate word segmentations.

Key components:
- TransducerSegmenter: Builds segmentation paths through automata
- TransducerMatcher: Executes single transducer matching
- traverse_transducer: Core transducer traversal algorithm

Usage:
    from heritage.modules.transducer_segmenter import segment_with_transducers
    segmentations = segment_with_transducers(text, lexicon)
"""

from typing import List, Dict, Tuple, Set, Optional
from dataclasses import dataclass
from .transducers import Transducers, Node
from .word import Word


@dataclass
class Segmentation:
    """A complete segmentation path."""
    words: List[str]
    path: List[Tuple[int, int]]  # list of (node_id, offset) pairs
    
    def __repr__(self):
        return f"Seg({' + '.join(self.words)})"
    
    def __str__(self):
        return ' + '.join(self.words)


class TransducerMatcher:
    """Matches a word through a single transducer automaton."""
    
    def __init__(self, transducers: Transducers, root_name: str):
        self.transducers = transducers
        self.root_id = transducers.get_root(root_name)
        self.root_node = transducers.nodes[self.root_id]
    
    def match(self, codes: List[int]) -> List[Tuple[List[int], List[int]]]:
        """Match codes through transducer, returning [(input, output), ...]."""
        results: List[Tuple[List[int], List[int]]] = []
        self._traverse(self.root_node, codes, 0, [], [])
        return results
    
    def _traverse(self, node: Node, codes: List[int], pos: int, 
                  input_acc: List[int], output_acc: List[int]) -> None:
        """Recursive traversal of transducer."""
        if pos == len(codes):
            # End of input
            if node.accept:
                # Successful match
                results = []
                # Apply rules if any (output transformations)
                if node.rules:
                    for (w, u, v) in node.rules:
                        # Rule: if we see 'u', output 'v' instead
                        # For now, simplified: apply rules to accumulated output
                        pass
                results.append((list(input_acc), list(output_acc)))
            return
        
        # Try arcs (transitions)
        code = codes[pos]
        for (letter, target_id) in node.arcs:
            if letter == code:
                target_node = self.transducers.nodes[target_id]
                self._traverse(
                    target_node,
                    codes,
                    pos + 1,
                    input_acc + [code],
                    output_acc + [code]
                )


class TransducerSegmenter:
    """Segmentation using transducers."""
    
    def __init__(self, transducers: Transducers, lexicon_codes: Dict[str, List[int]]):
        """Initialize with loaded transducers and lexicon.
        
        Args:
            transducers: Loaded Transducers object
            lexicon_codes: Dict mapping words -> code lists
        """
        self.transducers = transducers
        self.lexicon = lexicon_codes
        self.lexicon_seqs = {tuple(v): k for k, v in lexicon_codes.items()}
        
        # Precompute which transducers might be useful
        self.matchers = {name: TransducerMatcher(transducers, name) 
                        for name in transducers.roots.keys()}
    
    def segment(self, codes: List[int]) -> List[Segmentation]:
        """Segment codes using transducers.
        
        Returns list of complete segmentations from start to end of codes.
        """
        # Find all segmentations
        segmentations: List[Segmentation] = []
        self._find_segmentations(codes, 0, [], segmentations)
        
        return segmentations
    
    def segment_greedy(self, text: str) -> Tuple[List[str], bool]:
        """Segment text greedily using lexicon lookup.
        
        This is a simple greedy approach: match longest words first from left to right.
        Works with words from the lexicon.
        
        Args:
            text: Text to segment
            
        Returns:
            (words_list, complete) where complete=True if entire text was covered
        """
        words = []
        offset = 0
        text_len = len(text)
        
        while offset < text_len:
            found = False
            # Try longest matches first
            for length in range(text_len - offset, 0, -1):
                substring = text[offset:offset+length]
                if substring in self.lexicon:
                    words.append(substring)
                    offset += length
                    found = True
                    break
            
            if not found:
                # No match found - skip this character
                words.append(f"[?:{text[offset]}]")
                offset += 1
        
        complete = (offset == text_len)
        return words, complete
    
    def _find_segmentations(self, codes: List[int], offset: int,
                           path: List[Tuple[int, str]], results: List[Segmentation]) -> None:
        """Recursively find all valid segmentations.
        
        Args:
            codes: Full code sequence
            offset: Current position in codes
            path: Current segmentation path (node_id, word)
            results: Output list of complete segmentations
        """
        # Base case: reached end
        if offset == len(codes):
            segmentations = [word for _, word in path]
            results.append(Segmentation(segmentations, []))
            return
        
        # Try to match lexicon entries at this offset
        for word, word_codes in self.lexicon.items():
            word_len = len(word_codes)
            if offset + word_len <= len(codes):
                if tuple(codes[offset:offset + word_len]) == tuple(word_codes):
                    # Match found
                    self._find_segmentations(
                        codes,
                        offset + word_len,
                        path + [(0, word)],  # placeholder node_id
                        results
                    )
    
    def traverse_automaton(self, codes: List[int], root_node_id: int) -> bool:
        """Check if codes can traverse through an automaton and accept.
        
        Returns True if there's a path through the automaton that accepts all codes.
        """
        node = self.transducers.nodes[root_node_id]
        return self._traverse_automaton_rec(node, codes, 0)
    
    def _traverse_automaton_rec(self, node: Node, codes: List[int], pos: int) -> bool:
        """Recursive automaton traversal."""
        if pos == len(codes):
            return node.accept
        
        code = codes[pos]
        for (letter, target_id) in node.arcs:
            if letter == code:
                target_node = self.transducers.nodes[target_id]
                if self._traverse_automaton_rec(target_node, codes, pos + 1):
                    return True
        
        return False
    
    def get_possible_targets(self, word: str) -> Dict[str, bool]:
        """Get which transducers can process this word.
        
        Requires word to be in lexicon (or provide codes directly).
        
        Returns dict: transducer_name -> can_accept
        """
        if word not in self.lexicon:
            return {}
        
        word_codes = self.lexicon[word]
        results = {}
        for name, root_id in self.transducers.roots.items():
            can_process = self.traverse_automaton(word_codes, root_id)
            results[name] = can_process
        return results


def segment_with_transducers(codes: List[int], transducers: Transducers, 
                            lexicon: Dict[str, List[int]]) -> List[Segmentation]:
    """Convenience function for segmentation with codes.
    
    Args:
        codes: Code sequence to segment (use Word or canon module to convert)
        transducers: Loaded Transducers object
        lexicon: Dict mapping words -> code lists
    
    Returns:
        List of Segmentation objects
    """
    segmenter = TransducerSegmenter(transducers, lexicon)
    return segmenter.segment(codes)
