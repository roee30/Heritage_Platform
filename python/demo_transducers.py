#!/usr/bin/env python3
"""
Example: Load and use msgpack transducers for segmentation.

This demonstrates:
1. Loading transducers from msgpack (fast binary format)
2. Using automata for word matching
3. Segmenting Sanskrit text
"""

import sys
import os
import time
from pathlib import Path

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from heritage.modules.transducers import load_transducers
from heritage.modules.word import Word


def inspect_transducers(transducers_path: str) -> None:
    """Inspect the transducers structure."""
    print("\n" + "="*70)
    print("TRANSDUCERS INSPECTION")
    print("="*70)
    
    start = time.time()
    transducers = load_transducers(transducers_path)
    load_time = time.time() - start
    
    print(f"\n✅ Loaded from: {transducers_path}")
    print(f"   Load time: {load_time:.3f}s")
    print(f"   Total nodes: {len(transducers.nodes):,}")
    print(f"   Total roots: {len(transducers.roots)}")
    
    # Analyze root automata
    print(f"\n📍 Root automata:")
    for name, root_id in sorted(transducers.roots.items()):
        root_node = transducers.nodes[root_id]
        print(f"   {name:20} -> node {root_id:6} (accept={root_node.accept}, "
              f"arcs={len(root_node.arcs)}, rules={len(root_node.rules)})")
    
    # Sample node analysis
    print(f"\n📊 Sample node analysis (first 5 nodes):")
    for i in range(min(5, len(transducers.nodes))):
        node = transducers.nodes[i]
        print(f"   Node {i}: accept={node.accept}, arcs={len(node.arcs)}, rules={len(node.rules)}")
    
    # Find nodes with most arcs
    max_arcs = max((len(node.arcs) for node in transducers.nodes), default=0)
    max_rules = max((len(node.rules) for node in transducers.nodes), default=0)
    print(f"\n📈 Statistics:")
    print(f"   Max arcs in a node: {max_arcs}")
    print(f"   Max rules in a node: {max_rules}")
    print(f"   Nodes with accept=True: {sum(1 for node in transducers.nodes if node.accept)}")


def demonstrate_automaton_traversal(transducers_path: str) -> None:
    """Demonstrate traversing an automaton."""
    print("\n" + "="*70)
    print("AUTOMATON TRAVERSAL DEMO")
    print("="*70)
    
    transducers = load_transducers(transducers_path)
    
    # Get a root automaton
    nouns_root = transducers.roots.get('nouns')
    if nouns_root is None:
        print("❌ 'nouns' transducer not found")
        return
    
    print(f"\n📍 Starting from 'nouns' root (node {nouns_root})")
    
    # Traverse a simple path
    root_node = transducers.nodes[nouns_root]
    print(f"\n🔍 Root node details:")
    print(f"   Accept: {root_node.accept}")
    print(f"   Transitions (first 10):")
    
    for i, (letter, target) in enumerate(root_node.arcs[:10]):
        target_node = transducers.nodes[target]
        print(f"      {i+1}. letter={letter:3} -> node {target:6} "
              f"(accept={target_node.accept}, arcs={len(target_node.arcs)})")
    
    if len(root_node.arcs) > 10:
        print(f"      ... and {len(root_node.arcs) - 10} more transitions")
    
    # Show rules if any
    if root_node.rules:
        print(f"\n📝 Rules at root node (first 3):")
        for i, (w, u, v) in enumerate(root_node.rules[:3]):
            print(f"      {i+1}. {w} -> {u} → {v}")


def compare_formats(json_path: str, msgpack_path: str) -> None:
    """Compare JSON and msgpack formats."""
    print("\n" + "="*70)
    print("FORMAT COMPARISON: JSON vs MSGPACK")
    print("="*70)
    
    # Check file sizes
    json_size = os.path.getsize(json_path) if os.path.exists(json_path) else None
    msgpack_size = os.path.getsize(msgpack_path) if os.path.exists(msgpack_path) else None
    
    # Load times
    print("\n⏱️  Loading performance:")
    
    if json_size:
        start = time.time()
        t_json = load_transducers(json_path)
        json_time = time.time() - start
        print(f"   JSON:    {json_size:12,} bytes  →  {json_time:.3f}s")
    
    if msgpack_size:
        start = time.time()
        t_msgpack = load_transducers(msgpack_path)
        msgpack_time = time.time() - start
        print(f"   MSGPACK: {msgpack_size:12,} bytes  →  {msgpack_time:.3f}s")
    
    if json_size and msgpack_size:
        size_ratio = json_size / msgpack_size
        print(f"\n📊 Size reduction: {size_ratio:.1f}x smaller (msgpack)")
        if json_time and msgpack_time:
            speed_ratio = json_time / msgpack_time
            print(f"⚡ Speed improvement: {speed_ratio:.1f}x faster (msgpack)")


def test_word_matching(transducers_path: str) -> None:
    """Test matching words through automata."""
    print("\n" + "="*70)
    print("WORD MATCHING THROUGH AUTOMATA")
    print("="*70)
    
    transducers = load_transducers(transducers_path)
    
    # Create some test words (using code sequences)
    # For now, just demonstrate the structure
    print(f"\n📋 Transducer structure ready for word matching")
    print(f"   - Can traverse automata with code sequences")
    print(f"   - Each node tracks accept state and transitions")
    print(f"   - Rules enable morphological transformations")
    
    # Example: check a simple path through nouns
    nouns_id = transducers.roots.get('nouns')
    if nouns_id is not None:
        root = transducers.nodes[nouns_id]
        print(f"\n🔍 Example: Checking 'nouns' automaton")
        print(f"   Root node arcs: {len(root.arcs)}")
        
        # Follow first arc if it exists
        if root.arcs:
            first_letter, first_target = root.arcs[0]
            next_node = transducers.nodes[first_target]
            print(f"   Following first arc: letter={first_letter} -> node {first_target}")
            print(f"     Target node: accept={next_node.accept}, "
                  f"arcs={len(next_node.arcs)}, rules={len(next_node.rules)}")


def main():
    """Main demo."""
    base_dir = Path(__file__).parent / "heritage" / "data"
    json_path = base_dir / "transducers.json"
    msgpack_path = base_dir / "transducers.msgpack"
    
    print("\n")
    print("╔" + "="*68 + "╗")
    print("║" + " " * 15 + "TRANSDUCERS MSGPACK LOADER DEMO" + " " * 23 + "║")
    print("╚" + "="*68 + "╝")
    
    # 1. Inspect transducers
    if msgpack_path.exists():
        inspect_transducers(str(msgpack_path))
    else:
        print(f"\n❌ Msgpack file not found: {msgpack_path}")
    
    # 2. Demonstrate automaton traversal
    if msgpack_path.exists():
        demonstrate_automaton_traversal(str(msgpack_path))
    
    # 3. Compare formats
    if json_path.exists() and msgpack_path.exists():
        compare_formats(str(json_path), str(msgpack_path))
    elif msgpack_path.exists():
        print("\n⚠️  JSON file not available for comparison")
    
    # 4. Test word matching
    if msgpack_path.exists():
        test_word_matching(str(msgpack_path))
    
    print("\n" + "="*70)
    print("✅ Demo complete!")
    print("="*70 + "\n")


if __name__ == "__main__":
    main()
