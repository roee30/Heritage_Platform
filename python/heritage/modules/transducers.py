"""Loader for transducers JSON exported from OCaml.

Exports:
  load_transducers(path) -> Transducers

Transducers holds:
  - nodes: list of Node objects (indexed by id)
  - roots: dict mapping transducer name -> root node id

Node fields:
  - accept (bool)
  - arcs: list of (letter:int, target:int)
  - rules: list of (w,u,v) where each is a list of ints
"""
from typing import List, Dict, Tuple
import json
import os


class Node:
    def __init__(self, accept: bool, arcs: List[Tuple[int, int]], rules: List[Tuple[List[int], List[int], List[int]]]):
        self.accept = accept
        self.arcs = arcs
        self.rules = rules

    def __repr__(self):
        return f"Node(accept={self.accept}, arcs={len(self.arcs)}, rules={len(self.rules)})"


class Transducers:
    def __init__(self, nodes: List[Node], roots: Dict[str, int]):
        self.nodes = nodes
        self.roots = roots

    def get_root(self, name: str) -> int:
        return self.roots[name]


def load_transducers(path: str):
    if not os.path.exists(path):
        raise FileNotFoundError(path)
    with open(path, 'r', encoding='utf-8') as f:
        data = json.load(f)
    version = data.get('version', 1)
    if version != 1:
        raise RuntimeError(f"Unsupported transducers export version: {version}")

    roots = data['roots']
    node_objs: List[Node] = []
    for n in data['nodes']:
        # n: dict with keys accept, arcs, rules
        accept = bool(n.get('accept', False))
        arcs = [(int(a[0]), int(a[1])) for a in n.get('arcs', [])]
        rules = []
        for r in n.get('rules', []):
            # each r is [w,u,v]
            if len(r) == 3:
                rules.append((list(map(int, r[0])), list(map(int, r[1])), list(map(int, r[2]))))
        node_objs.append(Node(accept, arcs, rules))

    return Transducers(node_objs, roots)


if __name__ == '__main__':
    import sys
    p = sys.argv[1] if len(sys.argv) > 1 else 'python/heritage/data/transducers.json'
    t = load_transducers(p)
    print(f'Loaded transducers: nodes={len(t.nodes)}, roots={list(t.roots.keys())[:10]}')
