"""Loader for transducers exported from OCaml.

Supports both JSON and msgpack binary formats.

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
from typing import List, Dict, Tuple, Any, Union
import json
import os
import struct


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
    
    # Auto-detect format based on file extension or content
    if path.endswith('.msgpack'):
        return _load_transducers_msgpack(path)
    elif path.endswith('.json'):
        return _load_transducers_json(path)
    else:
        # Try msgpack first, fall back to json
        try:
            return _load_transducers_msgpack(path)
        except:
            return _load_transducers_json(path)


def _load_transducers_json(path: str):
    """Load transducers from JSON format."""
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


def _decode_msgpack_value(data: bytes, offset: int) -> Tuple[Any, int]:
    """Decode a single msgpack value and return (value, new_offset)."""
    if offset >= len(data):
        raise ValueError("Unexpected end of msgpack data")
    
    byte = data[offset]
    
    # Nil
    if byte == 0xc0:
        return None, offset + 1
    
    # Booleans
    if byte == 0xc2:
        return False, offset + 1
    if byte == 0xc3:
        return True, offset + 1
    
    # Positive fixnum (0x00-0x7f)
    if byte <= 0x7f:
        return byte, offset + 1
    
    # Negative fixnum (0xe0-0xff)
    if byte >= 0xe0:
        return struct.unpack('b', bytes([byte]))[0], offset + 1
    
    # Uint8
    if byte == 0xcc:
        return data[offset + 1], offset + 2
    
    # Uint16
    if byte == 0xcd:
        val = struct.unpack('>H', data[offset + 1:offset + 3])[0]
        return val, offset + 3
    
    # Uint32
    if byte == 0xce:
        val = struct.unpack('>I', data[offset + 1:offset + 5])[0]
        return val, offset + 5
    
    # Int8
    if byte == 0xd0:
        val = struct.unpack('b', bytes([data[offset + 1]]))[0]
        return val, offset + 2
    
    # Int16
    if byte == 0xd1:
        val = struct.unpack('>h', data[offset + 1:offset + 3])[0]
        return val, offset + 3
    
    # Int32
    if byte == 0xd2:
        val = struct.unpack('>i', data[offset + 1:offset + 5])[0]
        return val, offset + 5
    
    # Float32
    if byte == 0xca:
        val = struct.unpack('>f', data[offset + 1:offset + 5])[0]
        return val, offset + 5
    
    # Float64
    if byte == 0xcb:
        val = struct.unpack('>d', data[offset + 1:offset + 9])[0]
        return val, offset + 9
    
    # Fixraw (0xa0-0xbf)
    if 0xa0 <= byte <= 0xbf:
        length = byte - 0xa0
        text = data[offset + 1:offset + 1 + length].decode('utf-8', errors='replace')
        return text, offset + 1 + length
    
    # Raw16
    if byte == 0xda:
        length = struct.unpack('>H', data[offset + 1:offset + 3])[0]
        text = data[offset + 3:offset + 3 + length].decode('utf-8', errors='replace')
        return text, offset + 3 + length
    
    # Raw32
    if byte == 0xdb:
        length = struct.unpack('>I', data[offset + 1:offset + 5])[0]
        text = data[offset + 5:offset + 5 + length].decode('utf-8', errors='replace')
        return text, offset + 5 + length
    
    # Fixarray (0x90-0x9f)
    if 0x90 <= byte <= 0x9f:
        count = byte - 0x90
        arr = []
        offset += 1
        for _ in range(count):
            val, offset = _decode_msgpack_value(data, offset)
            arr.append(val)
        return arr, offset
    
    # Array16
    if byte == 0xdc:
        count = struct.unpack('>H', data[offset + 1:offset + 3])[0]
        arr = []
        offset = offset + 3
        for _ in range(count):
            val, offset = _decode_msgpack_value(data, offset)
            arr.append(val)
        return arr, offset
    
    # Array32
    if byte == 0xdd:
        count = struct.unpack('>I', data[offset + 1:offset + 5])[0]
        arr = []
        offset = offset + 5
        for _ in range(count):
            val, offset = _decode_msgpack_value(data, offset)
            arr.append(val)
        return arr, offset
    
    # Fixmap (0x80-0x8f)
    if 0x80 <= byte <= 0x8f:
        count = byte - 0x80
        obj = {}
        offset += 1
        for _ in range(count):
            key, offset = _decode_msgpack_value(data, offset)
            val, offset = _decode_msgpack_value(data, offset)
            obj[key] = val
        return obj, offset
    
    # Map16
    if byte == 0xde:
        count = struct.unpack('>H', data[offset + 1:offset + 3])[0]
        obj = {}
        offset = offset + 3
        for _ in range(count):
            key, offset = _decode_msgpack_value(data, offset)
            val, offset = _decode_msgpack_value(data, offset)
            obj[key] = val
        return obj, offset
    
    # Map32
    if byte == 0xdf:
        count = struct.unpack('>I', data[offset + 1:offset + 5])[0]
        obj = {}
        offset = offset + 5
        for _ in range(count):
            key, offset = _decode_msgpack_value(data, offset)
            val, offset = _decode_msgpack_value(data, offset)
            obj[key] = val
        return obj, offset
    
    raise ValueError(f"Unknown msgpack format byte: 0x{byte:02x}")


def _load_transducers_msgpack(path: str):
    """Load transducers from msgpack binary format."""
    with open(path, 'rb') as f:
        data = f.read()
    
    # Decode the root msgpack structure
    obj, _ = _decode_msgpack_value(data, 0)
    
    version = obj.get('version', 1)
    if version != 1:
        raise RuntimeError(f"Unsupported transducers export version: {version}")
    
    # roots is a map with string keys -> int node ids
    roots = obj.get('roots', {})
    
    # nodes is an array of node data
    nodes_data = obj.get('nodes', [])
    
    # Convert nodes to Node objects
    node_objs: List[Node] = []
    for node_data in nodes_data:
        if node_data is None:
            # Placeholder node
            node_objs.append(Node(False, [], []))
        else:
            accept = bool(node_data.get('accept', False))
            
            # arcs: array of 2-element arrays [letter, target_id]
            arcs = []
            for arc in node_data.get('arcs', []):
                if isinstance(arc, list) and len(arc) == 2:
                    arcs.append((int(arc[0]), int(arc[1])))
            
            # rules: array of 3-element arrays [w, u, v]
            rules = []
            for rule in node_data.get('rules', []):
                if isinstance(rule, list) and len(rule) == 3:
                    w = [int(x) for x in rule[0]] if isinstance(rule[0], list) else []
                    u = [int(x) for x in rule[1]] if isinstance(rule[1], list) else []
                    v = [int(x) for x in rule[2]] if isinstance(rule[2], list) else []
                    rules.append((w, u, v))
            
            node_objs.append(Node(accept, arcs, rules))
    
    return Transducers(node_objs, roots)


def load_transducers_old(path: str):
    """Deprecated: Use load_transducers instead."""
    return load_transducers(path)


def _load_transducers_json_old(path: str):
    """Legacy implementation - kept for reference."""
    pass


if __name__ == '__main__':
    import sys
    p = sys.argv[1] if len(sys.argv) > 1 else 'python/heritage/data/transducers.json'
    t = load_transducers(p)
    print(f'Loaded transducers: nodes={len(t.nodes)}, roots={list(t.roots.keys())[:10]}')
