Dump OCaml transducers to JSON and load in Python

1) Build and run the OCaml helper

From the repository root (PowerShell):

dune build tools/dump_transducers.exe
dune exec -- tools/dump_transducers.exe python/heritage/data/transducers.json

2) Load from Python

The Python loader is at python/heritage/modules/transducers.py
Example:
python -c "from heritage.modules.transducers import load_transducers; t=load_transducers('python/heritage/data/transducers.json'); print(len(t.nodes))"

3) Integration

Call load_transducers() from python/heritage/modules/stubs.py and wire the returned object into the segmentation/dispatcher pipeline.

Notes

- The export format is simple JSON: {"version":1, "roots":{...}, "nodes":[...]}.
- If you prefer msgpack/binary, I can change the exporter and loader.
