#!/usr/bin/env python3
"""Test that directly validates Devanagari output from CLI."""

import sys
import os
from pathlib import Path

# Add heritage package to path
package_dir = Path(__file__).parent / "python"
sys.path.insert(0, str(package_dir))

from heritage.modules.interface import process_request

# Simulate CLI invocation for "idamapi"
env = {
    'text': 'idamapi',
    't': 'VH',  # VH = Velthuis transliteration
}

# Get output
output = process_request(env)

# Write to file (bypassing any terminal redirection issues)
output_file = Path(__file__).parent / "devanagari_output.html"
with open(output_file, 'w', encoding='utf-8') as f:
    f.write(output)

print(f"Output written to {output_file}")
print(f"Output length: {len(output)} characters")

# Extract and display just the Devanagari part
if "Devanagari:" in output:
    start = output.find("Devanagari:")
    end = output.find("</p>", start)
    if end > start:
        deva_line = output[start:end]
        print(f"Devanagari line: {deva_line}")

# Save just the HTML body to inspect
body_start = output.find("<body>")
body_end = output.find("</body>") + 7
if body_start >= 0 and body_end > body_start:
    body_only = output[body_start:body_end]
    body_file = Path(__file__).parent / "devanagari_body.html"
    with open(body_file, 'w', encoding='utf-8') as f:
        f.write(body_only)
    print(f"Body written to {body_file}")
