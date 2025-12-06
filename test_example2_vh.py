#!/usr/bin/env python3
"""Generate HTML output for example2 input after normalizing IAST->VH and save to disk."""
import sys
from pathlib import Path
package_dir = Path(__file__).parent / "python"
sys.path.insert(0, str(package_dir))
from heritage.modules.interface import process_request
from heritage.modules.stubs import _normalize_iast_to_vh

input_text = "abhiṣekaḥ rāmas tu sītā saumitriḥ eva ca"
# Normalize each whitespace-separated token to VH using function from stubs
parts = [p for p in input_text.split() if p]
vh_parts = []
for p in parts:
    vh = _normalize_iast_to_vh(p)
    if vh:
        vh_parts.append(vh)
    else:
        vh_parts.append(p)
vh_text = " ".join(vh_parts)
print("Normalized to VH:", vh_text)

env = {
    'text': vh_text,
    't': 'VH',
}
output = process_request(env)

out_file = Path(__file__).parent / "example2_generated_vh.html"
with open(out_file, 'w', encoding='utf-8') as f:
    f.write(output)
print(f"Wrote generated output to {out_file}")
body_start = output.find("<body>")
body_end = output.find("</body>") + 7
if body_start >= 0 and body_end > body_start:
    body_file = Path(__file__).parent / "example2_generated_vh_body.html"
    with open(body_file, 'w', encoding='utf-8') as f:
        f.write(output[body_start:body_end])
    print(f"Wrote body-only to {body_file}")
else:
    print("No <body> found in output")
