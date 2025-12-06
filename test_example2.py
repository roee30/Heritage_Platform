#!/usr/bin/env python3
"""Generate HTML output for example2 input and save to disk for comparison."""
import sys
from pathlib import Path
package_dir = Path(__file__).parent / "python"
sys.path.insert(0, str(package_dir))
from heritage.modules.interface import process_request

text = "abhiṣekaḥ rāmas tu sītā saumitriḥ eva ca"
env = {
    'text': text,
    't': 'VH',
}
output = process_request(env)
out_file = Path(__file__).parent / "example2_generated.html"
with open(out_file, 'w', encoding='utf-8') as f:
    f.write(output)
print(f"Wrote generated output to {out_file}")

# Also write body-only for quick inspection
body_start = output.find("<body>")
body_end = output.find("</body>") + 7
if body_start >= 0 and body_end > body_start:
    body_file = Path(__file__).parent / "example2_generated_body.html"
    with open(body_file, 'w', encoding='utf-8') as f:
        f.write(output[body_start:body_end])
    print(f"Wrote body-only to {body_file}")
else:
    print("No <body> found in output")
