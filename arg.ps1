param([string]$text)
[Console]::InputEncoding = [System.Text.UTF8Encoding]::new()
[Console]::OutputEncoding = [System.Text.UTF8Encoding]::new()

$ErrorActionPreference = "Stop"
# Ensure script runs in its directory
Set-Location -LiteralPath $PSScriptRoot

# Define tag function
function Tag($tag, $content, $extra="") {
    "<$tag $extra>$content</$tag>"
}
# Save input
$text | Set-Content -Encoding UTF8 input.txt

# Process through iast
#$inputText = Get-Content -Raw -Encoding Utf8 input.txt
#$processed = & .\iast-wrapper.ps1 "$inputText"
#$processed | Set-Content -Encoding UTF8 iast-out.txt
$processed = $text

# Set QUERY_STRING and run interface
$env:QUERY_STRING = "t=VH&text=$processed"
$interfaceOutput = (& ML\interface.exe)
# echo $interfaceOutput; exit 1

# Read style files
$style = Get-Content -Raw style.css,tooltip.css

# Output to stdout
$styleTag = (Tag "style" $style)
echo "$interfaceOutput`n$styleTag"
