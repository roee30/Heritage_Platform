param([string]$text)
[Console]::InputEncoding = [System.Text.UTF8Encoding]::new()
[Console]::OutputEncoding = [System.Text.UTF8Encoding]::new()

# set-psdebug -trace 2
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
chcp 65001 >$null
$processed = (get-content -encoding utf8 input.txt | & .\iast -a -f input.txt).replace('.a',"'")
$processed | set-content -encoding utf8 output.txt
# echo $processed; exit
# $processed = $text

# Set QUERY_STRING and run interface
$env:QUERY_STRING = "t=VH&text=$processed"
# $interfaceOutput = (& ML\interface.exe)
$interfaceOutput = (& .\_build\install\default\bin\interface.exe)
# echo $interfaceOutput; exit 1

# Read style files
$style = Get-Content -Raw style.css,tooltip.css

# Output to stdout
$styleTag = (Tag "style" $style)
echo "$interfaceOutput`n$styleTag"
