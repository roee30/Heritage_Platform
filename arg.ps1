param([string]$text)
[Console]::InputEncoding = [System.Text.UTF8Encoding]::new()
[Console]::OutputEncoding = [System.Text.UTF8Encoding]::new()

# set-psdebug -trace 2
$ErrorActionPreference = "Stop"
Set-Location -LiteralPath $PSScriptRoot

function Tag($tag, $content, $extra="") {
    "<$tag $extra>$content</$tag>"
}
$text | Set-Content -Encoding UTF8 input.txt

chcp 65001 >$null
$processed = (get-content -encoding utf8 input.txt | & .\iast -a -f input.txt).replace('.a',"'")
$processed | set-content -encoding utf8 output.txt

$env:QUERY_STRING = "t=VH&text=$processed"
#$interfaceOutput = (& .\_build\install\default\bin\interface.exe)
$interfaceOutput = (& .\interface.exe)

$style = Get-Content -Raw style.css,tooltip.css

$styleTag = (Tag "style" $style)
echo "$interfaceOutput`n$styleTag"
