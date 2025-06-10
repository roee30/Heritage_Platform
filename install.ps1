$ErrorActionPreference = "Stop"
$isAdmin = ([Security.Principal.WindowsPrincipal] [Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole] "Administrator")
if (-not $isAdmin) {
    Start-Process powershell "-NoProfile -ExecutionPolicy Bypass -File `"$PSCommandPath`"" -Verb RunAs
    exit $LASTEXITCODE
}
Set-ExecutionPolicy Unrestricted

function ignore() {}

$dir="$(resolve-path $PSCommandPath\..)"
$config="$env:APPDATA\GoldenDict\config"
if (!(test-path $config)) {
    Write-Host "config file not found at $config"
    exit 1
}
$x=[xml](get-content $config)
$name='heritage'
$node=[xml]"<program commandLine=`"powershell -ExecutionPolicy Unrestricted -file &quot;$dir\arg.ps1&quot; &quot;%GDWORD%&quot;`" enabled=`"1`" icon=`"`" id=`"$name`" name=`"$name`" type=`"2`"/>"
$x.GetElementsByTagName('programs').AppendChild($x.ImportNode($node.DocumentElement, $true)) | ignore
$x.save($config)
echo 'config update success'
pause
