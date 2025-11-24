$ErrorActionPreference = "Stop"
Set-PSDebug -Trace 1
#invoke-WebRequest -Uri https://github.com/ocaml/opam/releases/download/2.3.0/opam-2.3.0-x86_64-windows.exe -outfile opam.exe
#.\opam-new.exe env | % {invoke-expression $_}
#opam update
$env:PATH="$env:PATH;C:\BuildTools\VC\Tools\MSVC\14.29.30133\bin\Hostx64\x64"
#copy C:\BuildTools\VC\Tools\MSVC\14.29.30133\bin\Hostx64\x64\ml64.exe .
.\opam --version
.\opam init -y --reinit -ni
.\opam repository remove default
.\opam repository add default https://github.com/ocaml/opam-repository.git
#.\opam update
.\opam install -y camlp4 uutf camlp-streams
