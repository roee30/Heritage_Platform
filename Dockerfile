# Warning: does not work
FROM ocaml/opam:windows-msvc
WORKDIR C:/
COPY opam-new.exe C:/opam.exe
COPY docker-install.ps1 C:/
RUN powershell -ExecutionPolicy Unrestricted c:\docker-install.ps1
