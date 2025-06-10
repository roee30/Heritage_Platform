# GoldenDict Sanskrit Assistant
Complete Sanskrit word analysis and sandhi splitter for GoldenDict.

A fork of the Sanskrit [Heritage Platform](https://gitlab.inria.fr/huet/Heritage_Platform) that builds an `interface(.exe)`.
Uses [Heritage Resources](https://gitlab.inria.fr/huet/Heritage_Resources) as a submodule and the [Zen Package](https://gitlab.inria.fr/huet/Zen.git) as a literal part of 
the repo itself.

Released under the original packages' LGPL license.

# Installation

## Windows

### Automatic
Download `sanskrit-assistant.zip` from [releases](https://github.com/roee30/Heritage_Platform/releases), unzip it anywhere, and run `install.ps1` from the command line.
This will modify your GoldenDict configuration file to run Sanskrit analysis on selected words.


### Manual
If `install.ps1` does not work, Enter Edit > Dictionaries > Programs, press Add, and:
1. Check the `enabled` box
1. Select `type`: `HTML`
1. Name: `heritage`
1. Command line: `powershell -file Path\To\Heritage_Platform\arg.ps1 %GDWORD%`, where `Path\To` is replaced by your unzip location.

All set!

## Linux
Requirements: 
- python
- opam
- opam packages: camlp4 camlp-streams uutf

Install opam, with it install OCaml 4.07.1 and required packages, and make:
```bash
opam switch create 4.07.1
opam install camlp4 uutf camlp-streams
make new
```

The resulting binary is located in `ML/interface`. 

Then, follow the instructions for manual Windows installation, replacing `arg.ps1` with `arg.sh`.

# Building
## Windows
```
docker run -it --rm -v .:c:\src -w c:\src make
```
## Linux
See Installation > Linux
