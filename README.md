# GoldenDict Sanskrit Assistant
Complete Sanskrit word analysis and sandhi splitter for GoldenDict.

A fork of the Sanskrit [Heritage Platform](https://gitlab.inria.fr/huet/Heritage_Platform) that builds an `interface(.exe)`.
Uses [Heritage Resources](https://gitlab.inria.fr/huet/Heritage_Resources) as a submodule and the [Zen Package](https://gitlab.inria.fr/huet/Zen.git) as a literal part of 
the repo itself.

Released under the original packages' LGPL license.

# For dummies on Windows
1. Download and run the GoldenDict 32-bit Windows installer [here](http://goldendict.org/download.php)
1. Recommended: install the Heritage Project's GoldenDict files
    1. Download all tar archives linked in the bottom of [this page](https://sanskrit.inria.fr/goldendict.html)
    1. Extract all archives in the same directory of your choice
    1. In GoldenDict, go to Edit > Dictionaries > Sources > Files > Add, paste the directory path, and mark Recursive
1. Download `sanskrit-assistant.zip` from [releases](https://github.com/roee30/Heritage_Platform/releases), unzip it anywhere, and run `install.exe`.
This will modify your GoldenDict configuration file to run Sanskrit analysis on selected words.
1. Select any Sanskirt word (in devanagari) and Press Ctrl+c twice. You should see something like this:
![Image](https://github.com/user-attachments/assets/b59ff27b-1a9d-4407-8c87-0df9b3c8c79e)
Hover on sandhi suggestions to show morphological analysis and dictionary entry link.

# Installation

## Windows

### Automatic
Download `sanskrit-assistant.zip` from [releases](https://github.com/roee30/Heritage_Platform/releases), unzip it anywhere, 
and run `install.exe` (by double clicking) or `install.ps1` (from the command line).
This will modify your GoldenDict configuration file to run Sanskrit analysis on selected words.


### Manual
If `install.ps1` does not work, Enter Edit > Dictionaries > Programs, press Add, and:
1. Check the `enabled` box
1. Select `type`: `HTML`
1. Name: `heritage`
1. Command line: `powershell -ExecutionPolicy Unrestricted -file "Path\To\sanskrit-assistant\arg.ps1" "%GDWORD%"`, where `Path\To` is replaced by the path of
the directory of the extracted archive.

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
