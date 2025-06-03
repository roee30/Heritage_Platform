FROM ocaml/opam:windows-msvc
RUN opam install camlp4 uutf camlp-streams
WORKDIR c:\src
