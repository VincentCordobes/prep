.PHONY: default build install uninstall test clean

default: build

build:
	dune build

install:
	opam install . --deps-only --yes
	dune install
	dune build @install

uninstall:
	dune uninstall

test:
	dune runtest

clean:
	dune clean && rm -rf ./_build *.opam *.merlin

start:
	dune exec src/main.exe --profile release

