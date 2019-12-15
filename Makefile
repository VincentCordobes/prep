.PHONY: clean
clean:
	dune clean

.PHONY: start
start:
	dune exec --profile release

.PHONY: build
build:
	dune build
