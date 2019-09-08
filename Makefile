.PHONY: clean
clean:
	dune clean

.PHONY: start
start:
	dune exec ./main.exe --profile release

.PHONY: build
build:
	dune build ./main.exe
