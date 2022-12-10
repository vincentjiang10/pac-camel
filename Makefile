.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

clean:
	dune clean
	rm -f pac_camel.zip

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh

zip:
	rm -f pac_camel.zip
	zip -r pac_camel.zip . -x@exclude.lst

cloc: 
	make clean
	cloc --by-file --include-lang=OCaml .  