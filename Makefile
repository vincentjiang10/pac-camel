build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec src/main.exe

clean:
	dune clean