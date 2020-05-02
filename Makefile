all:
	ocamllex lexer.mll
	menhir parse.mly
	ocamlc exp.ml parse.mli parse.ml lexer.ml main.ml

.PHONY: clean

clean:
	rm *.cmi *.cmo parse.mli parse.ml lexer.ml
