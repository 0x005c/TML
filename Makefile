all:
	ocamllex lexer.mll
	menhir parse.mly
	ocamlc -o toyml type.ml exp.ml infer.ml parse.mli parse.ml lexer.ml value.ml eval.ml main.ml

.PHONY: clean

clean:
	rm toyml *.cmi *.cmo parse.mli parse.ml lexer.ml
