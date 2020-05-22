all:
	ocamllex lexer.mll
	menhir parse.mly
	ocamlc -o tml type.ml exp.ml value.ml builtin.ml infer.ml \
		parse.mli parse.ml lexer.ml eval.ml main.ml

.PHONY: clean

clean:
	rm tml *.cmi *.cmo parse.mli parse.ml lexer.ml
