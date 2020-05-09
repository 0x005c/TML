all:
	ocamllex lexer.mll
	menhir parse.mly
	ocamlc -o toyml type.ml exp.ml value.ml builtin.ml infer.ml \
		parse.mli parse.ml lexer.ml eval.ml main.ml

.PHONY: clean

clean:
	rm toyml *.cmi *.cmo parse.mli parse.ml lexer.ml
