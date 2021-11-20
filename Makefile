DEPS := 				\
	ast.ml ast.mli			\
	codegen.ml codegen.mli		\
	consts.ml consts.mli		\
	cse.ml cse.mli			\
	folding.ml folding.mli		\
	lexer.mll			\
	opl.ml				\
	parser.ml parser.mli		\
	scoping.ml scoping.mli		\
	token.ml token.mli		\
	undeclared.ml undeclared.mli

bin/opl: $(DEPS) _build
	ocaml setup.ml -build
	ocaml setup.ml -install

_build:
	oasis setup
	ocaml setup.ml -configure --prefix $$(pwd)

.PHONY: clean
clean:
	rm -f bin/opl
