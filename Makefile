
all:
	ocamlbuild -use-ocamlfind main.native

test: all
	./run

count: all
	./run | grep YES | wc -l

