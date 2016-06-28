
all:
	ocamlbuild -use-ocamlfind main.native

clean:
	ocamlbuild -clean

test: all
	./run

count: all
	./run | grep YES | wc -l

