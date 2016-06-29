
all : build

build:
	ocamlbuild -use-ocamlfind main.native

clean:
	ocamlbuild -clean

test: build
	./run

count: build
	./run | grep YES | wc -l

analyze:
	ocamlbuild -use-ocamlfind analyze.native
	./analyze.native results-1001.json

