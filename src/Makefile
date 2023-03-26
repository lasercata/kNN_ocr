.PHONY: depend all clean

all: depend print_mnist test main

test: mnist.cmx prioQueue.cmx seq_test.cmx knn.cmx test.cmx
	ocamlopt -o $@ $^

print_mnist: mnist.cmx print_mnist.cmx
	ocamlopt -o $@ $^

main: mnist.cmx prioQueue.cmx seq_test.cmx knn.cmx test.cmx main.cmx
	ocamlopt -o $@ $^

clean:
	rm -f *.cmi *.cmo *.cmx *.o test print_mnist main

cleanup:
	rm -f *.cmi *.cmo *.cmx *.o

depend:
	ocamldep *.ml *.mli > .depend

%.cmx: %.ml
	ocamlopt -c -o $@ $<

%.cmi: %.mli
	ocamlopt -c -o $@ $<

include .depend
