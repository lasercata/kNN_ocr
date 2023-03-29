.PHONY: depend all clean

all: depend kNN_Ocr123

kNN_Ocr123: mnist.cmx print_mnist.cmx prioQueue.cmx seq_test.cmx knn.cmx test.cmx main.cmx
	ocamlopt -o $@ $^

clean:
	rm -f *.cmi *.cmo *.cmx *.o kNN_Ocr123

cleanup:
	rm -f *.cmi *.cmo *.cmx *.o

depend:
	ocamldep *.ml *.mli > .depend

%.cmx: %.ml
	ocamlopt -c -o $@ $<

%.cmi: %.mli
	ocamlopt -c -o $@ $<

include .depend
