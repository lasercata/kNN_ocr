.PHONY: all clean

all: print_mnist main

print_mnist: mnist.cmx print_mnist.cmx
	ocamlopt -o $@ $^

main: mnist.cmx prioQueue.cmx knn.cmx main.cmx
	ocamlopt -o $@ $^

clean:
	rm -f *.cmi *.cmo *.cmx *.o print_mnist main

%.cmx: %.ml
	ocamlopt -c -o $@ $<

%.cmi: %.mli
	ocamlopt -c -o $@ $<

# Dépendances générées automatiquement avec ocamldep
knn.cmo : \
    prioQueue.cmi \
    knn.cmi
knn.cmx : \
    prioQueue.cmx \
    knn.cmi
knn.cmi :
main.cmo : \
    mnist.cmi \
    knn.cmi
main.cmx : \
    mnist.cmx \
    knn.cmx
mnist.cmo : \
    mnist.cmi
mnist.cmx : \
    mnist.cmi
mnist.cmi :
print_mnist.cmo : \
    mnist.cmi
print_mnist.cmx : \
    mnist.cmx
prioQueue.cmo : \
    prioQueue.cmi
prioQueue.cmx : \
    prioQueue.cmi
prioQueue.cmi :
seq_test.cmo :
seq_test.cmx :
