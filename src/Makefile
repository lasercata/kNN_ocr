.PHONY: all clean

all: print_mnist main

test: seq_test.cmx test.cmx
	ocamlopt -o $@ $^

print_mnist: mnist.cmx print_mnist.cmx
	ocamlopt -o $@ $^

main: mnist.cmx prioQueue.cmx seq_test.cmx knn.cmx main.cmx
	ocamlopt -o $@ $^

clean:
	rm -f *.cmi *.cmo *.cmx *.o test print_mnist main

%.cmx: %.ml
	ocamlopt -c -o $@ $<

%.cmi: %.mli
	ocamlopt -c -o $@ $<

# To generate the dependencies automatically, run in src :
# ocamldep -all *

knn.cmo : \
	prioQueue.cmi \
	seq_test.cmi \
	knn.cmi
knn.cmx : \
	prioQueue.cmx \
	seq_test.cmx \
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

seq_test.cmo : \
	seq_test.cmi
seq_test.cmx : \
	seq_test.cmi
seq_test.cmi :

test.cmo : \
	seq_test.cmi \
	test.cmi
test.cmi : \
	seq_test.cmi \
	test.cmi
test.cmx : \
	seq_test.cmi \
	seq_test.cmx \
	test.cmi

