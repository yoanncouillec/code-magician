all:
	ocamlfind ocamlopt magician.ml

clean:
	rm -rf *.cm* *.o *~ *.out
