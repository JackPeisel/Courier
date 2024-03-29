MAIN := cr
OBJS := ast.cmo lexer.cmo parser.cmo eval.cmo main.cmo

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

$(MAIN): $(OBJS)
	ocamlc -o $@ $^

lexer.ml: lexer.mll
	ocamlbuild -no-hygiene $<

parser.ml: parser.mly
	ocamlbuild -use-menhir -no-hygiene $<

parser.mli: parser.mly
	ocamlbuild -use-menhir -no-hygiene $<

clean:
	rm -f *.cmo *.cmi lexer.ml parser.ml parser.mli $(MAIN)

run:
	make -s && ./cr demo/demo.cr

# Dependencies generated by `ocamldep -bytecode *.mli *.ml`.
ast.cmo :
eval.cmo : ast.cmo
lexer.cmo : parser.cmi
main.cmo : parser.cmi lexer.cmo eval.cmo
parser.cmo : ast.cmo parser.cmi
parser.cmi : ast.cmo
