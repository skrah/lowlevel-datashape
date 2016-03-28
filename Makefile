
#
# Use strict flags for development only.
# FLAGS = -w @A-4 -safe-string -principal -g
#
FLAGS =
OBJS = ast.cmo parser.cmo lexer.cmo visualize.cmo lib.cmo


default: indent


indent: $(OBJS) indent.ml
	ocamlc $(FLAGS) -o indent $(OBJS) indent.ml

ast.cmi: Makefile ast.mli
	ocamlc $(FLAGS) -c ast.mli

ast.cmo: Makefile ast.ml ast.cmi
	ocamlc $(FLAGS) -c ast.ml


parser.ml: Makefile parser.mly ast.cmi
	ocamlyacc parser.mly

parser.mli: Makefile parser.ml

parser.cmi: Makefile parser.ml parser.mli ast.cmi
	ocamlc $(FLAGS) -c parser.mli

parser.cmo: Makefile parser.ml ast.cmi parser.cmi
	ocamlc $(FLAGS) -c parser.ml


lexer.ml: Makefile lexer.mll parser.cmi
	ocamllex lexer.mll

lexer.cmi: Makefile lexer.ml parser.cmi
	ocamlc $(FLAGS) -c lexer.mli

lexer.cmo: Makefile lexer.ml parser.cmi lexer.cmi
	ocamlc $(FLAGS) -c lexer.ml

lexer.cmx: Makefile lexer.ml parser.cmx lexer.cmi
	ocamlopt $(FLAGS) -c lexer.ml


lib.cmi: Makefile lib.mli ast.cmi
	ocamlc $(FLAGS) -c lib.mli

lib.cmo: Makefile lib.ml parser.cmi lexer.cmi lib.cmi
	ocamlc $(FLAGS) -c lib.ml


visualize.cmi: Makefile visualize.mli ast.cmi
	ocamlc $(FLAGS) -c visualize.mli

visualize.cmo: Makefile visualize.ml ast.cmi visualize.cmi
	ocamlc $(FLAGS) -c visualize.ml


dep: FORCE
	@ocamldep *.ml *.mli | sed "s/://g" | perl -p -e 's/\\\n//' | tr -s " " | sort


clean: FORCE
	rm -f *.cmi *.cmo *.cmx *.o parser.ml parser.mli lexer.ml indent

FORCE:



