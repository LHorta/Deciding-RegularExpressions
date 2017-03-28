#OCB_FLAGS   = -use-ocamlfind -use-menhir -I src -cflags -annot,-w,+A-41-44-45
OCB_FLAGS   = -use-ocamlfind -use-menhir -I src -cflags -annot,-easy-type-errors # uses menhir
PACKAGE = regexParser.tgz
OCB = 		ocamlbuild $(OCB_FLAGS)

all: 		native byte # profile debug

clean:
			$(OCB) -clean

native:  	sanity
			$(OCB) main.native

byte: 		sanity
			$(OCB) main.byte

profile: 	sanity
			$(OCB) -tag profile main.native

debug: 		sanity
			$(OCB) -tag debug main.byte

sanity:
			# check that menhir is installed, use "opam install menhir"
			which menhir

test: 		native
			./main.native

package:
			tar cvzf $(PACKAGE) Makefile src/ _tags .merlin tests/ .gitignore

.PHONY: 	all clean byte native profile debug sanity test package
