SRCS=range.mli range.ml types.ml parser.mli parser.ml lexer.ml typeenv.mli typeenv.ml subst.ml primitives.ml typecheck.ml evaluator.mli evaluator.ml tohtml.mli tohtml.ml main.ml

ifeq ($(OS), Windows_NT)
  TARGET=bin/lambda-next-box.exe
else
  TARGET=bin/lambda-next-box
endif
all: lexer.ml parser.mli parser.ml parser.output $(TARGET)
lexer.ml: lexer.mll
	ocamllex lexer.mll
parser.mli parser.ml parser.output: parser.mly
	ocamlyacc -v parser.mly
$(TARGET): $(SRCS)
	ocamlopt -o $(TARGET) $^

clean:
	rm -f $(TARGET) lexer.ml parser.mli parser.ml parser.output *.cmi *.cmo *.cmx *.o

clean-sub:
	rm -f lexer.ml parser.mli parser.ml parser.output *.cmi *.cmx *.o
