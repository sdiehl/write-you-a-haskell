all:
	alex Lexer.x
	happy Parser.y
	ghc --make Main -o Main
clean:
	rm -f *.o *.hi Parser.hs Lexer.hs Main
