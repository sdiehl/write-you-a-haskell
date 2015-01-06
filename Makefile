PANDOC = pandoc
IFORMAT = markdown
FLAGS = --standalone --toc --toc-depth=2 --mathjax --highlight-style pygments
TEMPLATE = page.tmpl
STYLE = css/style.css

SRC = $(wildcard *.md)
OBJ = $(SRC:.md=.html)

all: $(OBJ) top

includes: includes.hs
	ghc --make $<

%.html: %.md includes
	./includes < $< | $(PANDOC) -c $(STYLE) --template $(TEMPLATE) -s -f $(IFORMAT) -t html $(FLAGS) -o $@

%.pdf: %.md includes
	./includes < $< | $(PANDOC) -c -s -f $(IFORMAT) --latex-engine=xelatex $(FLAGS) -o $@

top:
	./includes < index.md | $(PANDOC) -c $(STYLE) --template $(TEMPLATE) -s -f $(IFORMAT) -t html $(FLAGS) -o tutorial.html

clean:
	-rm pages/*.html
