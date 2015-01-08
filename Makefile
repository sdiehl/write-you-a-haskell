PANDOC = pandoc
IFORMAT = markdown
FLAGS = --standalone --toc --toc-depth=2 --mathjax --highlight-style pygments
TEMPLATE = page.tmpl
STYLE = css/style.css
FILTER = includes.hs

SRC = $(wildcard *.md)
OBJ = $(SRC:.md=.html)

all: $(OBJ) top

%.html: %.md $(FILTER)
	$(PANDOC) -c $(STYLE) --filter ${FILTER} --template $(TEMPLATE) -s -f $(IFORMAT) -t html $(FLAGS) -o $@ $<

%.pdf: %.md $(FILTER)
	$(PANDOC) --filter ${FILTER} -f $(IFORMAT) --latex-engine=xelatex $(FLAGS) -o $@ $<

pdf: $(FILTER)
	$(PANDOC) --filter ${FILTER} -f $(IFORMAT) --latex-engine=xelatex $(FLAGS) -o WYAH.pdf 0*.md

top: $(FILTER)
	$(PANDOC) -c $(STYLE) --filter ${FILTER} --template $(TEMPLATE) -s -f $(IFORMAT) -t html $(FLAGS) -o tutorial.html index.md

clean:
	-rm *.html *.pdf
