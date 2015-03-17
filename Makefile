PANDOC = pandoc
IFORMAT = markdown
MATHJAX = "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
FLAGS = --standalone --toc --toc-depth=2 --mathjax=$(MATHJAX) --highlight-style pygments
STYLE = css/style.css
FILTER = includes.hs
TEMPLATE_HTML = template.html
TEMPLATE_TEX = template.latex
PNG_IMAGES = $(patsubst %.pdf,%.png,$(wildcard img/*.pdf))

#SRC = $(wildcard *.md)
SRC = 000_introduction.md \
      001_basics.md \
      002_parsers.md \
      003_lambda_calculus.md \
      004_type_systems.md \
      005_evaluation.md \
      006_hindley_milner.md \
      007_path.md \
      008_extended_parser.md \
      #009_datatypes.md \
      #010_renamer.md \
      #011_pattern_matching.md \
      #012_systemf.md
      #026_llvm.md
OBJ = $(SRC:.md=.html)

all: $(OBJ) top

index: index.html

img/%.png: img/%.pdf
	convert -density 150 $< $@

%.html: %.md $(FILTER)
	$(PANDOC) -c $(STYLE) --filter ${FILTER} --template $(TEMPLATE_HTML) -s -f $(IFORMAT) -t html $(FLAGS) -o $@ $<

%.pdf: %.md $(FILTER)
	$(PANDOC) --filter ${FILTER} -f $(IFORMAT)  --template $(TEMPLATE_TEX) --latex-engine=xelatex $(FLAGS) -o $@ $<

%.epub: %.md $(FILTER)
	$(PANDOC) --filter ${FILTER} -f $(IFORMAT) $(FLAGS) -o $@ $<

pdf: $(FILTER)
	# $(PANDOC) --filter ${FILTER} -f $(IFORMAT) --template $(TEMPLATE_TEX) --latex-engine=xelatex $(FLAGS) -o WYAH.pdf title.md 0*.md contributing.md
	$(PANDOC) --filter ${FILTER} -f $(IFORMAT) --template $(TEMPLATE_TEX) --latex-engine=xelatex $(FLAGS) -o WYAH.pdf title.md $(SRC)

epub: $(FILTER)
	$(PANDOC) --filter ${FILTER} -f $(IFORMAT) $(FLAGS) --epub-cover-image=img/cover-kindle.jpg -o WYAH.epub title.md 0*.md

top: $(FILTER)
	$(PANDOC) -c $(STYLE) --filter ${FILTER} --template $(TEMPLATE_HTML) -s -f $(IFORMAT) -t html $(FLAGS) -o tutorial.html index.md

clean:
	-rm *.html *.pdf
