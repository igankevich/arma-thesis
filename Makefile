THESIS_RU = arma-thesis-ru
THESIS_EN = arma-thesis
SLIDES_RU = arma-slides-ru
SLIDES_EN = arma-slides
NOTES_RU = arma-notes-ru
REVIEW_RU = arma-review-ru
ABSTRACT_RU = arma-abstract-ru

FLAGS = \
	-interaction=nonstopmode \
	-output-directory=build \
	-pdf \
	-xelatex \
	-bibtex \
	-shell-escape

export TEXINPUTS=$(PWD)//:

all: build build/$(THESIS_RU).pdf build/$(THESIS_EN).pdf build/$(SLIDES_RU).pdf build/$(NOTES_RU).pdf build/$(SLIDES_EN).pdf

build/$(THESIS_RU).pdf: build/$(THESIS_RU).tex preamble.tex bib/*
	-latexmk $(FLAGS) -f $<

build/$(THESIS_EN).pdf: build/$(THESIS_EN).tex preamble.tex bib/*
	-latexmk $(FLAGS) -f $<

build/$(SLIDES_RU).pdf: build/$(SLIDES_RU).tex slides-preamble.tex math.tex fonts.tex org.tex slides-titlepage-ru.tex
	-latexmk $(FLAGS) -f $<

build/$(SLIDES_EN).pdf: build/$(SLIDES_EN).tex slides-preamble.tex math.tex fonts.tex org.tex slides-titlepage.tex
	-latexmk $(FLAGS) -f $<

build/$(NOTES_RU).pdf: build/$(NOTES_RU).tex build/$(SLIDES_RU).pdf slides-preamble.tex math.tex fonts.tex
	-latexmk $(FLAGS) -f $<

build/$(REVIEW_RU).pdf: build/$(REVIEW_RU).tex preamble.tex math.tex fonts.tex
	-latexmk $(FLAGS) -f $<

build/$(ABSTRACT_RU).odt: $(ABSTRACT_RU).org
	org export $< odt
	mv -v $(ABSTRACT_RU).odt build/$(ABSTRACT_RU).odt

build/$(THESIS_EN).tex: $(THESIS_EN).org
	org export $< latex
	mv $(THESIS_EN).tex $@

build/$(THESIS_RU).tex: $(THESIS_RU).org
	org export $< latex
	mv $(THESIS_RU).tex $@

build/$(SLIDES_RU).tex: $(SLIDES_RU).org
	org export $< beamer
	mv $(SLIDES_RU).tex $@

build/$(SLIDES_EN).tex: $(SLIDES_EN).org
	org export $< beamer
	mv $(SLIDES_EN).tex $@

build/$(REVIEW_RU).tex: $(REVIEW_RU).org
	org export $< latex
	mv $(REVIEW_RU).tex $@

build/$(NOTES_RU).tex: build/$(SLIDES_RU).tex
	sed -r -e 's/\\documentclass\[(.*)\]\{(.*)\}/\\documentclass[\1]{article}\\usepackage{beamerarticle}\\include{fonts}/g' < $< > $@
	#sed -r -e 's/\\documentclass\[(.*)\]\{(.*)\}/\\documentclass[\1,notes]{\2}/g' < $< > $@

clean:
	rm -f build/$(THESIS_EN)*
	rm -f build/$(THESIS_RU)*
	rm -f build/$(SLIDES_RU)*
	rm -f build/$(SLIDES_EN)*
	rm -f build/$(NOTES_RU)*
	rm -f build/$(REVIEW_RU)*

build:
	@mkdir -p build

.PHONY: clean all build
