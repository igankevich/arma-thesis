PHD_RU = arma-thesis-ru
PHD_EN = arma-thesis
SLIDES = arma-slides
SLIDES_WITH_NOTES = $(SLIDES)-with-notes
FLAGS = -interaction=nonstopmode \
	-output-directory=build \
	-pdf \
	-xelatex \
	-bibtex \
	-shell-escape

export TEXINPUTS=$(PWD)//:

all: build/$(PHD_RU).pdf build/$(PHD_EN).pdf build/$(SLIDES).pdf build/$(SLIDES_WITH_NOTES).pdf

build/$(PHD_RU).pdf: $(PHD_RU).tex preamble.tex bib/*
	latexmk $(FLAGS) -f $(PHD_RU).tex
	true

build/$(PHD_EN).pdf: $(PHD_EN).tex preamble.tex bib/*
	latexmk $(FLAGS) -f $(PHD_EN).tex
	true

build/$(SLIDES).pdf: $(SLIDES).tex slides-preamble.tex math.tex
	latexmk $(FLAGS) -f $(SLIDES).tex
	true

build/$(SLIDES_WITH_NOTES).pdf: $(SLIDES_WITH_NOTES).tex slides-preamble.tex math.tex
	latexmk $(FLAGS) -f $(SLIDES_WITH_NOTES).tex
	true

$(PHD_EN).tex: $(PHD_EN).org
	org export $< latex

$(PHD_RU).tex: $(PHD_RU).org
	org export $< latex

$(SLIDES).tex: $(SLIDES).org
	org export $< beamer

$(SLIDES_WITH_NOTES).tex: $(SLIDES).tex
	sed -r -e 's/\\documentclass\[(.*)\]\{(.*)\}/\\documentclass[\1]{article}\\usepackage[\1]{beamerarticle}\\include{fonts}/g' < $< > $@
	#sed -r -e 's/\\documentclass\[(.*)\]\{(.*)\}/\\documentclass[\1,notes]{\2}/g' < $< > $@

clean:
	rm -f build/$(PHD_EN)*
	rm -f build/$(PHD_RU)*
	rm -f build/$(SLIDES)*
