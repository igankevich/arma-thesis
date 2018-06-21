PHD_RU = arma-thesis-ru
PHD_EN = arma-thesis
SLIDES = arma-slides
FLAGS = -interaction=nonstopmode \
	-output-directory=build \
	-pdf \
	-xelatex \
	-bibtex \
	-shell-escape

export TEXINPUTS=$(PWD)//:

all: build/$(PHD_RU).pdf build/$(PHD_EN).pdf build/$(SLIDES).pdf

build/$(PHD_RU).pdf: $(PHD_RU).tex preamble.tex bib/*
	latexmk $(FLAGS) -f $(PHD_RU).tex
	true

build/$(PHD_EN).pdf: $(PHD_EN).tex preamble.tex bib/*
	latexmk $(FLAGS) -f $(PHD_EN).tex
	true

build/$(SLIDES).pdf: $(SLIDES).tex slides-preamble.tex math.tex
	latexmk $(FLAGS) -f $(SLIDES).tex
	true

$(PHD_EN).tex: $(PHD_EN).org
	org export $< latex

$(PHD_RU).tex: $(PHD_RU).org
	org export $< latex

$(SLIDES).tex: $(SLIDES).org
	org export $< beamer

clean:
	rm -f build/$(PHD_EN)*
	rm -f build/$(PHD_RU)*
	rm -f build/$(SLIDES)*
