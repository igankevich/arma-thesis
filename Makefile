PHD_RU = arma-thesis-ru
PHD_EN = arma-thesis
FLAGS = -interaction=nonstopmode \
	-output-directory=build \
	-pdf \
	-xelatex \
	-bibtex \
	-shell-escape

export TEXINPUTS=$(PWD)//:

all: build/$(PHD_RU).pdf build/$(PHD_EN).pdf

build/$(PHD_RU).pdf: $(PHD_RU).tex preamble.tex bib/*
	latexmk $(FLAGS) -f $(PHD_RU).tex
	true

build/$(PHD_EN).pdf: $(PHD_EN).tex preamble.tex bib/*
	latexmk $(FLAGS) -f $(PHD_EN).tex
	true

clean:
	rm -f build/$(PHD_EN)*
	rm -f build/$(PHD_RU)*
