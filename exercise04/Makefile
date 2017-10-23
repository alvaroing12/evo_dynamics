NAME:=Report04
LATEXFLAGS=-halt-on-error -shell-escape
BIBLIOGRAPHY:=bibliografia.bib
RNWSECTIONS:=$(wildcard sec*.Rnw)
TEXSECTIONS:=$(RNWSECTIONS:.Rnw=.tex)

.PHONY: all backup clean distclean

all: $(NAME).pdf

%.tex: %.Rnw
	Rscript -e "library(knitr); knit('$<')"

$(NAME).pdf: $(NAME).tex $(BIBLIOGRAPHY) $(TEXSECTIONS)
	@pdflatex $(LATEXFLAGS) $(NAME).tex
#	-bibtex $(NAME)
#	-pdflatex $(LATEXFLAGS) $(NAME).tex
#	-pdflatex $(LATEXFLAGS) $(NAME).tex

backup: $(NAME).pdf
	mv -f $(NAME).pdf $(NAME).old.pdf

clean:
	$(RM) -r figure
	$(RM) sec*.tex
	$(RM) $(NAME).tex
	$(RM) *.aux *.bbl *.blg *.log *.out *.pyg

distclean: clean
	$(RM) $(NAME).pdf
