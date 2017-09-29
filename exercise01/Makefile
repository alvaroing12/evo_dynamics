NAME:=Report01
LATEXFLAGS=-halt-on-error -shell-escape
BIBLIOGRAPHY:=bibliografia.bib

.PHONY: all backup clean distclean

all: $(NAME).pdf

$(NAME).tex:
	Rscript -e "library(knitr); knit('$(NAME).Rnw')"

$(NAME).pdf: $(NAME).tex $(BIBLIOGRAPHY) $(wildcard sections/*.tex)
	@pdflatex $(LATEXFLAGS) $(NAME).tex
#	-bibtex $(NAME)
#	-pdflatex $(LATEXFLAGS) $(NAME).tex
#	-pdflatex $(LATEXFLAGS) $(NAME).tex

backup: $(NAME).pdf
	mv -f $(NAME).pdf $(NAME).old.pdf

clean:
	$(RM) $(NAME).aux $(NAME).bbl $(NAME).blg $(NAME).log $(NAME).out $(NAME).pyg

distclean: clean
	$(RM) $(NAME).pdf
