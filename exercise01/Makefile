NAME:=Report01
LATEXFLAGS=-halt-on-error -shell-escape
BIBLIOGRAPHY:=bibliografia.bib
INCLUDEFILES:=sec1.tex sec2.tex

.PHONY: all backup clean distclean

all: $(NAME).pdf

%.tex: %.Rnw
	Rscript -e "library(knitr); knit('$<')"

$(NAME).pdf: $(NAME).tex $(BIBLIOGRAPHY) $(INCLUDEFILES)
	@pdflatex $(LATEXFLAGS) $(NAME).tex
#	-bibtex $(NAME)
#	-pdflatex $(LATEXFLAGS) $(NAME).tex
#	-pdflatex $(LATEXFLAGS) $(NAME).tex

backup: $(NAME).pdf
	mv -f $(NAME).pdf $(NAME).old.pdf

clean:
	$(RM) sec*.tex
	$(RM) $(NAME).tex
	$(RM) $(NAME).aux $(NAME).bbl $(NAME).blg $(NAME).log $(NAME).out $(NAME).pyg

distclean: clean
	$(RM) $(NAME).pdf
