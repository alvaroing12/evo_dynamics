NAME:=Report01
LATEXFLAGS=-halt-on-error -shell-escape
BIBLIOGRAPHY:=bibliografia.bib
INCLUDEFILES=

.PHONY: all backup clean create distclean

all: $(NAME).pdf

#$(NAME).pdf: $(BIBLIOGRAPHY) $(wildcard sections/*.tex)
$(NAME).pdf: $(NAME).tex $(wildcard sections/*.tex)
	@pdflatex $(LATEXFLAGS) $(NAME).tex
#	- bibtex $(NAME)
#	- pdflatex $(LATEXFLAGS) $(NAME).tex
#	- pdflatex $(LATEXFLAGS) $(NAME).tex

backup: $(NAME).pdf
	mv -f $(NAME).pdf $(NAME).old.pdf

clean: backup
	rm -f $(NAME).aux $(NAME).blg $(NAME).log $(NAME).out $(NAME).pyg $(NAME).pdf

create: $(NAME).tex $(INCLUDEFILES)

distclean:
	rm -f $(NAME).aux $(NAME).bbl $(NAME).blg $(NAME).log $(NAME).out $(NAME).pyg $(NAME).pdf $(NAME).old.pdf

fix: $(NAME).tex $(INCLUDEFILES)
	pdflatex $(LATEXFLAGS) $^
	rm -f $(NAME).aux $(NAME).blg $(NAME).log $(NAME).out $(NAME).pyg

