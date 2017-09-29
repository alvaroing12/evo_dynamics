NAME:=Report01
LATEXFLAGS=-halt-on-error -shell-escape
BIBLIOGRAPHY:=bibliografia.bib

.PHONY: all backup clean distclean

all: $(NAME).pdf

#$(NAME).pdf: $(BIBLIOGRAPHY) $(wildcard sections/*.tex)
$(NAME).pdf: $(NAME).tex $(wildcard sections/*.tex)
	@pdflatex $(LATEXFLAGS) $(NAME).tex
#	- bibtex $(NAME)
#	- pdflatex $(LATEXFLAGS) $(NAME).tex
#	- pdflatex $(LATEXFLAGS) $(NAME).tex

backup: $(NAME).pdf
	mv -f $(NAME).pdf $(NAME).old.pdf

clean:
	$(RM) $(NAME).aux $(NAME).blg $(NAME).log $(NAME).out $(NAME).pyg

distclean: clean
	$(RM) $(NAME).pdf
