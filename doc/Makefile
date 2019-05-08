.PHONY: clean

all:

grammar.pdf: grammar.tex
	latexmk -shell-escape -bibtex -f -pdf grammar.tex

grammar.ps: grammar.tex
	latexmk -shell-escape -bibtex -f -ps grammar.tex

clean:
	rm -rf *.aux *.dvi *.bbl *.blg *.fdb_latexmk *.fls *.log *.out
	rm -rf *.pdf *.ps
