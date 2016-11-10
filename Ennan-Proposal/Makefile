OUT = proposal
GRAY = gray

all: $(OUT).pdf	# $(GRAY).pdf

$(OUT).pdf: $(wildcard *.tex *.bib *.bbl *.cls figs/*)
	pdflatex $(OUT)
	bibtex $(OUT)
	pdflatex $(OUT)
	pdflatex $(OUT)

bib:
	bibtex $(OUT)
	pdflatex $(OUT)

clean:
	rm -f *~ *.aux *.dvi *.bbl *.blg *.log proposal.pdf
