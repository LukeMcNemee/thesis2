filename=thesis

all: pdf

pdf:
	pdflatex -output-directory=build ${filename}.tex
	cp bibliography.bib build
	cd build && biber ${filename}.bcf
	pdflatex -output-directory=build ${filename}.tex
	pdflatex -output-directory=build ${filename}.tex
	cp build/${filename}.pdf .

clean:
	rm -f build/${filename}.* build/bibliography.bib

read: pdf
	evince ${filename}.pdf

once:
	pdflatex -output-directory=build ${filename}.tex
	cp build/${filename}.pdf .
