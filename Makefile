
full:
	mkdir -p out
	xelatex -output-directory=out/ pdf/darbas.tex
	biber out/darbas.bcf
	xelatex -output-directory=out/ pdf/darbas.tex

tex:
	xelatex -output-directory=out/ pdf/darbas.tex

biber:
	biber out/darbas.bcf

out/darbas.pdf: pdf/darbas.tex
	xelatex -output-directory=out/ pdf/darbas.tex

open: out/darbas.pdf
	open out/darbas.pdf
