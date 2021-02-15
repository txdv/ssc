
full:
	mkdir -p out
	xelatex -output-directory=out/ pdf/darbas.tex
	biber out/darbas.bcf
	xelatex -output-directory=out/ pdf/darbas.tex

tex:
	xelatex -output-directory=out/ pdf/darbas.tex

biber:
	biber out/darbas.bcf

run:
	open out/darbas.pdf
