
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

out/planas.pdf: pdf/planas.tex
	xelatex -output-directory=out/ pdf/planas.tex

open: out/darbas.pdf
	open out/darbas.pdf

planas: out/planas.pdf
	open out/planas.pdf

planb:
	xelatex -output-directory=out/ pdf/planas.tex
	biber out/planas.bcf
	xelatex -output-directory=out/ pdf/planas.tex
	open out/planas.pdf

aspell:
	aspell check pdf/planas.tex
