


xname='_executive_summary' #### EDIT HERE





#Rscript _example_A.R
#Rscript _example_B.R
#R CMD Sweave _example_A.Snw
#R CMD Sweave _example_B.Snw


########### use pdflatex CLI

rm  "$xname".aux
rm  "$xname".bbl
pdflatex -interaction=nonstopmode   "$xname".tex
bibtex   "$xname"
pdflatex -interaction=nonstopmode   "$xname".tex
pdflatex -interaction=nonstopmode   "$xname".tex



####### OR ##### use latexmk

latexmk -pdf "$xname".tex
latexmk -c

