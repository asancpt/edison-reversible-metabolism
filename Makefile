run:
	rm -rf ./result \;
	Rscript index.R \;
	cat README.md

see:
	open result/report.html

