cp index.R $1.R
zip -FSr -ll releases/$1.zip *.R lib simrc assets *.Rmd PK34.R74/*.OUT PK34.R74/*.CTL Makefile data-raw
rm $1.R
rm -rf tests
mkdir tests
unzip releases/$1.zip -d tests
cd tests
make run

