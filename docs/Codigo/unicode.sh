#!/bin/sh

# Convertimos los ficheros a ISO-8859-1
for file in $(ls *.hs)
do
	echo $file
	iconv -f=UTF8 -t=LATIN-9 $file > $file.txt
done
