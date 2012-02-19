#!/bin/sh
set -e

FILES=`find httpd-2.2.22 -name *.i`

for FILE in ${FILES}; do
    echo ${FILE}
    ./parse --gcc ${FILE} --print >${FILE}.pp.c
    gcc -c ${FILE} -o test.o
    gcc -c ${FILE}.pp.c -o test.o
done
