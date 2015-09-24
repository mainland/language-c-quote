#!/bin/sh
set -e

FILES=`find httpd-2.4.16 -name *.i`

for FILE in ${FILES}; do
    echo ${FILE}
    ./parse --gcc --blocks ${FILE} --print >${FILE}.pp.c
    gcc -c ${FILE} -o test.o
    gcc -c ${FILE}.pp.c -o test.o
done
