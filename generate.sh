#!/bin/bash
echo $1 $2
SRC=$1
OUT=$2
TMP=tmp
rm -fR $TMP $OUT ; ./bogroll $SRC $TMP && (cd $TMP && zip -X -0 -r -r -UN=UTF8  ../$OUT mimetype META-INF OPS/ )
