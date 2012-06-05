#!/bin/sh
for i in `cat OCAMLTESTTARGETS.txt`
do
  if [ -f $i ]; then
    files="$files $i" 
  fi
done
./lexertest $files
