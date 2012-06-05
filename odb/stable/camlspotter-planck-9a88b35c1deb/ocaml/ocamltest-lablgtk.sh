#!/bin/sh
for i in `cat OCAMLTESTTARGETS-lablgtk.txt`
do
  if [ -f $i ]; then
    if ! ./parser $i; then echo $i; echo; fi
  fi
done
