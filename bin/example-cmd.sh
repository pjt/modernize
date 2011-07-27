#!/bin/sh

OUT=out.txt

lein trampoline run \
   resources/bigdict-moby.txt \
   ~/dev/sa-repos/works/volOne/trunk/calender/cal-words.txt \
   "i j" \
   "u v" \
   "v u" \
   "vv w" \
   "\u00F5 on om" \
   "\u0169 un um" \
   "\u1EDB en em" \
   "\u00E3 an am" \
   > $OUT

lein run -m modernize.to-java-static-map $OUT Lookup
