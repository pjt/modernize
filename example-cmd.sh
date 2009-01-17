#!/bin/sh

java -jar modernize.jar data/bigdict-moby.txt data/th-words.txt \
   "i j" \
   "u v" \
   "v u" \
   "vv w" \
   "\u00F5 on om" \
   "\u0169 un um" \
   "\u1EDB en em" \
   "\u00E3 an am"
