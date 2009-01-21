#!/bin/sh

#java -server -Xmx256m -jar modernize.jar data/bigdict-moby.txt data/th-words.txt \
java -server -Xmx256m -cp `clj --print-cp`:modernize.jar modernize \
   ~/dev/sa-repos/works/volOne/trunk/theatre/bigdict-moby.txt \
   ~/dev/sa-repos/works/volOne/trunk/letters/let-words.txt \
   "i j" \
   "u v" \
   "v u" \
   "vv w" \
   "\u00F5 on om" \
   "\u0169 un um" \
   "\u1EDB en em" \
   "\u00E3 an am"
