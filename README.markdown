Modernize
=========

Lib for permuting items in a wordlist & finding those permutations that match
a set of known words; used by me to modernize spelling of words with letters
that were interchangeable (u and v), undifferentiated (i for j), or were commonly 
abbreviated (ā, ē, ō -- vowels with macrons -- for am or an, em or en, etc.) in
early modern printing.

See bin/example-cmd.sh for example of how to run the program. It follows essentially 
3 steps:

  1 Run the modernize code against the tokens from a work

  2 Run the output of 1 through the to-java-static-map code to produce a Java 
  source file for a static map of spelling changes to be made, which is then 
  compiled

  3 Run the modernize-text.xsl stylesheet against the work whose tokens went 
  into 1, with the java class file compiled in 2 on the classpath

Step 3 isn't included in example-cmd.sh; it'll look something like:

  `saxon -cp <dir in which is Lookup.class> calender.xml modernize-text.xsl`

assuming your saxon shell wrapper accepts a -cp switch; if it doesn't, it's

   `java -cp saxon9.jar:<dir in which is Lookup.class> net.sf.saxon.Transform calender.xml modernize-text.xsl`

