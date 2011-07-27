(ns modernize.to-java-static-map
  (:use [clojure.contrib duck-streams str-utils shell-out]))

(def java-tmpl
"import java.util.Map;
import java.util.HashMap;
import java.util.Collections;

public class $classname$ {
   public static final Map<String,String> WORDS;
   static {
      HashMap<String,String> words = new HashMap<String,String>();
      $map-puts$
      WORDS = Collections.unmodifiableMap(words);
   }
   public static boolean contains(String word) {
      return WORDS.containsKey(word);
   }
   public static String get(String word) {
      return WORDS.get(word);
   }
   public static void main(String[] args) {
      if (contains(args[0])) {
         System.out.println(get(args[0]));
      }
   }
}
")

(defn read-wordlist [in]
   (into {} (map #(vec (.split % " => ")) (read-lines in))))

(defn as-java-puts [m]
   (for [[k v] m :when (not= k v)]
      (format "words.put(\"%s\",\"%s\");" k v))) 

(defn -main [& args]
   (let [word-file  (first args)
         class-name (second args)
         puts (as-java-puts (read-wordlist word-file))
         out  (.replace java-tmpl "$classname$" class-name)
         out  (.replace out "$map-puts$" (str-join "\n" puts))]
      (spit (str class-name ".java") out)
      (println "Compiling" (str class-name ".java") "...")
      (println (sh "javac" "-encoding" "UTF8" (str class-name ".java")))))
            

