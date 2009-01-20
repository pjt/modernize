(use '[clojure.contrib duck-streams str-utils shell-out])


(def java-tmpl
"import java.util.Map;
import java.util.HashMap;
import java.util.Collections;

public class $classname$ {
   public static final Map<String,String> LIST;
   static {
      HashMap<String,String> list = new HashMap<String,String>();
      $map-puts$
      LIST = Collections.unmodifiableMap(list);
   }
   public static boolean contains(String word) {
      return LIST.containsKey(word);
   }
   public static String get(String word) {
      return LIST.get(word);
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
      (format "list.put(\"%s\",\"%s\");" k v))) 

(when *command-line-args*
   (let [class-name (first *command-line-args*)
         puts (as-java-puts (read-wordlist System/in))
         out  (.replace java-tmpl "$classname$" class-name)
         out  (.replace out "$map-puts$" (str-join "\n" puts))]
      (spit (str class-name ".java") out)
      (println (sh "javac" "-encoding" "UTF8" (str class-name ".java")))))
            

