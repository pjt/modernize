(ns modernize
   (:gen-class)
   (:use permutation 
         (clojure.contrib 
            str-utils 
            seq-utils
            command-line))
   (:load "modernize_utils"))

(defn word-perms
   "Takes word & alternate sequence (items separated by space, where first item
   triggers permutation), returns variations on word with all permuations of 
   alternates (or coll of just word, if no alternates found in word)."
   [#^String word #^String alts]
   (let [alt-seq  (seq (.split alts " "))
         ;pattern  (re-pattern (apply str "(?i)" (interpose "|" alt-seq)))
         pattern  (re-pattern (str "(?i)" (first alt-seq)))
         matches  (re-seq pattern word)
         case-fns (map #(if (lower? %) lower upper) matches)
         fix-case (fn [perms] 
                     (map #(%1 %2)
                        case-fns perms))
         pieces   (re-split pattern word -1)
         hits     (dec (count pieces))]
      (if (zero? hits)
         (list word)                         ; note "" on end of perms coll
         (map #(apply str (interleave pieces (concat (fix-case %) [""]))) 
            (apply map-perm list (take hits (repeat alt-seq)))))))    

(defn changed-chars-context
   "Given new and origanal word, returns changed characters in 
   context of surrounding two characters."
   [w orig]
   (let
      [[w orig] (map lower [w orig])
       pat (re-pattern (apply str (interpose "|" 
                                    (map (partial apply str) (common-subs w orig)))))
       same (re-seq pat w)
       old-diff (re-split pat orig -1)
       new-diff (re-split pat w -1)]
      (for [[i [o n]] (indexed (map list old-diff new-diff)) 
                  :when (not= o n)]
         (str
            (last (nth same (dec i) ""))
            "[" o "->" n "]"
            (first (nth same i ""))))))

(def *lookup-modifications*
   #^{:doc "Translation table for dictionary lookup: [old new]."}
  [["s$"   ""]
   ["nesse$"""]
   ["ie$"   "y"]
   ["e$"   ""]
   ;["es"   ""]
   ["ed$"   ""]
   ["k$"   "ck"]
   ["ing$"  ""]
   ["yng$"  ""]
   ["yng$"  "ing"]
   ["ish$"  ""]
   ["ieth$" "y"]
   ["est$"  ""]
   ["eth$"  ""]
   ["eth$"  "e"]
   ["é"  "e"]
   ["y"  "i"]
   ["k"  "ck"]
   ["ou" "u"]
   ["au" "a"]
   ["(.)\\1" "$1"]
   ["ou" "oo"]])
   
(defn mod-lookup*
   "Inner lookup modifier: modifies word recursively down list of translations."
   [orig word mods]
   (when mods
      (distinct
         (let [mod (first mods)
              [pat repl] [(re-pattern (first mod)) (second mod)]
              new-word (re-sub pat repl word)]
           (lazy-cons [new-word orig] (mod-lookup* orig new-word (rest mods)))))))

(defn mod-lookup
   "Lookup modifier: returns distinct words made from making transformations 
   in *lookup-modifications*. Returns mods as vectors of [modified original]."
   [word]
   (distinct
      (mapcat #(mod-lookup* word word %) 
         (iter-rest *lookup-modifications*))))

(defn word-perms-test
   "Tests if any of permutations of word across given alternate sequences
   are present in a dictionary. Dictionary must be a set; alternate 
   sequences must be strings, with items separated by whitespace: \"vv w\" 
   \"i j\", where presence of the first item in the word triggers permutation."
   [dict word & alts]
   (let 
      [perms
         (reduce (fn [acc alt] (mapcat #(word-perms % alt) acc))
               [word] alts)
       wordforms (distinct perms)
       ; test if straight permuation yields match
       matches   (set (filter #(dict (lower %)) wordforms))
       match-mod
         ; if not, try doing lookup-modifications on each wordform
         (when (empty? matches)
            (loop [wfs wordforms]
              (when-let [w (first wfs)]
                (or (some #(when (dict (lower (first %))) %) (mod-lookup w))
                  (recur (rest wfs))))))]
      [word
         {:orig         word
          :permutations perms 
          :matches  
               (cond 
                  (not-empty matches), matches
                  match-mod, (conj #{} (second match-mod))
                  :else, #{})
          :lookup-form  (first match-mod)
          :change-patterns
             (when (and (not-empty matches) (not (matches word)))
               (mapcat #(changed-chars-context % word) matches))}]))
         
(defn run-perms-test
   "Runs a word-perms-test on each word in words, using
   dict as dictionary, & letter pairs to permute."
   [dict words & pairs]
   (into {}
      (map #(apply word-perms-test dict % pairs) words)))
      

;; Hooks for -main
;;    Inputs needed: (1) dictionary, (2) words to correct, (3) alternate sequences
;;    Supplementary inputs: (4) lookup-translation rules, (5) list of fallback mappings
;;
;;    Outputs: (1) list of successful corrections (or of all matches), 
;;                (2) list of failures

(defn- parse-file-to-ds
   [f ds] ; vector or map
   (into (empty ds)
      (for [l (line-seq (Rdr (Fis f)))] 
         (into [] (.split l "\\s")))))

(defn- ext-split
   [#^String fname]
   (map (partial apply str) (split-at (.lastIndexOf fname ".") fname)))

(defn -main
   [& args]
   (with-command-line args
      "modernize: modernize spelling based on supplied alternations
      Usage: modernize <dictionary-file> <wordlist-file> <alterations>+
      Options:"
      [[lookup 
         "file that contains lookup modification rules" nil]
       [fallback 
         "file that contains fallback mappings for words" nil]
       ;[out-correct 
       ;  "file in which to write corrections (words found in dict)" System/out]
       ;[out-missed 
       ;  "file in which to write missed words (words not found in dict)" nil]
       [config 
         "file that contains all options -- if given, all other options ignored" nil]
       rest-args]
      (if (and (< (count rest-args) 3) (not config))
         (do 
            (println "modernize: too few arguments; needs dictionary, "
                     "wordlist, & alternates")
            (System/exit 1))
         (or config
            (let
               [[dict-f words-f & pairs] rest-args
                dict    (get-file-set dict-f)
                words   (if (= words-f "-")
                           (into #{} (line-seq (Rdr System/in)))
                           (get-file-set words-f))
                mods    (if lookup
                           (parse-file-to-ds lookup [])
                           *lookup-modifications*)
                [out-win out-lose]
                  (let [[nm ext] (ext-split words-f)]
                     [(str nm ".matched" ext) (str nm ".notmatched" ext)])]
               (binding [*lookup-modifications* mods]
                  (let [results (apply run-perms-test dict words pairs)]
                     (println (count (matches results)))
                     (println (count (no-matches results))))))))))

         
       
