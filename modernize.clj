(ns modernize
   (:gen-class)
   (:use file-io
         permutation 
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

(def *fallbacks* 
   ; dynamically rebound to hash-map of fallback mappings
   ; for words not found in supplied dictionary
   nil)

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
                (or (some 
                        #(when (dict (lower (first %))) %) 
                        (mod-lookup w))
                  (recur (rest wfs))))))
       fallback  (when *fallbacks*
                     (*fallbacks* word))]
      [word
         {:orig         word
          :permutations perms 
          :matches  
               (cond 
                  (not-empty matches), matches
                  match-mod, #{(second match-mod)}
                  fallback, #{fallback}
                  :else, #{})
          :lookup-form  (first match-mod)
          :fallback     fallback
          :change-patterns
             (when (and (not-empty matches) (not (matches word)))
               (mapcat #(changed-chars-context % word) matches))}]))
         
(defn run-perms-test
   "Runs a word-perms-test on each word in words, using
   dict as dictionary, & letter pairs to permute."
   [dict words & pairs]
   (into {}
      (pmap #(apply word-perms-test dict % pairs) words)))
      ; pmap is marginally slower on small colls of words,
      ; reasonably faster (~20%) on large ones
      

;; Hooks for -main
;;    Inputs needed: (1) dictionary, (2) words to correct, (3) alternate sequences
;;    Supplementary inputs: (4) lookup-translation rules, (5) list of fallback mappings
;;
;;    Outputs: (1) list of successful corrections (or of all matches), 
;;                (2) list of failures

(defn- parse-file-to-ds
   [f ds] ; vector or map
   (with-open [br (BRdr (Fis f))]
      (into (empty ds)
         (for [l (remove empty? (line-seq br))] 
            (into [] (.split l "\\s"))))))

(defn- ext-split
   [#^String fname]
   (map (partial apply str) (split-at (.lastIndexOf fname ".") fname)))

(defn write-results
   [f coll]
   (with-out-file f
      (doseq [i coll]
         (println i))))

(defn write-verbose-results
   [wins losses words-f]
   (let [[nm ext] (ext-split words-f)
        [out-win out-simple-win out-lose]
            (map #(str nm % ext)
               [".matched" ".simple-matched" ".notmatched"])]
      (write-results out-win 
         (map word-perm-to-string wins))
      (write-results out-simple-win 
         (map #(word-perm-to-string % :abbrev) wins))
      (write-results out-lose 
         (map word-perm-to-string losses))))

(def interactive-opts
   {"matches"      [matches word-perm-to-string]
    "new-matches"  [new-matches word-perm-to-string]
    "no-matches"   [no-matches word-perm-to-string]
    "fallbacks"    [fallback-matches word-perm-to-string]
    "trumped"      [trumped-fallbacks word-perm-to-string]
    "patterns"     [extract-patterns 
                     (fn [[k v]] (str k ": " v))]})

(defn interact
   "Provide interactive examination of results"
   [opts results]
   (let [base-cmds {"show" #(println (str-join "\n" %)) 
                     "count" #(println (count %))
                     "write" #(write-results %2 %1)}]
      (loop [in (read-line)]
         (when-not (empty? in)
            (try
               (let [args (.split in "\\s+")
                     base (or (base-cmds (first args)) 
                              (throw (Exception. (str (first args) " not found"))))
                     [cmd fmt]  (or (opts (second args))
                                    (throw (Exception. (str (second args) 
                                                               " not found"))))]
                  (apply base (map fmt (cmd results)) (nthrest args 2)))
               (catch Exception e 
                  (println (.getMessage e)))
               (finally (flush)))
            (recur (read-line))))))
      

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
       [interactive?
         "turns on interactive session for examining results" false]
       [verbose-out?
         "turns on verbose output of files: matches, new matches, unmatched" false]
       [config 
         "file that contains all options -- if given, all other options ignored" nil]
       rest-args]
      (if (and (< (count rest-args) 3) (not config))
         (do 
            (println "modernize: too few arguments; needs dictionary,"
                     "wordlist, & alternates")
            (System/exit 1))
         (or config
            (let
               [[dict-f words-f & pairs] rest-args
                dict    (get-file-set dict-f)
                words   (if (= words-f "-")
                           (into #{} (line-seq (BRdr System/in)))
                           (get-file-set words-f))
                mods    (if lookup
                           (parse-file-to-ds lookup [])
                           *lookup-modifications*)
                fallbacks
                        (when fallback
                           (parse-file-to-ds fallback {}))]
               (binding [*lookup-modifications* mods *fallbacks* fallbacks]
                  (let [results  (apply run-perms-test dict words pairs)
                        wins     (matches results)
                        new-wins (new-matches results)
                        losses   (no-matches results)]
                     (binding [*out* *err*]
                        (println
                           (format
                              "%15s %d\n%15s %d (%d)\n%15s %d\n"
                              "Total words:" (count words) 
                              "Matched (new):" (count wins) (count new-wins) 
                              "Failed:" (count losses))))
                     (if-not interactive?
                        (if verbose-out?
                           (write-verbose-results wins losses words-f)
                           (println (str-join "\n"
                              (map #(word-perm-to-string % :abbrev) new-wins))))
                        (interact interactive-opts results))
                     (System/exit 0))))))))

         
       
