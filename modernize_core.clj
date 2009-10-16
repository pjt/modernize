(in-ns 'modernize)

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
            (apply cartesian-product (take hits (repeat alt-seq)))))))    

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
   (when (seq mods)
      (distinct
         (let [mod (first mods)
              [pat repl] [(re-pattern (first mod)) (second mod)]
              new-word (re-sub pat repl word)]
           (lazy-seq (cons [new-word orig] (mod-lookup* orig new-word (rest mods))))))))
           ;(lazy-cons [new-word orig] (mod-lookup* orig new-word (rest mods)))))))

(defn mod-lookup
   "Lookup modifier: returns distinct words made from making transformations 
   in *lookup-modifications*. Returns mods as vectors of [modified original]."
   [word]
   (distinct
      (mapcat #(mod-lookup* word word %) 
         (iter-next *lookup-modifications*))))

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
                  (recur (next wfs))))))
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
      
