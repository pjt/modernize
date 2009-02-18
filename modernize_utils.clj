(in-ns 'modernize)
(use 'clojure.contrib.str-utils)
(require '[clojure.contrib.mmap :as mm])

(defn lower [#^String s] (.toLowerCase s))
(defn upper [#^String s] (.toUpperCase s))
(defn lower? [#^String s] (every? #(Character/isLowerCase %) s))
(defn upper? [#^String s] (every? #(Character/isUpperCase %) s))

(defn longest
   "Return longest item, where lenth is (count item)."
   [& items]
   (first (sort-by count (comp - compare) items)))

(defn iter-next
   "Takes the first (count coll) items from calls to (iterate next coll).
   If passed function as first argument, calls it on each invocation of
   next, i.e. (iterate #(func (next %)) coll)."
   ([coll] (take (count coll) (iterate next coll)))
   ([func coll] (take (count coll) (iterate #(func (next %)) coll))))

(defn common-subs
   "Returns sorted set of common contiguous subsequences of a and b. 
   Implements a Longest Common Subsequence algorithm, but keeps all 
   subseqs found. With help from
      http://www.ics.uci.edu/~dan/class/161/notes/6/Dynamic.html
   * Note that since subsequences here are contiguous, this is really the
   longest common substring problem, generalized to any collection."
   [a b]
   (reduce
      (fn [acc a-val]
         (loop [inner-acc [] a a-val b b]
            (if (and a b)
               (if (= (first a) (first b))
                  (recur (conj inner-acc (first a)) (next a) (next b))
                  (if (empty? inner-acc)
                     (recur inner-acc a (next b))
                     (conj acc inner-acc)))
               (if (empty? inner-acc) acc (conj acc inner-acc)))))
      #{} (iter-next a)))

(defn lcs
   "Longest Common (contiguous) Subsequence; see common-subs."
   [a b]
   (apply longest (common-subs a b)))

(defn freq
   "Return unique items of coll keyed to number of times
   they appear in coll."
   [coll]
   (reduce 
      (fn [acc item] (assoc acc item (inc (get acc item 0)))) 
      {} coll))

(defn no-matches
   "Return perms for which no dictionary match was found."
   [results]
   (filter (fn [[k v]] (empty? (:matches v))) results))

(defn matches
   "Return perms for which at least one dictionary match was found."
   [results]
   (remove (fn [[k v]] (empty? (:matches v))) results))

(defn fallback-matches
   "Return perms for which a fallback match was found."
   [results]
   (remove (fn [[k v]] (nil? (:fallback v))) results))

(defn trumped-fallbacks
   "Returns perms for which a fallback match as well as a dictionary
   match was found."
   [results]
   (filter (fn [[k v]] (not ((:matches v) (:fallback v)))) results))
 
(defn new-matches
   "Return perms whose dictionary match differs from the original
   word (case insensitive)."
   [results]
   (filter 
      (fn [[k v]] 
         (let [in-dict (:matches v)]
            (and (not-empty in-dict)
                (not (in-dict k)))))
      results))

(defn extract-patterns
   "Extract the change contexts from the perms."
   [results]
   (freq (mapcat (comp :change-patterns val) results)))

(defn word-perm-to-string
   "Returns a string representation of a word-perms-test,
   given as leaf node. Give extra arg (:abbrev) for just
   original & match(es)."
   [[k v] & abbreviated]
   (let [matches (str-join " " (:matches v))]
      (if abbreviated
         (format "%s => %s" k matches)
         (apply str
            (format "%s\n\t(dict) %s\n" k matches)
            (for [inner-k (filter (partial not= :matches) (keys v))]
               (format "\t(%s) %s\n" inner-k 
                  (let [strings (inner-k v)]
                     (cond 
                       (nil? strings)
                         ""
                       (coll? strings)
                         (str-join " " strings)
                       :else
                         strings))))))))

(defn get-file-set
   "Returns dictionary as set of /usr/share/dict/words items,
   or from any other newline-separated wordlist."
   [& path]
   (let [path (or (first path) "/usr/share/dict/words")]
      (with-open [r (reader (mm/buffer-stream (mm/mmap path)))]
         (into #{} (line-seq r)))))

;(defn get-file-set
;   "Returns dictionary as set of /usr/share/dict/words items,
;   or from any other newline-separated wordlist."
;   [& path]
;   (let [path (or (first path) "/usr/share/dict/words")]
;      (set (line-seq (Rdr (Fis path))))))

(defn read-pairs-dict
   [path]
   (into {} 
     (map #(.split % "\\s+") 
       (read-lines path))))

(defn learn-from-lem
   "Takes spelling-lemma pairs & learns the correct spelling where 
   spelling differs according to one of passed pairs. E.g.
   (learn-from-lem \"trauailes\" \"travail\" \"ij\" \"uv\")
      => \"travailes\""
   [w lem & pairs]
   (let [pairs (map set (vec pairs))
       [w lem] (map lower [w lem])
        new
         (apply str
            (map
              (fn [w-char l-char]
                (if (and (not= w-char l-char)
                       (some #(and (% w-char) (% l-char)) pairs))
                     l-char
                     w-char))
              w lem))]
      (if (> (count w) (count lem))
         (str new (subs w (count lem)))
         new)))

(defn rjust-cols
   "Given pairs of strings, returns string formatted with the strings 
   in the first position right-justified to the length of the longest 
   string. E.g.
      \"hello\" \"there\"
      \"hi\"    \"you\"
         =>
      \"hello there\"
      \"   hi you\""
   [& strings]
   (let [columns (partition 2 (map str strings))
         max (apply max (map (comp count first) columns))
         fmt (str "%" max "s %s")]
      (str-join "\n"
         (for [col columns]
            (apply format fmt col)))))
