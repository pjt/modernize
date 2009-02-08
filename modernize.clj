(ns modernize
   (:gen-class)
   (:use (clojure.contrib 
            duck-streams
            str-utils 
            seq-utils
            combinatorics
            command-line))
   (:load "modernize_utils" "modernize_core"))

;; Hooks for -main
;;    Inputs needed: (1) dictionary, (2) words to correct, (3) alternate sequences
;;    Supplementary inputs: (4) lookup-translation rules, (5) list of fallback mappings
;;
;;    Outputs: (1) list of successful corrections (or of all matches), 
;;                (2) list of failures

(defn- parse-file-to-ds
   [f ds] ; vector or map
   (into (empty ds)
      (for [l (remove empty? (read-lines f))] 
         (into [] (.split l "\\s")))))

(defn- ext-split
   [#^String fname]
   (map (partial apply str) (split-at (.lastIndexOf fname ".") fname)))

(defn write-verbose-results
   [wins losses words-f]
   (let [[nm ext] (ext-split words-f)
        [out-win out-simple-win out-lose]
            (map #(str nm % ext)
               [".matched" ".simple-matched" ".notmatched"])]
      (write-lines out-win 
         (map word-perm-to-string wins))
      (write-lines out-simple-win 
         (map #(word-perm-to-string % :abbrev) wins))
      (write-lines out-lose 
         (map word-perm-to-string losses))))

(def interactive-opts
   {"matches"      [matches word-perm-to-string]
    "new-matches"  [new-matches word-perm-to-string]
    "no-matches"   [no-matches word-perm-to-string]
    "fallbacks"    [fallback-matches word-perm-to-string]
    "trumped"      [trumped-fallbacks word-perm-to-string]
    "patterns"     [extract-patterns 
                     (fn [[k v]] (format "%10s: %s" k v))]})

(defn interact
   "Provide interactive examination of results"
   [opts results]
   (let [base-cmds {"show" #(println (str-join "\n" %)) 
                    "count" #(println (count %))
                    "write" #(write-lines %2 %1)}
         read-line   #(do (print ">> ") (flush) (read-line))]
      (printf "Commands: (%s) (%s) [filename]\n"
         (str-join "|" (keys base-cmds))
         (str-join "|" (keys opts)))
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
Usage: modernize <dictionary-file> <wordlist-file> <alternations>+
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
                           (into #{} (read-lines System/in))
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
                           (rjust-cols
                              "Total words:" (count words)
                              "Matched (new):" 
                                 (format "%d (%d)" (count wins) (count new-wins))
                              "Failed:" (count losses))))
                     (if-not interactive?
                        (if verbose-out?
                           (write-verbose-results wins losses words-f)
                           (println (str-join "\n"
                              (map #(word-perm-to-string % :abbrev) wins))))
                        (interact interactive-opts results))
                     (System/exit 0))))))))

(when *command-line-args* 
   (apply -main *command-line-args*))
       
