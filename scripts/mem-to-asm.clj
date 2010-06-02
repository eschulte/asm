#! /usr/bin/env clojure

(load-file "/home/eschulte/research/genprog/asm/src/asm-gp.clj")(in-ns 'asm-gp)

(def asm-file (read-asm (second *command-line-args*)))

(def pointer 0)

(while (not (= "main:" (:line (nth asm-file pointer))))
       (def pointer (inc pointer)))
(def pointer (inc pointer))

(def mapping {})

(while-let
 [line (read-line)]
 ;; build up the mapping
 (when-let [matches (re-matches #"([\w]+) <main\+([\d]+)>:.*" line)]
   ;; step past .L lines which aren't noticed by gdb
   (while (and (string? (nth asm-file pointer))
               (re-matches #"\.L.+" (nth asm-file pointer)))
          (def pointer (inc pointer)))
   ;; associate this memory address with this line in the asm-file
   (def mapping (assoc mapping (nth matches 1) pointer))
   (def pointer (inc pointer)))
 ;; apply the mapping
 (when-let [hex (second (re-matches #"([\w]+) in main \(\)" line))]
   (println (mapping hex))))
