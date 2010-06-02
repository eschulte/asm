#! /usr/bin/env clj

(load-file "/home/eschulte/research/genprog/asm/src/asm-gp.clj")
(in-ns 'asm-gp)

(def asm-file (:representation (read-asm (second *command-line-args*))))
(message "asm file: %s %d lines" (second *command-line-args*) (count asm-file))
(def bin-file (nth *command-line-args* 2))
(message "bin-file: %s" bin-file)
(def mapping {})

(defn function-lines [asm]
  (filter identity
          (map #(and (string? %)
                     (if-let [match (re-matches #"([^\.].+):" %)]
                       (second match)))
               (map :line asm))))
(defn gdb-disassemble
  "Takes the path to a binary, and the name of the symbol to be
  disassembled."  [path function]
  (s/sh "gdb" "--batch"
        (format "--eval-command=disassemble %s" function) path))

(dorun
 (map ;; for every function defined in the file
  (fn [func]
    (let [lines (seq (.split (gdb-disassemble bin-file func) "\n"))]
      ;; step to beginning of function
      (def pointer 0)
      (while (not (= (format "%s:" func) (:line (nth asm-file pointer))))
             (def pointer (inc pointer)))
      (def pointer (inc pointer))
      (message "\t%s:%d %d lines" func (dec pointer) (count lines))
      (dorun
       (map ;; build up the mapping of memory address to LOC
        #(when-let [matches (re-matches
                             #"[\s]*([\w]+)[\s]*<[\w+]*\+([\d]+)>:.*" %)]
           ;; step past .L lines which aren't noticed by gdb
           (while (and (string? (:line (nth asm-file pointer nil)))
                       (re-matches #"\.L.+" (:line (nth asm-file pointer nil))))
                  (def pointer (inc pointer)))
           ;; associate this memory address with this line in the asm-file
           (def mapping (assoc mapping (nth matches 1) pointer))
           (def pointer (inc pointer)))
        lines))))
  (function-lines asm-file)))

(write-obj "mapping.clj" mapping)
