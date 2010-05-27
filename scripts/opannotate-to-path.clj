#! /usr/bin/env clj

;; [[file:~/research/genprog/asm/asm-gp.org::#opannotate-to-path][block-57]]
(load-file "/home/eschulte/research/genprog/asm/src/asm-gp.clj")(in-ns 'asm-gp)

(def mapping (read-obj "mapping.clj"))

(while-let
 [line (read-line)]
 ;;     10 27.7778 : 804ba03:
 (when-let [match (re-matches #"[\s]+([\d]+)[\s]+([\.\d]+)[\s]+:[\s]+([\w]+):.*" line)]
   (dorun
    (map
     (fn [_] (if-let [line (or (mapping (format "0x0000000000%s" (nth match 3)))
                               (mapping (format "0x0%s" (nth match 3))))]
               (println line)))
     (range (Integer/parseInt (nth match 1)))))))
;; block-57 ends here
