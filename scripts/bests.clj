#! /usr/bin/env clj
(load-file "/home/eschulte/research/genprog/asm/src/asm-gp.clj")(in-ns 'asm-gp)

(def ops {:swap 0 :delete 0 :append 0 :crossover 0})
(def opsize 0)
(def total 0)
(def trials 0)

(defn ingest-ops [operations]
  (doseq [op operations]
    (if (list? op)
      (ingest-ops op)
      (def ops (assoc ops op (inc (op ops)))))))

(while-let
 [line (read-line)]
 (when-let [best (read-obj line)]
   (def total (inc total))
   (def trials (+ trials (:trials best)))
   (def opsize (+ opsize (.size (:operations best))))
   (ingest-ops (:operations best))))

(println "total" total (/ total total))
(println "trials" trials (/ trials total))
(println "opsize" opsize (/ opsize total))
(println "ops" ops)