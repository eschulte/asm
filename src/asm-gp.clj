
(ns asm-gp
    (:require (clojure.contrib
               (duck-streams :as f)
               (shell-out :as s))
              (clojure.contrib.generic (math-functions :as m)))
    (:import (java.io BufferedReader InputStreamReader File)
             (java.util ArrayList Collections)))

(defmacro while-let
  "Like while, but uses when-let instead of when."
  [test & body]
  `(loop []
     (when-let ~test
       ~@body
       (recur))))

(defn message
  [fmt & args]
  (println (apply format (cons fmt args))))

(defmacro with-timeout [ms & body]
  `(let [f# (future ~@body)]
     (.get f# ~ms java.util.concurrent.TimeUnit/MILLISECONDS)))

(defn write-obj
  "Write a clojure object to a file" [f obj]
  (f/spit f (pr-str obj)))

(defn read-obj
  "Read a clojure object from a file" [f]
  (with-in-str (slurp f) (read)))

(defn shuffle
  "Shuffles coll using a Java ArrayList." [coll]
  (let [l (ArrayList. coll)] (Collections/shuffle l) (seq l)))

(defn place
  "Pick a random location from a sequence"
  [lst]
  (rand-int (.size lst)))

(defn pick
  "Pick and return a random element from a sequence."
  [lst] (nth lst (place lst)))

(defn weighted-place
  "Pick a random location in an asm individual weighted by the
   associated bad-path."
  ([asm] (weighted-place asm :bad-weight))
  ([asm weight-key]
     (pick
      ((fn [index asm assoc] ;; expand each place by it's weight
         (if (empty? asm)
           assoc
           (recur
            (inc index)
            (rest asm)
            (concat (repeat (m/ceil (or (weight-key (first asm)) 0)) index) assoc))))
       0 asm (range (.size asm))))))

(defn weighted-pick
  "Return a random element in an asm individual weighted by the
   associated bad-path"
  ([asm]
     (nth asm (weighted-place asm)))
  ([asm weight-key]
     (nth asm (weighted-place asm weight-key))))

(defn read-asm
  "Read in an assembly file as list and parse cmd lines."
  [path]
  {:representation
   (map (fn [el]
          {:line (if-let [part (re-matches #"\t(.*)\t(.*)" el)]
                   (rest part)
                   el)})
        (f/read-lines path))
   :compile nil :fitness nil :trials nil :operations nil})

(defn write-asm
  [f asm]
  (f/write-lines
   (f/file-str f)
   (map #(let [line (:line %)]
           (if (not (string? line))
             (apply str "\t" (interpose "\t" line)) line))
        (:representation asm))))

(def target-fitness 10)
(def max-generations 10)
(def population-size 40)
(def tournament-size 3)
(def use-tournament false)
(def max-section-size 1)
(def crossover-rate 0.1)
(def sticky-crossover-rate 0.75)
(def fitness-cache-path
     (.getPath (f/file-str "~/research/code/data/fitness-cache.clj")))
(def good-mult 1)
(def bad-mult 5)
(def compiler "gcc")
(def compiler-flags nil) ;; (list "-pthread")
(def test-dir nil)  ;; "~/research/code/gcd/"
(def test-timeout 2000)
(def test-good nil) ;; "./test-good.sh"
(def test-bad nil)  ;; "./test-bad.sh"
(def java-class-nest nil)

(defn read-path
  "Read the given path giving the raw sum of the value for each
  instruction."  [path-to-path]
  (reduce
   (fn [a f] (assoc a f (inc (get a f 0)))) {}
   (map (fn [arg] (Integer/parseInt arg))
        (f/read-lines path-to-path))))

(defn smooth-path
  "Smooth the given path by blurring with a 1-D Gaussian, then taking
  the log of all values -- with a min value of 1 for each
  instruction."  [path]
  (let [kernel {-3 0.006, -2 0.061, -1 0.242, 0 0.383, 1 0.242, 2 0.061, 3 0.006}]
    ;; log of the blurred weights
    (reduce
     (fn [accum el] (assoc accum (first el) (m/log (inc (second el))))) {}
     ;; 1D Gaussian Smoothing of weights
     (reduce
        (fn [accum el]
          (reduce
           (fn [a f]
             (let [place (+ (first el) (first f))]
               (assoc a place
                      (+ (get a place 0)
                         (* (second f) (second el))))))
           accum kernel)) {}
           path))))

(defn path-
  "Subtract one path from another." [left right]
  (reduce (fn [l r] (dissoc l (first r))) left right))

(defn apply-path
  "Apply the weights in a path to a GP individual"
  [asm key path]
  (assoc asm
    :representation
    (reduce #(let [place (first %2) weight (second %2)]
               (if (< place (.size %1))
                 (concat
                  (take place %1)
                  (list (assoc (nth %1 place) key weight))
                  (drop (inc place) %1))
                 %1)) (:representation asm) path)))

(defn section-length
  "Limit the size of sections of ASM used for GP operations."
  [single length]
  (if single
    (if (number? single) (min single length) 1)
    (inc (rand-int (min max-section-size length)))))

(defn swap-asm
  "Swap two lines or sections of the asm."
  ([asm] (swap-asm asm nil))
  ([asm single]
     (assoc asm
       :representation
       (let [asm (:representation asm)
             first (weighted-place asm)
             second (weighted-place asm)]
         (if (= first second)
           asm
           (let [left (min first second)
                 right (max first second)
                 left-length
                 (section-length single
                                 (.size (take (- right left) (drop left asm))))
                 right-length (section-length single (.size (drop right asm)))]
             (concat
              (take left asm)
              (take right-length (drop right asm))
              (take (- right (+ left left-length))
                    (drop (+ left left-length) asm))
              (take left-length (drop left asm))
              (drop (+ right right-length) asm)))))
       :operations (cons :swap (:operations asm)))))

(defn delete-asm
  "Delete a line or section from the asm.  Optional second argument
will force single line deletion rather than deleting an entire
section."
  ([asm] (delete-asm asm nil))
  ([asm single]
     (assoc asm
       :representation
       (let [asm (:representation asm)
             start (weighted-place asm)
             length (section-length single (.size (drop start asm)))]
         (concat (take start asm) (drop (+ start length) asm)))
       :operations (cons :delete (:operations asm)))))

(defn append-asm
  "Inject a line from the asm into a random location in the asm.
  Optional third argument will force single line injection rather than
  injecting an entire section."
  ([asm] (append-asm asm nil))
  ([asm single]
     (assoc asm
       :representation
       (let [asm (:representation asm)
             start (weighted-place asm :good-weight)
             length (section-length single (.size (drop start asm)))
             point (weighted-place asm)]
         (concat (take point asm) (take length (drop start asm))
                 (drop point asm)))
       :operations (cons :append (:operations asm)))))

(defn mutate-asm
  "Mutate the asm with either delete-asm, append-asm, or swap-asm.
  For now we're forcing all changes to operate by line rather than
  section." [asm]
  (let [choice (rand-int 3)]
    (cond
     (= choice 0) (delete-asm asm)
     (= choice 1) (append-asm asm)
     (= choice 2) (swap-asm asm))))

(defn crossover-asm
  "Takes two individuals and returns the result of performing single
  point crossover between then."  [mother father]
  {:representation
   (let [mother (:representation mother) father (:representation father)]
     (if (< (rand) sticky-crossover-rate)
       ;; homologous crossover -- does taking a mid-point first bias things?
       (let [mid (weighted-place mother)
             mother-l (take mid mother) mother-r (drop mid mother)
             father-l (take mid father) father-r (drop mid father)
             mid-l (when (not (empty? mother-l)) (weighted-place mother-l))
             mid-r (when (not (empty? mother-r)) (weighted-place mother-r))]
         (concat (if mid-l (take mid-l mother-l) '())
                 (if mid-l (drop mid-l father-l) '())
                 (if mid-r (take mid-r father-r) '())
                 (if mid-r (drop mid-r mother-r) '())))
       ;; traditional 2-point crossover
       (let [mid-m (weighted-place mother)
             mid-f (weighted-place father)
             mother-l (take mid-m mother) mother-r (drop mid-m mother)
             father-l (take mid-f father) father-r (drop mid-f father)
             mid-ml (when (not (empty? mother-l)) (weighted-place mother-l))
             mid-mr (when (not (empty? mother-r)) (weighted-place mother-r))
             mid-fl (when (not (empty? father-l)) (weighted-place father-l))
             mid-fr (when (not (empty? father-r)) (weighted-place father-r))]
         (concat (if mid-ml (take mid-ml mother-l) '())
                 (if mid-fl (drop mid-fl father-l) '())
                 (if mid-fr (take mid-fr father-r) '())
                 (if mid-mr (drop mid-mr mother-r) '())))))
   :operations (list :crossover
                     (list (:operations mother)
                           (:operations father)))
   :compile nil :fitness nil :trials (max (get :trials mother 0)
                                          (get :trials father 0))})

(defn compile-asm
  "Compile the asm, set it's :compile field to the path to the
  compiled binary if successful or to nil if unsuccessful."  [asm]
  (let [asm-source (.getPath (File/createTempFile "variant" ".S"))
        asm-bin (.getPath (File/createTempFile "variant" "bin"))]
    (write-asm asm-source asm)
    (assoc asm
      :compile
      (when (= 0 (:exit
                  (apply
                   s/sh
                   (concat
                    (apply list compiler compiler-flags)
                    (list "-o" asm-bin asm-source :return-map true)))))
        (s/sh "chmod" "+x" asm-bin)
        asm-bin))))

(def fitness-cache (ref {}))

(def fitness-count (ref 0))

(defn evaluate-asm
  "Take an individual, evaluate it and pack it's score into
  it's :fitness field."  [asm]
  ;; increment our global fitness counter
  (dosync (alter fitness-count inc))
  (assoc
      ;; evaluate the fitness of the individual
      (if (@fitness-cache (.hashCode (:representation asm)))
        (assoc asm ;; cache hit
          :fitness (@fitness-cache (.hashCode (:representation asm)))
          :compile true)
        (let [asm (compile-asm asm) ;; cache miss
              test-good (.getPath (f/file-str test-dir test-good))
              test-bad (.getPath (f/file-str test-dir test-bad))
              bin (:compile asm)
              run-test (fn [test mult]
                         (* mult
                            (try
                             (let [out-file (.getPath (File/createTempFile "variant" ".out"))]
                               (with-timeout test-timeout (s/sh test bin out-file))
                               (.size (f/read-lines out-file)))
                             (catch java.util.concurrent.TimeoutException e 0))))]
          (assoc asm
            :fitness ((dosync (alter fitness-cache assoc (.hashCode
                                                          (:representation asm))
                                     (if bin ;; new fitness
                                       (+ (run-test test-good good-mult)
                                          (run-test test-bad bad-mult))
                                       0)))
                      (.hashCode (:representation asm))))))
    :trials @fitness-count))

(defn populate
  "Return a population starting with a baseline individual.
  Pass :group true as optional arguments to populate from a group of
  multiple baseline individuals."
  [asm & opts]
  ;; this doesn't work as list? will return true no matter what, we
  ;; must use an optional keyword argument...
  (let [asm (if (get (apply hash-map opts) :group false)
              asm (list asm))]
    ;; calculate their fitness
    (pmap #(evaluate-asm %)
          ;; include the originals
          (concat asm
                  ;; create random mutants
                  (take (- population-size (.size asm))
                        (repeatedly #(mutate-asm (pick asm))))))))

(defn tournament
  "Select an individual from the population via tournament selection."
  [population n]
  (take n
        (repeatedly
         (fn []
           (last
            (sort-by :fitness
                     (take tournament-size
                           (repeatedly #(pick population)))))))))

(defn stochastic-universal-sample
  "Stochastic universal sampling"
  [population n]
  (let [total-fit (reduce #(+ %1 (:fitness %2)) 0 population)
        step-size (/ total-fit n)]
    (loop [pop (reverse (sort-by :fitness (shuffle population)))
           accum 0 marker 0
           result '()]
      (if (> n (.size result))
        (if (> marker (+ accum (:fitness (first pop))))
          (recur (rest pop) (+ accum (:fitness (first pop))) marker result)
          (recur pop accum (+ marker step-size) (cons (first pop) result)))
        result))))

(defn select-asm [population n]
  (if use-tournament
    (tournament population n)
    (stochastic-universal-sample population n)))

(defn evolve
  "Build a population from a baseline individual and evolve until a
solution is found or the maximum number of generations is reached.
Return the best individual present when evolution terminates."
  [asm]
  (loop [population (populate asm)
         generation 0]
    (let [best (last (sort-by :fitness population))
          mean (/ (float (reduce + 0 (map :fitness population))) (.size population))]
      ;; write out the best so far
      (message "generation %d mean-score %S best{:fitness %S, :trials %d}"
               generation mean (:fitness best) (:trials best))
      (write-obj (format "variant.gen.%d.best.%S.clj" generation (:fitness best))
                 best)
      (if (>= (:fitness best) target-fitness)
        (do ;; write out the winner to a file and return
          (message "success after %d generations and %d fitness evaluations"
                   generation @fitness-count)
          (write-obj "best.clj" best) best)
        (if (>= generation max-generations)
          (do ;; print out failure message and return the best we found
            (message "failed after %d generations and %d fitness evaluations"
                     generation @fitness-count) best)
          (recur
           (select-asm
            (concat
             (dorun
              (pmap #(evaluate-asm %)
                    (concat
                     (take (Math/round (* crossover-rate population-size))
                           (repeatedly
                            (fn [] (apply crossover-asm (select-asm population 2)))))
                     (pmap #(mutate-asm %)
                          (select-asm population
                                      (Math/round (* (- 1 crossover-rate) population-size)))))))
             population)
            population-size)
           (+ generation 1)))))))
