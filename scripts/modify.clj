#! /usr/bin/env clj
(load-file "/home/eschulte/research/genprog/asm/src/asm-gp.clj")
(in-ns 'asm-gp)
(require ['clojure.contrib.command-line :as 'cmd])
(cmd/with-command-line (rest *command-line-args*)
  "Prototype No-Specification Machine-code-level Bug-Fixer\n\tmodify [opts] baseline.s"
  [[gcc         "use X to compile C files" "gcc"]
   [ldflags     "use X as LDFLAGS when compiling" nil]
   [good        "use X as good-test command" "./test-good.sh"]
   [bad         "use X as bad-test command" "./test-bad.sh"]
   [bad-factor  "multiply 'bad' testcases by X for utility" 5]
   [good-factor "multiply 'good' testcases by X for utility" 1]
   [sanity-test "sanity fitness of baseline individual"]
   [max         "best fitness possible is X" 10]
   [fit-cache   "path to the fitness cache"]
   [good-path   "file specifying the good path" "good.path"]
   [bad-path    "file specifying the good path" "bad.path"]
   [path-sub?   "subtract the good path from the bad path"]
   [pop         "use population size of X" 40]
   [gen         "use X genetic algorithm generations" 10]
   [cross-rate  "percentage of population generated through crossover" 0.1]
   [sticky-cross-rate "percentage crossover to be done stickily" 0.75]
   [tour?       "use tournament selection for sampling"]
   [tour-size   "tournament size" 3]
   [java?       "operate on a Java .class file"]
   [class-nest  "class directory nesting for java .class files"]
   rest]

  ;; ;; over-define some functions for working with Java .class files
  ;; (when java?
  ;;   (load-file "/home/eschulte/research/genprog/asm/src/bytecode-gp.clj"))

  ;; define GP parameters
  (let [to_int (fn [in] (if (string? in)
                          (Integer/parseInt in)
                          in))]
    (def target-fitness (to_int max))
    (def max-generations (to_int gen))
    (def population-size (to_int pop))
    (def use-tournament tour?)
    (def tournament-size (to_int tour-size))
    (def crossover-rate (if (string? cross-rate)
                          (Float/parseFloat cross-rate) cross-rate))
    (def sticky-crossover-rate
         (if (string? sticky-cross-rate)
           (Float/parseFloat sticky-cross-rate) sticky-cross-rate))
    (def good-mult (to_int good-factor))
    (def bad-mult (to_int bad-factor))
    (def compiler gcc)
    (def compiler-flags ldflags)
    (def test-dir "./")
    (def test-timeout 2000)
    (def test-good good)
    (def test-bad bad)
    (def java-class-nest class-nest))

  ;; save configuration to file
  (write-obj "config.clj"
             (list
              "target-fitness" max
              "max-generations" gen
              "population-size" pop
              "use-tournament" tour?
              "crossover-rate" crossover-rate
              "sticky-crossover-rate" sticky-crossover-rate
              "good-mult" good-factor
              "bad-mult" bad-factor
              "good-path" good-path
              "bad-path" bad-path
              "path-sub" path-sub?
              "compiler" gcc
              "compiler-flags" ldflags
              "test-dir" "./"
              "test-timeout" 2000
              "test-good" good
              "test-bad" bad))

  ;; run evolution
  (doseq [baseline-path rest]
    (let [fitness-cache (ref (if fit-cache (read-obj fit-cache) {}))
          good-path (if good-path (read-path good-path) nil)
          bad-path (if bad-path (read-path bad-path) nil)
          bad-path (if (and bad-path good-path path-sub?)
                     (path- bad-path good-path)
                     bad-path)
          baseline (apply-path
                    (apply-path
                     (read-asm baseline-path)
                     :good-path (smooth-path good-path))
                    :bad-path (smooth-path bad-path))]
      ;; (when java?
      ;;   (def base-class (.parse (new org.apache.bcel.classfile.ClassParser baseline-path))))
      ;; sanity check
      (when sanity-test
        (assert (= (:fitness (evaluate-asm baseline)) sanity-test)))
      (evolve baseline)
      (if fit-cache (write-obj fit-cache @fitness-cache)))))