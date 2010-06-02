
(in-ns 'asm-gp)
(import '(org.apache.bcel.classfile ClassParser)
        '(org.apache.bcel.generic ClassGen MethodGen InstructionList))

(def base-class nil)

(defn read-asm
  "Read in a .class file to a list of Byte-code instructions.  For now
  we'll just be working with the main function." [path]
  {:representation
   (let [class (new ClassGen (.parse (new ClassParser path)))]
     (map
      (fn [meth]
        (.getInstructionList
         (new MethodGen
              meth
              (.getClassName class)
              (.getConstantPool class)))) (.getMethods class)))
   :compile nil :fitness nil :trials nil :operations nil})

(defn write-asm
  "Write a list of Byte-code instructions to a file.  Return f if the
  write was successful, and nil otherwise." [f lst]
  (if (not base-class)
    (message "base class is uninitialized!"))
  (try
   (let [cls (new ClassGen base-class)]
     (map
      (fn [base lst]
        (let [mth (new MethodGen base
                       (.getClassName cls)
                       (.getConstantPool cls))]
          (.setPositions lst false)
          (.setInstructionList mth lst)
          (.setMaxStack mth)
          (.setMaxLocals mth)
          (.removeLineNumbers mth)
          (.replaceMethod cls base (.getMethod mth))))
      (.getMethods cls) (:representation lst))
     (.dump (.getJavaClass cls) f))
   f
   (catch Exception e nil)))

(defmacro gp-op-wrapper
  "Wrap a GP operation in a try/catch block which will return an empty
  InstructinoList if any errors are thrown while manipulating the
  individual."  [& body] `(try ~@body (catch Exception _# (InstructionList.))))

(defn instrs-place
  "Return a random location from a list of instruction lists."
  [instrs]
  (let [meth_num (rand-int (.size instrs))]
    (list meth_num (rand-int (.size (nth instrs meth_num))))))

(defn instrs-pick
  "Pick an instruction from a list of instruction lists."
  [instrs place]
  (nth (.getInstructionHandles (nth instrs (first place))) (second place)))

(defn swap-asm
  "Swap two instructions in this InstructionList.  Not Weighted."
  ([asm _] (swap-asm asm))
  ([asm]
     (assoc asm
       :representation
       (gp-op-wrapper
        (let [asm (map #(.copy %) (:representation asm))
              left (instrs-place asm)
              right (instrs-place asm)]
          (message "%S" left) (message "%S" right)
          asm))
       :operations (cons :swap (:operations asm)))))

(defn append-asm
  "Append an instruction somewhere in this InstructionList.  Not
  Weighted.  Return a copy of the original if the operations fail."
  ([asm _] (append-asm asm))
  ([asm]
     (assoc asm
       :representation
       (gp-op-wrapper
        (let [asm (.copy (:representation asm))
              handles (seq (.getInstructionHandles asm))]
          (.append asm
                   (pick handles)
                   (.getInstruction (pick handles)))
          asm))
       :operations (cons :append (:operations asm)))))

(defn delete-asm
  "Remove an instruction from list InstructionList.  Not Weighted"
  ([asm _] (delete-asm asm))
  ([asm]
     (assoc asm
       :representation
       (gp-op-wrapper
        (let [asm (.copy (:representation asm))
              handles (seq (.getInstructionHandles asm))]
          (.delete asm (pick handles))
          asm))
       :operations (cons :delete (:operations asm)))))

(defn compile-asm
  "Compile the asm and return a path to the resulting binary.  Return
  nil if the compilation (write) fails."  [asm]
  (let [asm-dir (str
                 (.getPath (File/createTempFile "variant" "")) "/"
                 (or java-class-nest ""))]
    (s/sh "rm" "-rf" asm-dir) (s/sh "mkdir" "-p" asm-dir)
    (assoc asm
      :compile
      (if (write-asm (str asm-dir "/" (.getClassName base-class) ".class") asm)
        asm-dir
        nil))))
