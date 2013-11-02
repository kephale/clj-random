(ns clj-random.core
  (:import [org.uncommons.maths.random 
            JavaRNG MersenneTwisterRNG CellularAutomatonRNG CMWC4096RNG AESCounterRNG XORShiftRNG
            DefaultSeedGenerator]))

(def #^:dynamic *seed-length* 
  {:java 8
   :cellularautomaton 4
   :mersennetwister 16
   :aescounter 16
   :xorshift 20
   :cmwc4096 8})

(def #^:dynamic *seed-generator*
  (DefaultSeedGenerator/getInstance))

(defn generate-java-seed
  "Generate a seed for the default Java RNG."
  []
  (.generateSeed *seed-generator* (:java *seed-length*)))

(defn make-java-rng
  "Make the default Java RNG."
  ([]
    (make-java-rng (generate-java-seed)))
  ([seed]
    (JavaRNG. seed))) 
    
(def #^:dynamic *RNG*
  (make-java-rng))

(defn generate-mersenne-seed
  "Generate a seed for the Mersenne-Twister RNG."
  []
  (.generateSeed *seed-generator* (:mersennetwister *seed-length*)))

(defn make-mersennetwister-rng
  "Make a Mersenne-Twister RNG."
  ([]
    (make-mersennetwister-rng (generate-mersenne-seed)))
  ([seed]
    (MersenneTwisterRNG. seed)))

(defn generate-cellularautomaton-seed
  "Generate a seed for the cellular automaton RNG."
  []
  (.generateSeed *seed-generator* (:cellularautomaton *seed-length*)))

(defn make-cellularautomaton-rng
  "Make a Cellular Automaton RNG."
  ([]
    (make-cellularautomaton-rng (generate-cellularautomaton-seed)))
  ([seed]
    (CellularAutomatonRNG. seed)))

;; Requires 16KB of seed data
#_(defn make-cmwc4096-rng
  "Make a Complementary Multiply With Carry (CMWC) RNG."
  ([]
    (make-cmwc4096-rng (.generateSeed *seed-generator* (:cmwc4096 *seed-length*))))
  ([seed]
    (CMWC4096RNG. seed)))

(defn generate-aescounter-seed
  "Generate a seed for the AES counter RNG."
  []
  (.generateSeed *seed-generator* (:aescounter *seed-length*)))

(defn make-aescounter-rng
  "Make an AES block cipher counter into a RNG. Nonlinear"
  ([]
    (make-aescounter-rng (generate-aescounter-seed)))
  ([seed]
    (AESCounterRNG. seed)))

(defn generate-xorshift-seed
  "Generate a seed for the XOR shift RNG."
  []
  (.generateSeed *seed-generator* (:xorshift *seed-length*)))

(defn make-xorshift-rng
  "Make a XOR shift RNG (fast)."
  ([]
    (make-xorshift-rng (generate-xorshift-seed)))
  ([seed]
    (XORShiftRNG. seed)))

(defn seed-to-string
  "Convert a byte-array seed into a string."
  [seed]
  (reduce #(str %1 " " %2)
          (map str (seq seed))))      

(defn lrand
  "A local random double in [0,n], where n is 1 if no arguments are specified."
  ([]
    (lrand 1.0))
  ([n]
    (* n (.nextDouble *RNG*)))
  ([min max]
    (let [w (- max min)]
      (+ (* w (.nextDouble *RNG*)) min))))

(defn lrand-int
  "A local random int (actually a long) in [0,n]."
  [n]
  (.nextInt *RNG* n))

(defn lrand-long
  "A local random long."
  []
  (.nextLong *RNG*))

(defn lrand-gaussian
  "A local random gaussian."
  []
  (.nextGaussian *RNG*))

(defn lrand-nth
  "Return a random element of a sequence."
  [coll]
  (nth coll (lrand-int (count coll))))

(defn lshuffle
  "Return a random permutation of coll (Adapted from clojure.core)"
  {:static true}
  [^java.util.Collection coll]
  (let [al (java.util.ArrayList. coll)]
    (java.util.Collections/shuffle al *RNG*)
    (clojure.lang.RT/vector (.toArray al))))

(defn lrand-bytes
  "Return a random byte array."
  [n]
  (let [b (byte-array n)]
    (.nextBytes *RNG* b)
    b))

(defmacro with-rng
  "Use a specific RNG with all lrand calls within the body."
  [my-rng & body]
  `(binding [*RNG* ~my-rng]
     ~@body))

