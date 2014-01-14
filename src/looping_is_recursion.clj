(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc exp]
                 (if (zero? exp)
                   acc
                   (recur (* base acc) (dec exp))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc a-seq]
                 (if (empty? a-seq)
                   acc
                   (recur (first a-seq) (rest a-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [acc s1 s2]
                 (cond
                   (and (empty? s1) (empty? s2)) acc
                   (or (empty? s1) (empty? s2)) false
                   :else (recur (= (first s1) (first s2)) (rest s1) (rest s2))))]
    (helper true seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [index 0
         sequence a-seq]
    (cond
      (empty? sequence) nil
      (pred (first sequence)) index
      :else (recur (inc index) (rest sequence)))))

(defn avg
  "What should be the average of an empty sequence?
  Here, it will blow up in your face 8p"
  [a-seq]
  (loop [sum 0
         quantity 0
         sequence a-seq]
    (if (empty? sequence)
      (/ sum quantity)
      (recur (+ sum (first sequence)) (inc quantity) (rest sequence)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set element]
                 (if (contains? a-set element)
                   (disj a-set element)
                   (conj a-set element)))]
    (loop [acc #{}
           sequence a-seq]
      (if (empty? sequence)
        acc
        (recur (toggle acc (first sequence)) (rest sequence))))))

(defn fast-fibo [n]
  (loop [fib 0
         fib-before 1 ;This feels kind of like cheating
         number n]
    (if (zero? number)
      fib
      (recur (+ fib fib-before) fib (dec number)))))

(defn cut-at-repetition [a-seq]
  (loop [elements-so-far #{}
         seq-before-cut []
         sequence a-seq]
    (if (or
          (empty? sequence)
          (contains? elements-so-far (first sequence)))
      seq-before-cut
      (recur (conj elements-so-far (first sequence)) (conj seq-before-cut (first sequence)) (rest sequence)))))

