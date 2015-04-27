(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? exp) 1
                   (if (== 1 n) acc
                     (recur (* acc base) (dec n)))))]
    (helper base exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc bs]
                 (if (empty? bs) acc
                 (recur (first bs) (rest bs))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (cond
   (empty? seq1) (empty? seq2)
   (empty? seq2) (empty? seq1)
   (not= (first seq1) (first seq2)) false
   :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [acc a-seq n 0]
    (cond
     (empty? acc) nil
     (pred (first acc)) n
     :else (recur (rest acc) (inc n)))))

(defn avg [a-seq]
  (loop [acc 0 cnt 0 s a-seq]
    (if (empty? s) (/ acc cnt)
     (recur (+ acc (first s)) (inc cnt) (rest s)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [acc #{} s a-seq]
    (if (empty? s) acc
     (recur (toggle acc (first s)) (rest s)))))

(defn fast-fibo [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else (loop [n-2 0 n-1 1 i n]
                (if(= i 0) n-2
                  (recur n-1 (+ n-1 n-2) (dec i))))))

(defn cut-at-repetition [a-seq]
  (loop [seen #{} left [] s a-seq]
    (cond
     (empty? s) left
     (contains? seen (first s)) left
     :else (recur (conj seen (first s)) (conj left (first s)) (rest s)))))
