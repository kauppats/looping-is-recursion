(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc k]
                  (if (zero? k)
                    acc
                    (recur (* acc base) (dec k))
                  )
               )
       ]
    (helper 1 exp)
  )
)

(defn last-element [a-seq]
  (let [helper (fn [n]
                 (cond
                   (empty? n) nil
                   (== (count n) 1) (first n)
                   :else (recur (rest n))
                 )
               )
       ]
    (helper a-seq)
  )
)

(defn seq= [seq1 seq2]
  (let [helper (fn [s1 s2]
                 (cond
                   (and (empty? s1) (empty? s2)) true
                   (or (empty? s1) (empty? s2)) false
                   (not= (first s1) (first s2)) false
                   :else (recur (rest s1) (rest s2))
                 )
               )
       ]
    (helper seq1 seq2)
  )
)

(defn find-first-index [pred a-seq]
  (loop [index 0]
    (cond
      (>= index (count a-seq)) nil
      (pred (get a-seq index)) index
      :else (recur (inc index))
    )
  )
)

(defn avg [a-seq]
  (loop [_sum 0
         _count 0
        ]
    (if (== _count (count a-seq))
      (/ _sum _count)
      (recur (+ _sum (get a-seq _count)) (inc _count))
    )
  )
)

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)
  )
)

(defn parity [a-seq]
  (loop [_index 0
         _set (set a-seq)
        ]
    (if (== _index (count a-seq))
      (clojure.set/difference (set a-seq) _set)
      (let [_item (get a-seq _index)]
        (recur (inc _index) (toggle _set _item))
      )
    )
  )
)

(defn fast-fibo [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    :else (loop [f 1
                 f-1 0
                 idx (- n 1)
                ]
            (if (== idx 0)
              f
              (recur (+ f f-1) f (dec idx))
            )
          )
  )
)

(defn cut-at-repetition [a-seq]
  (loop [_inits []
         _rest a-seq
        ]
    (let [_item (first _rest)]
      (cond
        (empty? _rest) _inits
        (contains? (set _inits) _item) _inits
        :else (recur (conj _inits _item) (rest _rest))
      )
    )
  )
)

