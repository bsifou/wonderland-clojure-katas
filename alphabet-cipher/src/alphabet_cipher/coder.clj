(ns alphabet-cipher.coder)

(def a-z (mapv char (range (int \a) (inc (int \z)))))


(defn rotate [n xs]
  (take (count xs) (drop n (cycle xs))))

(defn to-lower-case [c]
  (let [offset 32
        ci (int c)
        ai (int \a)]
    (if (< ci ai)
      (char (+ ci 32))
      c)))

(defn to-lowre-case [c]
  (Character/toLowerCase c))

(defn offset [c]
  (- (int (to-lower-case c)) (int \a)))


(defn encode-c [row col]
  (nth (rotate (offset row) a-z) (offset col)))

(defn decode-c [row col]
  (apply nth a-z (keep-indexed #(when (= %2 col) %1) (rotate (offset row) a-z))))

;(keep-indexed #(when (= %2 \c) %1)  [\a \b \c])


(defn encode [keyword message]
  "encodeme"
  (apply str (map encode-c (cycle keyword) message)))

(defn decode [keyword message]
  (apply str (map decode-c (cycle keyword) message)))


;; only check for the first 2 repeating words 

(defn decipher [cipher message]
  "decypherme"
  (apply str
         (reduce (fn [r [a b]]
                 (if 
                     (and (= (first r) a)
                          (= (fnext r) b))
                     (reduced r)
                     (conj r a)))
               []
               (partition 2 1 (map #(nth a-z (mod (- (int %1) (int %2)) 26)) 
                                   cipher message)))))



(map #(nth a-z (mod (- (int %1) (int %2)) 26)) 
                                   cipher message)


(def words (apply str (take 30 (cycle "vigilance"))))



(defn shortest [s]
  (loop [i 1]
    (if (= (subs s 0 i) (subs s i (* 2 i)))
      (subs s 0 i)
      (recur (inc i)))))
