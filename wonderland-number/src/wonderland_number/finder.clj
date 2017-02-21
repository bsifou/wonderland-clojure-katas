(ns wonderland-number.finder)

(def start 100000)

(def end 1000000)

(def test-nums [2 3 4 5 6])

(defn hold-test [n t]
  (= (set (str n)) (set (str (* n t)))))


(defn wonderland-number []
  (first (drop-while
        #(not-every? (partial hold-test %) test-nums)
        (range start end))))









