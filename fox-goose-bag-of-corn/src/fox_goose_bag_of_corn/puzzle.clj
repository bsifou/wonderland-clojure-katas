(ns fox-goose-bag-of-corn.puzzle
  (:require [clojure.set :as set]))

;; start from the first move & form there generate a tree, where each node of tree 
;; is the next move (ie. the state of world), we keep recursing and buillding our world, 
;; till  we reach the leaves (that is move similar to final-move)


;; state of world: is [moves] (vector of moves)
;; a move is [[lfe side stuff] [boat stuff] [right stuff]]

(def start-world [[#{:fox :goose :corn :you} #{:boat} #{}]])

(def final-move [#{} #{:boat} #{:fox :goose :corn :you}])

;; we generate possible moves depending on where we are (where :you reside)


(defmulti possible-next-moves (fn [[left boat right]]
                                (condp some #{:you}
                                  left :left
                                  boat :boat
                                  right :right)))

;; what animal we pick
;; TODO: for now I will not allow to not pick any animal 
(defmethod possible-next-moves :left [[left boat right]]
  "Return all possible moves from left"
  (conj
   (for [animal left]
     [(disj left animal :you) (conj boat :you animal) right])
    [(disj left :you) (conj boat :you) right]))


;; I will also include the posiblity of when you bring an animal
;; (when there is conflict)  with, not just yourself 
(defmethod possible-next-moves :right [[left boat right]]
  "Return all possible moves from right"
  (conj (for [animal (disj right :you)]
          [left (conj boat :you animal) (disj right :you animal)])
        [left (conj boat :you) (disj right :you)])) ;;is that necessary? 

;; put the animal either on left or right 

(defmethod possible-next-moves :boat [[left boat right]]
  "Return all possible moves from boat. I can go right or left."
  (let [[animal] (remove #{:boat :you} boat)]
      [[(if animal (conj left :you animal) (conj left :you)) #{:boat} right]
       [left #{:boat} (if animal (conj right :you animal) (conj right :you))]]))



(defn conflicted-move? [left _ right]
  ((some-fn (partial set/subset?  #{:goose :corn})
            (partial set/subset? #{:fox :goose})) left right))

(defn eatable?
  "Check the position for an eatable condition (goose eats corn, fox eats goose)
  1. You can't have a goose with a fox or a goose with corn unless you are with them
  2. You don't have to worry about the boat because you are the only one with them then"
  [pos]
  (let [[left boat right] pos]
    (cond
                                        ;(or (not-allowed left) (not-allowed right))
      (not (left :you)) (or (and (every? left [:goose :fox]))
                            (and (every? left [:goose :corn])))
      (not (right :you)) (or (and (every? right [:goose :fox]))
                             (and (every? right [:goose :corn])))
      :else false)))

;(remove fox-goose-bag-of-corn.puzzle-2/eatable? test-moves) 

(defn visited? [prev-moves move]
  (some (partial = move) prev-moves))



(defn next-moves [moves]
  (->> (last moves)
       (possible-next-moves)
      (remove eatable?)
      (remove (partial visited? moves))))

(defn river-crossing-plan []
  (first (all-moves start-world)))


(defn all-moves 
  "Searching the solutions space by looking for the last of path to be the goal.
  Use next-moves to get return steps to try on the path"
  [curr-moves]
;  (prn curr-moves)
  (let [pos (last curr-moves)]
    (cond
      (= final-move pos) [curr-moves]
      :else  (let [next-moves (next-moves curr-moves)]
               (if (seq? next-moves)
                 (mapcat #(all-moves (conj curr-moves %)) next-moves))))))


;(clojure.pprint/pprint (river-crossing-plan))


















