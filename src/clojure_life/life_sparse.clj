;; Copyright 2011, Alex Miller, Apache 2 license
(ns clojure-life.life-sparse
  (:import [clojure.lang IPersistentMap]))

;;;; Data structure manipulationap

(defprotocol World
  (rows [world] "Number of rows in world")
  (cols [world] "Number of columns in world")
  (alive? [world row col] "Is the cell at [row col] alive?")
  (mark-alive [world row col] "Mark the cell at [row col] alive"))

;; The world is represented by a map of row index to a set of column indices.
;; Both rows and columns wrap around to the other side.

(extend-protocol World
  IPersistentMap
  (rows [world] (:rows world))
  (cols [world] (:cols world))
  (alive? [world r c]
          (boolean (some #{c} (world r))))
  (mark-alive [world row col]
              (merge-with into {row #{col}} world)))

(defn init-world
  "Create a new world. rc-pairs is a seq of [row col] of cells to
   initially mark alive."
  [rows cols & rc-pairs]
  (reduce #(mark-alive %1 (first %2) (second %2))
          {:rows rows :cols cols}
          rc-pairs))

;; data structure functions below this point don't need to understand
;; internal structure of the world data

(defn render [world]
  (println "render" (class world))
  (doseq [row (range (rows world))]
    (doseq [col (range (cols world))]
      (print (if (alive? world row col) \# \.)))
    (println)))

(defn world-seq [world]
  (for [row (range (rows world))
        col (range (cols world))]
    [row col (alive? world row col)]))

(defn neighbors
  "Get neighbor count at row-column position in world."
  [world r c]
  {:post [(<= 0 % 8)]}
  (count
   (filter true?
    (for [row (range (dec r) (+ r 2))
          col (range (dec c) (+ c 2))
          :when (not (and (= row r) (= col c)))]
      (alive? world row col)))))

(defn map-world
  "Map a rule-fn over every cell in the world.  The rule-fn should be
   of the form:
     (fn [row col alive? neighbor-count]) -> boolean"
  [rule-fn world]
  (->> (world-seq world)
       (map (fn [[row col alive]]
              (when (rule-fn row col alive (neighbors world row col))
                [row col])))
       (remove nil?)
       (apply init-world (rows world) (cols world))))

;;;; Apply the life rules using the "world" data structure

(defn life-rule
  "Determine whether a cell should live in the next tick based the current value and
   the neighbor count."
  [row col alive neighbor-count]
  (if alive
    (<= 2 neighbor-count 3)
    (= neighbor-count 3)))

(defn update-world
  "Take world and compute new world based on the life function."
  [world]
  (map-world life-rule world))

;;;; Main entry

(defn life
  "Starting with an initial world, compute specified iterations."
  [init-world iterations]
  (loop [remaining-iterations iterations
         world init-world]
    (if (= 0 remaining-iterations)
      (println "Simulation complete.")
      (do
        (println)
        (println "Iteration" (- iterations remaining-iterations) ":")
        (render world)
        (recur (dec remaining-iterations) (update-world world))))))

;;;; Test with glider

(defn test-glider
  "Return initial world with a glider prepped."
  []
  (let [iters 10
        glider-world (init-world 10 10
                                 [1 3]
                                 [2 1]
                                 [2 3]
                                 [3 2]
                                 [3 3])]
    (life glider-world iters)))
