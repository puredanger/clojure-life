(ns clojure-life.life-seq)

;;;; Data structure manipulation

(defn rows [world] (count world))
(defn cols [world] (count (first world)))

(defn alive? [world r c]
  (boolean (get-in world [(mod r (rows world)) (mod c (cols world))])))

(defn world-seq [world]
  (for [row (range (rows world))
        col (range (cols world))]
    [row col (alive? world row col)]))

(defn render [world]
  (doseq [line world]
    (doseq [cell line]
      (print (if cell \# \.)))
    (println)))

(defn mark-alive
  [world [row col]]
  (update-in world [row col] (constantly true)))

(defn init-world
  "Create a new instance of the world that implements the World protocol."
  [rows cols & rc-pairs]
  (reduce mark-alive
          (vec (repeat rows (vec (repeat cols false))))
          rc-pairs))

(defn neighbors
  "Get neighbor count at row column position in world."
  [world r c]
  {:post [(and (<= 0 % 8))]}
  (count
   (filter true?
    (for [row (range (dec r) (+ r 2))
          col (range (dec c) (+ c 2))
          :when (not (and (= row r) (= col c)))]
      (alive? world row col)))))

(defn apply-rule
  [world rule-fn]
  (let [rows (count world)
        cols (count (first world))]
    (apply init-world rows cols
           (remove nil?
                   (map (fn [[row col alive]]
                          (when (rule-fn row col alive (neighbors world row col))
                            [row col]))
                        (world-seq world))))))

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
  (apply-rule world life-rule))

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
