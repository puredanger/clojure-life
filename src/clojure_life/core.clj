(ns clojure-life.core)

(def DEAD 0)
(def ALIVE 1)

(defn alive? [cell] (= cell ALIVE))
(defn dead? [cell] (= cell DEAD))

(defn render
  "Render a world to the screen."
  [world]
  (doseq [line world]
    (doseq [cell line]
      (print (if (dead? cell) \. \#)))
    (println)))

(defn next-cell
  "Compute next cell value based on the current value and neighbor-count."
  [current neighbor-count]
  (if (or (and (alive? current) (and (>= neighbor-count 2) (<= neighbor-count 3)))
          (and (dead? current) (and (= neighbor-count 3))))
    ALIVE
    DEAD))

(defn tor
  "Calculate an in-world (torus) value for x.  x may be negative
   or out of bounds."
  [x n]
  (mod x n))

(defn get-cell
  "Get cell value at row column position in world."
  [r c world]
  (let [rows (count world)
        cols (count (first world))]
    (nth (nth world (tor r rows)) (tor c cols))))

(defn neighbors
  "Get neighbor count at row column position in world."
  [r c world]
  {:post [(and (<= 0 % 8))]}
  (apply +
         (for [row (range (dec r) (+ r 2))
               col (range (dec c) (+ c 2))
               :when (not (and (= row r) (= col c)))]
           (get-cell row col world))))

(defn update-cell
  "Take row column position and return new cell value."
  [r c world]
  (let [n (neighbors r c world)
        current (get-cell r c world)]
    (next-cell current n)))

(defn update-row
  "Take row number in world and return updated row."
  [r world]
  (let [row (nth world r)]
    (map #(update-cell r %1 world) (range (count row)))))

(defn update-world
  "Take world and compute new world based on the life function."
  [world]
  (let [rows (count world)
        cols (count (first world))]
    (map #(update-row %1 world) (range rows))))

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

(defn test-glider
  "Return initial world with a glider prepped."
  []
  (let [iters 20
        glider-world (concat
                      [[0 0 0 0 0 0 0 0 0 0]
                       [0 0 0 1 0 0 0 0 0 0]
                       [0 1 0 1 0 0 0 0 0 0]
                       [0 0 1 1 0 0 0 0 0 0]]
                      (repeat 6 (repeat 10 0)))]
    (life glider-world iters)))
