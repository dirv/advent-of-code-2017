(ns advent-of-code.three)

(enable-console-print!)

(def location 361527)
;(def location 30)
(defn abs [n] (max n (- n)))

(defn calculate-distance [size direction number]
  (let [half-distance (int (/ size 2))]
    (if (= direction :up)
      (+ 1 half-distance (abs (- half-distance (- location number))))
      (+ half-distance (abs (- half-distance (- location number)))))))

(defn move [size direction number]
  (cond
    (> (+ number size) location)
    (calculate-distance size direction number)
    (= direction :right)
    (move size :up (+ number size))
    (= direction :up)
    (move (inc size) :left (+ number size))
    (= direction :left)
    (move size :down (+ number size))
    (= direction :down)
    (move (inc size) :right (+ number size))))

(defn- calculate-value [[x y] grid]
  (apply + (for [dx [-1 0 1]
                 dy [-1 0 1]]
             (get-in grid [(+ x dx) (+ y dy)] 0))))

(defn- calculate-new-size [size direction]
  (case direction
    :up size
    :left (inc size)
    :down size
    :right (inc size)))

(defn- update-direction [direction size side-distance]
  (if (= size side-distance)
    (case direction
      :up :left
      :left :down
      :down :right
      :right :up)))

(defn- update-position [direction [x y]]
  (case direction
    :up [x (dec y)]
    :down [x (inc y)]
    :left [(dec x) y]
    :right [(inc x) y]))

(defn move-two [size direction side-distance position grid]
  (let [score (calculate-value position grid)
        new-grid (assoc-in grid position score)]
    (println score)
    (println new-grid)
    (if (> score location)
      score
      (if-let [new-direction (update-direction direction size side-distance)]
        (move-two (calculate-new-size size new-direction) new-direction 1 (update-position new-direction position) new-grid)
        (move-two size direction (inc side-distance) (update-position direction position) new-grid)))))

(defn play-move-to []
  (move-two 1 :right 1 [1 0] { 0 { 0 1}}))
