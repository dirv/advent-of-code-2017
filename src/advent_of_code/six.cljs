(ns advent-of-code.six)
(enable-console-print!)

(def start [0 2 7 0])
(def start [2 8 8 5 4 2 3 1 5 5 1 2 15 13 5 14])

(defn- next-pile [xs current-pile]
  (mod (inc current-pile) (count xs)))

(defn redistribute [xs number current-pile]
  (if (zero? number)
    xs
    (redistribute
      (update xs current-pile inc)
      (dec number)
      (next-pile xs current-pile))))

(defn- first-max-key [xs]
  (let [max-x (apply max xs)]
    (first (filter #(= max-x (get xs %)) (range 0 (count xs))))))

(defn reallocate-biggest [xs]
  (let [biggest-pile (first-max-key xs)]
    (println biggest-pile)
    (redistribute (assoc xs biggest-pile 0) (get xs biggest-pile) (next-pile xs biggest-pile))))

(defn run-one []
  (loop [xs start
         all-moves #{}]
    (let [new-xs (reallocate-biggest xs)]
      (if (contains? all-moves new-xs)
        (inc (count all-moves))
        (recur new-xs (conj all-moves new-xs))))))

(defn run-two []
  (loop [xs start
         all-moves {}]
    (let [new-xs (reallocate-biggest xs)]
      (if (contains? all-moves new-xs)
        (- (count all-moves) (get all-moves new-xs))
        (recur new-xs (assoc all-moves new-xs (count all-moves)))))))
