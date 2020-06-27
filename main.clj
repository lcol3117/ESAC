(defn dist
  [a b]
  (do
    (def d-range (range (count a)))
    (def d-items (map (fn [x] (Math/pow 2 (- (get b x) (get a x)))) d-range))
    (def sum-d (reduce + d-items))
    (Math/sqrt sum-d)))
(defn max-dist
  [data]
  (+ 2 (* 2 (-
    (->> data
      (flatten)
      (apply max))
    (->> data
      (flatten)
      (apply min))))))
(defn avg
  [given-list]
  (if (empty? given-list) nil (* (/ (count given-list)) (reduce + given-list))))
(defn density
  [data r point]
  (do
    (def data-without-point
      (into [] (remove (fn [x] (= point x)) data)))
    (def dists
      (map (fn [x] (dist point x)) data-without-point))
    (def dists-within
      (filter (fn [x] (> r x)) dists))
    (* (/ r) (let [d (avg (into [] dists-within))] (if (nil? d) 0 d)))))
(defn num-within
  [data r point]
  (do
    (def data-without-point
      (into [] (remove (fn [x] (= point x)) data)))
    (def dists
      (map (fn [x] (dist point x)) data-without-point))
    (def dists-within
      (filter (fn [x] (> r x)) dists))
    (inc (count dists-within))))
(defn acquire-density-radius
  [data point]
  (do
    (def density-range (->> data
      (max-dist)
      (inc)
      (range 1)))
    (def densities
      (into [] (map (fn [x] (* x (num-within data x point))) density-range)))
    (def possibilities-range (->> densities
      (count)
      (dec)
      (range)
      (into [])))
    (def density-deltas
      (map (fn [x] (- (get densities (inc x)) (get densities x))) possibilities-range))
    (def max-density-delta (apply max density-deltas))
    (def max-delta-index (.indexOf density-deltas max-density-delta))
    (inc (/ max-delta-index 2))))
(defn select-exemplar ? nooo
  [data point]
  (do
    (def r (acquire-density-radius data point))
    (def points-within (filter (fn [x] (> r (dist point x))) data))
    (reduce (fn [a,x] (max a (density data r x))) points-within)))
