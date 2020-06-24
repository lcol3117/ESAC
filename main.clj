(defn density-transform
  [data r]
  (map (fn [x] (density data r x)) data))
(defn density
  [data r point]
  (do
    (def dists (map (fn [x] (dist point x))))
    (def within (filter (fn [x] (> r x)) dists))
  ))
