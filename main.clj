(defn density-transform
  [data r]
  (map (fn [x] (density data r x)) data))
(defn density
  [data r point]
  (->> (map (fn [x] (dist point x)))
    (filter (fn [x] (> r x)))
  ))
