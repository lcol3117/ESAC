(defn density-transform
  [data r]
  (map (fn [x] (density data r x)) data))
(defn density
  [data r point]
  (do
    (def dists
      (map (fn [x] (dist point x)) data))
    (def dists-within
      (filter (fn [x] (> r x)) dists))
    (* (/ r) avg dists-within)
  ))
(defn avg
  [given-list]
  (* (/ (count given-list)) (reduce + given-list)))