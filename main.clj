(defn dist
    [a b]
    (do
        (def d-range (range (count a)))
        (def d-items (map (fn [x] (Math/pow 2 (- (get b x) (get a x)))) d-range))
        (def sum-d (reduce + d-items))
        (Math/sqrt sum-d)))
(defn avg
  [given-list]
  (* (/ (count given-list)) (reduce + given-list)))
(defn density
  [data r point]
  (do
    (def dists
      (map (fn [x] (dist point x)) data))
    (def dists-within
      (filter (fn [x] (> r x)) dists))
    (* (/ r) (avg dists-within))))
(defn density-transform
  [data r]
  (map (fn [x] (density data r x)) data))
