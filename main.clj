(defn density-transform
  [data]
  (map (fn [x] (density data x)) data))
