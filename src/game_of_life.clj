(ns game-of-life)

(defn cell-tick [{:keys [alive? neighbours]}]
  {:pre [(<= 0 neighbours 8)]}
  (cond
    (= neighbours 2) alive?
    (= neighbours 3) true
    :else            false))

(defn explode [coords]
  (for [[x y]   coords
        delta-x [-1 0 +1]
        delta-y [-1 0 +1]
        :let [center? (= delta-x delta-y 0)]]
    {[(+ x delta-x) (+ y delta-y)]
     {:alive?     center?
      :neighbours (if center? 0 1)}}))

(defn cell-merge [{a1 :alive? n1 :neighbours}
                  {a2 :alive? n2 :neighbours}]
  {:alive? (or a1 a2) :neighbours (+ n1 n2)})

(defn world-tick [coords]
  (->> (explode coords)
       (reduce (partial merge-with cell-merge) {})
       (filter #(cell-tick (second %)))
       (into {})
       keys
       (into #{})))
