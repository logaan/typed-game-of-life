(ns typed-game-of-life
  (require [clojure.core.typed :refer :all])
  (import [clojure.lang
           IPersistentMap
           IPersistentSet]))

(ann cell-tick [CellDetails -> Boolean])
(defn cell-tick [{:keys [alive? neighbours]}]
  {:pre [(<= 0 neighbours 8)]}
  (cond
    (= neighbours 2) alive?
    (= neighbours 3) true
    :else            false))

(def-alias Coordinate
  '[AnyInteger AnyInteger])

(def-alias World
  (IPersistentSet Coordinate))

(def-alias CellDetails
  (HMap :mandatory {:alive? Boolean
                    :neighbours AnyInteger}
        :complete? true))

(def-alias Cell
  (IPersistentMap Coordinate CellDetails))

(ann explode [World -> (Seq Cell)])
(defn explode [coords]
  (for> :- Cell
        [[x y]   :- '[AnyInteger AnyInteger] coords
         delta-x :- AnyInteger [-1 0 +1]
         delta-y :- AnyInteger [-1 0 +1]
         :let [center? (= delta-x delta-y 0)]]
        {[(+ x delta-x) (+ y delta-y)]
         {:alive?     center?
          :neighbours (if center? 0 1)}}))

(ann cell-merge [CellDetails CellDetails -> CellDetails])
(defn cell-merge [{a1 :alive? n1 :neighbours}
                  {a2 :alive? n2 :neighbours}]
  {:alive? (or a1 a2)
   :neighbours (+ n1 n2)})

(ann world-tick [World -> World])
(defn world-tick [coords]
  (let [merge-cells (ann-form #(merge-with cell-merge %1 %2) (Fn [Cell Cell -> Cell]))
        cells (reduce merge-cells {} (explode coords))
        foo   (ann-form #(cell-tick (second %)) (Fn ['[Coordinate CellDetails] -> Boolean]))]
    (into #{} (keys (into {} (filter foo cells))))))

(assert (= #{[3 2] [2 2] [1 2]}
           (world-tick [[2 1] [2 2] [2 3]])))


