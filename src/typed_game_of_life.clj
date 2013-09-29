(ns typed-game-of-life
  (:require [clojure.core.typed :refer :all]))

(def-alias Coords '[Int Int])
(def-alias CellDetails '{:alive? Boolean :neighbours Int})
(def-alias Cell (Map Coords CellDetails))

(ann cell-tick [CellDetails -> Boolean])
(defn cell-tick [{:keys [alive? neighbours]}]
  (cond (= neighbours 2) alive?
        (= neighbours 3) true
        :else            false))

(ann explode [(Set Coords) -> (Seq Cell)])
(defn explode [coords]
  (for> :- Cell
        [[x y]   :- Coords coords
         delta-x :- Int [-1 0 +1]
         delta-y :- Int [-1 0 +1]
         :let [center? (= delta-x delta-y 0)]]
    {[(+ x delta-x) (+ y delta-y)]
     {:alive? center? :neighbours (if center? 0 1)}}))

(ann cell-merge [CellDetails CellDetails -> CellDetails])
(defn cell-merge [{a1 :alive? n1 :neighbours}
                  {a2 :alive? n2 :neighbours}]
  {:alive? (or a1 a2) :neighbours (+ n1 n2)})

(ann merge-cells [Cell Cell -> Cell])
(defn merge-cells [accumulator new-cell]
  (merge-with cell-merge accumulator new-cell))

(ann tick-cell-pair ['[Coords CellDetails] -> Boolean])
(defn tick-cell-pair [[coords cell-details]]
  (cell-tick cell-details))

(ann world-tick [(Set Coords) -> (Set Coords)])
(defn world-tick [coords]
  (->> (explode coords)
       (reduce merge-cells {})
       (filter tick-cell-pair)
       (into {})
       keys
       (into #{})))

; 7 whitespace
; 37 loc
