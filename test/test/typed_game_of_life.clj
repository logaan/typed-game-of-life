(ns test.typed-game-of-life
  (require [typed-game-of-life :refer :all]
           [clojure.test :refer :all]))

(deftest cell-tick-test
  (is (thrown? java.lang.AssertionError (cell-tick true -1)))

  (is (not (cell-tick true 1))) 
  (is (not (cell-tick false 1))) 

  (is (cell-tick true 2))
  (is (not (cell-tick false 2))) 

  (is (cell-tick true 3))
  (is (cell-tick false 3))

  (is (not (cell-tick true 4))) 
  (is (not (cell-tick false 4)))
  
  (is (thrown? java.lang.AssertionError (cell-tick true 9))))

(run-tests)

