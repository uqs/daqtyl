(ns ftraqtyl
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]))

(defn deg2rad [degrees]
  (* (/ degrees 180) pi))

(def trackball-r (+ (/ 34 2) 0.5))  ; 34 mm ball plus offset
(def trackball-outer-r (+ trackball-r 2))

 ; PMW3360/3389 board
(def sensor-sans-pins
  (->> (union
         ; hull the upper part for printing the overhangs at an angle
         (->> (hull
                (cube 28.5 21.5 3.5)
                (->> (cube 21.5 19.5 1)(translate [0 0 5]))
                ))
         (->> (cube 21.5 21.5 4)(translate [0 0 -2]))
         )
       ))
(def sensor
  (union sensor-sans-pins
         ; the angled header pins
         (->> (cube 21.5 12 5)(translate [0 -13 -4.5]))
         ))

(defn trackholder-cyl [l zdeg]
  (let [
        r trackball-r
        h 70
        outer-r (+ r 3)
        slant 0
        ]
    (->> (cylinder [(/ outer-r 1.5) outer-r] h) (with-fn 100)
         (translate [0 0 (/ h -2)])
         (multmatrix [[1 0 -0.0 0]
                      [0 1 slant 0]
                      [0 0 1 0]])
         (rotate (deg2rad zdeg) [0 0 1])
         ))
  )

(defn trackholder [l zdeg]
  (let [
        r trackball-r
        pimple (fn [r] (difference
                         (->> (sphere r) (with-fn 40))
                         ; snip off 4mm from the top of the pimple
                         (->> (sphere 4) (with-fn 40) (translate [0 0 (+ (- -4 r) 0.4)]))
                         ))
        ]
    (->>
      (difference
        (trackholder-cyl l 0)
        ; subtract a sphere with dents
        (->> (difference (with-fn 100 (sphere r))
                         (for [x [[0 2] [120 2] [240 2]]] (->>
                                                            (pimple (second x))
                                                            (translate [0 0 (+ r 1.0)])
                                                            (rotate (deg2rad 120) [1 0 0])
                                                            (rotate (deg2rad (+ (first x) 60)) [0 0 1])
                                                            )
                           )
                         ))
        ; subtract the hole through the bottom
        (->> (cylinder 9.5 5) (translate [0 0 (- 0 r)]))
        ; subtract the sensor board shape including a trajectory to insert/remove it
        (->> (union
               sensor
               (->> sensor (rotate (deg2rad 0) [1 0 0]) (translate [0 -5 0]))
               (->> sensor (rotate (deg2rad 0) [1 0 0]) (translate [0 -10 0]))
               (->> sensor (rotate (deg2rad 0) [1 0 0]) (translate [0 -15 0]))
               (->> sensor (rotate (deg2rad 0) [1 0 0]) (translate [0 -20 0]))
               (->> sensor (rotate (deg2rad 10) [1 0 0]) (translate [0 -20 -2]))
               (->> sensor (rotate (deg2rad 16) [1 0 0]) (translate [0 -15 -3]))
               (->> sensor-sans-pins (rotate (deg2rad 30) [1 0 0]) (translate [0 -18 -6]))
               (->> sensor-sans-pins (rotate (deg2rad 45) [1 0 0]) (translate [0 -21 -9]))
               )
             (translate [0 0 (- -4.75 r)]) ; sensor needs 2.40mm distance from surface
             )
        (->> (cube 100 100 100)(translate [0 0 (- -50 l)]))
        )
      (rotate (deg2rad zdeg) [0 0 1])
      )
    )
  )


(spit "things/trackball-test.scad"
      (write-scad (union
                    (->> sensor (translate [0 0 23])(color [0.3 0.3 0.3 1]))
                    (->> (color [1 0 0 1] (sphere (/ 34 2))) (translate [0 0 0.5]))
                    (difference
                      (trackholder 50 0)
                      ;(->> (cube 100 200 200)(translate [50 0 0]))
                      ))))
