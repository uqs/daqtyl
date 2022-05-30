(ns daqtyl
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]))

(defn deg2rad [degrees]
  (* (/ degrees 180) pi))

(def trackball-r (+ (/ 44 2) 0.5))  ; 34 mm ball plus offset

 ; PWM3360 board
(def sensor-sans-pins
  (->> (union
         ; hull the upper part for printing the overhangs at an angle
         (->> (hull
                (cube 28.5 21.5 3.5)
                (->> (cube 21.5 19.5 1)(translate [0 0 4.5])) ; (1-8)/2=3.5 + tolerance
                ))
         (->> (cube 21.5 21.5 4)(translate [0 0 -2]))
         )
       ))
(def sensor
  (union sensor-sans-pins
         ; the angled header pins
         (->> (cube 21.5 12 5)(translate [0 -13 -4.5]))
         ))

(defn trackholder [l zdeg]
  (let [
        r trackball-r
        h 70
        outer-r (+ r 2)
        pimple (fn [r] (difference
                         (->> (sphere r)
                              (with-fn 40))
                         ; snip off 4mm from the top of the pimple
                         (->> (sphere 4) (with-fn 40) (translate [0 0 (+ (- -4 r) 0.4)]))
                         ))
        ]
    (->>
      (difference
        (->> (cylinder outer-r h) (with-fn 100) (translate [0 0 (/ h -2)]))
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
        ;(->> (cylinder 10 8) (translate [0 0 (- 0 r)]))
        (->> (hull (->> (cylinder 9 9)(translate [2 0 0])) (->> (cylinder 9 9)(translate [-2 0 0])))(translate [0 0 (- 0 r)]))
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
             (translate [0 0 (- -4.25 r)]) ; half cube width plus thickness
             )
        ;(for [i [0 1 2 3 4 5]]
        ;  (let [
        ;        angle (cond (< i 3) 0 :else (deg2rad (* (- i 3) 5)))
        ;        y (* i -5)
        ;        z (* i -1)
        ;        ]
        ;    (->> sensor  (translate [0 y z])(rotate angle [1 0 0]))
        ;  ))
        ;(hull
        ;(->> sensor (rotate (deg2rad 5) [1 0 0]) (translate [0 -5 -1]))
        ;(->> sensor (rotate (deg2rad 10) [1 0 0]) (translate [0 -10 -3]))
        ;)
        (->> (cube 100 100 100)(translate [0 0 (- -50 l)]))
        )
      (rotate (deg2rad zdeg) [0 0 1])
      )
    )
  )


(spit "things/trackball-test.scad"
      (write-scad (union
                    (->> sensor (translate [0 0 20]))
                    (difference
                      (trackholder 50 0)
                      ;(->> (cube 100 200 200)(translate [50 0 0]))
                      ))))
