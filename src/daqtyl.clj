(ns daqtyl
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]))

(defn deg2rad [degrees]
  (* (/ degrees 180) pi))

;;;;;;;;;;;;;;;;;;;;;;
;; Shape parameters ;;
;;;;;;;;;;;;;;;;;;;;;;

(def nrows 4)
(def ncols 5)

(def α (/ π 12))                        ; curvature of the columns
(def β (/ π 36))                        ; curvature of the rows
(def centerrow (- nrows 3))             ; controls front-back tilt
(def centercol 4)                       ; controls left-right tilt / tenting (higher number is more tenting)
(def tenting-angle (/ π 9))            ; or, change this for more precise tenting control

(def first-15u-row 0)                   ; controls which should be the first row to have 1.5u keys on the outer column
(def last-15u-row 2)                    ; controls which should be the last row to have 1.5u keys on the outer column

(def column-style :standard)

(defn column-offset [column]
    (cond (= column 0) [1 0 0]
          (= column 2) [0 2.82 -4.5]
          (>= column 4) [0 -14 5.64]    ; original [0 -5.8 5.64]
          :else [0 0 0]))

(def thumb-offsets [10 -4 7])

; controls overall height; original=9 with centercol=3; use 16 for centercol=2
(def keyboard-z-offset (cond (= ncols 6) 9
                             (= ncols 5) 3))

(def extra-width 2.5)                   ; extra space between the base of keys; original= 2
(def extra-height 0.5)                  ; original= 0.5

(def wall-z-offset -8)                 ; length of the first downward-sloping part of the wall (negative)
(def wall-xy-offset 5)                  ; offset in the x and/or y direction for the first downward-sloping part of the wall (negative)
(def wall-thickness 2)                  ; wall thickness parameter; originally 5

;; Settings for column-style == :fixed
;; The defaults roughly match Maltron settings
;; http://patentimages.storage.googleapis.com/EP0219944A2/imgf0002.png
;; Fixed-z overrides the z portion of the column ofsets above.
;; NOTE: THIS DOESN'T WORK QUITE LIKE I'D HOPED.
(def fixed-angles [(deg2rad 10) (deg2rad 10) 0 0 0 (deg2rad -15) (deg2rad -15)])
(def fixed-x [-41.5 -22.5 0 20.3 41.4 65.5 89.6])  ; relative to the middle finger
(def fixed-z [12.1    8.3 0  5   10.7 14.5 17.5])
(def fixed-tenting (deg2rad 0))

; If you use Cherry MX or Gateron switches, this can be turned on.
; If you use other switches such as Kailh, you should set this as false
(def create-side-nubs? false)

;;;;;;;;;;;;;;;;;;;;;;;
;; General variables ;;
;;;;;;;;;;;;;;;;;;;;;;;

(def lastrow (dec nrows))
(def cornerrow (dec lastrow))
(def lastcol (dec ncols))

;;;;;;;;;;;;;;;;;
;; Switch Hole ;;
;;;;;;;;;;;;;;;;;

(def keyswitch-height 14.15)
(def keyswitch-width 14.15)

(def sa-profile-key-height 12.7)

(def plate-thickness 4)
(def side-nub-thickness 4)
(def retention-tab-thickness 1.5)
(def retention-tab-hole-thickness (- (+ plate-thickness 0.5) retention-tab-thickness))
(def mount-width (+ keyswitch-width 3.2))
(def mount-height (+ keyswitch-height 2.7))

; plate here means the single support structure a switch will snap into
(def single-plate
  (let [top-wall (->> (cube (+ keyswitch-width 3) 1.5 (+ plate-thickness 0.5))
                      (translate [0
                                  (+ (/ 1.5 2) (/ keyswitch-height 2))
                                  (- (/ plate-thickness 2) 0.25)]))
        left-wall (->> (cube 1.8 (+ keyswitch-height 3) (+ plate-thickness 0.5))
                       (translate [(+ (/ 1.8 2) (/ keyswitch-width 2))
                                   0
                                   (- (/ plate-thickness 2) 0.25)]))
        side-nub (->> (binding [*fn* 30] (cylinder 1 2.75))
                      (rotate (/ π 2) [1 0 0])
                      (translate [(+ (/ keyswitch-width 2)) 0 1])
                      (hull (->> (cube 1.5 2.75 side-nub-thickness)
                                 (translate [(+ (/ 1.5 2) (/ keyswitch-width 2))
                                             0
                                             (/ side-nub-thickness 2)])))
                      (translate [0 0 (- plate-thickness side-nub-thickness)]))
        plate-half (union top-wall left-wall (if create-side-nubs? (with-fn 100 side-nub)))
        top-nub (->> (cube 5 5 retention-tab-hole-thickness)
                     (translate [(+ (/ keyswitch-width 2.5)) 0 (- (/ retention-tab-hole-thickness 2) 0.5)]))
        top-nub-pair (union top-nub
                            (->> top-nub
                                 (mirror [1 0 0])
                                 (mirror [0 1 0])))]
    (difference
     (union plate-half
            (->> plate-half
                 (mirror [1 0 0])
                 (mirror [0 1 0])))
     (->>
      top-nub-pair
      (rotate (/ π 2) [0 0 1])))))

; plate structure for the EVQWGD001 rotary encoder
(def encoder-height 14.15)
(def encoder-width 16.00)
(def encoder-plate
  (let [plate-thickness 4.0
        top-wall (->> (cube (+ encoder-width 3) 1.5 plate-thickness)
                      (translate [0
                                  (+ (/ 1.5 2) (/ encoder-height 2))
                                  (/ plate-thickness 2)]))
        left-wall (->> (cube 1.6 (+ encoder-height 3) plate-thickness)
                       (translate [(+ (/ 2.2 2) (/ encoder-width 2))
                                   0
                                   (/ plate-thickness 2)]))
        plate-half (union top-wall left-wall)
        bridge-thickness 1.5
        bridge-top-recess 0.8
        bridge (->> (->> (difference
                           (cube 8 (+ encoder-height 2) bridge-thickness)
                           ; cutouts for little nubs
                           (->> (cube 2 2 2) (translate [-4 3 0]))
                           (->> (cube 2 2 2) (translate [-4 -4 0]))
                           ))
                    (translate [-2.5 0 (- (- (/ plate-thickness 1) (/ bridge-thickness 2)) bridge-top-recess)]))
        ]
    (->>
      (difference (union plate-half
                    bridge
                    (->> plate-half
                         (mirror [1 0 0])
                         (mirror [0 1 0])))
                  ; carve out a corner for better access to the pins
             (->> (cube 3 5 20 :center false)(rotate (deg2rad 90)[0 0 1])(translate [8.55 -7.8 -4])))
             (translate [0 0 3.75])
             )))

(spit "things/encoder-test.scad"
      (write-scad encoder-plate))

(def encoder-cap
  (let [x 1
        ]
    (->> (cylinder 6 15)
         (rotate (deg2rad 90) [0 1 0])
         ; TODO: bump z-height some more, maybe 2mm, need to wait for keycaps to decide.
         (translate [0 0 (+ 5 8)]) ;; 8 is plate-thickness from above
         (color [0 0 0 1])
         )
  ))

(def encoder-fill (hull encoder-plate))

;;;;;;;;;;;;;;;;
;; SA Keycaps ;;
;;;;;;;;;;;;;;;;

(def sa-length 18.25)
(def sa-double-length 37.5)
(def sa-cap {1 (let [bl2 (/ 18.5 2)
                     m (/ 17 2)
                     key-cap (hull (->> (polygon [[bl2 bl2] [bl2 (- bl2)] [(- bl2) (- bl2)] [(- bl2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[m m] [m (- m)] [(- m) (- m)] [(- m) m]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 6]))
                                   (->> (polygon [[6 6] [6 -6] [-6 -6] [-6 6]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 12])))]
                 (->> key-cap
                      (translate [0 0 (+ 5 plate-thickness)])
                      (color [220/255 163/255 163/255 1])))
             2 (let [bl2 sa-length
                     bw2 (/ 18.25 2)
                     key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[6 16] [6 -16] [-6 -16] [-6 16]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 12])))]
                 (->> key-cap
                      (translate [0 0 (+ 5 plate-thickness)])
                      (color [127/255 159/255 127/255 1])))
             1.5 (let [bl2 (/ 18.25 2)
                       bw2 (/ 27.94 2)
                       key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 0.05]))
                                     (->> (polygon [[11 6] [-11 6] [-11 -6] [11 -6]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 12])))]
                   (->> key-cap
                        (translate [0 0 (+ 5 plate-thickness)])
                        (color [240/255 223/255 175/255 1])))})

;; Fill the keyholes instead of placing a a keycap over them
(def keyhole-fill (->> (cube keyswitch-height keyswitch-width plate-thickness)
                       (translate [0 0 (/ plate-thickness 2)])))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Placement Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(def columns (range 0 ncols))
(def rows (range 0 nrows))

(def cap-top-height (+ plate-thickness sa-profile-key-height))
(def row-radius (+ (/ (/ (+ mount-height extra-height) 2)
                      (Math/sin (/ α 2)))
                   cap-top-height))
(def column-radius (+ (/ (/ (+ mount-width extra-width) 2)
                         (Math/sin (/ β 2)))
                      cap-top-height))
(def column-x-delta (+ -1 (- (* column-radius (Math/sin β)))))

(defn apply-key-geometry [translate-fn rotate-x-fn rotate-y-fn column row shape]
  (let [column-angle (* β (- centercol column))
        placed-shape (->> shape
                          (translate-fn [0 0 (- row-radius)])
                          (rotate-x-fn  (* α (- centerrow row)))
                          (translate-fn [0 0 row-radius])
                          (translate-fn [0 0 (- column-radius)])
                          (rotate-y-fn  column-angle)
                          (translate-fn [0 0 column-radius])
                          (translate-fn (column-offset column)))
        column-z-delta (* column-radius (- 1 (Math/cos column-angle)))
        placed-shape-ortho (->> shape
                                (translate-fn [0 0 (- row-radius)])
                                (rotate-x-fn  (* α (- centerrow row)))
                                (translate-fn [0 0 row-radius])
                                (rotate-y-fn  column-angle)
                                (translate-fn [(- (* (- column centercol) column-x-delta)) 0 column-z-delta])
                                (translate-fn (column-offset column)))
        placed-shape-fixed (->> shape
                                (rotate-y-fn  (nth fixed-angles column))
                                (translate-fn [(nth fixed-x column) 0 (nth fixed-z column)])
                                (translate-fn [0 0 (- (+ row-radius (nth fixed-z column)))])
                                (rotate-x-fn  (* α (- centerrow row)))
                                (translate-fn [0 0 (+ row-radius (nth fixed-z column))])
                                (rotate-y-fn  fixed-tenting)
                                (translate-fn [0 (second (column-offset column)) 0]))]
    (->> (case column-style
               :orthographic placed-shape-ortho
               :fixed        placed-shape-fixed
               placed-shape)
         (rotate-y-fn  tenting-angle)
         (translate-fn [0 0 keyboard-z-offset]))))

(defn key-place [column row shape]
  (apply-key-geometry translate
                      (fn [angle obj] (rotate angle [1 0 0] obj))
                      (fn [angle obj] (rotate angle [0 1 0] obj))
                      column row shape))

(defn rotate-around-x [angle position]
  (mmul
   [[1 0 0]
    [0 (Math/cos angle) (- (Math/sin angle))]
    [0 (Math/sin angle)    (Math/cos angle)]]
   position))

(defn rotate-around-y [angle position]
  (mmul
   [[(Math/cos angle)     0 (Math/sin angle)]
    [0                    1 0]
    [(- (Math/sin angle)) 0 (Math/cos angle)]]
   position))

(defn key-position [column row position]
  (apply-key-geometry (partial map +) rotate-around-x rotate-around-y column row position))

(defn key-holes [& {:keys [extra-top-row] :or {extra-top-row false}}]
  (apply union
         (conj
           (for [column columns
                 row rows
                 :when (or (.contains [3] column)
                           (not= row lastrow))]
             (->> single-plate (key-place column row)))
           ; XXX not symmetrical, the right side needs to rotate this 180!
           ; we need to mirror it here, as the left model later on get applied
           ; another mirroring operation, so we need to undo the damage here and below.
           (if extra-top-row (->> encoder-plate (mirror [-1 0 0]) (translate [0 0 3]) (key-place 2 lastrow))
             (->> single-plate (key-place 2 lastrow)))
           ; mouse keys go here, the middle one uses a different plate for the encoder
           (if extra-top-row (->> single-plate (key-place 1 -1)))
           (if extra-top-row (->> encoder-plate (mirror [-1 0 0]) (translate [0 0 3]) (rotate (deg2rad 180)[0 0 1]) (key-place 2 -1)))
           (if extra-top-row (->> single-plate (key-place 3 -1)))
           )))

(defn caps [& {:keys [extra-top-row] :or {extra-top-row false}}]
  (apply union
         (conj (for [column columns
                     row rows
                     :when (or (.contains [3] column)
                               (not= row lastrow))]
                 (->> (sa-cap 1) (key-place column row)))
               (if extra-top-row (->> encoder-cap (key-place 2 lastrow))
                 (->> (sa-cap 1) (key-place 2 lastrow)))
               (if extra-top-row (->> (sa-cap 1) (key-place 1 -1)))
               (if extra-top-row (->> encoder-cap (key-place 2 -1)))
               (if extra-top-row (->> (sa-cap 1) (key-place 3 -1)))
               )))

; only used to project the shadow on the bottom plate
(defn caps-fill [& {:keys [extra-top-row] :or {extra-top-row false}}]
  (apply union
         (conj (for [column columns
                     row rows
                     :when (or (.contains [2 3] column)
                               (not= row lastrow))]
                 (key-place column row keyhole-fill))
               (if extra-top-row (key-place 2 lastrow encoder-fill))
               (if extra-top-row (key-place 1 -1 keyhole-fill))
               (if extra-top-row (key-place 2 -1 encoder-fill))
               (if extra-top-row (key-place 3 -1 keyhole-fill))
               )))

;;;;;;;;;;;;;;;;;;;;
;; Web Connectors ;;
;;;;;;;;;;;;;;;;;;;;

(def web-thickness 4.5)
(def post-size 0.1)
(def web-post (->> (cube post-size post-size web-thickness)
                   (translate [0 0 (+ (/ web-thickness -2)
                                      plate-thickness)])))

(def post-adj (/ post-size 2))
(def web-post-tr (translate [(- (/ mount-width 1.95) post-adj) (- (/ mount-height 1.95) post-adj) 0] web-post))
(def web-post-tl (translate [(+ (/ mount-width -1.95) post-adj) (- (/ mount-height 1.95) post-adj) 0] web-post))
(def web-post-bl (translate [(+ (/ mount-width -1.95) post-adj) (+ (/ mount-height -1.95) post-adj) 0] web-post))
(def web-post-br (translate [(- (/ mount-width 1.95) post-adj) (+ (/ mount-height -1.95) post-adj) 0] web-post))

(defn triangle-hulls [& shapes]
  (apply union
         (map (partial apply hull)
              (partition 3 1 shapes))))

(defn connectors [& {:keys [extra-top-row] :or {extra-top-row false}}]
  (apply union
         (concat
          ;; Row connections
          (for [column (range 0 (dec ncols))
                row (range 0 lastrow)]
            (triangle-hulls
             (key-place (inc column) row web-post-tl)
             (key-place column row web-post-tr)
             (key-place (inc column) row web-post-bl)
             (key-place column row web-post-br)))
          ;; Column connections
          (for [column columns
                row (range 0 cornerrow)]
            (triangle-hulls
             (key-place column row web-post-bl)
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tl)
             (key-place column (inc row) web-post-tr)))
          ;; Diagonal connections
          (for [column (range 0 (dec ncols))
                row (range 0 cornerrow)]
            (triangle-hulls
             (key-place column row web-post-br)
             (key-place column (inc row) web-post-tr)
             (key-place (inc column) row web-post-bl)
             (key-place (inc column) (inc row) web-post-tl)))

          (if extra-top-row
            (let [z-offset 6]
              (concat
                ;; Row connections for the encoder plate, offset by 3.5mm
                (for [column [1] row [-1]]
                  (triangle-hulls
                    (key-place (inc column) row (->> web-post-tl (translate [-1 0 z-offset])))
                    (key-place column row (->> web-post-tr ))
                    (key-place (inc column) row (->> web-post-bl (translate [-1 0 z-offset])))
                    (key-place column row (->> web-post-br )))
                  )
                (for [column [2] row [-1]]
                  (triangle-hulls
                    (key-place (inc column) row (->> web-post-tl (translate [0 0 0])))
                    (key-place column row (->> web-post-tr (translate [1 0 z-offset])))
                    (key-place (inc column) row (->> web-post-bl (translate [0 0 0])))
                    (key-place column row (->> web-post-br (translate [1 0 z-offset]))))
                  )
                ;; Column connections
                (for [column (range 1 4) row [-1]]
                  (let [z-offset (cond (= column 2) [0 0 z-offset] :else [0 0 0])]
                    (triangle-hulls
                      (key-place column row (->> web-post-bl (translate z-offset)))
                      (key-place column row (->> web-post-br (translate z-offset)))
                      (key-place column (inc row) web-post-tl)
                      (key-place column (inc row) web-post-tr)
                      )))
                ;; Diagonal connections
                (for [column [1] row [-1]]
                  (triangle-hulls
                    (key-place column row web-post-br)
                    (key-place column (inc row) web-post-tr)
                    (key-place (inc column) row (->> web-post-bl (translate [0 0 z-offset])))
                    (key-place (inc column) (inc row) web-post-tl)
                    ))
                (for [column [2] row [-1]]
                  (triangle-hulls
                    (key-place column row (->> web-post-br (translate [0 0 z-offset])))
                    (key-place column (inc row) web-post-tr)
                    (key-place (inc column) row web-post-bl)
                    (key-place (inc column) (inc row) web-post-tl)
                    ))
              )))
          )))

; Thumb cluster
(def thumborigin
  (map + (key-position 1 cornerrow [(/ mount-width 2) (- (/ mount-height 2)) 0])
       thumb-offsets))

; My version of a 3-button cluster, I call it the micro cluster
(defn thumb-r-place [shape]
  (->> shape
       (rotate (deg2rad  10) [1 0 0])
       (rotate (deg2rad  -5) [0 1 0])
       (rotate (deg2rad  17) [0 0 1])
       (translate thumborigin)
       (translate [-14 -7.2 3])))
(defn thumb-m-place [shape & {:keys [offset] :or {offset [0 0 0]}}]
  (->> shape
       (rotate (deg2rad  10) [1 0 0])
       (rotate (deg2rad   0) [0 1 0])
       (rotate (deg2rad  25.5) [0 0 1])
       (translate thumborigin)
       (translate offset)
       (translate [-34 -15 2.2])))
(defn thumb-l-place [shape]
  (->> shape
       (rotate (deg2rad  10) [1 0 0])
       (rotate (deg2rad   5) [0 1 0])
       (rotate (deg2rad  33) [0 0 1])
       (translate thumborigin)
       (translate [-52 -25 3])))

(defn thumb-1x-layout [shape] nil)

(defn thumb-15x-layout [shape]
  (union
   (thumb-r-place shape)
   (thumb-m-place shape)
   (thumb-l-place shape)))

(def thumbcaps
  (union
   (thumb-1x-layout (sa-cap 1))
   (thumb-15x-layout (rotate (/ π 2) [0 0 1] (sa-cap 1.5)))))

(def thumbcaps-fill
  (union
   (thumb-1x-layout keyhole-fill)
   (thumb-15x-layout (rotate (/ π 2) [0 0 1] keyhole-fill))))

(def thumb
  (union
   (thumb-1x-layout single-plate)
   (thumb-15x-layout single-plate)))

;;;;;;;;;;
;; Case ;;
;;;;;;;;;;

(defn bottom [p]
  (->> (project p)
       (extrude-linear {:height 1 :twist 0 :convexity 0})
       ; this was in the original, but it just extends everything below the xy plane and we later need to cut it off
       ; XXX actually, not moving it down by 1mm means the object will hover above the plane
       (translate [0 0 -1])
       ))

(defn bottom-hull [& p]
  (hull p (bottom p)))

(def left-wall-x-offset 4)
(def left-wall-z-offset 1)

(defn left-key-position [row direction]
  (map - (key-position 0 row [(* mount-width -0.5) (* direction mount-height 0.5) 0]) [left-wall-x-offset 0 left-wall-z-offset]) )

(defn left-key-place [row direction shape]
  (translate (left-key-position row direction) shape))

(defn wall-locate1 [dx dy] [(* dx wall-thickness) (* dy wall-thickness) -1])
(defn wall-locate2 [dx dy] [(* dx wall-xy-offset) (* dy wall-xy-offset) wall-z-offset])
(defn wall-locate3 [dx dy] [(* dx (+ wall-xy-offset wall-thickness)) (* dy (+ wall-xy-offset wall-thickness)) wall-z-offset])

(defn wall-brace [place1 dx1 dy1 post1 place2 dx2 dy2 post2]
  (union
   (hull
    (place1 post1)
    (place1 (translate (wall-locate1 dx1 dy1) post1))
    (place1 (translate (wall-locate2 dx1 dy1) post1))
    (place1 (translate (wall-locate3 dx1 dy1) post1))
    (place2 post2)
    (place2 (translate (wall-locate1 dx2 dy2) post2))
    (place2 (translate (wall-locate2 dx2 dy2) post2))
    (place2 (translate (wall-locate3 dx2 dy2) post2)))
   (bottom-hull
    (place1 (translate (wall-locate2 dx1 dy1) post1))
    (place1 (translate (wall-locate3 dx1 dy1) post1))
    (place2 (translate (wall-locate2 dx2 dy2) post2))
    (place2 (translate (wall-locate3 dx2 dy2) post2)))))

(defn key-wall-brace [x1 y1 dx1 dy1 post1 x2 y2 dx2 dy2 post2]
  (wall-brace (partial key-place x1 y1) dx1 dy1 post1
              (partial key-place x2 y2) dx2 dy2 post2))

; TODO: merge this with wall-brace somehow, optional args or something.
(defn wall-brace-flat [place1 dx1 dy1 post1 place2 dx2 dy2 post2 yoffset]
  (union
   (hull
    (place1 post1)
    (place1 (translate (wall-locate1 dx1 dy1) post1))
    (place1 (translate (wall-locate2 dx1 (- dy1 (first yoffset))) post1))
    (place1 (translate (wall-locate3 dx1 (- dy1 (first yoffset))) post1))
    (place2 post2)
    (place2 (translate (wall-locate1 dx2 dy2) post2))
    (place2 (translate (wall-locate2 dx2 (- dy2 (last yoffset))) post2))
    (place2 (translate (wall-locate3 dx2 (- dy2 (last yoffset))) post2))
    )
   (bottom-hull
     (place1 (translate (wall-locate2 dx1 (- dy1 (first yoffset))) post1))
     (place1 (translate (wall-locate3 dx1 (- dy1 (first yoffset))) post1))
     (place2 (translate (wall-locate2 dx2 (- dy2 (last yoffset))) post2))
     (place2 (translate (wall-locate3 dx2 (- dy2 (last yoffset))) post2))
    )
   ))

(defn key-wall-brace-flat [x1 y1 dx1 dy1 post1 x2 y2 dx2 dy2 post2 yoffset]
  (wall-brace-flat (partial key-place x1 y1) dx1 dy1 post1
                   (partial key-place x2 y2) dx2 dy2 post2
                   yoffset
                   ))

(def right-wall
    (union (key-wall-brace lastcol 0 0 1 web-post-tr lastcol 0 1 0 web-post-tr)
             (union (for [y (range 0 lastrow)] (key-wall-brace lastcol y 1 0 web-post-tr lastcol y 1 0 web-post-br))
                    (for [y (range 1 lastrow)] (key-wall-brace lastcol (dec y) 1 0 web-post-br lastcol y 1 0 web-post-tr)))
           (key-wall-brace lastcol cornerrow 0 -1 web-post-br lastcol cornerrow 1 0 web-post-br)
           ))

(defn thumb-connectors [& {:keys [encoder] :or {encoder false}}]
  (let [z-offset (cond encoder [0 0 6] :else [0 0 0])
        wide-left (cond encoder [-1 0 0] :else [0 0 0])
        wide-right (cond encoder [1 0 0] :else [0 0 0])
        ]
  (union
   (triangle-hulls    ; top two
    (thumb-m-place web-post-tr)
    (thumb-m-place web-post-br)
    (thumb-r-place web-post-tl)
    (thumb-r-place web-post-bl))
   (triangle-hulls
    (thumb-m-place web-post-tl)
    (thumb-l-place web-post-tr)
    (thumb-m-place web-post-bl)
    (thumb-l-place web-post-br)
    (thumb-m-place web-post-bl)
    )
   ; The steep bit from thumb key up towards main keyboard, it's many parts.
   ; Need to do special things for 1.5u keys, sigh.
   (let [
         left-offset [-1.3 4.5 0.99]
         right-offset [1.2 6 0.4]
         ]
     (union
       (color [0 1 1 1] (hull
        (left-key-place cornerrow -1 web-post)
        (left-key-place cornerrow -1 (translate (wall-locate1 -1 0) web-post))
        (left-key-place cornerrow -1 (translate (wall-locate2 -1 0) web-post))
        (left-key-place cornerrow -1 (translate (wall-locate3 -1 0) web-post))
        (thumb-m-place web-post-tl :offset left-offset)))
       (color [1 0 1 1] (hull
        (left-key-place cornerrow -1 web-post)
        (left-key-place cornerrow -1 (translate (wall-locate1 -1 0) web-post))
        (key-place 0 cornerrow web-post-bl)
        (thumb-m-place web-post-tl :offset left-offset)))
       (color [0 1 0 1] (triangle-hulls
        (thumb-m-place web-post-tl :offset left-offset)
        (key-place 0 cornerrow web-post-bl)
        (thumb-m-place web-post-tr :offset right-offset)
        (key-place 0 cornerrow web-post-br)
        ))
       (color [1 0 0 1] (triangle-hulls
        (thumb-m-place web-post-tl)
        (thumb-m-place web-post-tl :offset left-offset)
        (thumb-m-place web-post-tr :offset right-offset)
        (key-place 0 cornerrow web-post-br)
        (thumb-m-place web-post-tr)
        (thumb-m-place web-post-tr :offset right-offset)
        (thumb-m-place web-post-tl)
        ))
     ))
   (triangle-hulls
    (thumb-m-place web-post-tr)
    (key-place 0 cornerrow web-post-br)
    (thumb-r-place web-post-tl)
    (key-place 1 cornerrow web-post-bl)
    (thumb-r-place web-post-tr)
    (key-place 1 cornerrow web-post-br)
    )
   ; snaking around the first extra key
   (color [0 1 0 1] (triangle-hulls
    (thumb-r-place web-post-tr)
    (key-place 1 cornerrow web-post-br)
    (key-place 2 lastrow (->> web-post-tl (translate wide-left)(translate z-offset)))
    (thumb-r-place web-post-tr)
    (key-place 2 lastrow (->> web-post-bl (translate wide-left)(translate z-offset)))
    (thumb-r-place web-post-tr)
    (key-place 2 lastrow (->> web-post-bl (translate wide-left)(translate z-offset)))
    (thumb-r-place web-post-br)
    (key-place 2 lastrow (->> web-post-br (translate wide-right)(translate z-offset)))
    (key-place 3 lastrow (->> web-post-bl (translate [0 0 0])))
    (key-place 2 lastrow (->> web-post-tr (translate wide-right)(translate z-offset)))
    (key-place 3 lastrow (->> web-post-tl (translate [0 0 0])))
    ))
   ; snaking around the second extra key
   (triangle-hulls
     (key-place 3 lastrow web-post-tr)
     (key-place 3 lastrow web-post-br)
     (key-place 3 lastrow web-post-tr)
     (key-place 4 cornerrow web-post-bl))
   ; connect first extra key to regular matrix
   (color [1 0 1 1] (triangle-hulls
                      (key-place 1 cornerrow web-post-br)
                      (key-place 2 lastrow (->> web-post-tl (translate z-offset)))
                      (key-place 2 cornerrow web-post-bl)
                      (key-place 2 lastrow (->> web-post-tr (translate z-offset)))
                      (key-place 2 cornerrow web-post-br)
                      (key-place 3 cornerrow web-post-bl)
                      (key-place 2 lastrow (->> web-post-tr (translate z-offset)))
                      (key-place 3 lastrow web-post-tl)
                      ))
   ; connect second extra key to regular matrix
   (color [1 1 0 1] (triangle-hulls
                      (key-place 3 lastrow web-post-tl)
                      (key-place 3 cornerrow web-post-bl)
                      (key-place 3 lastrow web-post-tr)
                      (key-place 3 cornerrow web-post-br)
                      (key-place 4 cornerrow web-post-bl)
                      ))
   )))

(def thumb-wall
  (union
   ; thumb walls
   ; left back
   (wall-brace thumb-l-place  0  1 web-post-tr thumb-l-place  0  1 web-post-tl)
   ; left side
   (wall-brace thumb-l-place -1  0 web-post-tl thumb-l-place -1  0 web-post-bl)
   ; front walls
   (wall-brace thumb-r-place  0 -1 web-post-br thumb-r-place  0 -1 web-post-bl)
   (wall-brace thumb-m-place  0 -1 web-post-br thumb-m-place  0 -1 web-post-bl)
   (wall-brace thumb-l-place  0 -1 web-post-br thumb-l-place  0 -1 web-post-bl)
   ; thumb corners
   (wall-brace thumb-l-place -1  0 web-post-bl thumb-l-place  0 -1 web-post-bl)
   (wall-brace thumb-l-place -1  0 web-post-tl thumb-l-place  0  1 web-post-tl)
   ; thumb tweeners
   (wall-brace thumb-r-place  0 -1 web-post-bl thumb-m-place  0 -1 web-post-br)
   (wall-brace thumb-m-place  0 -1 web-post-bl thumb-l-place  0 -1 web-post-br)
   (wall-brace-flat thumb-r-place  0 -1 web-post-br (partial key-place 3 lastrow)  0 -1 web-post-bl [0 -1])
   ; clunky bit on the top left thumb connection  (normal connectors don't work well)
   (bottom-hull
    (left-key-place cornerrow -1 (translate (wall-locate2 -1 0) web-post))
    (left-key-place cornerrow -1 (translate (wall-locate3 -1 0) web-post))
    (thumb-l-place (translate (wall-locate2 0 1) web-post-tr))
    (thumb-l-place (translate (wall-locate3 0 1) web-post-tr))
    )
   (hull
    (left-key-place cornerrow -1 (translate (wall-locate2 -1 0) web-post))
    (left-key-place cornerrow -1 (translate (wall-locate3 -1 0) web-post))
    (thumb-l-place (translate (wall-locate2 0 1) web-post-tr))
    (thumb-l-place (translate (wall-locate3 0 1) web-post-tr))
    (thumb-m-place web-post-tl))
   (hull
    (thumb-l-place web-post-tr)
    (thumb-l-place (translate (wall-locate1 0 1) web-post-tr))
    (thumb-l-place (translate (wall-locate2 0 1) web-post-tr))
    (thumb-l-place (translate (wall-locate3 0 1) web-post-tr))
    (thumb-m-place web-post-tl))
   ))

(spit "things/thumb-test.scad"
      (write-scad (difference
                    (union
                      thumb
                      (thumb-connectors)
                      thumb-wall)
                    (union
                      (->> (cube 100 100 100) (translate [8 -50 0]) (rotate (deg2rad 5) [0 0 1]))
                      (->> (cube 100 100 100) (translate [-60 -50 -18]))
                      ))))


(defn back-wall [& {:keys [extra-top-row] :or {extra-top-row false}}]
  (union
    ;(for [x (range 0 ncols)] (key-wall-brace x 0 0 1 web-post-tl x       0 0 1 web-post-tr))
    ;(for [x (range 1 ncols)] (key-wall-brace x 0 0 1 web-post-tl (dec x) 0 0 1 web-post-tr))
    (if extra-top-row
      (vector
        ; x/y is placement of key, 0/0 being top-left on the right model
        ; dx1/dy1 moves the post a bit, scaled by wall-xy-locate
        ; ditto for the second post. So you move to key pos 0/0 and put down a
        ; wall from its top-left to top-right position. Then you brace from key
        ; 0/0's top-right to 1/-1's top left, etc.
        (key-wall-brace 0 0 0 1 web-post-tl 0 0 -0.5 1 web-post-tr)
        (color [1 1 0 1] (key-wall-brace 0 0 -0.5 1 web-post-tr 1 -1 0 1 web-post-tl))
        (color [1 1 0 1] (hull
                           (key-place 1 -1 web-post-tl)
                           (key-place 1  0 web-post-tl)
                           (key-place 0  0 web-post-tr)
                           ))
        (key-wall-brace 1 -1 0 1 web-post-tl 1 -1 0 1 web-post-tr)
        (color [0 1 0 1] (key-wall-brace-flat 2 -1 0 1 web-post-tl 1 -1 0 1 web-post-tr [0.47 0]))
        (color [1 0 0 1] (key-wall-brace-flat 2 -1 0 1 web-post-tl 2 -1 0 1 web-post-tr [0.47 0.47]))
        (color [0 0 1 1] (key-wall-brace-flat 3 -1 0 1 web-post-tl 2 -1 0 1 web-post-tr [0 0.47]))
        (key-wall-brace 3 -1 0 1 web-post-tl 3 -1 0 1 web-post-tr)
        ; these use a dx=2 offset to make the wall thicker, needs also a bespoke triangle hull.
        (color [1 1 0 1] (key-wall-brace 3 -1 0 1 web-post-tr 4 0 2 1 web-post-tl))
        (key-wall-brace 4 0 2 1 web-post-tl 4 0 0 1 web-post-tr)
        (color [1 0 0 1] (hull
                           (key-place 3 -1 web-post-tr)
                           (key-place 3  0 web-post-tr)
                           (key-place 4  0 web-post-tl)
                           ))
        (cond
          (= lastcol 5) (union
                          (color [1 1 0 1] (key-wall-brace 4 0 0 1 web-post-tr 5 0 0 1 web-post-tl))
                          (key-wall-brace 5 0 0 1 web-post-tl 5 0 0 1 web-post-tr)
                          ))
       )
      (vector
        (for [x (range 0 2)    ] (key-wall-brace x 0 0 1 web-post-tl x       0 0 1 web-post-tr))
        (key-wall-brace 3 0 0 1 web-post-tl 3       0 0 1 web-post-tr)
        (for [x (range 5 ncols)] (key-wall-brace x 0 0 1 web-post-tl x       0 0 1 web-post-tr))
        (for [x (range 1 2)    ] (key-wall-brace x 0 0 1 web-post-tl (dec x) 0 0 1 web-post-tr))
        (for [x (range 5 ncols)] (key-wall-brace x 0 0 1 web-post-tl (dec x) 0 0 1 web-post-tr))
        (key-wall-brace 3 0 0 1 web-post-tr 4 0 1 1 web-post-tl)
        (key-wall-brace 4 0 1 1 web-post-tl 4 0 0 1 web-post-tr)
        (color [0 1 0 1] (key-wall-brace-flat 2 0 0 1 web-post-tl 1 0 0 1 web-post-tr [0.43 0]))
        (color [1 0 0 1] (key-wall-brace-flat 2 0 0 1 web-post-tl 2 0 0 1 web-post-tr [0.43 0.43]))
        (color [0 0 1 1] (key-wall-brace-flat 3 0 0 1 web-post-tl 2 0 0 1 web-post-tr [0 0.43]))
      ))))

(def left-wall (union
  (for [y (range 0 lastrow)] (union
                                                   (wall-brace (partial left-key-place y 1) -1 0 web-post (partial left-key-place y -1) -1 0 web-post)
                                                   (hull (key-place 0 y web-post-tl)
                                                         (key-place 0 y web-post-bl)
                                                         (left-key-place y  1 web-post)
                                                         (left-key-place y -1 web-post))))
  (for [y (range 1 lastrow)] (union
                                                   (wall-brace (partial left-key-place (dec y) -1) -1 0 web-post (partial left-key-place y  1) -1 0 web-post)
                                                   (hull (key-place 0 y       web-post-tl)
                                                         (key-place 0 (dec y) web-post-bl)
                                                         (left-key-place y        1 web-post)
                                                         (left-key-place (dec y) -1 web-post)
                                                         )))
  (wall-brace (partial key-place 0 0) 0 1 web-post-tl (partial left-key-place 0 1) -0.6 1 web-post)
  (wall-brace (partial left-key-place 0 1) -0.6 1 web-post (partial left-key-place 0 1) -1 0 web-post)
  ))


(def front-wall (union
  (key-wall-brace-flat 3 lastrow  0 -1 web-post-bl 3 lastrow   0 -1 web-post-br [-1 -1])
  (key-wall-brace-flat 3 lastrow  0 -1 web-post-br 4 cornerrow 2 -1 web-post-bl [-1 0])
  (key-wall-brace 4 cornerrow 2 -1 web-post-bl 4       cornerrow 0 -1 web-post-br)
  (for [x (range 5 ncols)] (key-wall-brace x cornerrow 0 -1 web-post-bl x       cornerrow 0 -1 web-post-br))
  (for [x (range 5 ncols)] (key-wall-brace x cornerrow 0 -1 web-post-bl (dec x) cornerrow 0 -1 web-post-br))
  ))

(defn case-walls [& {:keys [extra-top-row] :or {extra-top-row false}}]
  (union
   thumb-wall
   right-wall
   (back-wall :extra-top-row extra-top-row)
   left-wall
   front-wall
   ))

; Offsets for the controller/trrs holder cutout
(def holder-offset
  (case nrows
    4 -3.5
    5 0
    6 2.2))

(def notch-offset
  (case nrows
    4 3.35
    5 0.15
    6 -5.07))

; Cutout for controller/trrs jack holder
(def usb-holder-ref (key-position 0 0 (map - (wall-locate2  0  -1) [0 (/ mount-height 2) 0])))
(def usb-holder-position (map + [(+ 18.8 holder-offset) 18.7 1.3] [(first usb-holder-ref) (second usb-holder-ref) 2]))
(def usb-holder-space  (translate (map + usb-holder-position [-1.5 (* -1 wall-thickness) 2.9]) (cube 28.666 30 12.4)))
(def usb-holder-notch  (translate (map + usb-holder-position [-1.5 (+ 4.4 notch-offset) 2.9]) (cube 31.366 1.3 12.4)))
(def trrs-notch        (translate (map + usb-holder-position [-10.33 (+ 3.6 notch-offset) 6.6]) (cube 8.4 2.4 19.8)))

; Screw insert definition & position
(defn screw-insert-shape [bottom-radius top-radius height]
  (union
    (->> (binding [*fn* 30] (cylinder [bottom-radius top-radius] height)))
    (translate [0 0 (/ height 2)] (sphere top-radius))))

(defn screw-insert [column row bottom-radius top-radius height offset col]
  (let [shift-right   (= column lastcol)
        shift-left    (= column 0)
        shift-up      (and (not (or shift-right shift-left)) (= row 0))
        shift-down    (and (not (or shift-right shift-left)) (>= row lastrow))
        position      (if shift-up     (key-position column row (map + (wall-locate2  0  1) [0 (/ mount-height 2) 0]))
                        (if shift-down  (key-position column row (map - (wall-locate2  0 -2.5) [0 (/ mount-height 2) 0]))
                          (if shift-left (map + (left-key-position row 0) (wall-locate3 -1 0))
                            (key-position column row (map + (wall-locate2  1  0) [(/ mount-width 2) 0 0])))))]
    (->> (screw-insert-shape bottom-radius top-radius height)
         (translate (map + offset [(first position) (second position) (/ height 2)]))
         (color col))))

(def bottom-plate-thickness 2.6)

(defn screw-insert-all-shapes [bottom-radius top-radius height]
  (union (screw-insert 0 0        bottom-radius top-radius height [9 10.5 bottom-plate-thickness] [1 0 0]) ; red
         (screw-insert 0 lastrow  bottom-radius top-radius height [0 2 bottom-plate-thickness] [1 1 0]) ; yellow
         ; FIXME later
         ;(screw-insert 2 0        bottom-radius top-radius height [9.5 -4.5 bottom-plate-thickness] [0 0 1]) ; blue
         (screw-insert 1 lastrow  bottom-radius top-radius height [5 15 bottom-plate-thickness] [1 0 1]) ; fuchsia
         (screw-insert lastcol 0        bottom-radius top-radius height [-21 (cond (= lastcol 4) 13 (= lastcol 5) 9) bottom-plate-thickness] [0 1 1]) ; aqua
         (screw-insert lastcol lastrow  bottom-radius top-radius height [-21 13 bottom-plate-thickness] [0 1 0]) ; green
         )
  )

; Hole Depth Y: 4.4
(def screw-insert-height 6)

; Hole Diameter C: 4.1-4.4
(def screw-insert-bottom-radius (/ 4.2 2))
(def screw-insert-top-radius (/ 4.0 2))
(def screw-insert-holes  (->> (screw-insert-all-shapes screw-insert-bottom-radius screw-insert-top-radius screw-insert-height)
                              (translate [0 0 -0.01])))

; Wall Thickness W:  2.1--3.0
(def screw-insert-outers (screw-insert-all-shapes (+ screw-insert-bottom-radius 3.0) (+ screw-insert-top-radius 2.1) (+ screw-insert-height 1.1)))
(def screw-insert-screw-holes  (screw-insert-all-shapes 1.7 1.7 350))
(def plate-screw-recess  (screw-insert-all-shapes 3.1 1.95 2.1)) ;; creates the recess for screws in bottom plate

; Wrist rest cutout from https://github.com/crystalhand/dactyl-keyboard.git
;;Wrist rest to case connections
(def wrist-rest-on 1)
(def wrist-rest-back-height 23)	;;height of the back of the wrist rest--Default 34
(def wrist-rest-angle 5) 	;;angle of the wrist rest--Default 20
(def wrist-rest-rotation-angle 9);;0 default The angle in counter clockwise the wrist rest is at
(def wrist-rest-ledge 0)	;;The height of ledge the silicone wrist rest fits inside
(def wrist-rest-y-angle 10)	;;0 Default.  Controls the wrist rest y axis tilt (left to right)

(def wrist-translate-x (+ (first thumborigin) 10))
(def right_wrist_connecter_x   (if (== ncols 5) 13 17))
(def middle_wrist_connecter_x  (if (== ncols 5) -5 -4))
(def left_wrist_connecter_x    (if (== ncols 5) -25 -25))
(def wrist_right_nut_y         (if (== ncols 5) 10 20.5))
(def wrist_brse_position_x -1)
(def wrist_brse_distance_y -35)     ;; Distance from wrist rest to keyboard

(def cut-bottom
  (->>(cube 300 300 100)(translate [0 0 -50]))
)

(def h-offset
  (* (Math/tan(/ (* π wrist-rest-angle) 180)) 88)
)

(def scale-cos
  (Math/cos(/ (* π wrist-rest-angle) 180))
)

(def scale-amount
  (/ (* 83.7 scale-cos) 19.33)
)

(def wrist-rest
  (difference
    (scale [4.25  scale-amount  1]
           (difference (union
                         (difference
                           ;the main back circle
                           (scale [1.3, 1, 1](->> (cylinder 10 150)(with-fn 200)
                                                  (translate [0 0 0])))
                           ;front cut cube and circle
                           (scale [1.1, 1, 1](->> (cylinder 7 201)(with-fn 200)
                                                  (translate [0 -13.4 0]))
                                  (->> (cube 18 10 201)(translate [0 -12.4 0]))))
                         ;;side fillers
                         (->> (cylinder 6.8 190)(with-fn 200) (translate [-6.15 -0.98 0]))
                         (->> (cylinder 6.8 190)(with-fn 200) (translate [6.15 -0.98 0]))
                         ;;heart shapes at bottom
                         (->> (cylinder 5.9 190)(with-fn 200) (translate [-6.35 -2 0]))
                         (scale [1.01, 1, 1](->> (cylinder 5.9 190)(with-fn 200)
                                                 (translate [6.35 -2. 0])))
                         )
                       )
           )
    cut-bottom
    )
)

(def wrist-rest-base
  (->>
           (difference
             (scale [1.08 1.08 1] wrist-rest)
             (->> (cube 200 200 200)(translate [0 0 (+ (+ (/ h-offset 2) (- wrist-rest-back-height h-offset) ) 100)]) (rotate  (/ (* π wrist-rest-angle) 180)  [1 0 0])(rotate  (/ (* π wrist-rest-y-angle) 180)  [0 1 0]))
             (if (not (zero? wrist-rest-ledge))
               (->> (difference
                      wrist-rest
                      (->> (cube 200 200 200)(translate [0 0 (- (+ (/ h-offset 2) (- wrist-rest-back-height h-offset) ) (+ 100  wrist-rest-ledge))]) (rotate  (/ (* π wrist-rest-angle) 180)  [1 0 0])(rotate  (/ (* π wrist-rest-y-angle) 180)  [0 1 0]))
                      )
                    )
               )
             )
           )
  )

; These cut out the holes on the case side (so screw head sits flush), as well
; as the cube to receive the nut on the connector itself.
(def rest-case-cuts
  (union
    ;;right cut
    (->> (cylinder 1.85 50)(with-fn 30) (rotate  (/  π 2)  [1 0 0])(translate [right_wrist_connecter_x 20 4.5]))
    (->> (cylinder 2.9 5.2)(with-fn 50) (rotate  (/  π 2)  [1 0 0])(translate [right_wrist_connecter_x 37 4.5]))
    (->> (cube 6 3 12.2)(translate [right_wrist_connecter_x 22.0 1.5]))
    ;;middle cut
    (->> (cylinder 1.85 50)(with-fn 30) (rotate  (/  π 2)  [1 0 0])(translate [middle_wrist_connecter_x 20 4.5]))
    (->> (cylinder 2.9 5.2)(with-fn 50) (rotate  (/  π 2)  [1 0 0])(translate [middle_wrist_connecter_x 36.5 4.5]))
    (->> (cube 6 3 12.2)(translate [middle_wrist_connecter_x 22.0 1.5]))
    ;;left
    (->> (cylinder 1.85 50)(with-fn 30) (rotate  (/  π 2)  [1 0 0])(translate [left_wrist_connecter_x 22 4.5]))
    (->> (cylinder 2.9 5.2)(with-fn 50) (rotate  (/  π 2)  [1 0 0])(translate [left_wrist_connecter_x 33.7 4.5]))
    (->> (cube 6 3 12.2)(translate [left_wrist_connecter_x 20.0 1.5]))
    )
)

(def rest-case-connectors
  (difference
    (union
      (scale [1 1 1.6] (->> (cylinder 6 60)(with-fn 200) (rotate (/ π 2) [1 0 0])(translate [right_wrist_connecter_x 2 0])))
      (scale [1 1 1.6] (->> (cylinder 6 60)(with-fn 200) (rotate (/ π 2) [1 0 0])(translate [middle_wrist_connecter_x 2 0])))
      (scale [1 1 1.6] (->> (cylinder 6 60)(with-fn 200) (rotate (/ π 2) [1 0 0])(translate [left_wrist_connecter_x -1 0])))
      )
    )
)

(def wrest-wall-cut
  (->> (for [xyz (range 1.00 10 3)] ;controls the scale last number needs to be lower for thinner walls
         (union
           (translate [1 xyz 1] (case-walls))
           )
         )
))

(def wrist-rest-build
  (let [base (->> wrist-rest-base (translate [wrist_brse_position_x wrist_brse_distance_y 0])(rotate  (/ (* π wrist-rest-rotation-angle) 180)  [0 0 1]))
        cutout (->> base (scale [0.90 0.90 1.0])(translate [0 -3 -8]))
        _translate (fn [x] (translate [wrist-translate-x (- (second thumborigin) 50) 0] x))
        ]
  (difference
    (->> (union
           base
           (->> (difference
                  rest-case-connectors
                  rest-case-cuts
                  cut-bottom
                  )
                )
           )
         _translate
         )
    wrest-wall-cut
    ; This doesn't save material, actually. It's better to have this solid and
    ; use 20% infill, which uses 74g of material, vs. the cutout, adding more
    ; wall surface and resulting in 84g of material needed (and much longer
    ; print time, due to supports)
    ;(_translate cutout)
    )
  )
  )

(spit "things/encoder-welltest.scad"
      (write-scad (mirror [-1 0 0]
                          (intersection ;(->> (cube 60 140 50)(translate [-35 0 30]))
                    (union
                      (key-holes :extra-top-row true)
                      (connectors :extra-top-row true)
                      (thumb-connectors :encoder true)
                      thumb
                      ))))
      )

; put it all together
(def model-right (difference
                   (union
                     (key-holes)
                     (connectors)
                     thumb
                     (thumb-connectors)
                     (difference (union (case-walls)
                                        screw-insert-outers)
                                 ;usb-holder-space
                                 ;trrs-notch
                                 ;usb-holder-notch
                                 (if (== wrist-rest-on 1) (->> rest-case-cuts (translate [wrist-translate-x (- (second thumborigin) (- 56 nrows)) 0])))
                                 screw-insert-holes))
                   cut-bottom
                   ))

(def model-left (difference
                  (union
                    (key-holes :extra-top-row true)
                    (connectors :extra-top-row true)
                    thumb
                    (thumb-connectors :encoder true)
                    (difference (union (case-walls :extra-top-row true)
                                       screw-insert-outers)
                                (if (== wrist-rest-on 1) (->> rest-case-cuts (translate [wrist-translate-x (- (second thumborigin) (- 56 nrows)) 0])))
                                screw-insert-holes))
                  cut-bottom
                  ))

; Cut away the walls from the bottom plate again, so it recedes fully. Requires sufficient keyboard-z-offset.
(defn plate-cutout [shape & {:keys [extra-top-row] :or {extra-top-row false}}]
  (union
    (difference
      shape
      (translate [0 0 -10] screw-insert-screw-holes)
      (translate [0 0 -3.4] plate-screw-recess)
      (union
        (for [xy (range 0.994 1.14 0.019)]
          (->> (case-walls :extra-top-row extra-top-row)
               (scale [xy xy 1.0])
               (translate [0 0 -0.01]))))
      )))

(def plate-right
        (extrude-linear
          {:height bottom-plate-thickness :center false}
          (project
            (difference
              (union
                (key-holes)
                (connectors)
                thumb
                (thumb-connectors)
                (case-walls)
                thumbcaps-fill
                (caps-fill)
                screw-insert-outers)
              ))))

(def plate-left
  (difference
        (extrude-linear
          {:height bottom-plate-thickness :center false}
          (project
            (difference
              (union
                (key-holes :extra-top-row true)
                (connectors :extra-top-row true)
                thumb
                (thumb-connectors :encoder true)
                (case-walls :extra-top-row true)
                thumbcaps-fill
                (caps-fill :extra-top-row true)
                screw-insert-outers)
              )))
        ))

(defn trackholder [l zdeg]
  (let [r 17.00
        h 70
        outer-r (+ r 1)
        d (/ r 2)
        c (* r 3)
        ; steel balls will be glued in here
        pimple (fn [r] (->> (sphere r)
                            (with-fn 40)))
        ; the cutout for the sensor
        cutout (hull (union
                      (->> (cylinder 2.5 6)(translate [2 0 0]))
                      (->> (cylinder 2.5 6)(translate [-2 0 0]))
                ))
        ; where the ball will sit
        bowl (union
               (difference
                 (fa! 1)
                 (fs! 1)
                 (sphere (+ r 1.5))
                 (sphere (+ r 0.1))
                 (->> (cube c c c)
                      (translate [0 0 (/ c 2)]))
                 (for [deg '(0)] (->>
                                            cutout
                                            (translate [0 0 r])
                                            (rotate (deg2rad 135) [1 0 0])
                                            (rotate (deg2rad deg) [0 0 1])
                                            ))
                 ; through hole to the bottom
                 (->> (cylinder 8 10)(translate [0 0 (* r -1)]))
                 )
               ; three balls to meld/glue steel balls into
               (for [x [[0 2] [120 2] [240 2]]] (->>
                                         (pimple (second x))
                                         (translate [0 0 (+ r 1.0)])
                                         (rotate (deg2rad 120) [1 0 0])
                                         (rotate (deg2rad (+ (first x) 60)) [0 0 1])
                                         )
                 )
               )
        ; cutout to reach the sensor board
        sensor-wall-cut (->> (cube 28.2 25 90)
                             (translate [0 (* -1 r) (- (/ h 4) 3)]))
        ; top outer rim connecting to the cylinder
        ring (->>
               (difference
                 (fa! 1)
                 (fs! 1)
                 (cylinder outer-r 2)
                 (cylinder (+ r 1) 4)
                 ;; FIXME results in ugly edges, maybe rotate around z-axis by 45 deg
                 sensor-wall-cut
                 )
               (translate [0 0 -1]))
        ; the hollow cylinder
        cyl-r (/ outer-r 2)
        cyl (->>
              (union (difference
                 (fa! 1)
                 (fs! 1)
                 (cylinder [ (+ cyl-r 2) (+ outer-r 2) ] h)
                 (cylinder [ cyl-r outer-r ] (+ h 1))
                 sensor-wall-cut
                 )
                 )
              (translate [0 0 (/ h -2)])
              (multmatrix [[1 0 -0.0 0]
                           [0 1 0.15 0]
                           [0 0 1 0]])
              )
        ; PWM3360 board
        sensor (difference
                 (->> (union
                        (difference
                          (union
                            (->> (cube 2 12 30 :center false)(translate [-14 -14 -20]))
                            (->> (cube 2 12 30 :center false)(translate [ 12 -14 -20]))
                            )
                          ; actual max dimensions, but much thinner on the side.
                          ;(cube 28.5 21.5 6.7)
                          ; cut away the sensor cube from the supports, then also a window
                          (->> (cube 28.5 21.5 3)(translate [0 0 0.5]))
                          (->> (cube 28.5 21.5 10)(translate [0 4 6]))
                          ; cut off things that would stick out, ugh, this is horribly hacky
                          (->> (cube 28.5 12 6)(rotate (deg2rad -35) [1 0 0])(translate [0 -9 8]))
                          )
                        ; turn this on to see if the sensor fits the overall model
                        ;(->> (cube 28.5 21.5 3)(translate [0 0 1])) (->> (cube 21.5 21.5 8)(translate [0 3 0.5]))
                        )
                      (translate [0 0 (+ 5.35 r)]) ; half cube width plus thickness
                      (rotate (deg2rad 135) [1 0 0])
                      )
                 (->> (sphere (+ r 1))(with-fn 60))
                 )
        ]
    (difference
      (->> (union
           bowl
           ring
           cyl
           sensor
           ;(->> (sphere 17)(with-fn 60)(translate [0 0 0.8]))
           )
         (rotate (deg2rad zdeg) [0 0 1]))
      (->> (cube 100 100 100)(translate [0 0 (- -50 l)]))
    )))

(spit "things/trackball-test.scad"
      (write-scad (difference
                    (trackholder 50 0)
                    ;(->> (cube 100 200 200)(translate [50 0 0]))
                    )))

; Trackballs on the top/back and side of keyboard.
(def trackball-top-pos [-30 50 (+ 30 keyboard-z-offset)])
(def trackball-top
  (union (->> (trackholder (last trackball-top-pos) 0)
              (translate trackball-top-pos)
              (color [1 0 0 1]))
         ))

(def trackball-side-pos [-99 -20 (+ 55 keyboard-z-offset)])
(def trackball-side
  (union (->> (trackholder (last trackball-side-pos) 90)
              (translate trackball-side-pos)
              (color [0 0 1 1]))
         ))

; janky ass way to cut through to the trackball holders left side
(def trackball-cutouts
  (union
    (fa! 1)
    (->> (cube 20 28 80) (translate [-88 -20 9]))
    (->> (sphere 19.6) (translate trackball-side-pos))
    ; top side
    (->> (cube 28 20 50) (translate [-30 43 0]))
    (->> (sphere 19.6) (translate trackball-top-pos))
  ))

(defn spit-all-test []
  (future
    (let [
          file "things/all-test.scad"
          old (try (slurp file) (catch Exception e))
          new (write-scad
                (union
                  (translate [130 0 0] (union (difference model-right
                                                          trackball-cutouts
                                                          )
                                              (->> (plate-cutout plate-right) (translate [0 0 -30]))
                                              thumbcaps
                                              (caps)
                                              wrist-rest-build
                                              trackball-top
                                              trackball-side
                                              ))
                  (translate [-130 0 0] (mirror [-1 0 0] (union
                                                           model-left
                                                           (->> (plate-cutout plate-left :extra-top-row true) (translate [0 0 -30]))
                                                           thumbcaps
                                                           (caps :extra-top-row true)
                                                           wrist-rest-build
                                                           )))))
          ]
      (cond (not= old new) (spit file new))
      )))

(defn spit-left-test []
  (future
    (let [
          file "things/left-test.scad"
          old (try (slurp file) (catch Exception e))
          new (write-scad
                (mirror [-1 0 0]
                      (union model-left
                             (->> (plate-cutout plate-left :extra-top-row true) (translate [0 0 -30]))
                             thumbcaps
                             (caps :extra-top-row true)
                             wrist-rest-build
                             )))
          ]
      (cond (not= old new) (spit file new))
)))
(defn spit-left []
  (future
    (let [
          file "things/left.scad"
          old (try (slurp file) (catch Exception e))
          new (write-scad (mirror [-1 0 0] model-left))
          ]
      (cond (not= old new) (spit file new))
)))
(defn spit-left-plate []
  (future
    (let [
          file "things/left-plate.scad"
          old (try (slurp file) (catch Exception e))
          new (write-scad (->> (plate-cutout plate-left :extra-top-row true) (mirror [-1 0 0])))
          ]
      (cond (not= old new) (spit file new))
)))
(defn spit-left-palm-rest []
  (future
    (let [
          file "things/left-palm-rest.scad"
          old (try (slurp file) (catch Exception e))
          new (write-scad (mirror [-1 0 0] wrist-rest-build))
          ]
      (cond (not= old new) (spit file new))
)))
(defn spit-right-test []
  (future
    (let [
          file "things/right-test.scad"
          old (try (slurp file) (catch Exception e))
          new (write-scad
                (union (difference model-right
                                   trackball-cutouts
                                   )
                       (->> (plate-cutout plate-right) (translate [0 0 -30]))
                       thumbcaps
                       (caps)
                       wrist-rest-build
                       trackball-top
                       trackball-side
                       ))
          ]
      (cond (not= old new) (spit file new))
)))
(defn spit-right []
  (future
    (let [
          file "things/right.scad"
          old (try (slurp file) (catch Exception e))
          new (write-scad
                (union (difference model-right
                                   trackball-cutouts
                                   )
                       trackball-top
                       trackball-side
                       ))
          ]
      (cond (not= old new) (spit file new))
)))
(defn spit-right-plate []
  (future
    (let [
          file "things/right-plate.scad"
          old (try (slurp file) (catch Exception e))
          new (write-scad (plate-cutout plate-right))
          ]
      (cond (not= old new) (spit file new))
)))
(defn spit-right-palm-rest []
  (future
    (let [
          file "things/right-palm-rest.scad"
          old (try (slurp file) (catch Exception e))
          new (write-scad wrist-rest-build)
          ]
      (cond (not= old new) (spit file new))
)))

(defn -main [& args]
  (if (seq args)
    (let [arg-to-func (fn [arg] (case arg
                                  "all-test" (spit-all-test)
                                  "left-test" (spit-left-test)
                                  "left" (spit-left)
                                  "left-plate" (spit-left-plate)
                                  "left-palm-rest" (spit-left-palm-rest)
                                  "right-test" (spit-right-test)
                                  "right" (spit-right)
                                  "right-plate" (spit-right-plate)
                                  "right-palm-rest" (spit-right-palm-rest)
                                  "everything" (conj nil (spit-all-test) (spit-left-test) (spit-left) (spit-left-plate) (spit-left-palm-rest) (spit-right-test) (spit-right) (spit-right-plate) (spit-right-palm-rest))
                                  ))
          work-items (flatten (map arg-to-func args))
          ]
      (dorun work-items)
      (println "threads started, please wait")
      (run! deref work-items)
      (println "threads ended")
      (shutdown-agents)
      ))
  )
