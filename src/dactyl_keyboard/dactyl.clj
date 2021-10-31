(ns dactyl-keyboard.dactyl
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
(def ncols 6)

(def α (/ π 12))                        ; curvature of the columns
(def β (/ π 36))                        ; curvature of the rows
(def centerrow (- nrows 3))             ; controls front-back tilt
(def centercol 4)                       ; controls left-right tilt / tenting (higher number is more tenting)
(def tenting-angle (/ π 9))            ; or, change this for more precise tenting control

(def first-15u-row 0)                   ; controls which should be the first row to have 1.5u keys on the outer column
(def last-15u-row 2)                    ; controls which should be the last row to have 1.5u keys on the outer column

(def column-style :standard)

(defn column-offset [column]
    (cond (= column 2) [0 2.82 -4.5]
          (>= column 4) [0 -12 5.64]    ; original [0 -5.8 5.64]
          :else [0 0 0]))

(def thumb-offsets [8 -4 7])

(def keyboard-z-offset 15)               ; controls overall height; original=9 with centercol=3; use 16 for centercol=2

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
(def encoder-height 14.15)
(def encoder-width 16.15)

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
(def encoder-plate
  (let [plate-thickness 8
        top-wall (->> (cube (+ encoder-width 3) 1.5 (+ plate-thickness 0.5))
                      (translate [0
                                  (+ (/ 1.5 2) (/ encoder-height 2))
                                  (- (/ plate-thickness 2) 0.25)]))
        left-wall (->> (cube 1.8 (+ encoder-height 3) (+ plate-thickness 0.5))
                       (translate [(+ (/ 1.8 2) (/ encoder-width 2))
                                   0
                                   (- (/ plate-thickness 2) 0.25)]))
        plate-half (union top-wall left-wall)
        bridge (->> (cube 8 (+ encoder-height 3) (/ plate-thickness 3))
                    (translate [0 0 (- plate-thickness (/ plate-thickness 6))]))
        ]
    (difference
     (union plate-half
            bridge
            (->> plate-half
                 (mirror [1 0 0])
                 (mirror [0 1 0])))
     )))

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
                 :when (or (.contains [2 3] column)
                           (not= row lastrow))]
             (->> single-plate (key-place column row)))
           ; mouse keys go here, the middle one uses a different plate for the encoder
           (if extra-top-row (->> single-plate (key-place 1 -1)))
           (if extra-top-row (->> encoder-plate (key-place 2 -1)))
           (if extra-top-row (->> single-plate (key-place 3 -1)))
           )))

(defn caps [& {:keys [extra-top-row] :or {extra-top-row false}}]
  (apply union
         (conj (for [column columns
                     row rows
                     :when (or (.contains [2 3] column)
                               (not= row lastrow))]
                 (->> (sa-cap 1)
                      (key-place column row)))
               (if extra-top-row (->> (sa-cap 1) (key-place 1 -1)))
               ;not a cap, need to put an encoder wheel here) XXX
               ;(if extra-top-row (->> (sa-cap 1) (key-place 2 -1)))
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
               (if extra-top-row (key-place 1 -1 keyhole-fill))
               (if extra-top-row (key-place 2 -1 keyhole-fill))
               (if extra-top-row (key-place 3 -1 keyhole-fill))
               (list (key-place 0 0 keyhole-fill)
                     (key-place 0 1 keyhole-fill)
                     (key-place 0 2 keyhole-fill)))))

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
            (concat
              ;; Row connections
              (for [column (range 1 3)
                    row [-1]]
                (triangle-hulls
                  (key-place (inc column) row web-post-tl)
                  (key-place column row web-post-tr)
                  (key-place (inc column) row web-post-bl)
                  (key-place column row web-post-br)))
              ;; Column connections
              (for [column (range 1 4)
                    row [-1]]
                (triangle-hulls
                  (key-place column row web-post-bl)
                  (key-place column row web-post-br)
                  (key-place column (inc row) web-post-tl)
                  (key-place column (inc row) web-post-tr)))
              ;; Diagonal connections
              (for [column (range 1 3)
                    row [-1]]
                (triangle-hulls
                  (key-place column row web-post-br)
                  (key-place column (inc row) web-post-tr)
                  (key-place (inc column) row web-post-bl)
                  (key-place (inc column) (inc row) web-post-tl)))
              ))
          )))

; Thumb cluster
(def thumborigin
  (map + (key-position 1 cornerrow [(/ mount-width 2) (- (/ mount-height 2)) 0])
       thumb-offsets))

; My version of a 3-button cluster, I call it the micro cluster
(defn thumb-r-place [shape]
  (->> shape
       (rotate (deg2rad  18) [1 0 0])
       (rotate (deg2rad -10) [0 1 0])
       (rotate (deg2rad  10) [0 0 1]) ; original 10
       (translate thumborigin)
       (translate [-15 -10 5]))) ; original 1.5u  (translate [-12 -16 3])
(defn thumb-m-place [shape]
  (->> shape
       (rotate (deg2rad  14) [1 0 0])
       (rotate (deg2rad -18) [0 1 0])
       (rotate (deg2rad  25) [0 0 1]) ; original 10
       (translate thumborigin)
       (translate [-35 -16 0]))) ; original 1.5u (translate [-32 -15 -2])))
(defn thumb-l-place [shape]
  (->> shape
       (rotate (deg2rad  10) [1 0 0])
       (rotate (deg2rad -27) [0 1 0])
       (rotate (deg2rad  35) [0 0 1])
       (translate thumborigin)
       (translate [-51 -25 -8]))) ;        (translate [-51 -25 -12])))

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

(def thumb-post-tr (translate [(- (/ mount-width 2) post-adj)  (- (/ mount-height  2) post-adj) 0] web-post))
(def thumb-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height  2) post-adj) 0] web-post))
(def thumb-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
(def thumb-post-br (translate [(- (/ mount-width 2) post-adj)  (+ (/ mount-height -2) post-adj) 0] web-post))

(def thumb-connectors
  (union
   (triangle-hulls    ; top two
    (thumb-m-place web-post-tr)
    (thumb-m-place web-post-br)
    (thumb-r-place thumb-post-tl)
    (thumb-r-place thumb-post-bl))
   (triangle-hulls
    (thumb-m-place web-post-tl)
    (thumb-l-place web-post-tr)
    (thumb-m-place web-post-bl)
    (thumb-l-place web-post-br)
    (thumb-m-place web-post-bl)
    )
   (triangle-hulls    ; top two to the main keyboard, starting on the left
    (thumb-m-place web-post-tl)
    (key-place 0 cornerrow web-post-bl)
    (thumb-m-place web-post-tr)
    (key-place 0 cornerrow web-post-br)
    (thumb-r-place thumb-post-tl)
    (key-place 1 cornerrow web-post-bl)
    (thumb-r-place thumb-post-tr)
    (key-place 1 cornerrow web-post-br)
    (key-place 2 lastrow web-post-tl)
    (key-place 2 lastrow web-post-bl)
    (thumb-r-place thumb-post-tr)
    (key-place 2 lastrow web-post-bl)
    (thumb-r-place thumb-post-br)
    (key-place 2 lastrow web-post-br)
    (key-place 3 lastrow web-post-bl)
    (key-place 2 lastrow web-post-tr)
    (key-place 3 lastrow web-post-tl)
    (key-place 3 cornerrow web-post-bl)
    (key-place 3 lastrow web-post-tr)
    (key-place 3 cornerrow web-post-br)
    )
   ; this makes no sense here, it's not for the thumb cluster XXX
   (triangle-hulls
    (key-place 1 cornerrow web-post-br)
    (key-place 2 lastrow web-post-tl)
    (key-place 2 cornerrow web-post-bl)
    (key-place 2 lastrow web-post-tr)
    (key-place 2 cornerrow web-post-br)
    (key-place 3 cornerrow web-post-bl))
   ))

;;;;;;;;;;
;; Case ;;
;;;;;;;;;;

(defn bottom [height p]
  (->> (project p)
       (extrude-linear {:height height :twist 0 :convexity 0})
       ; this was in the original, but it just extends everything below the xy plane and we later need to cut it off
       ;(translate [0 0 (- (/ height 2) 10)])
       ))

(defn bottom-hull [& p]
  (hull p (bottom 0.001 p)))

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

(def thumb-wall
  (union
   ; thumb walls
   ; left back
   (wall-brace thumb-l-place  0  1 web-post-tr thumb-l-place  0  1 web-post-tl)
   ; left side
   (wall-brace thumb-l-place -1  0 web-post-tl thumb-l-place -1  0 web-post-bl)
   ; front walls
   (wall-brace thumb-r-place  0 -1 web-post-br thumb-r-place  0 -1 thumb-post-bl)
   (wall-brace thumb-m-place  0 -1 web-post-br thumb-m-place  0 -1 web-post-bl)
   (wall-brace thumb-m-place  0 -1 web-post-br thumb-m-place  0 -1 web-post-bl)
   (wall-brace thumb-l-place  0 -1 web-post-br thumb-l-place  0 -1 web-post-bl)
   ; thumb corners
   (wall-brace thumb-l-place -1  0 web-post-bl thumb-l-place  0 -1 web-post-bl)
   (wall-brace thumb-l-place -1  0 web-post-tl thumb-l-place  0  1 web-post-tl)
   ; thumb tweeners
   (wall-brace thumb-r-place  0 -1 web-post-bl thumb-m-place  0 -1 web-post-br)
   (wall-brace thumb-m-place  0 -1 web-post-bl thumb-l-place  0 -1 web-post-br)
   (wall-brace thumb-r-place  0 -1 thumb-post-br (partial key-place 3 lastrow)  0 -1 web-post-bl)
   ; clunky bit on the top left thumb connection  (normal connectors don't work well)
   (bottom-hull
    (left-key-place cornerrow -1 (translate (wall-locate2 -1 0) web-post))
    (left-key-place cornerrow -1 (translate (wall-locate3 -1 0) web-post))
    (thumb-l-place (translate (wall-locate2 -0.3 1) web-post-tr))
    (thumb-l-place (translate (wall-locate3 -0.3 1) web-post-tr)))
   (hull
    (left-key-place cornerrow -1 (translate (wall-locate2 -1 0) web-post))
    (left-key-place cornerrow -1 (translate (wall-locate3 -1 0) web-post))
    (thumb-l-place (translate (wall-locate2 -0.3 1) web-post-tr))
    (thumb-l-place (translate (wall-locate3 -0.3 1) web-post-tr))
    (thumb-m-place web-post-tl))
   (hull
    (left-key-place cornerrow -1 web-post)
    (left-key-place cornerrow -1 (translate (wall-locate1 -1 0) web-post))
    (left-key-place cornerrow -1 (translate (wall-locate2 -1 0) web-post))
    (left-key-place cornerrow -1 (translate (wall-locate3 -1 0) web-post))
    (thumb-m-place web-post-tl))
   (hull
    (left-key-place cornerrow -1 web-post)
    (left-key-place cornerrow -1 (translate (wall-locate1 -1 0) web-post))
    (key-place 0 cornerrow web-post-bl)
    (thumb-m-place web-post-tl))
   (hull
    (thumb-l-place web-post-tr)
    (thumb-l-place (translate (wall-locate1 -0.3 1) web-post-tr))
    (thumb-l-place (translate (wall-locate2 -0.3 1) web-post-tr))
    (thumb-l-place (translate (wall-locate3 -0.3 1) web-post-tr))
    (thumb-m-place web-post-tl))
   ))

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
        (key-wall-brace 0 0 0 1 web-post-tl 0 0 0 1 web-post-tr)
        (color [1 1 0 1] (key-wall-brace 0 0 0 1 web-post-tr 1 -1 0 1 web-post-tl))
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
        (color [1 1 0 1] (key-wall-brace 4 0 0 1 web-post-tr 5 0 0 1 web-post-tl))
        (key-wall-brace 5 0 0 1 web-post-tl 5 0 0 1 web-post-tr)
       )
      (vector
        (for [x (range 0 2)    ] (key-wall-brace x 0 0 1 web-post-tl x       0 0 1 web-post-tr))
        (for [x (range 3 ncols)] (key-wall-brace x 0 0 1 web-post-tl x       0 0 1 web-post-tr))
        (for [x (range 1 2)    ] (key-wall-brace x 0 0 1 web-post-tl (dec x) 0 0 1 web-post-tr))
        (for [x (range 4 ncols)] (key-wall-brace x 0 0 1 web-post-tl (dec x) 0 0 1 web-post-tr))
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
  (key-wall-brace 3 lastrow  0 -1 web-post-bl 3 lastrow   0 -1 web-post-br)
  (key-wall-brace 3 lastrow  0 -1 web-post-br 4 cornerrow 0 -1 web-post-bl)
  (for [x (range 4 ncols)] (key-wall-brace x cornerrow 0 -1 web-post-bl x       cornerrow 0 -1 web-post-br))
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
   (->> (binding [*fn* 30]
                 (cylinder [bottom-radius top-radius] height)))))

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

(def screw-offset-tr [-3 8 bottom-plate-thickness])
(def screw-offset-br [-10 11.5 bottom-plate-thickness])
(def screw-offset-tl [6.2 10.4 bottom-plate-thickness])
(def screw-offset-bl [-3 5.5 bottom-plate-thickness])
(def screw-offset-tm [9.5 -4.5 bottom-plate-thickness])
(def screw-offset-bm [0 12 bottom-plate-thickness])

(defn screw-insert-all-shapes [bottom-radius top-radius height]
  (union (screw-insert 0 0        bottom-radius top-radius height screw-offset-tl [1 0 0]) ; red
         (screw-insert 0 lastrow  bottom-radius top-radius height screw-offset-bl [1 1 0]) ; yellow
         ; FIXME later
         ;(screw-insert 2 0        bottom-radius top-radius height screw-offset-tm [0 0 1]) ; blue
         (screw-insert 1 lastrow  bottom-radius top-radius height screw-offset-bm [1 0 1]) ; fuchsia
         (screw-insert lastcol 0        bottom-radius top-radius height screw-offset-tr [0 1 1]) ; aqua
         (screw-insert lastcol lastrow  bottom-radius top-radius height screw-offset-br [0 1 0]) ; green
         )
  )

; Hole Depth Y: 4.4
(def screw-insert-height 6)

; Hole Diameter C: 4.1-4.4
(def screw-insert-bottom-radius (/ 4.0 2))
(def screw-insert-top-radius (/ 3.9 2))
(def screw-insert-holes  (screw-insert-all-shapes screw-insert-bottom-radius screw-insert-top-radius screw-insert-height))

; Wall Thickness W:\t1.65
(def screw-insert-outers (screw-insert-all-shapes (+ screw-insert-bottom-radius 1.65) (+ screw-insert-top-radius 1.65) (+ screw-insert-height 1)))
(def screw-insert-screw-holes  (screw-insert-all-shapes 1.7 1.7 350))
(def plate-screw-recess  (screw-insert-all-shapes 3.1 1.95 2.1)) ;; creates the recess for screws in bottom plate

; Wrist rest cutout for https://github.com/crystalhand/dactyl-keyboard.git
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
                         ;; TODO: figure out why these glitch in the OpenSCAD view, it annoys me.
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
    (scale [1 1 1] ;;;;scale the wrist rest to the final size after it has been cut
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
           ))
  )

; These cut out the holes on the case side (so screw head sits flush), as well
; as the cube to receive the nut on the connector itself.
(def rest-case-cuts
  (union
    ;;right cut
    (->> (cylinder 1.85 50)(with-fn 30) (rotate  (/  π 2)  [1 0 0])(translate [right_wrist_connecter_x 23.5 4.5]))
    (->> (cylinder 2.9 5.2)(with-fn 50) (rotate  (/  π 2)  [1 0 0])(translate [right_wrist_connecter_x 31.5 4.5]))
    (->> (cube 6 3 12.2)(translate [right_wrist_connecter_x 19.0 1.5]))
    ;;middle cut
    (->> (cylinder 1.85 50)(with-fn 30) (rotate  (/  π 2)  [1 0 0])(translate [middle_wrist_connecter_x 20 4.5]))
    (->> (cylinder 2.9 5.2)(with-fn 50) (rotate  (/  π 2)  [1 0 0])(translate [middle_wrist_connecter_x 34.0 4.5]))
    (->> (cube 6 3 12.2)(translate [middle_wrist_connecter_x 19.0 1.5]))
    ;;left
    (->> (cylinder 1.85 60)(with-fn 30) (rotate  (/  π 2)  [1 0 0])(translate [left_wrist_connecter_x 21 4.5]))
    (->> (cylinder 2.9 5.2)(with-fn 50) (rotate  (/  π 2)  [1 0 0])(translate [left_wrist_connecter_x (+ 29.5 nrows) 4.5]))
    (->> (cube 6 3 12.2)(translate [left_wrist_connecter_x 19.0 1.5]))
    )
)

(def rest-case-connectors
  (difference
    (union
      (scale [1 1 1.6] (->> (cylinder 6 60)(with-fn 200) (rotate  (/  π 2)  [1 0 0])(translate [right_wrist_connecter_x 0 0])))
      (scale [1 1 1.6] (->> (cylinder 6 60)(with-fn 200) (rotate  (/  π 2)  [1 0 0])(translate [middle_wrist_connecter_x 0 0])))
      (scale [1 1 1.6] (->> (cylinder 6 60)(with-fn 200) (rotate  (/  π 2)  [1 0 0])(translate [left_wrist_connecter_x 0 0])))
      )
    )
)

(def wrest-wall-cut
  (->> (for [xyz (range 1.00 10 3)];controls the scale last number needs to be lower for thinner walls
         (union
           (translate[1, xyz,1] (case-walls))
           ;(translate [0 0 -3])
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
    (_translate rest-case-cuts)
    wrest-wall-cut
    (_translate cutout)
    )
  )
  )

; put it all together

(def model-right (difference
                   (union
                     (key-holes)
                     (connectors)
                     thumb
                     thumb-connectors
                     (difference (union (case-walls)
                                        screw-insert-outers)
                                 ;usb-holder-space
                                 ;trrs-notch
                                 ;usb-holder-notch
                                 (if (== wrist-rest-on 1) (->> rest-case-cuts (translate [wrist-translate-x (- (second thumborigin) (- 56 nrows)) 0])))
                                 screw-insert-holes))
                   ;(translate [0 0 -20] (cube 350 350 40))
                   ))

(def model-left (difference
                  (union
                    (key-holes :extra-top-row true)
                    (connectors :extra-top-row true)
                    thumb
                    thumb-connectors
                    (difference (union (case-walls :extra-top-row true)
                                       screw-insert-outers)
                                (if (== wrist-rest-on 1) (->> rest-case-cuts (translate [wrist-translate-x (- (second thumborigin) (- 56 nrows)) 0])))
                                screw-insert-holes))
                  ;(translate [0 0 -20] (cube 350 350 40))
                  ))

; Cut away the walls from the bottom plate again, so it recedes fully. Requires sufficient keyboard-z-offset.
(defn plate-cutout [shape & {:keys [extra-top-row] :or {extra-top-row false}}]
  (union
    (difference
      shape
      (translate [0 0 -10] screw-insert-screw-holes)
      (translate [0 0 -3.4] plate-screw-recess)
      (union
        (for [xy (range 0.998 1.14 0.02)]
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
                thumb-connectors
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
                thumb-connectors
                (case-walls :extra-top-row true)
                thumbcaps-fill
                (caps-fill :extra-top-row true)
                screw-insert-outers)
              )))
        (translate [0 0 -10] screw-insert-screw-holes)
        (translate [0 0 -3.4] plate-screw-recess)
        ))


(def newtrack
  (let [r 17
        outer-r (+ r 4)
        d (/ r 2)
        c (* r 3)
        pimple (fn [r] (->> (sphere r)
                            (with-fn 40)))
        cutout (hull (union
                      (->> (cylinder 2 6)(translate [0 2 0]))
                      (->> (cylinder 2 6)(translate [0 -2 0]))
                ))
        bowl (union
               (difference
                 (fa! 1)
                 (fs! 1)
                 (sphere (+ r 2))
                 (sphere (+ r 0.1))
                 (->> (cube c c c)
                      (translate [0 0 (/ c 2)]))
                 (for [deg '(0 120 240)] (->>
                                            cutout
                                            (translate [0 0 r])
                                            (rotate (deg2rad 120) [1 0 0])
                                            (rotate (deg2rad deg) [0 0 1])
                                            ))
                 (->> (sphere 6)(translate [0 0 (* r -1)]))
                 )
               (for [x [[0 1.1] [120 1.2] [240 1.3]]] (->>
                                         (pimple (second x))
                                         (translate [0 0 (+ r 0.6)])
                                         (rotate (deg2rad 135) [1 0 0])
                                         (rotate (deg2rad (+ (first x) 60)) [0 0 1])
                                         )
                 )
               )
        ring (->>
               (difference
                 (fa! 1)
                 (fs! 1)
                 (cylinder outer-r 2)
                 (cylinder (+ r 1) 4)
                 )
               (translate [0 0 -1]))
        len 40
        cyl (->>
              (difference
                 (fa! 1)
                 (fs! 1)
                 (cylinder (+ outer-r 2) len)
                 (cylinder outer-r (+ len 1))
                 )
               (translate [0 0 (/ len -2)]))
        ]
    (union
      bowl
      ring
      cyl
      )
    ))


(def trackball
        (union
          newtrack
          (->> (sphere 17) (color [1 1 1 1]))
          ))


; Trackball on the top/back of keyboard.

; TODO: add test? var, add print out ball if set, set it in local scope for the test print, print both halves there.

(def trackball-top
  (union (->> trackball
              (translate [-30 55 30])
              (color [1 0 0 1]))
         ;(difference trackball-top
         ;            ;(hull (union back-wall (->> (cube 1 1 1)(translate [0 -100 0]))))
         ;            (->> (cube 100 100 100)(translate [0 0 -50]))
         ;            )
         ))

(defn move-sideball [shape]
  (->> shape
       (translate [-100 -25 60])
       ))
(def trackball-side
  (union (->> (move-sideball trackball) (color [0 0 1 1]))
         ;(difference trackball-side-holder (hull (union left-wall (cube 1 1 1))))
         ))

;(spit "things/all-test.scad"
;      (write-scad (union
;                    (translate [130 0 0] (union model-right
;                                                (->> plate-right (translate [0 0 -30]))
;                                                thumbcaps
;                                                (caps)
;                                                wrist-rest-build
;                                                trackball-top
;                                                trackball-side
;                                                ))
;                    (translate [-130 0 0] (mirror [-1 0 0] (union
;                                                             model-left
;                                                             (->> plate-left (translate [0 0 -30]))
;                                                             thumbcaps
;                                                             (caps :extra-top-row true)
;                                                             wrist-rest-build
;                                                             ))))))

(spit "things/right-test.scad"
      (write-scad (union model-right
                         ;thumbcaps
                         ;(caps)
                         ;wrist-rest-build
                         trackball-top
                         trackball-side
                         )))


;(spit "things/left-test.scad"
;      (write-scad (mirror [-1 0 0]
;                          (union model-left
;                                 (->> plate-left (translate [0 0 -30]))
;                                 thumbcaps
;                                 (caps :extra-top-row true)
;                                 wrist-rest-build
;                                 ))))

;(spit "things/right.scad"
;      (write-scad (union model-right trackball-top trackball-side)))
;
;(spit "things/left.scad"
;      (write-scad (mirror [-1 0 0] model-left)))
;
;(spit "things/right-plate.scad"
;      (write-scad plate-right))
;
;(spit "things/left-plate.scad"
;      (write-scad (mirror [-1 0 0] plate-left)))
;
;(spit "things/right-wrist-rest.scad"
;      (write-scad wrist-rest-build))
;
;(spit "things/left-wrist-rest.scad"
;      (write-scad (scale [-1,1,1] wrist-rest-build)))

(defn -main [dum] 1)  ; dummy to make it easier to batch
