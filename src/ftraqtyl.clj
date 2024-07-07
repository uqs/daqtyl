(ns ftraqtyl
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all])
  (:load "trackball"))

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

(defn column-offset [column]
    (cond (= column 0) [1.35 0 0.3]
          (= column 2) [0 2.82 -4.5]
          (>= column 4) [1 -14.5 5.64]    ; original [0 -5.8 5.64]
          :else [0 0 0]))

(defn column-rotation [column]
    (cond (= column 0) 0
          (= column 2) 0
          (>= column 4) (deg2rad 3)
          :else 0))

(defn column-twist [column]
    (cond (= column 0) (deg2rad 5)
          (= column 2) 0
          (>= column 4) 0
          :else 0))

(defn column-splay [column]
    (cond (= column 0) 0
          (= column 2) 0
          (>= column 4) (deg2rad -1)
          :else 0))

(def thumb-offsets [10 -4 7])

; controls overall height; original=9 with centercol=3; use 16 for centercol=2
(def keyboard-z-offset (cond (= ncols 6) 9
                             (= ncols 5) 3))

(def extra-width 2.5)                   ; extra space between the base of keys; original= 2
(def extra-height 0.5)                  ; original= 0.5

(def wall-z-offset -8)                 ; length of the first downward-sloping part of the wall (negative)
(def wall-xy-offset 5)                  ; offset in the x and/or y direction for the first downward-sloping part of the wall (negative)
(def wall-thickness 2)                  ; wall thickness parameter; originally 5

; If you use Cherry MX or Gateron switches, this can be turned on.
; If you use other switches such as Kailh, you should set this as false
(def create-side-nubs? true)

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
(def retention-tab-thickness 1.3)
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
                      (translate [(+ (/ keyswitch-width 1.95)) 0 1])
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
             )))

(spit "things/encoder-test.scad"
      (write-scad encoder-plate))

(def encoder-cap
  (->> (cylinder 6 15)
       (rotate (deg2rad 90) [0 1 0])
       (translate [0 0 10])
       (color [0 1 0 0.3])
       )
  )

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

(def slim-web-post (->> (cube post-size post-size (/ web-thickness 2))
                        (translate [0 0 (+ (/ web-thickness -4)
                                           plate-thickness)])))
; These allow for a tighter spacing of keys to not collide with keycaps
(def slim-web-post-tr (translate [(- (/ mount-width 1.95) post-adj) (- (/ mount-height 1.95) post-adj) 0] slim-web-post))
(def slim-web-post-tl (translate [(+ (/ mount-width -1.95) post-adj) (- (/ mount-height 1.95) post-adj) 0] slim-web-post))
(def slim-web-post-br (translate [(- (/ mount-width 1.95) post-adj) (+ (/ mount-height -1.95) post-adj) 0] slim-web-post))
(def slim-web-post-bl (translate [(+ (/ mount-width -1.95) post-adj) (+ (/ mount-height -1.95) post-adj) 0] slim-web-post))

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

(defn apply-key-geometry [translate-fn rotate-x-fn rotate-y-fn rotate-z-fn column row shape]
  (let [column-angle (* β (- centercol column))
        placed-shape (->> shape
                          (rotate-y-fn  (column-twist column))
                          (translate-fn [0 0 (- row-radius)])
                          (rotate-x-fn  (* α (- centerrow row)))
                          (translate-fn [0 0 row-radius])
                          (translate-fn [0 0 (- column-radius)])
                          (rotate-y-fn  column-angle)
                          (translate-fn [0 0 column-radius])
                          (rotate-x-fn (column-rotation column))
                          (rotate-z-fn (column-splay column))
                          (translate-fn (column-offset column)))
        ]
    (->> placed-shape
         (rotate-y-fn  tenting-angle)
         (translate-fn [0 0 keyboard-z-offset]))))

(defn key-place [column row shape]
  (apply-key-geometry translate
                      (fn [angle obj] (rotate angle [1 0 0] obj))
                      (fn [angle obj] (rotate angle [0 1 0] obj))
                      (fn [angle obj] (rotate angle [0 0 1] obj))
                      column row shape))

; NOTE: could merge with key-place, but need to know about left/right side then ...
(defn enc-place [column row shape]
  (let [shape-or-post (cond (= shape slim-web-post-tl) (->> shape (translate [0 0 -3]))
                            (= shape slim-web-post-br) (->> shape (translate [0 0 -3]))
                            (= shape slim-web-post-tr) (->> shape (translate [0 0 -3]))
                            (= shape slim-web-post-bl) (->> shape (translate [0 0 -3]))
                            (= shape web-post-tl) (->> shape (translate [-1 0 0]))
                            (= shape web-post-tr) (->> shape (translate [1 0 0]))
                            (= shape web-post-br) (->> shape (translate [1 0 0]))
                            (= shape web-post-bl) (->> shape (translate [-1 0 0]))
                            :else shape)]
    (cond (= row -1) (->> shape-or-post
                          (translate [0 0 8])
                          (rotate (deg2rad -10)[1 0 0])
                          (key-place column row))
          (= row 3) (->> shape-or-post
                         (rotate (deg2rad 10)[1 0 0])
                         (rotate (deg2rad -10)[0 1 0])
                         (rotate (deg2rad -3)[0 0 1])
                         (translate [-1.5 -4 12])
                         (key-place column row))
          :else (key-place column row shape-or-post)
          )))

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

(defn rotate-around-z [angle position]
  (mmul
   [[(Math/cos angle) (- (Math/sin angle)) 0]
    [(Math/sin angle) (Math/cos angle)     0]
    [0 0 1]]
   position))

(defn key-position [column row position]
  (apply-key-geometry (partial map +) rotate-around-x rotate-around-y rotate-around-z column row position))

(defn key-holes [& {:keys [extra-encoder] :or {extra-encoder false}}]
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
           (if extra-encoder
             (->> encoder-plate (mirror [-1 0 0]) (enc-place 2 lastrow))
             (->> single-plate (key-place 2 lastrow)))
           )))

(defn caps [& {:keys [extra-encoder] :or {extra-encoder false}}]
  (apply union
         (conj (for [column columns
                     row rows
                     :when (or (.contains [3] column)
                               (not= row lastrow))]
                 (->> (sa-cap 1) (key-place column row)))
               (if extra-encoder (->> encoder-cap (enc-place 2 lastrow))
                 (->> (sa-cap 1) (key-place 2 lastrow)))
               )))

; only used to project the shadow on the bottom plate
(defn caps-fill [& {:keys [extra-encoder] :or {extra-encoder false}}]
  (apply union
         (conj (for [column columns
                     row rows
                     :when (or (.contains [3] column)
                               (not= row lastrow))]
                 (key-place column row keyhole-fill))
               (if extra-encoder (enc-place 2 lastrow encoder-fill)
                 (key-place 2 lastrow keyhole-fill))
               )))

(defn triangle-hulls [& shapes]
  (apply union
         (map (partial apply hull)
              (partition 3 1 shapes))))

(def connectors
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
          )))

; Thumb cluster
(def thumborigin
  (map + (key-position 1 cornerrow [(/ mount-width 2) (- (/ mount-height 2)) 0])
       thumb-offsets))

; My version of a 3-button cluster, I call it the micro cluster
(defn thumb-r-place [shape]
  (->> shape
       (rotate (deg2rad  18) [1 0 0])
       (rotate (deg2rad  -5) [0 1 0])
       (rotate (deg2rad  17) [0 0 1])
       (translate thumborigin)
       (translate [-14 -7.2 3])))
(defn thumb-m-place [shape & {:keys [offset] :or {offset [0 0 0]}}]
  (->> shape
       (rotate (deg2rad  18.5) [1 0 0])
       (rotate (deg2rad   0) [0 1 0])
       (rotate (deg2rad  25.5) [0 0 1])
       (translate thumborigin)
       (translate offset)
       (translate [-34 -15 2.0])))
(defn thumb-l-place [shape]
  (->> shape
       (rotate (deg2rad  19) [1 0 0])
       (rotate (deg2rad   5) [0 1 0])
       (rotate (deg2rad  32.5) [0 0 1])
       (translate thumborigin)
       (translate [-52.7 -25.4 2.7])))

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

(def left-wall-x-offset 2)  ; TODO reduce this further, but that needs some wall fixes
(def left-wall-z-offset 1)

(defn left-key-position [row direction]
  (map - (key-position 0 row [(* mount-width -0.5) (* direction mount-height 0.5) 0]) [left-wall-x-offset 0 left-wall-z-offset]) )

(defn left-key-place [row direction shape]
  (translate (left-key-position row direction) shape))

(defn wall-locate1 [dx dy] [(* dx wall-thickness) (* dy wall-thickness) -1])
(defn wall-locate2 [dx dy] [(* dx wall-xy-offset) (* dy wall-xy-offset) wall-z-offset])
(defn wall-locate3 [dx dy] [(* dx (+ wall-xy-offset wall-thickness)) (* dy (+ wall-xy-offset wall-thickness)) wall-z-offset])

(defn wall-brace [place1 dx1 dy1 post1 place2 dx2 dy2 post2
                  & {:keys [topoffset bottomoffset] :or {topoffset [[0 0 0] [0 0 0]] bottomoffset [0 0]}}]
  (union
   (hull
    (->> (place1 post1) (translate (first topoffset)))
    (->> (place1 (translate (wall-locate1 dx1 dy1) post1)) (translate (first topoffset)))
    ; bottomoffset only applies to the bottom
    (place1 (translate (wall-locate2 dx1 (- dy1 (first bottomoffset))) post1))
    (place1 (translate (wall-locate3 dx1 (- dy1 (first bottomoffset))) post1))
    (->> (place2 post2) (translate (last topoffset)))
    (->> (place2 (translate (wall-locate1 dx2 dy2) post2)) (translate (last topoffset)))
    ; bottomoffset only applies to the bottom
    (place2 (translate (wall-locate2 dx2 (- dy2 (last bottomoffset))) post2))
    (place2 (translate (wall-locate3 dx2 (- dy2 (last bottomoffset))) post2))
    )
   ; bottomoffset only applies to the bottom
   (bottom-hull
     (place1 (translate (wall-locate2 dx1 (- dy1 (first bottomoffset))) post1))
     (place1 (translate (wall-locate3 dx1 (- dy1 (first bottomoffset))) post1))
     (place2 (translate (wall-locate2 dx2 (- dy2 (last bottomoffset))) post2))
     (place2 (translate (wall-locate3 dx2 (- dy2 (last bottomoffset))) post2))
    )
   ))

(defn key-wall-brace [x1 y1 dx1 dy1 post1 x2 y2 dx2 dy2 post2
                      & {:keys [topoffset bottomoffset] :or {topoffset [[0 0 0] [0 0 0]] bottomoffset [0 0]}}]
  (wall-brace (partial key-place x1 y1) dx1 dy1 post1
              (partial key-place x2 y2) dx2 dy2 post2
              :bottomoffset bottomoffset :topoffset topoffset
              ))

(defn enc-wall-brace [place1 dx1 dy1 post1 place2 dx2 dy2 post2
                      & {:keys [topoffset bottomoffset] :or {topoffset [[0 0 0] [0 0 0]] bottomoffset [0 0]}}]
  (wall-brace (partial place1) dx1 dy1 post1
              (partial place2) dx2 dy2 post2
              :bottomoffset bottomoffset :topoffset topoffset
              ))

(def right-wall
  (let [
        startrow 0
        ]
    (union (key-wall-brace lastcol startrow 0 1 web-post-tr lastcol startrow 1 0 web-post-tr)
             (union (for [y (range startrow lastrow)] (key-wall-brace lastcol y 1 0 web-post-tr lastcol y 1 0 web-post-br))
                    (for [y (range (inc startrow) lastrow)] (key-wall-brace lastcol (dec y) 1 0 web-post-br lastcol y 1 0 web-post-tr)))
           (key-wall-brace lastcol cornerrow 0 -1 web-post-br lastcol cornerrow 1 0 web-post-br)
           )))

(defn thumb-connectors [& {:keys [encoder] :or {encoder false}}]
  (let [ place-func (cond encoder enc-place :else key-place) ]
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
         left-offset [-0.1 4.5 0.8]
         right-offset [1.2 6 -0.5]
         ]
     (union
       (color [0 1 1 1] (hull
        (left-key-place cornerrow -1 (translate (wall-locate1 -1 0) web-post))
        (left-key-place cornerrow -1 (translate (wall-locate2 -1 0) web-post))
        (left-key-place cornerrow -1 (translate (wall-locate3 -1 0) web-post))
        (thumb-m-place web-post-tl :offset left-offset)
        ))
       (color [0.8 0.5 0 1] (hull
        (left-key-place cornerrow -1 (translate (wall-locate3 -1 0) web-post))
        (thumb-m-place web-post-tl :offset left-offset)
        (thumb-m-place web-post-tl)
        ))
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
    (place-func 1 cornerrow slim-web-post-br)
    ; the full z-offset would make the keycap north of it collide when pressed.
    (place-func 2 lastrow slim-web-post-tl)
    (thumb-r-place web-post-tr)
    (place-func 2 lastrow web-post-bl)
    (thumb-r-place web-post-tr)
    (place-func 2 lastrow web-post-bl)
    (thumb-r-place web-post-br)
    (place-func 2 lastrow web-post-br)
    (key-place 3 lastrow web-post-bl)
    (place-func 2 lastrow web-post-tr)
    ))
   (hull
    (place-func 2 lastrow slim-web-post-tr)
    (place-func 2 lastrow web-post-tr)
    (place-func 2 lastrow slim-web-post-br)
    (place-func 2 lastrow web-post-br)
    (key-place 3 cornerrow web-post-bl)
    (key-place 3 lastrow web-post-tl)
    (key-place 3 lastrow web-post-bl))
   (color [1 0 0 1] (triangle-hulls
    (place-func 2 lastrow web-post-bl)
    (place-func 2 lastrow web-post-tl)
    (thumb-r-place web-post-tr)
    (place-func 2 lastrow slim-web-post-tl)
    ))
   (color [0 0 1 1] (triangle-hulls
    (place-func 2 lastrow web-post-tl)
    (place-func 2 lastrow slim-web-post-tl)
    (place-func 2 lastrow web-post-tr)
    ))
   ; second extra key to pinky column
   (triangle-hulls
     (key-place 3 lastrow web-post-tr)
     (key-place 3 lastrow web-post-br)
     (key-place 3 lastrow web-post-tr)
     (key-place 4 cornerrow web-post-bl))
   ; connect first extra key to regular matrix
   (color [1 0 1 1] (triangle-hulls
                      (place-func 1 cornerrow slim-web-post-br)
                      (place-func 2 lastrow slim-web-post-tl)
                      (place-func 2 cornerrow web-post-bl)
                      (place-func 2 lastrow slim-web-post-tl)
                      (place-func 2 cornerrow web-post-bl)
                      (place-func 2 lastrow slim-web-post-tr)
                      (place-func 2 cornerrow web-post-br)
                      (key-place 3 cornerrow web-post-bl)
                      (place-func 2 lastrow slim-web-post-tr)
                      (key-place 3 cornerrow web-post-bl)
                      (place-func 2 lastrow slim-web-post-tr)
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
   (wall-brace thumb-r-place  0 -1 web-post-br (partial key-place 3 lastrow)  0 -1 web-post-bl :bottomoffset [0 -1])
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


(def back-wall
  (union
    ;(for [x (range 0 ncols)] (key-wall-brace x 0 0 1 web-post-tl x       0 0 1 web-post-tr))
    ;(for [x (range 1 ncols)] (key-wall-brace x 0 0 1 web-post-tl (dec x) 0 0 1 web-post-tr))
      (vector
        (for [x (range 0 2)    ] (key-wall-brace x 0 0 1 web-post-tl x       0 0 1 web-post-tr))
        (key-wall-brace 3 0 0 1 web-post-tl 3       0 0 1 web-post-tr)
        (for [x (range 5 ncols)] (key-wall-brace x 0 0 1 web-post-tl x       0 0 1 web-post-tr))
        (for [x (range 1 2)    ] (key-wall-brace x 0 0 1 web-post-tl (dec x) 0 0 1 web-post-tr))
        (for [x (range 5 ncols)] (key-wall-brace x 0 0 1 web-post-tl (dec x) 0 0 1 web-post-tr))
        (key-wall-brace 3 0 0 1 web-post-tr 4 0 1 1 web-post-tl)
        (key-wall-brace 4 0 1 1 web-post-tl 4 0 0 1 web-post-tr)
        (color [0 1 0 1] (key-wall-brace 2 0 0 1 web-post-tl 1 0 0 1 web-post-tr :bottomoffset [0.43 0]))
        (color [1 0 0 1] (key-wall-brace 2 0 0 1 web-post-tl 2 0 0 1 web-post-tr :bottomoffset [0.43 0.43]))
        (color [0 0 1 1] (key-wall-brace 3 0 0 1 web-post-tl 2 0 0 1 web-post-tr :bottomoffset [0 0.43]))
      )))

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
  (key-wall-brace 3 lastrow  0 -1 web-post-bl 3 lastrow   0 -1 web-post-br :bottomoffset [-1 -1])
  (key-wall-brace 3 lastrow  0 -1 web-post-br 4 cornerrow 1 -1 web-post-bl :bottomoffset [-1 0])
  (key-wall-brace 4 cornerrow 1 -1 web-post-bl 4 cornerrow 0 -1 web-post-br)
  (for [x (range 5 ncols)] (key-wall-brace x cornerrow 0 -1 web-post-bl x       cornerrow 0 -1 web-post-br))
  (for [x (range 5 ncols)] (key-wall-brace x cornerrow 0 -1 web-post-bl (dec x) cornerrow 0 -1 web-post-br))
  ))

(def case-walls
  (union
   thumb-wall
   right-wall
   back-wall
   left-wall
   front-wall
   ))

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

(defn screw-insert-all-shapes [right bottom-radius top-radius height]
  (union (cond right
               (union)
               ;(screw-insert 0 0        bottom-radius top-radius height [5 -15.0 bottom-plate-thickness] [1 0 0]) ; red
               :else
               (screw-insert 0 0        bottom-radius top-radius height [9 10.0 bottom-plate-thickness] [1 0 0]) ; red
               )
         (screw-insert 0 lastrow  bottom-radius top-radius height [1.0 13 bottom-plate-thickness] [1 1 0]) ; yellow
         (cond right
               (screw-insert 2 0        bottom-radius top-radius height [-13 -7.5 bottom-plate-thickness] [0 0 1]) ; blue
               :else
               (screw-insert 2 0        bottom-radius top-radius height [7.5 -7.5 bottom-plate-thickness] [0 0 1]) ; blue
               )
         (screw-insert 1 lastrow  bottom-radius top-radius height [-50 0 bottom-plate-thickness] [1 0 1]) ; fuchsia
         (screw-insert lastcol 0        bottom-radius top-radius height [-21 (cond (= lastcol 4) 13 (= lastcol 5) 9) bottom-plate-thickness] [0 1 1]) ; aqua
         (screw-insert lastcol lastrow  bottom-radius top-radius height [-21 13.9 bottom-plate-thickness] [0 1 0]) ; green
         )
  )

; Hole Depth Y: 4.4
(def screw-insert-height 6)

; Hole Diameter C: 4.1-4.4
(def screw-insert-bottom-radius (/ 4.2 2))
(def screw-insert-top-radius (/ 4.0 2))
(defn screw-insert-holes [right] (->> (screw-insert-all-shapes right screw-insert-bottom-radius screw-insert-top-radius screw-insert-height)
                                      (translate [0 0 -0.01])))

; Wall Thickness W:  2.1--3.0
(defn screw-insert-outers [right] (screw-insert-all-shapes right (+ screw-insert-bottom-radius 3.0) (+ screw-insert-top-radius 2.1) (+ screw-insert-height 1.1)))
(defn screw-insert-screw-holes [right]  (screw-insert-all-shapes right 1.7 1.7 35))
(defn plate-screw-recess [right]  (screw-insert-all-shapes right 3.1 1.95 2.1)) ;; creates the recess for screws in bottom plate

; Wrist rest cutout from https://github.com/crystalhand/dactyl-keyboard.git
;;Wrist rest to case connections
(def wrist-rest-on 1)
(def wrist-rest-back-height 23)	;;height of the back of the wrist rest--Default 34
(def wrist-rest-angle 5) 	;;angle of the wrist rest--Default 20
(def wrist-rest-rotation-angle 9);;0 default The angle in counter clockwise the wrist rest is at
(def wrist-rest-ledge 0)	;;The height of ledge the silicone wrist rest fits inside
(def wrist-rest-y-angle 5)	;;0 Default.  Controls the wrist rest y axis tilt (left to right)

(def wrist-translate-x (+ (first thumborigin) 10))
(def right_wrist_connecter_x   (if (== ncols 5) 13 17))
(def middle_wrist_connecter_x  (if (== ncols 5) -5 -4))
(def left_wrist_connecter_x    (if (== ncols 5) -25 -25))
(def wrist_right_nut_y         (if (== ncols 5) 10 20.5))
(def wrist_brse_position_x -1)
(def wrist_brse_distance_y -40)     ;; Distance from wrist rest to keyboard

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
  (let [
        nut-cube (cube 5.7 2.5 12.2)
        ]
  (union
    ;;right cut
    (->> (cylinder 1.85 50)(with-fn 30) (rotate  (/  π 2)  [1 0 0])(translate [right_wrist_connecter_x 20 4.5]))
    (->> (cylinder 2.9 10)(with-fn 50) (rotate  (/  π 2)  [1 0 0])(translate [right_wrist_connecter_x 39.6 4.5]))
    (->> nut-cube (translate [right_wrist_connecter_x 22.0 1.5]))
    ;;middle cut
    (->> (cylinder 1.85 50)(with-fn 30) (rotate  (/  π 2)  [1 0 0])(translate [middle_wrist_connecter_x 20 4.5]))
    (->> (cylinder 2.9 10)(with-fn 50) (rotate  (/  π 2)  [1 0 0])(translate [middle_wrist_connecter_x 40.9 4.5]))
    (->> nut-cube (translate [middle_wrist_connecter_x 22.0 1.5]))
    ;;left
    (->> (cylinder 1.85 50)(with-fn 30) (rotate  (/  π 2)  [1 0 0])(translate [left_wrist_connecter_x 22 4.5]))
    (->> (cylinder 2.9 10)(with-fn 50) (rotate  (/  π 2)  [1 0 0])(translate [left_wrist_connecter_x 37.9 4.5]))
    (->> nut-cube (translate [left_wrist_connecter_x 20.0 1.5]))
    )
))

(def rest-case-connectors
  (difference
    (union
      (scale [1 1 1.6] (->> (cylinder 6 60)(with-fn 200) (rotate (/ π 2) [1 0 0])(translate [right_wrist_connecter_x 5 0])))
      (scale [1 1 1.6] (->> (cylinder 6 60)(with-fn 200) (rotate (/ π 2) [1 0 0])(translate [middle_wrist_connecter_x 5 0])))
      (scale [1 1 1.6] (->> (cylinder 6 60)(with-fn 200) (rotate (/ π 2) [1 0 0])(translate [left_wrist_connecter_x 5 0])))
      )
    )
)

(def wrest-wall-cut
  (->> (for [xyz (range 0 10 1)] ;controls the scale last number needs to be lower for thinner walls
         (union
           (translate [0 xyz 0] case-walls)
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
                          (intersection (->> (cube 100 65 60)(translate [-55 -40 30]))
                    (union
                      (key-holes :extra-encoder true)
                      connectors
                      (thumb-connectors :encoder true)
                      thumb
                      ))))
      )



; Trackball on the top/back side of keyboard.
(def trackball-top-height (+ 28 keyboard-z-offset))
(def trackball-top-pos [-28 (+ 30 trackball-outer-r) trackball-top-height])
(def trackball-top-cylinder-difference
    (->> (union
           (->> (cube 40 5 25)(rotate (deg2rad 30) [0 1 0]))
           (->>
             (sphere (/ trackball-outer-r 0.75))
             (scale [1 1 0.4])
             (translate [0 0 7])
             )
           )
         (translate trackball-top-pos)
         (translate [0 (- -0.3 trackball-outer-r) 0])))

(def trackball-top
  (union
    (difference
      (->> (trackholder (last trackball-top-pos) 0)
           (translate trackball-top-pos)
           (color [1 0 0 1]))
      trackball-top-cylinder-difference)
    ; to show the placement of the ball
    ;(->> (color [1 0 0 1] (sphere trackball-r))
    ;     (translate trackball-top-pos)
    ;     (translate [0 0 0.5]))
    )
  )

(spit "things/sensor-welltest.scad"
      (write-scad (intersection (->> (cube 55 80 60)(translate [-30 50 30]))
                                (union
                                  (difference
                                    (union
                                      (key-holes)
                                      connectors
                                      (thumb-connectors)
                                      thumb
                                      (difference case-walls
                                                  (->> sensor-cutout (translate trackball-top-pos)))
                                      )
                                    )
                                  (difference trackball-top (->> (hull back-wall)(translate [0 -3.3 -8])))
                                  )
                                ))
      )

; the cutout to slide into the wall
(def usb-holder (import "../things/usb_holder.stl"))
(def usb-holder-cutout (import "../things/usb_holder_cutout.stl"))

(def right-usb-cutout
  (let [ holder (union usb-holder-cutout (->> usb-holder-cutout (translate [0 0 (* -1 bottom-plate-thickness)]))) ]
  (->> (union
         holder
         (->> holder (translate [0 0.2 0]))
         (->> holder (translate [0.2 0 0]))
         (->> holder (translate [0.2 0.2 0]))
         )
       (rotate (deg2rad 0)[0 0 1])(rotate (deg2rad 180)[0 1 0]) (translate [-77.50 39.10 15.28]))
  ))

; put it all together
(def model-right (difference
                   (union
                     (key-holes)
                     connectors
                     (thumb-connectors)
                     thumb
                     (difference (union case-walls
                                        (screw-insert-outers true))
                                 ; TODO: need to add the intersection of wall and cutout to the bottom plate!
                                 right-usb-cutout
                                 (if (== wrist-rest-on 1) (->> rest-case-cuts (translate [wrist-translate-x (- (second thumborigin) (- 56 nrows)) 0])))
                                 (screw-insert-holes true)
                                 (->> sensor-cutout (translate trackball-top-pos))
                                 ))
                   cut-bottom
                   ))

(def left-cable-cutout
  (let [ cable-hole (->> (cylinder 3 20)(with-fn 32)(rotate (deg2rad 90)[1 0 0])) ]
  (->>
    cable-hole
    (translate [-60 40 18]))
  ))

(def model-left (difference
                  (union
                    (key-holes :extra-encoder true)
                    connectors
                    thumb
                    (thumb-connectors :encoder true)
                    (difference (union case-walls
                                       (screw-insert-outers false))
                                left-cable-cutout
                                (if (== wrist-rest-on 1) (->> rest-case-cuts (translate [wrist-translate-x (- (second thumborigin) (- 56 nrows)) 0])))
                                (screw-insert-holes false)))
                  cut-bottom
                  ))

; Cut away the walls from the bottom plate again, so it recedes fully. Requires sufficient keyboard-z-offset.
(defn plate-cutout [shape right]
  (union
    (difference
      shape
      (translate [0 0 -10] (screw-insert-screw-holes right))
      (translate [0 0 -3.4] (plate-screw-recess right))
      (union
        (for [xy (range 0.994 1.14 0.015)]
          (->> case-walls
               (scale [xy xy 1.0])
               (translate [0 0 -0.01]))))
      )))

; Not using bottom-plate-thickness fully, as the print won't be that exact anyway.
(def plate-right
        (extrude-linear
          {:height (- bottom-plate-thickness 0.05) :center false}
          (project
            (difference
              (union
                (key-holes)
                connectors
                thumb
                (thumb-connectors)
                case-walls
                thumbcaps-fill
                (caps-fill)
                ;screw-insert-outers
                )
              ))))

(def plate-left
  (difference
        (extrude-linear
          {:height (- bottom-plate-thickness 0.05) :center false}
          (project
            (difference
              (union
                (key-holes :extra-encoder true)
                connectors
                thumb
                (thumb-connectors :encoder true)
                case-walls
                thumbcaps-fill
                (caps-fill :extra-encoder true)
                ;screw-insert-outers
                )
              )))
        ))

(spit "things/encoder-walltest.scad"
      (write-scad (union
                    (->> single-plate (key-place 1 -1))
                    (->> encoder-plate (mirror [-1 0 0]) (translate [0 0 3]) (rotate (deg2rad 180)[0 0 1]) (key-place 2 -1))
                    (->> single-plate (key-place 3 -1))
                    (key-wall-brace 1 -1 0 1 web-post-tl 1 -1 0 1 web-post-tr)
                    (color [0 1 0 1] (key-wall-brace 2 -1 0 1 web-post-tl 1 -1 0 1 web-post-tr :bottomoffset [0.47 0] :topoffset [[0 -3 5] [0 0 0]]))
                    (color [1 0 0 1] (key-wall-brace 2 -1 0 1 web-post-tl 2 -1 0 1 web-post-tr :bottomoffset [0.47 0.47] :topoffset [[0 -3 5] [0 -3 5]]))
                    (color [0 0 1 1] (key-wall-brace 3 -1 0 1 web-post-tl 2 -1 0 1 web-post-tr :bottomoffset [0 0.47] :topoffset [[0 0 0] [0 -3 5]]))
                    (key-wall-brace 3 -1 0 1 web-post-tl 3 -1 0 1 web-post-tr)
                    )))

(defn spit-all-test []
  (future
    (let [
          file "things/all-test.scad"
          old (try (slurp file) (catch Exception e))
          new (write-scad
                (union
                  (translate [130 0 0] (union model-right
                                              (->> (plate-cutout plate-right true) (translate [0 0 -30]))
                                              thumbcaps
                                              (caps)
                                              wrist-rest-build
                                              trackball-top
                                              (->> usb-holder (rotate (deg2rad 180)[0 1 0]) (translate [-75 75 5]))
                                              ))
                  (translate [-130 0 0] (mirror [-1 0 0] (union
                                                           model-left
                                                           (->> (plate-cutout plate-left false) (translate [0 0 -30]))
                                                           thumbcaps
                                                           (caps :extra-encoder true)
                                                           wrist-rest-build
                                                           )))
                  ))
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
                             (->> (plate-cutout plate-left false) (translate [0 0 -30]))
                             thumbcaps
                             (caps :extra-encoder true)
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
          new (write-scad (->> (plate-cutout plate-left false) (mirror [-1 0 0])))
          ]
      (cond (not= old new) (spit file new))
)))
; Print these with 0.30mm layer height, for nice ridges to prevent palm from
; slipping. 20% infill is ok, 30% feels better.
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
                (union model-right
                       (->> (plate-cutout plate-right true) (translate [0 0 -30]))
                       thumbcaps
                       (caps)
                       wrist-rest-build
                       trackball-top
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
                (union model-right
                       (difference trackball-top (->> (hull back-wall)(translate [0 -3.3 -8])))
                       ))
          ]
      (cond (not= old new) (spit file new))
)))
(defn spit-right-plate []
  (future
    (let [
          file "things/right-plate.scad"
          old (try (slurp file) (catch Exception e))
          new (write-scad (plate-cutout plate-right true))
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
