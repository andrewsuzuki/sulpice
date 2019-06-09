(ns scad-testing.core
    (:refer-clojure :exclude [use import])
    (:require [scad-clj.scad :refer :all]
              [scad-clj.model :refer :all]))

;; Keyboard
;; by Andrew Suzuki
;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;
;; Parameters ;;
;;;;;;;;;;;;;;;;

(def rows 4)
(def cols 6)

; keyhole dimensions
(def keyhole-y 14.4) ;; Was 14.1, then 14.25
(def keyhole-x 14.4)
(def keyhole-z 4)

; space between center of keyholes (x direction)
(def keyhole-stagger-x 20)
; space between center of keyholes (y direction)
(def keyhole-stagger-y 20)

; fore-aft rotation step
(def rotate-fore-aft-step 4)

; home row index (from bottom=0)
; the home row is flat --
; the other rows curve away from it
(def home-row 1)

; optional y and z offsets for each column (zero-indexed)
(def col-offsets {0 {:y 0 :z -1} ; h col (index)
                  1 {:y 2 :z -1} ; j col (index)
                  2 {:y 8 :z -3} ; k col (middle)
                  3 {:y 6 :z -2} ; l col (ring)
                  4 {:y -5 :z 0} ; semi col (pinky)
                  5 {:y -5 :z 0}}) ; extra2 col (pinky)

;;;;;;;;;;;;;;;;
;; Calculated ;;
;;;;;;;;;;;;;;;;

(def keyhole-total-x
    (+ keyhole-x (* keyhole-bw 2)))

(def keyhole-total-y
    (+ keyhole-y (* keyhole-bw 2)))

; keyhole border width
(def keyhole-bw
    (/ (- keyhole-stagger-x keyhole-x) 2))

;;;;;;;;;;;
;; Utils ;;
;;;;;;;;;;;

(defn deg2rad [degrees]
    (/ (* degrees pi) 180))

;;;;;;;;;;;
;; Model ;;
;;;;;;;;;;;

; places describes the center of each keyhole
; used to position the keyholes themselves,
; as well as the connectors between keyholes
(def places-by-col
    (map (fn [col]
             (let [col-offset-y (or (get-in col-offsets [col :y] 0))
                   col-offset-z (or (get-in col-offsets [col :z] 0))]
                  (map (fn [row]
                           (let [rotation (deg2rad (* rotate-fore-aft-step (- row home-row)))
                                 ; TODO NOTE
                                 ; the compensation should be a sum of all previous rows
                                 ; as well (until the home row)
                                 rotation-z-compensate (* (/ keyhole-total-y 2) (Math/abs (Math/sin rotation)))]
                               [; rotate
                                [rotation 0 0]
                                ; translate
                                [(* col keyhole-stagger-x)
                                 (+ (* row keyhole-stagger-y) col-offset-y)
                                 (+ col-offset-z rotation-z-compensate)]]))
                       (range 0 rows))))
         (range 0 cols)))
(def places
    (apply concat places-by-col))

(def keyhole
  (let [keyhole-bw-2x (* keyhole-bw 2)
        top-wall (->> (cube (+ keyhole-x keyhole-bw-2x) keyhole-bw keyhole-z)
                      (translate [0
                                  (+ (/ keyhole-bw 2) (/ keyhole-y 2))
                                  (/ keyhole-z 2)]))
        left-wall (->> (cube keyhole-bw (+ keyhole-y keyhole-bw-2x) keyhole-z)
                       (translate [(+ (/ keyhole-bw 2) (/ keyhole-x 2))
                                   0
                                   (/ keyhole-z 2)]))
        side-nub (->> (binding [*fn* 30] (cylinder 1 2.75))
                      (rotate (/ pi 2) [1 0 0])
                      (translate [(+ (/ keyhole-x 2)) 0 1])
                      (hull (->> (cube keyhole-bw 2.75 keyhole-z)
                                 (translate [(+ (/ keyhole-bw 2) (/ keyhole-x 2))
                                             0
                                             (/ keyhole-z 2)]))))
        plate-half (union top-wall left-wall (with-fn 100 side-nub))]
    (union plate-half
           (->> plate-half
                (mirror [1 0 0])
                (mirror [0 1 0])))))

(def key-chute-positive
    (cube keyhole-total-x keyhole-total-y 100000))

(defn place [shape r t]
    (->> shape
         (rotate r)
         (translate t)))

(defn insert-key-post [shape [r t]]
    (union
        (difference shape
                    (place key-chute-positive r t))
        (place keyhole r t)))

(defn insert-key-posts [shape]
    (reduce
        insert-key-post
        shape
        places))

;;;;;;;;;;;;;
; Enclosure ;
;;;;;;;;;;;;;

(def enclosure-x 130)
(def enclosure-y 100)
(def enclosure-top-z 15)
(def enclosure-wall 10)
(def bottom-z 5)
(def enclosure-translate [50 31 -5])

; TODO remove? (+ vars?)
(def enclosure-top-old
    (let [blank (cube enclosure-x enclosure-y enclosure-top-z)
          cutout (translate [0 0 -5] (cube (- enclosure-x enclosure-wall) (- enclosure-y enclosure-wall) enclosure-top-z))
          final (difference blank cutout)]
         (translate enclosure-translate final)))

; look at places-by-col
; for each col, generate N and S walls based on position of those edges

(defn make-enclosure-part [r t]
    (let [[x y z] t
          meet-z (+ z 5)
          height (+ 15 meet-z)
          orig (cube (+ keyhole-total-x 3)
                     (+ keyhole-total-y 3)
                     height)
          cutout (->> orig (scale [0.9 0.9 1]) (translate [0 0 -3]))
          final (difference orig cutout)]
        (translate
            [x y (+ meet-z (/ height -2))]
            final)))

(def enclosure-top
    (union
        (concat
            (map
                (fn [col]
                    (union
                        (apply make-enclosure-part (first col))
                        (apply make-enclosure-part (last col))))
                          ; s (cube keyhole-total-x
                          ;         3 ; wall thickness
                          ;         (- 20 (get t-s 2)))]
                        ; (translate [(get t-s 0)
                          ;           (- (get t-s 1)
                          ;              (/ keyhole-total-y 2))
                          ;           (get t-s 2)]
                          ;   s)))

                places-by-col))))
        ; (translate enclosure-translate final)))

; brand new algorithm:
; 1. cover every column end with wall
; 2. look between every column. if same length, do nothing. else, use a little wall segment to join two column walls; placing it on the side of the shorter column.

(def wall-thickness 2)
(def wall-height 20)

(defn make-wall-col [t addsub]
     (translate [(get t 0)
                 (addsub (get t 1) (/ keyhole-total-y 2) (/ wall-thickness 2))
                 0]
        (cube keyhole-total-x wall-thickness wall-height)))

(def wall-cols
    (union
        (map (fn [col]
                 (let [[r-s t-s] (first col)
                       [r-n t-n] (last col)]
                     (union
                         (make-wall-col t-n +)
                         (make-wall-col t-s -))))
             places-by-col)))

(def wall-connectors-north
    (union
        (map (fn [[l-places r-places]]
                 (let [[r-l t-l] (last l-places)
                       [r-r t-r] (last r-places)
                       y-l (get t-l 1)
                       y-r (get t-r 1)
                       t-short (if (< y-l y-r) t-l t-r)
                       length (+ wall-thickness (Math/abs (- (get t-l 1) (get t-r 1))))]
                     (if (= y-l y-r)
                        nil
                        (translate [(let [m (- (/ keyhole-total-x 2) (/ wall-thickness 2))]
                                       (if (< y-l y-r)
                                         (+ (get t-l 0) m)
                                         (- (get t-r 0) m)))
                                    (+ (/ keyhole-total-y 2) (get t-short 1) (/ length 2))
                                    0]
                            (cube
                                wall-thickness
                                length
                                wall-height)))))
             (partition 2 1 places-by-col))))

(def wall-connectors-south
    (union
        (map (fn [[l-places r-places]]
                 (let [[r-l t-l] (first l-places)
                       [r-r t-r] (first r-places)
                       y-l (get t-l 1)
                       y-r (get t-r 1)
                       t-short (if (< y-l y-r) t-r t-l)
                       length (+ wall-thickness (Math/abs (- (get t-l 1) (get t-r 1))))]
                     (if (= y-l y-r)
                        nil
                        (translate [(let [m (- (/ keyhole-total-x 2) (/ wall-thickness 2))]
                                       (if (< y-l y-r)
                                         (- (get t-r 0) m)
                                         (+ (get t-l 0) m)))
                                    (- (get t-short 1) (/ keyhole-total-y 2) (/ length 2))
                                    0]
                            (cube
                                wall-thickness
                                length
                                wall-height)))))
             (partition 2 1 places-by-col))))

(def enclosure-top
    (translate [0 0 -5]
        (union
            wall-cols
            wall-connectors-north
            wall-connectors-south)))

(def bottom
    (translate [(first enclosure-translate) (second enclosure-translate) (- (-' enclosure-top-z) 0)]
        (cube enclosure-x enclosure-y bottom-z)))

;;;;;;;;;;;;;;;;;
; Thumb Cluster ;
;;;;;;;;;;;;;;;;;

(defn make-thumb-places [total radius step start]
    (map
        (fn [n]
            (let [rotate-angle (* n (deg2rad step))
                  unit-angle (+ (/ pi 2) rotate-angle) ; move to second quadrant for x/y calculation
                  x (* radius (Math/cos unit-angle))
                  y (* radius (Math/sin unit-angle))]
                [[0 0 rotate-angle]
                 [x (- y radius) 0]]))
        (range start (+ total start))))

(def thumb-places
    (make-thumb-curve-places 5 50 25 -1))

(def thumbs-base-height 12)
(def thumbs-base
    (let [whole (union (map (fn [[r t]] (place (cube 30 30 thumbs-base-height) r t)) thumb-curve-places))
          cutout (union (map (fn [[r t]] (place (cube 25 25 thumbs-base-height) r t)) thumb-curve-places))]
        (difference whole (translate [0 0 -5] cutout))))

(def thumbs
    (translate [0 -25 -5]
        (reduce insert-key-post
                (translate [0 0 -4.5] thumbs-base)
                thumb-curve-places)))

;;;;;;;;;;;;
; Finalize ;
;;;;;;;;;;;;

(def key-platform
    (-> enclosure-top
        (insert-key-posts)))

(def final
    (union
        thumbs
        key-platform))
        ; bottom))

(defn save [& body]
    (spit "things/right.scad" (apply write-scad body)))

(defn -main
    "Save keyboard as scad"
    [& args]
    (save final)
    (println "done"))

(-main)
