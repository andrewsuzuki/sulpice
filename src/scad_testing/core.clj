(ns scad-testing.core
    (:refer-clojure :exclude [use import])
    (:require [scad-clj.scad :refer :all]
              [scad-clj.model :refer :all]))

;; Keyboard
;; by Andrew Suzuki
;;;;;;;;;;;;;;;;;;;;;

; TODOS
; - DONE remove old code
; - DONE remove main wall near thumb
; - sides
; - bottom
; - ports
; - make rotation-z-compensate a sum of prev (up to home)
; - position walls relative to TOP of keyholes (instead of center) -- i.e. account for rotation
; - more documentation

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

; enclosure wall thickness
(def wall-thickness 2)
; enclosure height
(def wall-height 20)

;;;;;;;;;;;;;;;;
;; Calculated ;;
;;;;;;;;;;;;;;;;

; keyhole border width
(def keyhole-bw
    (/ (- keyhole-stagger-x keyhole-x) 2))

(def keyhole-total-x
    (+ keyhole-x (* keyhole-bw 2)))

(def keyhole-total-y
    (+ keyhole-y (* keyhole-bw 2)))

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

(defn insert-key-post [shape [r t] & flags]
    (union
        (if (contains? (set flags) :excavate)
            (difference
                shape
                (place key-chute-positive r t))
            shape)
        (place keyhole r t)))

(defn insert-key-posts [shape]
    (reduce
        insert-key-post
        shape
        places))

;;;;;;;;;;;;;
; Enclosure ;
;;;;;;;;;;;;;

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

; TODO
(def bottom
    nil)

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
    (make-thumb-places 5 50 25 -1))

(def thumbs-base-height 15)
(def thumbs-base-solid
    (union (map (fn [[r t]] (place (cube 30 30 thumbs-base-height) r t)) thumb-places)))
(def thumbs-base
    (let [cutout (union (map (fn [[r t]] (place (cube 28 28 thumbs-base-height) r t)) thumb-places))]
        (difference thumbs-base-solid (translate [0 0 -5] cutout))))

(def thumbs
    (translate [0 -20 -3]
        (reduce (fn [shape p] (insert-key-post shape p :excavate))
                (translate [0 0 -4.5] thumbs-base)
                thumb-places)))

;;;;;;;;;;;;
; Finalize ;
;;;;;;;;;;;;

(def key-platform
    (-> enclosure-top
        (difference (translate [0 -20 -7.5] thumbs-base-solid))
        (insert-key-posts)))

(def final
    (union
        thumbs
        key-platform
        bottom))

(defn save [& body]
    (spit "things/right.scad" (apply write-scad body)))

(defn -main
    "Save keyboard as scad"
    [& args]
    (save final)
    (println "done"))

(-main)
