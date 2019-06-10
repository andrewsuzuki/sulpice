(ns scad-testing.core
    (:refer-clojure :exclude [use import])
    (:require [scad-clj.scad :refer :all]
              [scad-clj.model :refer :all]))

;; Keyboard
;; by Andrew Suzuki
;;;;;;;;;;;;;;;;;;;;;

; TODOS
; - DONE remove old code
; - DONE new thumb enclosure
; - remove main wall near thumb
; - more vars
; - sides
; - bottom
; - ports
; - make rotation-z-compensate a sum of prev (up to home)
; - position walls relative to TOP of keyholes (instead of center) -- i.e. account for rotation
; - more documentation
; - name
; - github

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
; translate enclosure
(def main-wall-translate-z -5)

(def thumb-wall-height 15)

(def bottom-height 2)

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

(def base-z (-> wall-height
                (/ 2)
                (-')
                (+ main-wall-translate-z)))

;;;;;;;;;;;
;; Utils ;;
;;;;;;;;;;;

(defn deg2rad [degrees]
    (/ (* degrees pi) 180))

(defn place [shape r t]
    (->> shape
         (rotate r)
         (translate t)))

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

(def primary-keyholes
    (union (map (fn [[r t]] (place keyhole r t))
                places)))

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

; TODO subtract thumb cluster walls
(def enclosure-top
    (translate [0 0 main-wall-translate-z]
        (union
            wall-cols
            wall-connectors-north
            wall-connectors-south)))

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

(defn make-thumb-connector-face [[r t] & flags]
    (let [half-raw (/ keyhole-total-x 2)
          x-offset (if (contains? (set flags) :left)
                      half-raw
                      (-' half-raw))
          z-offset (/ keyhole-z 2)]
        (place (translate [x-offset 0 z-offset] (cube 0.0001 keyhole-total-y keyhole-z)) r t)))
(def thumb-connectors
    (union
        (map (fn [[t1 t2]]
                (let [f1 (make-thumb-connector-face t1 :right)
                      f2 (make-thumb-connector-face t2 :left)]
                    (hull f1 f2)))
             (partition 2 1 thumb-places))))

(def thumb-keyholes
    (union (map (fn [[r t]] (place keyhole r t))
                thumb-places)))

(def thumbs-base
    (let [keyhole-solid (cube keyhole-total-x keyhole-total-y keyhole-z)
          solid (union thumb-connectors
                     (union (map (fn [[r t]] (place keyhole-solid r t))
                                thumb-places)))
          solid-projection (project solid)
          wall-outline (difference
                          solid-projection
                          (offset (-' wall-thickness) solid-projection))
          bottom (extrude-linear {:height bottom-height} solid-projection)]
        (union
            (translate [0 0 (- (/ bottom-height 2) (/ thumb-wall-height 2))] bottom)
            (extrude-linear {:height thumb-wall-height} wall-outline))))

(def thumbs-offset-y -20)

(def thumbs
    (translate [0 thumbs-offset-y 0]
        (let [translate-top-z (- (+ base-z thumb-wall-height) keyhole-z)]
           (union (translate [0 0 translate-top-z]
                             (union
                                 thumb-keyholes
                                 thumb-connectors))
                  (translate [0 0 (+ base-z (/ thumb-wall-height 2))] thumbs-base)))))

;;;;;;;;;;
; Bottom ;
;;;;;;;;;;

; TODO
(def bottom
    nil)

;;;;;;;;;;;;
; Finalize ;
;;;;;;;;;;;;

(def final
    (union
        enclosure-top
        primary-keyholes
        thumbs
        bottom))

(defn save [& body]
    (spit "things/right.scad" (apply write-scad body)))

(defn -main
    "Save keyboard as scad"
    [& args]
    (save final)
    (println "done"))

(-main)
