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
; - DONE thumb cluster port
; - DONE make rotation-z-compensate a sum of prev (up to home)
; - DONE remove degree units (all radians)
; - position walls relative to TOP of keyholes (instead of center) -- i.e. account for rotation
; - sides
; - bottom
; - ports
; - screwholes
; - more documentation
; - name
; - github

;;;;;;;;;;;;;;;;
;; Parameters ;;
;;;;;;;;;;;;;;;;

(def include-bottom? false)

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
(def rotate-fore-aft-step (/ pi 45))

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
; translate enclosure z
; also equal to the z midpoint of the main wall
(def main-wall-translate-z -5)

; height of the bottom cover
(def bottom-height 2)

; y offset of thumb cluster
(def thumbs-offset-y -22)
; height of thumb walls
; keyhole z position is calculated from this
(def thumbs-wall-height 15)

(def port-from-bottom 5)

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

; base-z is the z-coordinate of the lowest point
; of the keyboard (excluding the bottom)
(def base-z (-> wall-height
                (/ 2)
                (-')
                (+ main-wall-translate-z)))

; the z coordinate of the center of all port cutouts
; (exterior and for interior thumb cluster port)
(def port-z
    (+ base-z port-from-bottom))

;;;;;;;;;;;
;; Utils ;;
;;;;;;;;;;;

(defn place [shape r t]
    (->> shape
         (rotate r)
         (translate t)))

;;;;;;;;;;;
;; Model ;;
;;;;;;;;;;;

(defn get-place-rotation [row]
    (* rotate-fore-aft-step (- row home-row)))

(defn get-z-compensate [row]
    (->> (if (< row home-row)
             (range row (inc home-row))
             (range home-row (inc row)))
         (map (fn [r]
                  (->> r
                       get-place-rotation
                       Math/sin
                       Math/abs
                       (* (/ keyhole-total-y 2)))))
         (apply +)))

; places describes the center of each keyhole
; used to position the keyholes themselves,
; as well as the connectors between keyholes
(def places-by-col
    (map (fn [col]
             (let [col-offset-y (or (get-in col-offsets [col :y] 0))
                   col-offset-z (or (get-in col-offsets [col :z] 0))]
                  (map (fn [row]
                           [; rotate
                            [(get-place-rotation row) 0 0]
                            ; translate
                            [(* col keyhole-stagger-x)
                             (+ (* row keyhole-stagger-y) col-offset-y)
                             (+ col-offset-z (get-z-compensate row))]])
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

(def enclosure-top
    (translate [0 0 main-wall-translate-z]
        (union
            wall-cols
            wall-connectors-north
            wall-connectors-south)))

;;;;;;;;;;;;;;;;;
; Thumb Cluster ;
;;;;;;;;;;;;;;;;;

(defn make-thumbs-places [total radius step start]
    (map
        (fn [n]
            (let [rotate-angle (* n step)
                  unit-angle (+ (/ pi 2) rotate-angle) ; move to second quadrant for x/y calculation
                  x (* radius (Math/cos unit-angle))
                  y (* radius (Math/sin unit-angle))]
                [[0 0 rotate-angle]
                 [x (- y radius) 0]]))
        (range start (+ total start))))

(def thumbs-places
    (make-thumbs-places 5 50 (/ pi 7) -1))

(defn make-thumbs-connector-face [[r t] & flags]
    (let [half-raw (/ keyhole-total-x 2)
          x-offset (if (contains? (set flags) :left)
                      half-raw
                      (-' half-raw))
          z-offset (/ keyhole-z 2)]
        (place (translate [x-offset 0 z-offset] (cube 0.0001 keyhole-total-y keyhole-z)) r t)))
(def thumbs-connectors
    (union
        (map (fn [[t1 t2]]
                (let [f1 (make-thumbs-connector-face t1 :right)
                      f2 (make-thumbs-connector-face t2 :left)]
                    (hull f1 f2)))
             (partition 2 1 thumbs-places))))

(def thumbs-keyholes
    (union (map (fn [[r t]] (place keyhole r t))
                thumbs-places)))

(def thumbs-solid-projection
    (let [keyhole-solid (cube keyhole-total-x keyhole-total-y keyhole-z)
          solid (union thumbs-connectors
                     (union (map (fn [[r t]] (place keyhole-solid r t))
                                thumbs-places)))]
      (project solid)))

(def thumbs-walls
    (->> thumbs-solid-projection
         (offset (-' wall-thickness))
         (difference thumbs-solid-projection)
         (extrude-linear {:height thumbs-wall-height})))

(def thumbs-bottom
    (->> thumbs-solid-projection
         (extrude-linear {:height bottom-height})))

(def thumbs
    (translate [0 thumbs-offset-y base-z]
       (union
           ; bottom
           (if include-bottom? (translate [0 0 (-' (/ bottom-height 2))] thumbs-bottom) nil)
           ; top
           (translate [0 0 (- thumbs-wall-height keyhole-z)]
               (union
                   thumbs-keyholes
                   thumbs-connectors))
           ; walls
           (translate [0 0 (/ thumbs-wall-height 2)] thumbs-walls))))

;;;;;;;;;;
; Bottom ;
;;;;;;;;;;

; TODO
(def primary-bottom
    nil)

;;;;;;;;;;;
; Cutouts ;
;;;;;;;;;;;

(def thumbs-port-cutout
    (->> (cylinder 3 15)
         (rotate [(/ pi 2) 0 0])
         (translate [0
                     (+ thumbs-offset-y (/ keyhole-total-y 2)) ; this is an estimate
                     port-z])))

(def cutouts
    (union
        thumbs-port-cutout))

;;;;;;;;;;;;
; Finalize ;
;;;;;;;;;;;;

(def final
    (difference
        (union
            enclosure-top
            primary-keyholes
            thumbs
            primary-bottom)
        cutouts))

(defn save [& body]
    (spit "things/right.scad" (apply write-scad body)))

(defn -main
    "Save keyboard as scad"
    [& args]
    (save final)
    (println "done"))

(-main)
