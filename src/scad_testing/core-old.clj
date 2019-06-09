(ns scad-testing.core
    (:refer-clojure :exclude [use import])
    (:require [scad-clj.scad :refer :all]
              [scad-clj.model :refer :all]))

;; Wanamaker Keyboard
;;   by Andrew Suzuki
;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;
;; Parameters ;;
;;;;;;;;;;;;;;;;

(def rows 6)
(def cols 7)

; keyhole dimensions
(def keyhole-y 14.4) ;; Was 14.1, then 14.25
(def keyhole-x 14.4)
(def keyhole-z 4)

; space between center of keyholes (x direction)
(def keyhole-stagger-x 20)
; space between center of keyholes (y direction)
(def keyhole-stagger-y 20)

; keyhole border width
(def keyhole-bw 1.5)

; which column is the index finger?
; (determines placement of the thumb cluster)
(def index-finger-col 2)

; optional y and z offsets for each column (zero-indexed)
(def col-offsets {0 {:y 0 :z -1} ; extra col (index)
                  1 {:y 0 :z -1} ; h col (index)
                  2 {:y 2 :z -1} ; j col (index)
                  3 {:y 8 :z -3} ; k col (middle)
                  4 {:y 6 :z -2} ; l col (ring)
                  5 {:y -5 :z 0} ; semi col (pinky)
                  6 {:y -5 :z 0}}) ; extra2 col (pinky)

;;;;;;;;;;;
;; Model ;;
;;;;;;;;;;;

; places describes the center of each keyhole
; used to position the keyholes themselves,
; as well as the connectors between keyholes
(def places
    (apply concat
        (map (fn [col]
                 (let [col-offset-y (or (get-in col-offsets [col :y] 0))
                       col-offset-z (or (get-in col-offsets [col :z] 0))]
                      (map (fn [row]
                               [[0 0 0] [(* col keyhole-stagger-x) (+ (* row keyhole-stagger-y) col-offset-y) col-offset-z]])
                           (range 0 rows))))
             (range 0 cols))))

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

(def keyholes
    (union
        (map (fn [[r t]]
                 (->> keyhole
                      (rotate r)
                      (translate t)))
             places)))

;;;;;;;;;;;;;;;;;;
; Col Connectors ;
;;;;;;;;;;;;;;;;;;

; note that these col connectors are less primitive than
; the row connectors below (just cubes between the keyholes)
(def col-connector
    (->> (cube (+ keyhole-x 3) (- keyhole-stagger-y keyhole-y 3) keyhole-z)
         (translate [0
                     (/ keyhole-stagger-y 2)
                     (/ keyhole-z 2)])))
(def col-connectors
    (union
        (map (fn [[r t]]
                 (->> col-connector
                      (rotate r)
                      (translate t)))
             places)))

;;;;;;;;;;;;;;;;;;
; Row Connectors ;
;;;;;;;;;;;;;;;;;;

(def super-thin-width 0.0000001)

(defn row-connector-face-left [col col-offset-y col-offset-z]
    (let [face-length (+ (* keyhole-stagger-y (dec rows)) keyhole-y (* keyhole-bw 2))
          translate-x (+ (* keyhole-stagger-x col) (/ keyhole-x 2) keyhole-bw)
          translate-y (+ (- (/ face-length 2) (/ keyhole-y 2) keyhole-bw) col-offset-y)
          translate-z (+ (/ keyhole-z 2) col-offset-z)]
        (translate [translate-x translate-y translate-z]
            (cube super-thin-width face-length keyhole-z))))

(defn row-connector-face-right [col col-offset-y col-offset-z]
    (let [face-length (+ (* keyhole-stagger-y (dec rows)) keyhole-y (* keyhole-bw 2))
          translate-x (- (* keyhole-stagger-x col) (/ keyhole-x 2) keyhole-bw)
          translate-y (+ (- (/ face-length 2) (/ keyhole-y 2) keyhole-bw) col-offset-y)
          translate-z (+ (/ keyhole-z 2) col-offset-z)]
        (translate [translate-x translate-y translate-z]
            (cube super-thin-width face-length keyhole-z))))

(def row-connectors
    (map (fn [[col1 col2]]
             (hull
                 (row-connector-face-left col1 (get-in col-offsets [col1 :y]) (get-in col-offsets [col1 :z]))
                 (row-connector-face-right col2 (get-in col-offsets [col2 :y]) (get-in col-offsets [col2 :z]))))
     (partition 2 1 (range 0 cols))))

;;;;;;;;;;;;;;
; Boundaries ;
;;;;;;;;;;;;;;

; TODO remove?

(def lowest-column-offset
    (->> col-offsets
         vals
         (map :y)
         (apply min)))

(def lower-boundaries nil)

(def boundaries
    (union
        lower-boundaries))

;;;;;;;;;;;;
; Finalize ;
;;;;;;;;;;;;

(def key-platform
    (union
        col-connectors
        row-connectors
        keyholes))

(def final
    key-platform)

(defn save [& body]
    (spit "things/right.scad" (apply write-scad body)))

(defn -main
    "Save keyboard as scad"
    [& args]
    (save final)
    (println "done"))

(-main)
