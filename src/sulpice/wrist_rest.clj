(ns sulpice.wrist-rest
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [clojure.java.io :refer [resource]]))

; Platform for butt-shaped silicone wrist rests, like this:
; https://www.amazon.com/LetGoShop-Heart-Shaped-Translucence-Ergonomic-Effectively/dp/B07C1VW6N1

;;;;;;;;;;;;;;;;
;; Parameters ;;
;;;;;;;;;;;;;;;;

(def trace-file
  (-> "wrist-rest-trace.svg"
      resource
      .getFile))

(assert trace-file "wrist rest trace must exist")

; Height of the wrist rest platform
(def platform-height 17)

; Additional wall height (above / around the edges of the platform)
(def extra-wall-height 3)

; z-dimension (thickness) of the wrist rest platform
; (filled in below the specified height)
(def platform-z 4)

; wall thickness
(def wall-thickness 3)

;;;;;;;;;;;;;;
;; Platform ;;
;;;;;;;;;;;;;;

(def wrist-rest-projection
  "the basic shape from svg (uses OpenSCAD svg import)"
  (import trace-file))

(def platform
  (->> wrist-rest-projection
       (extrude-linear {:height platform-z})
       (translate [0
                   0
                   (+ platform-height (/ platform-z 2))])))

(def walls
  (let [height (+ platform-height platform-z extra-wall-height)]
    (->> wrist-rest-projection
         (offset (-' wall-thickness))
         (difference wrist-rest-projection)
         (extrude-linear {:height height})
         (translate [0 0 (/ height 2)]))))

;;;;;;;;;;;;
; Finalize ;
;;;;;;;;;;;;

(def final
  (union platform walls))
