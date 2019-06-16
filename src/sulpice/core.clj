(ns sulpice.core
    (:refer-clojure :exclude [use import])
    (:require [scad-clj.scad :refer :all]
              [scad-clj.model :refer :all]
              [clojure.java.io :refer [make-parents]]))

;;;;;;;;;;;;;;;;;;
;; Sulpice       ;
;; Andrew Suzuki ;
;;;;;;;;;;;;;;;;;;

; TODO
; - [after first print] teensy / mcp holders
; - [after first print] trrs / usb breakout board holders

;;;;;;;;;;;;;;;;
;; Parameters ;;
;;;;;;;;;;;;;;;;

; include bottom in right.scad/left.scad exports
; (use for preview, not printing)
(def include-bottom? false)

; KEYHOLE PLACEMENT

(def rows 3)
(def cols 6)

; keyhole dimensions
(def keyhole-y 14.4) ;; Was 14.1, then 14.25
(def keyhole-x 14.4)
(def keyhole-z 4)
(def keyhole-nub-width 2.75)
(def keyhole-nub-height keyhole-z)

; space between center of keyholes (x direction)
(def keyhole-stagger-x 20)

; space between center of keyholes (y direction)
(def keyhole-stagger-y 20)

; fore-aft rotation step
(def rotate-fore-aft-step (/ pi 25))

; home row index (from bottom=0)
; the home row is flat --
; the other rows curve away from it
(def home-row 1)

; optional y and z offsets for each column (zero-indexed)
(def col-offsets {0 {:y 0 :z -1} ; h col (index)
                  1 {:y 2 :z -1} ; j col (index)
                  2 {:y 13 :z -5} ; k col (middle)
                  3 {:y 9 :z -4} ; l col (ring)
                  4 {:y -5 :z 0} ; semi col (pinky)
                  5 {:y -5 :z 0}}) ; extra2 col (pinky)

; configure placement/config of sheaths
; sheaths are thin skirts around primary keyholes
; that can cover any gaps (primarily due to
; column z-offsets or rotation between the columns
(def sheaths
    [{:col 1 :row 1 :height 4 :sides [:right]}
     {:col 1 :row 2 :height 4 :sides [:right]}
     {:col 4 :row 2 :height 4 :sides [:left]}])

; ENCLOSURE / WALL

; enclosure wall thickness
(def wall-thickness 2)

; enclosure height
(def wall-height 20)

; translate enclosure z
; also equal to the z midpoint of the primary wall
(def primary-wall-translate-z -5)

; amount to squish walls into keyholes (y direction)
; (crudely compensates for gaps due to keyhole rotation)
(def primary-wall-squish 1)

; height of the bottom cover
(def bottom-height 3)

; THUMBS

; height of thumb walls
; keyhole z position is calculated from this
(def thumbs-wall-height 18)

; total number of thumb switches
(def thumbs-total 5)

; radius of constructed thumb switch circle
(def thumbs-radius 50)

; step between thumb switches (in radians)
(def thumbs-step (/ pi 7))

; where to start the thumb switches in steps (0 is 12 o'clock)
(def thumbs-start -1)

; PORTS

; distance from bottom (not including bottom plate)
; to the centerpoint of exterior and interior ports
(def port-from-bottom 5)

; y offset from rear of case to the trrs port
(def trrs-port-offset 10)

; SCREWS

; self-tapping m3 insert outside diameter
(def screw-insert-diameter 4.6) ; TODO test + verify

; self-tapping m3 insert height
(def screw-insert-height 6)

; screw diameter (m3)
(def screw-diameter 3)

; screw head cone height
(def screw-cone-height 1.65)

; screw head cone max diameter (at end)
(def screw-cone-diameter 5.5)

; screw support cylinder diameter (surrounds screw insert)
(def screw-support-diameter 7)

; screw support cylinder height (surrounds screw insert)
(def screw-support-height (+ screw-insert-height 2))

; FRICTION PADS

; friction pad cutout dimensions
(def friction-cutout-x 7)
(def friction-cutout-y 7)
; z is inset into the bottom plate,
; the actual height of the pad
; will likely be higher
(def friction-cutout-z 1)

; MISC

; number of facets on arcs (openscad "$fn")
; see https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/Other_Language_Features#$fa,_$fs_and_$fn
(def cylinder-facet-number 50)

;;;;;;;;;;;;;;;;
;; Calculated ;;
;;;;;;;;;;;;;;;;

; keyhole border width
(def keyhole-bw
    (/ (- keyhole-stagger-x keyhole-x) 2))

; (equivalent to keyhole-stagger-x)
(def keyhole-total-x
    (+ keyhole-x (* keyhole-bw 2)))

; (equivalent to keyhole-stagger-y)
(def keyhole-total-y
    (+ keyhole-y (* keyhole-bw 2)))

; base-z is the z-coordinate of the lowest point
; of the keyboard (excluding the bottom)
(def base-z (-> wall-height
                (/ 2)
                (-')
                (+ primary-wall-translate-z)))

; the z coordinate of the center of all port cutouts
; (exterior and for interior thumb cluster port)
(def port-z
    (+ base-z port-from-bottom))

; y offset of thumb cluster
(def thumbs-offset-y
    (- primary-wall-squish
       wall-thickness
       keyhole-total-y))

; length of primary enclosure sidewall (y direction)
(def sidewall-y
    (-> rows
        (* keyhole-total-y)
        (+ (* wall-thickness 2))
        (- (* primary-wall-squish 2))))

; y-coordinate of the left corner
; (used for port positioning)
(def left-corner-y
    (+ (* keyhole-total-y (- rows 0.5))
       (get-in col-offsets [0 :y])
       wall-thickness
       (-' primary-wall-squish)))

;;;;;;;;;;;
;; Utils ;;
;;;;;;;;;;;

; place a shape at a given rotation and translation (rotates first)
(defn place [shape r t]
    (->> shape
         (rotate r)
         (translate t)))

; mirror across the x plane
(defn make-left [shape]
    (mirror [1 0 0] shape))

; cylinder with configured fn (facet number)
(defn cylinder-fn [& args]
    (->> args
         (apply cylinder)
         (with-fn cylinder-facet-number)))

;;;;;;;;;;;;;;;;;;;;;;;
;; Keyhole Placement ;;
;;;;;;;;;;;;;;;;;;;;;;;

; get rotation for keyhole given row index
(defn get-place-rotation [row]
    (* rotate-fore-aft-step (- row home-row)))

; given keyhole rotation from row index,
; determine an appropriate z compensation
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

; places (rotation and translation) for each keyhole, grouped by column
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

; places describes the center of each keyhole
; and any rotation to apply
(def places
    (apply concat places-by-col))

; a keyhole (thin perimeter + interior side nubs)
(def keyhole
  (let [keyhole-bw-2x (* keyhole-bw 2)
        ; walls
        top-wall (->> (cube (+ keyhole-x keyhole-bw-2x) keyhole-bw keyhole-z)
                      (translate [0
                                  (+ (/ keyhole-bw 2) (/ keyhole-y 2))
                                  (/ keyhole-z 2)]))
        left-wall (->> (cube keyhole-bw (+ keyhole-y keyhole-bw-2x) keyhole-z)
                       (translate [(+ (/ keyhole-bw 2) (/ keyhole-x 2))
                                   0
                                   (/ keyhole-z 2)]))
        ; nub
        side-nub (->> (cylinder-fn 1 keyhole-nub-width)
                      (rotate (/ pi 2) [1 0 0])
                      (translate [(+ (/ keyhole-x 2)) 0 1])
                      (hull (->> (cube keyhole-bw keyhole-nub-width keyhole-nub-height)
                                 (translate [(+ (/ keyhole-bw 2) (/ keyhole-x 2))
                                             0
                                             (/ keyhole-nub-height 2)])))
                      (translate [0 0 (- keyhole-z keyhole-nub-height)]))
        plate-half (union top-wall left-wall side-nub)]
    (union plate-half
           (->> plate-half
                (mirror [1 0 0])
                (mirror [0 1 0])))))

; all primary keyholes in their places
(def primary-keyholes
    (union (map (fn [[r t]] (place keyhole r t))
                places)))

; a solid cube representing the space of a keyhole
; (used later for bottom plate generation)
(def dummy-keyhole
    (cube keyhole-total-x keyhole-total-y keyhole-z))

; all dummy keyholes in their places (the same places as primary-keyholes)
(def primary-dummy-keyholes
    (union (map (fn [[r t]] (place dummy-keyhole r t))
                places)))

; make sheath (without final translation/rotation) from config map
; example: (make-sheath {:height 6 :sides [:right]})
; sides can be :left and/or :right
(defn make-sheath [{:keys [height sides]}]
    (let [wall (->> (cube 1 (+ keyhole-y (* keyhole-bw 2)) height)
                    (translate [0 0 (- (/ keyhole-z 2) (/ height 2))]))
          trans-x (+ keyhole-bw (/ keyhole-x 2) (/ -1 2))]
        (union
            (if (some #{:right} sides) (translate [trans-x 0 0] wall) nil)
            (if (some #{:left} sides) (translate [(-' trans-x) 0 0] wall) nil))))

; make and place all configured sheaths
(def primary-sheaths
    (union
        (map (fn [{:keys [row col] :as sconf}]
                (let [sheath (make-sheath sconf)
                      keyplace (-> places-by-col (nth col) (nth row))]
                    (if keyplace (apply (partial place sheath) keyplace) nil)))
             sheaths)))

;;;;;;;;;;;;;
; Enclosure ;
;;;;;;;;;;;;;

; create wall segment at the end (top or bottom) of a column
(defn make-wall-col [t addsub]
     (translate [(get t 0)
                 (addsub (get t 1) (/ keyhole-total-y 2) (/ wall-thickness 2) (-' primary-wall-squish))
                 0]
        (cube keyhole-total-x wall-thickness wall-height)))

; create all wall segments at ends of columns
(def wall-cols
    (union
        (map (fn [col]
                 (let [[r-s t-s] (first col)
                       [r-n t-n] (last col)]
                     (union
                         (make-wall-col t-n +)
                         (make-wall-col t-s -))))
             places-by-col)))

; wall connectors on northern end of primary enclosure
; wall connectors connect the wall-cols and are
; positioned between two columns along the longer column
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
                                    (+ (/ keyhole-total-y 2) (get t-short 1) (/ length 2) (-' primary-wall-squish))
                                    0]
                            (cube
                                wall-thickness
                                length
                                wall-height)))))
             (partition 2 1 places-by-col))))

; wall connectors on southern end of primary enclosure
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
                                    (- (get t-short 1) (/ keyhole-total-y 2) (/ length 2) (-' primary-wall-squish))
                                    0]
                            (cube
                                wall-thickness
                                length
                                wall-height)))))
             (partition 2 1 places-by-col))))

; raw sidewall shape (to be positioned)
(def sidewall-shape
    (cube
        wall-thickness
        sidewall-y
        wall-height))

; left sidewall
(def sidewall-left
    (translate
        [(- 0 (/ wall-thickness 2) (/ keyhole-total-x 2))
         (-
             (+ (/ sidewall-y 2)
                (get-in col-offsets [0 :y])
                primary-wall-squish)
             (/ keyhole-total-y 2)
             wall-thickness)
         0]
        sidewall-shape))

; right sidewall
(def sidewall-right
    (translate
        [(+
          (/ wall-thickness 2)
          (- (* cols keyhole-total-x) (/ keyhole-total-x 2)))
         (-
           (+ (/ sidewall-y 2)
              (get-in col-offsets [(dec cols) :y])
              primary-wall-squish)
           (/ keyhole-total-y 2)
           wall-thickness)
         0]
        sidewall-shape))

; all the walls of the primary enclosure in position
; (wall-cols, wall-connectors, sidewalls)
(def primary-enclosure
    (translate [0 0 primary-wall-translate-z]
        (union
            sidewall-left
            sidewall-right
            wall-cols
            wall-connectors-north
            wall-connectors-south)))

;;;;;;;;;;;;;;;;;
; Thumb Cluster ;
;;;;;;;;;;;;;;;;;

; locate the thumb keyholes
; place them along the arc of a circle with
; a specified radius separated by a step
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

; place thumbs (see above) according to parameters
(def thumbs-places
    (make-thumbs-places
        thumbs-total
        thumbs-radius
        thumbs-step
        thumbs-start))

; represents the thin face on the left/right side of a thumb keyhole
; used to hull together with the opposite side to connect the thumbholes below
(defn make-thumbs-connector-face [[r t] & flags]
    (let [half-raw (/ keyhole-total-x 2)
          x-offset (if (contains? (set flags) :left)
                      half-raw
                      (-' half-raw))
          z-offset (/ keyhole-z 2)]
        (place (translate [x-offset 0 z-offset] (cube 0.0001 keyhole-total-y keyhole-z)) r t)))

; hull together opposing thumb connector faces between thumb keyholes
(def thumbs-connectors
    (union
        (map (fn [[t1 t2]]
                (let [f1 (make-thumbs-connector-face t1 :right)
                      f2 (make-thumbs-connector-face t2 :left)]
                    (hull f1 f2)))
             (partition 2 1 thumbs-places))))

; place keyholes at thumb places
(def thumbs-keyholes
    (union (map (fn [[r t]] (place keyhole r t))
                thumbs-places)))

; project thumb cluster with dummy keyholes for a solid (hole-less) 2d projection
(def thumbs-solid-projection
    (let [solid (union thumbs-connectors
                     (union (map (fn [[r t]] (place dummy-keyhole r t))
                                 thumbs-places)))]
      (project solid)))

; offset the projection to form a 2d perimeter, then
; extrude it to form the walls of the thumb cluster
(def thumbs-walls
    (->> thumbs-solid-projection
         (offset (-' wall-thickness))
         (difference thumbs-solid-projection)
         (extrude-linear {:height thumbs-wall-height})))

; extrude the projection to form the bottom
(def thumbs-bottom
    (->> thumbs-solid-projection
         (extrude-linear {:height bottom-height})
         (translate [0
                     thumbs-offset-y
                     (- base-z (/ bottom-height 2))])))

; union the thumb cluster together and position it (excluding the bottom)
(def thumbs
    (translate [0 thumbs-offset-y base-z]
       (union
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

; project a dummy enclosure (with solid keyholes)
; then extrude up to form the bottom
(def primary-bottom
    (->> (union
             primary-enclosure
             primary-dummy-keyholes)
         (project)
         (extrude-linear {:height bottom-height})
         (translate [0 0 (- base-z (/ bottom-height 2))])))

;;;;;;;;;;;;;;;;;;;
; Screw Placement ;
;;;;;;;;;;;;;;;;;;;

; position four screws in the primary enclosure
; (one at each corner)
(def primary-screw-places
    (let [support-radius (/ screw-support-diameter 2)
          kx-half (/ keyhole-total-x 2)
          ky-half (/ keyhole-total-y 2)
          first-col (first places-by-col)
          last-col (last places-by-col)
          ns-wall-effective (- wall-thickness primary-wall-squish)
          compensate (fn [coord & flags]
                         (let [flagset (set flags)
                               out? (contains? flagset :out)
                               y? (contains? flagset :y)]
                             (-> coord
                                 (get (if y? 1 0))
                                 ((if out? + -) kx-half (if y? ns-wall-effective wall-thickness))
                                 ((if out? - +) support-radius))))
          bottom-left-t (-> first-col (first) (get 1))
          top-left-t (-> first-col (last) (get 1))
          bottom-right-t (-> last-col (first) (get 1))
          top-right-t (-> last-col (last) (get 1))
          left-wall-x (compensate bottom-left-t :x :in)
          right-wall-x (compensate bottom-right-t :x :out)]
         ; bottom left
        [[left-wall-x (compensate bottom-left-t :y :in)]
         ; top left
         [left-wall-x (compensate top-left-t :y :out)]
         ; bottom right
         [right-wall-x (compensate bottom-right-t :y :in)]
         ; top right
         [right-wall-x (compensate top-right-t :y :out)]]))

; position two screws at the far ends of the thumb cluster
; a bit of trig to account for z rotation; can probably be cleaned up
(def thumbs-screw-places
    (let [t-right (first thumbs-places)
          t-left (last thumbs-places)
          t-right-r (get-in t-right [0 2])
          t-right-t (get t-right 1)
          t-left-r (get-in t-left [0 2])
          t-left-t (get t-left 1)
          right [(-> t-right-r
                     (Math/cos)
                     (* (- (/ keyhole-total-x 2) (/ screw-support-diameter 2)))
                     (+ (get t-right-t 0)))
                 (-> t-right-r
                     (Math/sin)
                     (* (- (/ keyhole-total-x 2) (/ screw-support-diameter 2)))
                     (+ (get t-right-t 1) thumbs-offset-y))]
          left [(-> t-left-r
                    (Math/cos)
                    (* (- (/ screw-support-diameter 2) (/ keyhole-total-x 2)))
                    (+ (get t-left-t 0)))
                (-> t-left-r
                    (Math/sin)
                    (* (- (/ screw-support-diameter 2) (/ keyhole-total-x 2)))
                    (+ (get t-left-t 1) thumbs-offset-y))]]
        [right left]))

; all screw places (primary enclosure and thumb cluster)
(def screw-places
    (concat primary-screw-places thumbs-screw-places))

; generate solid supports within the enclosure
; as reinforcement for the eventual screw cutouts
(def screw-supports
    (let [support (cylinder-fn (/ screw-support-diameter 2)
                               screw-support-height)]
        (union
            (map (fn [t]
                     (translate [(first t)
                                 (second t)
                                 (+ base-z (/ screw-support-height 2))]
                                support))
                 screw-places))))

; (screw cutouts are done below in Cutouts section)

;;;;;;;;;;;;;;;;;
; Friction Pads ;
;;;;;;;;;;;;;;;;;

; locations of friction pads -- at the center of the four
; primary corner keyholes and at ends of thumb cluster (next to screws)
(def friction-places
    (map
        (fn [[r t]] [[0 0 (get r 2)]
                     [(get t 0)
                      (get t 1)
                      (+ base-z
                         (-' bottom-height)
                         (/ friction-cutout-z 2))]])
        [(-> places-by-col (first) (first))
         (-> places-by-col (first) (last))
         (-> places-by-col (last) (first))
         (-> places-by-col (last) (last))
         (-> thumbs-places (first) (update-in [1 1] (partial + thumbs-offset-y)))
         (-> thumbs-places (last) (update-in [1 1] (partial + thumbs-offset-y)))]))

; (friction pad cutouts are done below in Cutouts section)

;;;;;;;;;;;
; Cutouts ;
;;;;;;;;;;;

; cut out a cylinder between primary enclosure and
; thumb cluster for cable routing
(def thumbs-port-cutout
    (->> (cylinder-fn 3 15)
         (rotate [(/ pi 2) 0 0])
         (translate [0
                     (+ thumbs-offset-y (/ keyhole-total-y 2)) ; this is an estimate
                     port-z])))

; cylinder cutout on sides (both left and right keyboards) for trrs jack
(def trrs-port-cutout
    (->> (cylinder-fn 2 wall-thickness)
         (rotate [0 (/ pi 2) 0])
         (translate [(- 0 (/ keyhole-total-x 2) (/ wall-thickness 2))
                     (- left-corner-y trrs-port-offset)
                     port-z])))

; usb port cutout (LEFT SIDE)
; (though it's not translated off the x axis currently,
; so the side doesn't matter)
; usb mini-b ports are 6.8mm x 3mm
(def usb-port-cutout
    (->> (cube 6.8 wall-thickness 3)
         (translate [0
                     (- left-corner-y (/ wall-thickness 2))
                     port-z])))

; form the shape of the screw cutouts:
; - self-tapping insert
; - hole for screw
; - cone for screw head
; NOTE origin is located between insert and base hole;
; i.e. it should be translated to base-z (between enclosure and bottom)
(def screw-cutout-shape
    (union
        ; insert
        (translate [0 0 (/ screw-insert-height 2)]
                   (cylinder-fn (/ screw-insert-diameter 2) screw-insert-height))
        ; base hole
        (translate [0 0 (-' (/ bottom-height 2))]
                   (cylinder-fn (/ screw-diameter 2) bottom-height))
        ; base conical head
        (translate [0 0 (- (/ screw-cone-height 2) bottom-height)]
                   (cylinder-fn [(/ screw-cone-diameter 2) (/ screw-diameter 2)] screw-cone-height))))

; position screw cutouts
(def screw-cutouts
    (union
        (map (fn [t]
                 (translate [(first t) (second t) base-z]
                            screw-cutout-shape))
             screw-places)))

; friction pad cutout shape
(def friction-cutout-shape
    (cube
        friction-cutout-x
        friction-cutout-y
        friction-cutout-z))

; position friction pad cutouts
(def friction-cutouts
    (union
        (map (fn [[r t]]
                 (place friction-cutout-shape r t))
             friction-places)))

; Collect individual cutouts

; cutouts common to both sides (including bottoms)
(def universal-cutouts
    (union
        thumbs-port-cutout
        trrs-port-cutout
        screw-cutouts
        friction-cutouts))

; cutouts for right side
(def right-cutouts
    (union))

; cutouts for left side (relative to left mirror)
(def left-cutouts
    (union
        usb-port-cutout))

;;;;;;;;;;;;
; Finalize ;
;;;;;;;;;;;;

(def bottom-right
    (difference
        (union
            primary-bottom
            thumbs-bottom)
        universal-cutouts))

(def bottom-left
    (make-left bottom-right))

; everything common to both sides
(def final
    (union
        (difference
            (union
                primary-enclosure
                primary-keyholes
                primary-sheaths
                thumbs
                screw-supports)
            universal-cutouts)
        (if include-bottom? bottom-right nil)))

(def right
    (difference
        final
        right-cutouts))

(def left
    (difference
        (make-left final)
        left-cutouts))

(defn save []
    (spit "things/right.scad" (apply write-scad right))
    (spit "things/left.scad" (apply write-scad left))
    (spit "things/bottom-right.scad" (apply write-scad bottom-right))
    (spit "things/bottom-left.scad" (apply write-scad bottom-left)))

(defn -main
    "Save keyboard as scad"
    [& args]
    (make-parents "things/dummy.txt")
    (save)
    (println "Generated scad files"))
