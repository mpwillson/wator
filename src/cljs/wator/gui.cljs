(ns gui
  (:require [cljs.reader :as reader]
            [goog.string :as gstring] ; both this and next line required for
            [goog.string.format]))    ; goog.string.format

(def color-map {:yellow "#F6E74E", :red "#FF8888", :blue "#2C6978"})

(defn format 
  "Emulate clojure format via goog.string.format."
  [fmt & args]
  (apply gstring/format fmt args))

(defn canvas-available 
  []
  (.-CanvasRenderingContext2D js/window))

(defn get-element [id]
  (.getElementById js/document (name id)))

(defn get-value [id]
  (reader/read-string (.-value (get-element id))))

(defn set-value [id value]
  (set! (.-value (get-element id)) value))

(defn get-checked [id]
  (.-checked (get-element id)))

(defn set-checked [id value]
  (set! (.-checked (get-element id)) value))

(defn get-context [canvas]
  (.getContext canvas "2d"))

;; drawing many circles consumes much CPU time
(defn draw-symbol 
  [ctx x y size color]
  (set! (.-fillStyle ctx) (color color-map))
  (.fillRect ctx x y size size))

(defn clear []
  (let [canvas (get-element :watorCanvas)
        width (.-width canvas)
        height (.-height canvas)
        ctx (get-context canvas)]
    (set! (.-fillStyle ctx) (:blue color-map))
    (.fillRect ctx 0 0 width height)
    #_(.clearRect ctx 0 0 width height)))

