(ns wator
  (:require [cljs.reader :as reader]
            [gui] [simul])
  (:import [simul.Fish] [simul.Shark]))

(def version "0.1")
(def unsupported 
  (str "<h1>Unsupported Browser</h1>" 
       "<p>Please consider upgrading to the latest version of Chrome, Firefox, "
       "Opera, Safari or IE9.</p>"))

(def fish-color {nil :blue, simul.Shark :red, simul.Fish :yellow})

(def ocean-width 40)
(def ocean-height 40)
(def nfish 150)
(def nsharks 20)

(def running (atom nil))

(defn ^:export set-param [param]
  (let [param-key (keyword param)
        param-value (gui/get-value param)]
    (simul/set-param param-key param-value)))

(defn display-ocean []
  (let [canvas (gui/get-element :watorCanvas)
        ctx (gui/get-context canvas)]
    (doseq [x (range ocean-width) y (range ocean-height)]
      (let [value (get-in (simul/get-matrix) [x y])]
        (gui/draw-symbol ctx (* x 10) (* y 10) 5 
                         (fish-color (type value)))))))
  
(defn draw [time-stamp]
  (when @running
    (simul/next-chronon)
    (display-ocean)
    (gui/set-value  :nchronons (swap! running inc))
    (let [count (simul/get-population)]
      (doseq [k (keys count)] (gui/set-value k (k count)))
      (if (or (zero? (:nsharks count)) (zero? (:nfish count)))
        (reset! running nil)
        (js/window.requestAnimationFrame draw)))))

(defn start []
  (when (not @running)
    (reset! running 0)
    (js/window.requestAnimationFrame draw)))

(defn stop []
  (reset! running nil))

(defn reset[]
  (stop)
  (gui/clear)
  (simul/set-initial-population ocean-width ocean-height nfish nsharks))

(defn ^:export setup []
  (if (not (gui/canvas-available))
    (.write js/document unsupported)
    (do
      (gui/set-value :version (str "v" version))
      (doseq [[k v] @simul/breed-params] (gui/set-value k v))
      (reset))))
