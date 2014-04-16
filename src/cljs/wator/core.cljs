;;;; Wator simulation (as described by A, K. Dwedney)

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

;; ocean size
(def ocean-width 40)
(def ocean-height 40)
;; initial numbers of fish and sharks
(def nfish 150)
(def nsharks 20)

(def running 
  "When non-nil, simulation is running and holds chronon count."
  (atom nil))

(defn ^:export set-param 
  "Textbox callback to obtain gui breed-params and set for simulation."
  [param]
  (let [param-key (keyword param)
        param-value (gui/get-value param)]
    (simul/set-param param-key param-value)))

(defn display-ocean [ocean color-map]
  (gui/display-matrix (:matrix ocean) (:width ocean) (:height ocean) 
                      color-map))
  
(defn draw 
  "Process next chronon and update display. Continues to animate until
  one population reaches zero or @running is set nil."
  [time-stamp]
  (when @running
    (simul/next-chronon)
    (display-ocean @simul/ocean fish-color)
    (gui/set-value  :nchronons (swap! running inc))
    (let [count (simul/get-population)]
      (doseq [k (keys count)] (gui/set-value k (k count)))
      (if (or (zero? (:nsharks count)) (zero? (:nfish count)))
        (reset! running nil)
        (gui/animate draw)))))

(defn start []
  (when (not @running)
    (reset! running 0)
    (gui/animate draw)))

(defn stop []
  (reset! running nil))

(defn reset []
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
