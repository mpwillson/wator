(ns simul)

(def ocean (atom {:matrix nil :width 0 :height 0}))
(def breed-params (atom {:fishbreed 5 :sharkbreed 10 :sharkstarve 5}))

(defrecord NewFish [fish xy])

;; set/get breed parameters
(defn set-param [key value]
  (swap! breed-params assoc key value))

(defn get-param [key]
  (@breed-params key))

;; utility functions

(defn random-position [width height]
  [(rand-int width) (rand-int height)])

(defn make-matrix [m n]
  (vec (repeat m (vec (repeat n nil)))))

(defn set-matrix [x y val]
  (swap! ocean assoc :matrix (assoc-in (:matrix @ocean) [x y] val)))

(defn get-matrix []
  (:matrix @ocean))

(defn neighbours [x y width height]
  (let [deltas [[0 1] [1 1] [1 0] [1 -1] [-1 0] [0 -1] [-1 -1] [-1 1]]]
        (for [[dx dy] deltas]
          [(mod (+ x dx) width) (mod (+ y dy) height)])))

(defn filter-matrix [matrix xy filter-type]
  (let [target (get-in matrix xy)]
    (= (type target) filter-type)))

(defn neighbours-type [nbtype matrix width height x y]
  (let [ncells (neighbours x y width height)]
    (filter #(filter-matrix matrix % nbtype) ncells)))

(defn inc-attr [new-fish attribute]
  (let [f (:fish new-fish)]
        (assoc new-fish :fish (assoc f attribute (inc (attribute f))))))

(defn update [new-fish x y ocean]
  (let [matrix (:matrix ocean)]
    (assoc ocean :matrix
              (assoc-in matrix [x y] (:fish new-fish)))))

(defn insert-new [moved breed birth-type x y ocean]
  (cond
   (and moved breed) (update (NewFish. birth-type nil) x y ocean)
   moved (update (NewFish. nil nil) x y ocean)
   true ocean))

(defn coords[ocean]
  (for [x (range (:width ocean))
        y (range  (:height ocean))] [x y]))

;; functions to support new fish generation actions

(defn seek [new-fish type x y ocean]
  (let [matrix (:matrix ocean)
        width (:width ocean)
        height (:height ocean)
        [nx ny] (:xy new-fish)
        already-moved (or (not= x nx) (not= y ny))]
    (if already-moved
      new-fish
      (let [type-neighbours (neighbours-type type matrix width height x y)]
        (if (seq type-neighbours)
          (let [nxy (nth type-neighbours 
                         (rand-int (count type-neighbours)))]
            (assoc new-fish :xy nxy))
          new-fish)))))

(defn seek-lunch [new-fish prey x y ocean]
  (let [nf (seek new-fish prey x y ocean)
        [nx ny] (:xy nf)
        moved (or (not= x nx) (not= y ny))]
    (if moved
      (assoc nf :fish (assoc (:fish nf) :starve 0))
      (if (> (:starve (:fish nf)) (:sharkstarve @breed-params))
        (assoc nf :fish nil)
        (inc-attr nf :starve)))))

(defn breed-and-update [new-fish x y birth-type breed-age ocean]
    (let [f (:fish new-fish)
          [nx ny] (:xy new-fish)
          moved (or (not= x nx) (not= y ny))
          breed (and f (= 0 (mod (:age f) breed-age)))]
        (->> ocean (update new-fish nx ny)
           (insert-new moved breed birth-type x y))))

;; definition of ocean dwellers

(defprotocol Generate 
  (live [this x y ocean]))

(defrecord Fish [age] Generate
  (live [this x y ocean]
        (-> (NewFish. this [x y])
            (inc-attr :age)
            (seek nil x y ocean)
            (breed-and-update x y (Fish. 0) (:fishbreed @breed-params) ocean))))

(defrecord Shark [age starve] Generate
  (live [this x y ocean]
        (-> (NewFish. this [x y])
            (inc-attr :age)
            (seek-lunch Fish x y ocean)
            (seek nil x y ocean)
            (breed-and-update x y (Shark. 0 0)
                              (:sharkbreed @breed-params) ocean))))

;; ocean denizen constructors
(defn make-fish[]
  (Fish. (rand-int (:fishbreed @breed-params))))

(defn make-shark[]
  (Shark. (rand-int (:sharkbreed @breed-params)) 0))

;; create and update ocean state

(defn seed-ocean [matrix fish-create-fn count width height]
  (if (zero? count)
    matrix
    (let [[x y] (random-position width height)]
      (if (nil? (get-in matrix [x y]))
        (recur (assoc-in matrix [x y] (fish-create-fn))
               fish-create-fn (dec count) width height)
        (recur matrix fish-create-fn count width height)))))

(defn generate-initial-population [width height nfish nsharks]
  (let [matrix (-> (make-matrix width height)
                   (seed-ocean make-shark nsharks width height)
                   (seed-ocean make-fish nfish width height))]
    (swap! ocean assoc :matrix matrix :width width :height height)))

(defn process-fish [new-ocean [x y]]
  (let [f (get-in (:matrix @ocean) [x y])
        future-fish (get-in (:matrix new-ocean) [x y])]
    (if (and f (= f future-fish))
      (live f x y new-ocean)
      new-ocean)))

(defn next-generation []
    (swap! ocean (partial reduce process-fish) (coords @ocean)))

;; determine current ocean population

(defn count-population [count [x y]]
  (let [f (get-in (:matrix @ocean) [x y])]
    (condp  = (type f)
        Fish (assoc count :nfish (inc (:nfish count)))
        Shark (assoc count :nsharks (inc (:nsharks count)))
        nil count)))

(defn get-population []
  (reduce count-population {:nsharks 0, :nfish 0} (coords @ocean)))
