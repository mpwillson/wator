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

(defn moved? [xy nxy]
  (let [[x y] xy
        [nx ny] nxy]
    (or (not= x nx) (not= y ny))))

(defn neighbours [xy width height]
  (let [deltas [[0 1] [1 1] [1 0] [1 -1] [-1 0] [0 -1] [-1 -1] [-1 1]]
        [x y] xy]
        (for [[dx dy] deltas]
          [(mod (+ x dx) width) (mod (+ y dy) height)])))

(defn filter-matrix [matrix xy filter-type]
  (let [target (get-in matrix xy)]
    (= (type target) filter-type)))

(defn neighbours-type [nbtype matrix width height xy]
  (let [ncells (neighbours xy width height)]
    (filter #(filter-matrix matrix % nbtype) ncells)))

(defn inc-attr [new-fish attribute]
  (let [f (:fish new-fish)]
        (assoc new-fish :fish (assoc f attribute (inc (attribute f))))))

(defn update [new-fish xy ocean]
  (let [matrix (:matrix ocean)]
    (assoc ocean :matrix
              (assoc-in matrix xy (:fish new-fish)))))

(defn insert-new [moved breed birth-type xy ocean]
  (cond
   (and moved breed) (update (NewFish. birth-type nil) xy  ocean)
   moved (update (NewFish. nil nil) xy ocean)
   true ocean))

(defn coords[ocean]
  (for [x (range (:width ocean))
        y (range  (:height ocean))] [x y]))

;; functions to support new fish generation actions

(defn seek [new-fish type xy ocean]
  (let [matrix (:matrix ocean)
        width (:width ocean)
        height (:height ocean)]
    (if (moved? xy (:xy new-fish))
      new-fish
      (let [type-neighbours (neighbours-type type matrix width height xy)]
        (if (seq type-neighbours)
          (let [nxy (nth type-neighbours 
                         (rand-int (count type-neighbours)))]
            (assoc new-fish :xy nxy))
          new-fish)))))

(defn seek-lunch [new-fish prey xy ocean]
  (let [nf (seek new-fish prey xy ocean)]
    (if (moved? xy (:xy nf))
      (assoc nf :fish (assoc (:fish nf) :starve 0))
      (if (> (:starve (:fish nf)) (:sharkstarve @breed-params))
        (assoc nf :fish nil)
        (inc-attr nf :starve)))))

(defn breed-and-update [new-fish xy birth-type breed-age ocean]
    (let [f (:fish new-fish)
          nxy (:xy new-fish)
          breed (and f (= 0 (mod (:age f) breed-age)))]
        (->> ocean (update new-fish nxy)
           (insert-new (moved? xy nxy) breed birth-type xy))))

;; definition of ocean dwellers

(defprotocol Generate 
  (live [this xy ocean]))

(defrecord Fish [age] Generate
  (live [this xy ocean]
        (-> (NewFish. this xy)
            (inc-attr :age)
            (seek nil xy ocean)
            (breed-and-update xy (Fish. 0) 
                              (:fishbreed @breed-params) ocean))))

(defrecord Shark [age starve] Generate
  (live [this xy ocean]
        (-> (NewFish. this xy)
            (inc-attr :age)
            (seek-lunch Fish xy ocean)
            (seek nil xy ocean)
            (breed-and-update xy (Shark. 0 0)
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
    (let [xy (random-position width height)]
      (if (nil? (get-in matrix xy))
        (recur (assoc-in matrix xy (fish-create-fn))
               fish-create-fn (dec count) width height)
        (recur matrix fish-create-fn count width height)))))

(defn set-initial-population [width height nfish nsharks]
  (let [matrix (-> (make-matrix width height)
                   (seed-ocean make-shark nsharks width height)
                   (seed-ocean make-fish nfish width height))]
    (swap! ocean assoc :matrix matrix :width width :height height)))

(defn process-fish [new-ocean xy]
  (let [f (get-in (:matrix @ocean) xy)
        future-fish (get-in (:matrix new-ocean) xy)]
    (if (and f (= f future-fish))
      (live f xy new-ocean)
      new-ocean)))

(defn next-chronon []
    (swap! ocean (partial reduce process-fish) (coords @ocean)))

;; determine current ocean population

(defn count-population [count xy]
  (let [f (get-in (:matrix @ocean) xy)]
    (condp  = (type f)
        Fish (assoc count :nfish (inc (:nfish count)))
        Shark (assoc count :nsharks (inc (:nsharks count)))
        nil count)))

(defn get-population []
  (reduce count-population {:nsharks 0, :nfish 0} (coords @ocean)))
