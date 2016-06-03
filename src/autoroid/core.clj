(ns autoroid.core
  (:use
    autoroid.shell
    autoroid.command
    ))

(declare adb-shell fb-addr)

;; Key code.
(def key-home 3)
(def key-back 4)
(def key-power 26)
(def key-unlock 82)

;; Configuration.
(def ^:private cfgs
  (atom {
         ; header block size of frame buffer.
         ; a block is the byte array for a pixel(usually 4-byte).
         :num-fb-header-blocks 3

         :long-touch-dur 2000     ; duration(msec) for long touch.
         :flick-dur 50            ; duration(msec) for flick.
         :drag-speed 1.0          ; 1.0 = 1000px per 1000msec.
         :sdk-path nil
         :width nil
         :height nil
         :blink-dur 300
         :long-dur 15000
         :middle-dur 8000
         :short-dur 2000
         }))

(defn- display-size []
  (let [out (adb-shell (size-cmd))
        [_ w h] (re-matches #"\D+(\d+)x(\d+)\D+" out)]
    (if w
      [(Integer/parseInt w) (Integer/parseInt h)]
      nil)))

(defn- mk-fb-addr
  [cfgs]
  (fn [off]
    (+ off (:num-fb-header-blocks cfgs))))

(defn- def-fns [cfgs]
  (def adb-shell (mk-adb-shell cfgs))
  (def ^:private fb-addr (mk-fb-addr cfgs)))

(defn configure
  "Configures the library.
   Returns new configuration map."
  [config-map]
  (swap! cfgs merge config-map)
  (def-fns @cfgs)
  (when-not (and (:width config-map) (:height config-map))
    (let [[w h] (display-size)]
      (when-not w (throw (RuntimeException. "Unknown display size.")))
      (swap! cfgs merge {:width w :height h})))
  @cfgs)


;; Utility.
(defn sleep
  "Equivalent to Thread/sleep."
  [ms]
  (Thread/sleep ms))

(defn pause
  "Pauses for a while.
   d should be :none, :blink, :short, :middle, or :long.
   Repeats n times."
  ([d]
   (case d
     :none nil
     :blink (sleep (:blink-dur @cfgs))
     :long (sleep (:long-dur @cfgs))
     :middle (sleep (:middle-dur @cfgs))
     :short (sleep (:short-dur @cfgs))))
  ([d n]
   (dotimes [m n] (pause d))))

(def ^:dynamic ^{:doc "Makes `puts` silent. false by default."} *silent?* false)
(defn puts
  "Puts args unless *silent?* is true."
  [& args]
  (when-not *silent?*
    (apply println args)))

(defn exit
  "Equivalent to System/exit."
  ([] (exit 0))
  ([code]
   (System/exit code)))


;; Push keys or touch/drag screen.
(defn adb-key
  "Pushes a key and pause for a while."
  ([k]
   (adb-key k :blink))
  ([k d]
   (adb-shell (key-cmd k))
   (pause d)))

(defn adb-tap
  "Taps a point and pause for a while."
  ([x y]
   (adb-tap x y :short))
  ([x y d]
   (adb-shell (tap-cmd x y))
   (pause d)))

(defn adb-long-touch
  "Makes a long-touch and pause for a while."
  ([x y]
   (adb-long-touch x y :short))
  ([x y d]
   (adb-shell (long-touch-cmd x y (:long-touch-dur @cfgs)))
   (pause d)))

(defn adb-drag
  "Drags from a point to another point
   and pause for a while."
  ([x1 y1 x2 y2]
   (adb-drag x1 y1 x2 y2 :short))
  ([x1 y1 x2 y2 d]
   (adb-shell (drag-cmd x1 y1 x2 y2 (:drag-speed @cfgs)))
   (pause d)))

(defn adb-flick
  "Flicks from a point for a distance
   and pause for a while.
   `dir` should be up, :down, :right, or :left."
  ([x y dir dis]
   (adb-flick x y dir dis :short))
  ([x y dir dis d]
   (adb-shell (flick-cmd x y dir dis (:flick-dur @cfgs)))
   (pause d)))

(defn adb-start
  "Start an app(activity)."
  ([act]
   (adb-start act :short))
  ([act d]
   (adb-shell (start-cmd act))
   (pause d)))

(defn adb-stop
  "Stop an app by force."
  ([app]
   (adb-stop app :short))
  ([app d]
   (adb-shell (stop-cmd app))
   (pause d)))


;; Lock/Unlock
(defn screen-locked? []
  "Returns true if screen is off(locked)."
  (let [out (adb-shell (power-status-cmd))]
    (if (re-find #"Display Power: state=OFF" out) true false)))

(defn unlock-screen []
  "Turns the screen on and unlock."
  (when (screen-locked?)
    (adb-key key-power)
    (adb-key key-unlock)))

(defn lock-screen []
  "Turns the screen off."
  (when-not (screen-locked?)
    (adb-key key-power)))


;; Handling frame buffer.
(defn pixel-offset
  "From (x, y) to offset in frame buffer.
   You must specify width if using before configuration."
  ([x y] (pixel-offset x y (:width @cfgs)))
  ([x y w]
   (+ (* w y) x)))
(defn- fb-rgb-seq
  [out]
  (re-seq #"00000000 +([a-f\d]{2}) ([a-f\d]{2}) ([a-f\d]{2})" (clojure.string/lower-case out)))
(defn- fb-rgbstr-seq
  [out]
  (let [sq (fb-rgb-seq out)]
    (map (fn [[_ r g b]] (str r g b)) sq)))
(defn- fb-rgbval-seq
  [out]
  (let [sq (fb-rgbstr-seq out)]
    (map #(Integer/parseInt % 16) sq)))
(defn- fb-pixel-str
  [off refresh?]
  (let [a (fb-addr off)
        out (adb-shell (pixel-cmd a refresh?))]
    (first (fb-rgbstr-seq out))))
(defn fb-pixel-val-seq
  "Reads bulk pixels from frame buffer,
   returns values in a sequence of ints.
   locations of pixels should be made by pixel-offset function."
  [offs refresh?]
  (let [addrs (map fb-addr offs)
        out (adb-shell (pixel-seq-cmd addrs refresh?))]
    (fb-rgbval-seq out)))
(defn adb-pixel-str
  "Retrieves a pixel value in string(RRGGBB)."
  ([x y] (adb-pixel-str x y nil))
  ([x y refresh?]
   (or (fb-pixel-str (pixel-offset x y) refresh?) "")))

