(ns autoroid.core
  (:use
    autoroid.shell
    autoroid.command
    ))

(declare adb-shell fb-addr default-pause)

;; # Key code

(def key-home 3)
(def key-back 4)
(def key-power 26)
(def key-unlock 82)


;; # Configuration

(def ^:private cfgs
  (atom {
         ; header block size of frame buffer.
         ; a block is the byte array for a pixel(usually 4-byte).
         :num-fb-header-blocks 3

         :long-touch-dur 2000     ; duration(msec) for long touch.
         :flick-dur 50            ; duration(msec) for flick.
         :drag-speed 1.0          ; 1.0 = 1000px per 1000msec.
         :adb-path nil            ; directory path for adb(only for windows).
         :width nil               ; screen width(optional).
         :height nil              ; screen height(optional).
         :default-pause :blink
         :blink-dur 300
         :short-dur 2000
         :middle-dur 8000
         :long-dur 15000
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
  (def ^:private fb-addr (mk-fb-addr cfgs))
  (def ^:private default-pause (:default-pause cfgs)))

(defn configure
  "Configure the library.
  Return new configuration map.
  Should be called very first.

  Ex: `(configure {:flick-dur 100})`
  "
  [config-map]
  (swap! cfgs merge config-map)
  (def-fns @cfgs)
  (when-not (and (:width config-map) (:height config-map))
    (let [[w h] (display-size)]
      (when-not w (throw (RuntimeException. "Unknown display size.")))
      (swap! cfgs merge {:width w :height h})))
  @cfgs)


;; # Utilities

(defn sleep
  "Equivalent to Thread/sleep.

  Ex: `(sleep 100)`
  "
  [ms]
  (Thread/sleep ms))

(defn pause
  "Pause for a while.
  `d` should be `:none`, `:blink`, `:short`, `:middle`, or `:long`.
  Repeat `n` times.

  Ex: `(pause :short)`
  "
  ([d]
   (case d
     :none nil
     :blink (sleep (:blink-dur @cfgs))
     :long (sleep (:long-dur @cfgs))
     :middle (sleep (:middle-dur @cfgs))
     :short (sleep (:short-dur @cfgs))))
  ([d n]
   (dotimes [m n] (pause d))))

(def ^{:dynamic true
       :doc "Makes `puts` silent. false by default."}
  *silent?* false)

(defn puts
  "Put args unless `*silent?*` is true.

  Ex: `(puts \"hoge fuga piyo\")`
  "
  [& args]
  (when-not *silent?*
    (apply println args)))

(defn exit
  "Equivalent to System/exit."
  ([] (exit 0))
  ([code]
   (System/exit code)))


;; # Operations(push keys, touch screen, etc...)

(defn adb-key
  "Push a key and pause for a while.

  Ex: `(adb-key key-back)`
  "
  ([k]
   (adb-key k default-pause))
  ([k d]
   (adb-shell (key-cmd k))
   (pause d)))

(defn adb-tap
  "Tap on a point and pause for a while.

  Ex: `(adb-tap 600 900)`
  "
  ([x y]
   (adb-tap x y default-pause))
  ([x y d]
   (adb-shell (tap-cmd x y))
   (pause d)))

(defn adb-long-touch
  "Make a long-touch and pause for a while.

  Ex: `(adb-long-touch 600 900)`
  "
  ([x y]
   (adb-long-touch x y default-pause))
  ([x y d]
   (adb-shell (long-touch-cmd x y (:long-touch-dur @cfgs)))
   (pause d)))

(defn adb-drag
  "Drag from a point to another point
  and pause for a while.

  Ex: `(adb-drag 300 0 300 200)`
  "
  ([x1 y1 x2 y2]
   (adb-drag x1 y1 x2 y2 default-pause))
  ([x1 y1 x2 y2 d]
   (adb-shell (drag-cmd x1 y1 x2 y2 (:drag-speed @cfgs)))
   (pause d)))

(defn adb-flick
  "Flick from a point to a direction(`dir`) for a distance(`dis`)
  and pause for a while.
  `dir` should be `:up`, `:down`, `:right`, or `:left`.

  Ex: `(adb-flick 100 100 :down 200)`
  "
  ([x y dir dis]
   (adb-flick x y dir dis default-pause))
  ([x y dir dis d]
   (adb-shell (flick-cmd x y dir dis (:flick-dur @cfgs)))
   (pause d)))

(defn adb-start
  "Start an app(activity).

  Ex: `(adb-start \"jp.hoge.myapp/.MainAct\")`
  "
  [act d]
  (adb-shell (start-cmd act))
  (pause d))

(defn adb-stop
  "Stop an app by force.

  Ex: `(adb-stop \"jp.hoge.myapp\")`
  "
  [app d]
  (adb-shell (stop-cmd app))
  (pause d))


;; # Lock/Unlock

(defn screen-locked?
  "Return true if screen is off(locked)."
  []
  (let [out (adb-shell (power-status-cmd))]
    (if (re-find #"Display Power: state=OFF" out) true false)))

(defn unlock-screen
  "Turn the screen on and unlock."
  []
  (when (screen-locked?)
    (adb-key key-power)
    (adb-key key-unlock)))

(defn lock-screen
  "Turn the screen off."
  []
  (when-not (screen-locked?)
    (adb-key key-power)))


;; # Handling frame buffer.

(defn pixel-offset
  "Get offset from (x, y).
  The offset can be used with `fb-pixel-val-seq`.
  You must specify `w`(=screen width) if using before configuration."
  ([x y] (pixel-offset x y (:width @cfgs)))
  ([x y w]
   (+ (* w y) x)))

(defn- fb-rgb-seq
  [out]
  (re-seq
    #"00000000 +([a-f\d]{2}) ([a-f\d]{2}) ([a-f\d]{2})"
    (clojure.string/lower-case out)))

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
  (let [addr (fb-addr off)
        out (adb-shell (pixel-cmd addr refresh?))]
    (first (fb-rgbstr-seq out))))

(defn fb-pixel-val-seq
  "Read bulk pixels(specified by `offs`) from frame buffer,
  return values in a sequence of ints(each represents 0xrrggbb).

  `offs` should be made by values from `pixel-offset`.
  Truethy `refresh?` captures the frame buffer(takes some time).
  "
  [offs refresh?]
  (let [addrs (map fb-addr offs)
        out (adb-shell (pixel-seq-cmd addrs refresh?))]
    (fb-rgbval-seq out)))

(defn adb-pixel-str
  "Retrieve a pixel value in string(\"rrggbb\").

  Truethy `refresh?` captures the frame buffer(takes some time).
  "
  ([x y] (adb-pixel-str x y nil))
  ([x y refresh?]
   (or (fb-pixel-str (pixel-offset x y) refresh?) "")))

