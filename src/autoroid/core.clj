(ns autoroid.core
  (:use
    clojure.java.shell
    )
  )

(declare adb-shell)

;; Configuration.
(def ^:private cfgs
  (atom {:num-fb-header-blocks 3  ; header block size of frame buffer.
                                  ; a block is the byte array for a pixel.
         :long-touch-dur 2000     ; duration(msec) for long touch.
         :flick-dur 50            ; duration(msec) for flick.
         :drag-speed 1            ; 
         :sdk-path nil
         :width nil
         :height nil
         }))

;; Key code.
(def key-power 26)
(def key-unlock 82)
(def key-home 3)
(def key-back 4)
(def num-fb-header-blocks 3)  ; フレームバッファヘッダのブロック数。

(defn win-os?
  "Returns true if OS is Windows."
  []
  (-> "os.name"
      System/getProperty
      (->> (re-matches #"^Win.+"))
      (if true false)))

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
     :blink (sleep 300)
     :long (sleep 15000)
     :middle (sleep 8000)
     :short (sleep 2000)))
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

(defn- resolve-path
  [base end]
  #_(-> base
      (java.nio.file.Paths/get (into-array String [])) ; needs java8 which portage doesn't support yet.
      (.resolve end)
      (.toAbsolutePath)
      (.toString)))

(defn- run-sh [args]
  (let [{:keys [out err]} (apply sh args)]
    (if (= out "") err out)))

(defn- mk-adb-shell []
  (let [osshell (if (win-os?) ["cmd" "/c"] [])
        oscmd ["adb" "shell"]
        adb-dir (if-let [sdk-path (:sdk-path @cfgs)]
                  (resolve-path sdk-path "platform-tools")
                  ".")]
    (fn [andcmd]
      (let [andcmd (str andcmd "\r\nexit\r\n")]
        (run-sh (concat osshell oscmd (list :in andcmd :dir adb-dir)))))))

(defn- query-display-size []
  (let [out (adb-shell "wm size")
        [_ w h] (re-matches #"\D+(\d+)x(\d+)\D+" out)]
    [(Integer/parseInt w) (Integer/parseInt h)]))

(defn- configure-display-size []
  (let [[w h] (query-display-size)]
    (swap! cfgs merge {:width w :height h})))

(defn configure
  "Configures the library.
   Valid keys are :sdk-path, :width, and :height.
   Returns new configuration map."
  [configuration-map]
  (swap! cfgs merge configuration-map)
  (def adb-shell (mk-adb-shell))
  (when-not (and (:width @cfgs) (:height @cfgs))
    (configure-display-size))
  @cfgs)

;; Building a string for adb-shell.
(defn- key-cmd
  [k]
  (str "input keyevent " k))
(defn- tap-cmd
  [x y]
  (str "input touchscreen tap " x " " y))
(defn- long-touch-cmd
  [x y]
  (str "input touchscreen swipe " x " " y " " x " " y " " (:long-touch-dur @cfgs)))
(defn- drag-cmd
  [x1 y1 x2 y2]
  (let [dx (- x1 x2)
        dy (- y1 y2)
        d2 (quot (+ (* dx dx) (* dy dy)) 1000)
        dur (* d2 (:drag-speed @cfgs))]
    (str "input touchscreen swipe " x1 " " y1 " " x2 " " y2 " " dur)))
(defn- flick-cmd
  [x1 y1 dir dis]
  (let [delta-map {:up [0 -1] :down [0 1] :right [1 0] :left [-1 0]}
        [dx dy] (map #(* dis %) (dir delta-map))
        x2 (+ x1 dx)
        y2 (+ y1 dy)]
    (str "input touchscreen swipe " x1 " " y1 " " x2 " " y2 " " (:flick-dur @cfgs))))
(defn- dd-cmd
  [addr]
  (str "dd if=keeper.raw bs=4 count=1 skip=" addr " 2>/dev/null |hd"))
(defn- pixel-seq-cmd
  [addrs refresh?]
  (let [addrs-str (clojure.string/join " " addrs)]
    (str "cd /sdcard/Temp && "
         (if refresh? "screencap keeper.raw && " "")
         "for a in " addrs-str "; do " (dd-cmd "$a") "; done")))
(defn- pixel-cmd
  [addr refresh?]
  (pixel-seq-cmd [addr] refresh?))
(defn- start-cmd
  [act]
  (str "am start " act))
(defn- stop-cmd
  [app]
  (str "am force-stop " app))

;; Issueing adb shell command.
(defn adb-key
  "Pushes a key and pause for a while."
  ([k]
   (adb-key k :short))
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
   (adb-shell (long-touch-cmd x y))
   (pause d)))
(defn adb-drag
  "Drags from a point to another point
   and pause for a while."
  ([x1 y1 x2 y2]
   (adb-drag x1 y1 x2 y2 :short))
  ([x1 y1 x2 y2 d]
   (adb-shell (drag-cmd x1 y1 x2 y2))
   (pause d)))
(defn adb-flick
  "Flicks from a point for a distance
   and pause for a while.
   `dir` should be up, :down, :right, or :left."
  ([x y dir dis]
   (adb-flick x y dir dis :short))
  ([x y dir dis d]
   (adb-shell (flick-cmd x y dir dis))
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
;; Handling frame buffer.
(defn pixel-offset
  "From (x, y) to offset in frame buffer.
   You must specify width if using before configuration."
  ([x y] (pixel-offset x y (:width @cfgs)))
  ([x y w]
   (+ (* w y) x)))
(defn- fb-addr
  [off]
  (+ off num-fb-header-blocks))
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

(defn power-on? []
  "Returns true if screen is on."
  (let [uw (quot (:width @cfgs) 4)
        uh (quot (:height @cfgs) 4)]
  (not (= "000000"
          (adb-pixel-str uw uh :refresh)
          (adb-pixel-str (* uw 2) (* uh 2))
          (adb-pixel-str (* uw 3) (* uh 3))))))

(defn power-on []
  "Turns the screen on and unlock."
  (when-not (power-on?)
    (adb-key key-power)
    (adb-key key-unlock)))

(defn power-off []
  "Turns the screen off."
  (when (power-on?)
    (adb-key key-power)))
