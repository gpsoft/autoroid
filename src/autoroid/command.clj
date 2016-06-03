(ns autoroid.command)

;; Building a string for adb-shell.
(defn key-cmd
  [k]
  (str "input keyevent " k))

(defn tap-cmd
  [x y]
  (str "input touchscreen tap " x " " y))

(defn long-touch-cmd
  [x y dur]
  (str "input touchscreen swipe " x " " y " " x " " y " " dur))

(defn drag-cmd
  [x1 y1 x2 y2 speed]
  (let [dx (- x1 x2)
        dy (- y1 y2)
        d (Math/sqrt (+ (* dx dx) (* dy dy)))
        dur (int (* d speed))]
    (str "input touchscreen swipe " x1 " " y1 " " x2 " " y2 " " dur)))

(defn flick-cmd
  [x1 y1 dir dis dur]
  (let [delta-map {:up [0 -1] :down [0 1] :right [1 0] :left [-1 0]}
        [dx dy] (map #(* dis %) (dir delta-map))
        x2 (+ x1 dx)
        y2 (+ y1 dy)]
    (str "input touchscreen swipe " x1 " " y1 " " x2 " " y2 " " dur)))

(defn start-cmd
  [act]
  (str "am start " act))

(defn stop-cmd
  [app]
  (str "am force-stop " app))

(defn size-cmd
  []
  "wm size")

(defn power-status-cmd
  []
  "dumpsys power")






(defn- dd-cmd
  [addr]
  (str "dd if=keeper.raw bs=4 count=1 skip=" addr " 2>/dev/null |hd"))

(defn pixel-seq-cmd
  [addrs refresh?]
  (let [addrs-str (clojure.string/join " " addrs)]
    (str "cd /sdcard/Temp && "
         (if refresh? "screencap keeper.raw && " "")
         "for a in " addrs-str "; do " (dd-cmd "$a") "; done")))

(defn pixel-cmd
  [addr refresh?]
  (pixel-seq-cmd [addr] refresh?))
