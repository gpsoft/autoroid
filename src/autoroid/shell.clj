(ns autoroid.shell
  (:use
    [clojure.java.shell :only (sh)]
    ))

(defn- win-os?
  "Returns true if OS is Windows."
  []
  (-> "os.name"
      System/getProperty
      (->> (re-matches #"^Win.+"))
      (if true false)))

(defn- run-sh [args]
  (let [{:keys [out err exit]} (apply sh args)]
    (if (= exit 0) out nil)))

(defn mk-adb-shell [cfgs]
  "Make a function which calls adb shell command.'
   For win: cmd /c adb shell args...
   For others: adb shell args...
   'args' are command lines that will be run in adb shell.
   Returns stdout or nil.
   "
  (let [os-prefix (if (win-os?) ["cmd" "/c"] [])
        adb ["adb" "shell"]
        cur-dir (if-let [adb-path (:adb-path cfgs)]
                  adb-path
                  ".")
        ]
    (fn [args]
      (let [args (str args ";exit\r\n")
            out (run-sh (concat os-prefix adb (list :in args :dir cur-dir)))
            ; _ (print out)
            ]
        out))))
