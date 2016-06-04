# autoroid

A Clojure library designed to operate Android devices from remote(using adb).

# Usage

    (ns yourapp.core
      (:use
        autoroid.core))

    (defn -main
      [& args]
      (configure {:width 1200 :height 1920})
      (unlock-screen)
      (adb-start "jp.hoge.yourapp/.MainActivity")
      (adb-tap 300 200)
      (when (= (adb-pixel-str 500 32) "cc8800")
        (adb-long-touch 450 900))
      (adb-key key-back)
      (adb-stop "jp.hoge.yourapp"))

# Document

See [API document](docs/index.html).
