(ns longest-path.core
  (:require clojure.data.int-map))

(require 'clojure.edn)

(defn read-2tree [file]
  (with-open [r (java.io.PushbackReader. (clojure.java.io/reader file))]
     (loop [tree (transient (clojure.data.int-map/int-map))
            v (clojure.edn/read {:eof nil} r)]
       (if v
         (recur (assoc! tree v (clojure.edn/read {:readers {'s clojure.data.int-map/int-set}} r))
                (clojure.edn/read {:eof nil} r))
         tree))))

(defn write-2tree-to-file [s]
  (clojure.string/replace (subs s 1 (dec (.length s))) "#{" "#s #{"))

