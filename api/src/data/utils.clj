(ns data.utils
  (:require [clj-yaml.core :as yaml]
            [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:import (java.io File PushbackReader)
           (java.util UUID)
           (java.time Instant)))

(defn gen-uuid []
  (str (UUID/randomUUID)))

(defn ts-now []
  (int (.getEpochSecond (Instant/now))))

(defn read-yaml [^File f]
  (yaml/parse-string (slurp f)))

(defn read-edn [^File f]
  (with-open [r (io/reader f)]
    (edn/read (PushbackReader. r))))

(defn get-ext [^File f]
  (let [filename (.getName f)
        index (.lastIndexOf filename ".")]
    (when (not= index -1)
      (subs filename index (count filename)))))

(defn get-name [^File f]
  (let [filename (.getName f)
        index (.lastIndexOf filename ".")]
    (cond-> filename (not= index -1) (subs 0 index))))
