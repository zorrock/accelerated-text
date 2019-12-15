(ns acc-text.nlg.dictionary
  (:require [clojure.string :as str]))

(defn gen-id [& args]
  (->> args
       (map (comp
              #(str/replace % #"\s+" "-")
              name))
       (str/join "/")))

(defn flatten-dictionary [d]
  (into {} (for [[category {entries :entries :as item}] d
                 [parent-key {forms :forms :as parent-entry}] entries]
             (assoc
               (reduce-kv (fn [m key form]
                            (assoc
                              m
                              (gen-id category parent-key key)
                              (merge
                                (dissoc item :entries)
                                (dissoc parent-entry :forms)
                                (assoc form :value key))))
                          {}
                          forms)
               (gen-id category parent-key)
               (merge
                 (dissoc item :entries)
                 (assoc parent-entry :value parent-key))))))
