(ns data.entities.amr
  (:require [acc-text.nlg.dictionary :as dict]
            [clojure.java.io :as io]
            [data.entities.dictionary :as dict-entity]
            [data.utils :as utils])
  (:import (java.io File)))

(defn read-amr [^File f]
  (let [{:acc-text.nlg.amr/keys [event frames]} (utils/read-edn f)]
    {:id                 (utils/get-name f)
     :dictionary-item-id (apply dict/gen-id event)
     :thematic-roles     (transduce
                           (comp
                             (mapcat :syntax)
                             (map :role)
                             (remove (partial = :verb))
                             (map name)
                             (map (partial hash-map :type))
                             (distinct))
                           conj
                           frames)
     :frames             frames}))

(defn list-amr-files []
  (filter (comp
            #(re-find #"\.edn$" %)
            #(.getAbsolutePath ^File %))
          (-> (System/getenv "AMR_PATH")
              (or (io/resource "amr"))
              (io/file)
              (file-seq))))

(defn load-single [id]
  (when-let [f (some #(when (= (name id) (utils/get-name %)) %) (list-amr-files))]
    (utils/read-edn f)))

(defn load-all []
  (apply merge (map (fn [f]
                      {(utils/get-name f) (utils/read-edn f)})
                    (list-amr-files))))

(defn read-single [id]
  (when-let [f (some #(when (= (name id) (utils/get-name %)) %) (list-amr-files))]
    (read-amr f)))

(defn read-all [] (map read-amr (list-amr-files)))

(defn initialize []
  (doseq [{:acc-text.nlg.amr/keys [event]} (map utils/read-edn (list-amr-files))]
    (let [dictionary-item-id (apply dict/gen-id event)]
      (when-not (dict-entity/get-dictionary-item dictionary-item-id)
        (dict-entity/create-dictionary-item
          {:key          dictionary-item-id
           :partOfSpeech (first event)
           :name         (second event)
           :phrases      []})))))
