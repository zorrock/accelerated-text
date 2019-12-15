(ns data.entities.dictionary
  (:require [acc-text.nlg.dictionary :as dict]
            [api.config :refer [conf]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [data.db :as db]
            [data.utils :as utils]
            [mount.core :refer [defstate]]
            [clojure.tools.logging :as log])
  (:import (java.io File)))

(defstate reader-flags-db :start (db/db-access :reader-flag conf))
(defstate dictionary-combined-db :start (db/db-access :dictionary-combined conf))

(defn list-readers []
  (db/list! reader-flags-db 100))

(defn get-default-flags []
  (->> (list-readers)
       (map (fn [r] {(keyword (:id r)) :DONT_CARE}))
       (into {})))

(defn get-reader [key]
  (db/read! reader-flags-db key))

(defn list-dictionary []
  (db/list! dictionary-combined-db 100))

(defn get-dictionary-item [key]
  (when-not (str/blank? key)
    (db/read! dictionary-combined-db key)))

(defn text->phrase
  ([text parent-id default-usage]
   (text->phrase text parent-id default-usage (get-default-flags)))
  ([text parent-id default-usage default-flags]
   {:id    (format "%s/%s" parent-id (utils/gen-uuid))
    :text  text
    :flags (assoc default-flags :default default-usage)}))

(defn create-dictionary-item [{:keys [key name phrases partOfSpeech]}]
  (when-not (str/blank? name)
    (db/write! dictionary-combined-db key {:name         name
                                           :partOfSpeech partOfSpeech
                                           :phrases      (map #(text->phrase % key :YES) phrases)})))

(defn delete-dictionary-item [key]
  (db/delete! dictionary-combined-db key))

(defn update-dictionary-item [item]
  (db/update! dictionary-combined-db (:key item) (dissoc item :key)))

(defn list-dictionary-files []
  (filter (comp
            #(re-find #"\.edn$" %)
            #(.getAbsolutePath ^File %))
          (-> (System/getenv "DICT_PATH")
              (or (io/resource "dictionary"))
              (io/file)
              (file-seq))))

(defn load-dictionary []
  (transduce (map utils/read-edn) merge (list-dictionary-files)))

(defn load-dictionary-entries [keys]
  (->> (load-dictionary)
       (mapcat (fn [[category {entries :entries :as body}]]
                 (for [[parent-key {forms :forms :as parent-entry}] entries]
                   {category (assoc
                               body
                               :entries (cond-> (reduce-kv (fn [m key form]
                                                             (cond-> m
                                                                     (contains?
                                                                       (set keys)
                                                                       (dict/gen-id category parent-key key))
                                                                     (assoc
                                                                       (dict/gen-id parent-key key)
                                                                       (-> parent-entry
                                                                           (dissoc :forms)
                                                                           (merge form)
                                                                           (assoc :value key)))))
                                                           {}
                                                           forms)
                                                (contains?
                                                  (set keys)
                                                  (dict/gen-id category parent-key))
                                                (assoc
                                                  parent-key
                                                  (assoc parent-entry :value parent-key))))})))
       (apply (partial merge-with merge))))

(defn initialize []
  (doseq [[key {:keys [pos value forms]}] (dict/flatten-dictionary (load-dictionary))]
    (create-dictionary-item
      {:key          key
       :partOfSpeech (name pos)
       :name         value
       :phrases      (into [] (keys forms))})))
