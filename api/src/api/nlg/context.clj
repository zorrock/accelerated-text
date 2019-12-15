(ns api.nlg.context
  (:require [acc-text.nlg.semantic-graph :as sg]
            [api.nlg.dictionary :as dictionary]
            [data.entities.amr :as amr-entiry]
            [data.entities.dictionary :as dict-entity]))

(defn get-reader-profiles [reader-model]
  (or
    (seq
      (reduce-kv (fn [acc k v]
                   (cond-> acc
                           (true? v) (conj k)))
                 []
                 reader-model))
    [:default]))

(defn get-values [semantic-graph type]
  (->> (get semantic-graph ::sg/concepts)
       (filter #(= type (:type %)))
       (map :value)
       (set)))

(defn build-dictionary-context [semantic-graph reader-profiles]
  (reduce (fn [m value]
            (assoc m value (->> reader-profiles
                                (mapcat #(dictionary/search value %))
                                (into #{})
                                (sort)
                                (vec))))
          {}
          (get-values semantic-graph :dictionary-item)))

(defn remove-amr-examples [m]
  (reduce-kv (fn [m k v]
               (assoc m k (update
                            v
                            :acc-text.nlg.amr/frames
                            #(map (fn [m] (dissoc m :examples)) %))))
             {}
             m))

(defn build-context
  ([semantic-graph]
   (build-context semantic-graph {:default true}))
  ([semantic-graph reader-model]
   (let [_ (get-reader-profiles reader-model)]
     {:dictionary (dict-entity/load-dictionary)
      :amr        (remove-amr-examples
                    (select-keys
                      (amr-entiry/load-all)
                      (get-values semantic-graph :amr)))})))
