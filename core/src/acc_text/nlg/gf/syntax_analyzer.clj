(ns acc-text.nlg.gf.syntax-analyzer
  (:require [acc-text.nlg.gf.grammar :as grammar]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(defmulti transform-expression :pos)

(defmethod transform-expression :default [expr] expr)

(defmethod transform-expression :AUX [{{:keys [tense number]} :selectors} data]
  {:type   :verb
   :params []
   :value  (->> (-> ["copula"]
                    (cond-> (some? number) (cond (case number
                                                   :singular "Sg"
                                                   :plural "Pl")))
                    (cond-> (nil? number) (conj "Sg"))
                    (cond-> (some? tense) (conj (case tense
                                                  :present "VPres"
                                                  :past "VPast"))))
                (str/join " ")
                (format "(%s).s"))})

(defn transform [grammar data]
  (update grammar ::grammar/syntax (fn walk [syntax]
                                     (map #(if (sequential? %)
                                             (walk %)
                                             (transform-expression %))
                                          syntax))))

(s/fdef transform
        :args (s/cat :grammar :acc-text.nlg.gf.grammar/grammar :data map?)
        :ret :acc-text.nlg.gf.grammar/grammar)
