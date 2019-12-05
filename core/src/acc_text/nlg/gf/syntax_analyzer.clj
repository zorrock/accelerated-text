(ns acc-text.nlg.gf.syntax-analyzer
  (:require [acc-text.nlg.gf.grammar :as grammar]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(defmulti transform-expression :pos)

(defmethod transform-expression :default [expr] expr)

(defmethod transform-expression :AUX [{{tense :tense} :selectors}]
  {:type   :function
   :params []
   :value  (->> (cond-> ["copula" "Sg"]
                        (some? tense) (conj (case tense
                                              :past "VPast")))
                (str/join " ")
                (format "(%s)"))})

(defn transform [grammar]
  (update grammar ::grammar/syntax (fn walk [syntax]
                                     (map #(if (sequential? %)
                                             (walk %)
                                             (transform-expression %))
                                          syntax))))

(s/fdef transform
        :args (s/cat :grammar :acc-text.nlg.gf.grammar/grammar)
        :ret :acc-text.nlg.gf.grammar/grammar)
