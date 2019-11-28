(ns acc-text.nlg.gf.grammar.syntax-analyzer
  (:require [clojure.spec.alpha :as s]))

(s/def ::pos #{:VERB :LEX :ADP :NP})

(s/def ::role #{:agent :co-agent})

(s/def ::type #{:literal :function})

(s/def ::predicate string?)

(s/def ::syntax (s/keys :req-un [::pos ::role ::type]))

(defn predicate [pos]
  (condp = pos
    :AUX "(copula Sg)"
    nil))

(defn attach-predicate [{:keys [pos] :as m}]
  (if-let [pred (predicate pos)]
    (assoc m :predicate pred)
    m))

(defn add-predicates [{body :body}]
  (map attach-predicate body))
