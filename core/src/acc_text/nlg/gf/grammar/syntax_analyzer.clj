(ns acc-text.nlg.gf.grammar.syntax-analyzer
  (:require [clojure.spec.alpha :as s]
            [clojure.tools.logging :as log]))

(s/def ::pos #{:VERB :LEX :ADP :NP :AUX})

(s/def ::role #{"Agent" "co-Agent"})

(s/def ::type #{:literal :function :operator})

(s/def ::predicate string?)
(s/def ::value string?)

(s/def ::syntax (s/keys :opt-un [::pos ::role ::type ::value]))

(s/def ::predicate-syntax (s/merge ::syntax (s/keys :opt-un [::predicate])))

(defn predicate [pos]
  (condp = pos
    :AUX "(copula Sg)"
    nil))

(defn attach-predicate [{:keys [pos] :as m}]
  (if-let [pred (predicate pos)]
    (do (log/debugf "Predicate for %s => %s" pos pred)
        (assoc m :predicate pred))
    m))

(s/fdef attach-predicate
  :args (s/cat :syntax ::syntax)
  :ret ::predicate-syntax
  :fn (fn [{{syn :syntax} :args ret :ret}]
        (and (= (:pos syn) (:pos ret))
             (= (:function syn) (:function ret))
             (= (:role syn) (:role ret)))))

(defn add-predicates [{body :body :as grammar}]
  (assoc grammar :body (map attach-predicate body)))


