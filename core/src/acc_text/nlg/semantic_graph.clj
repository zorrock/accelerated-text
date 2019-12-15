(ns acc-text.nlg.semantic-graph
  (:require [clojure.spec.alpha :as s]))

(s/def :acc-text.nlg.semantic-graph.attributes/name string?)

(s/def ::attributes
  (s/keys :opt-un [:acc-text.nlg.semantic-graph.attributes/name]))

(s/def :acc-text.nlg.semantic-graph.concept/id keyword?)

(s/def :acc-text.nlg.semantic-graph.concept/type
  #{:document-plan :segment :data :quote :dictionary-item :amr :shuffle :sequence :condition :if-statement
    :default-statement :comparator :boolean :variable :reference :modifier})

(s/def :acc-text.nlg.semantic-graph.concept/value string?)

(s/def ::concepts
  (s/coll-of
    (s/keys :req-un [:acc-text.nlg.semantic-graph.concept/id
                     :acc-text.nlg.semantic-graph.concept/type]
            :opt-un [:acc-text.nlg.semantic-graph.concept/value
                     ::attributes])))

(s/def :acc-text.nlg.semantic-graph.relation/from keyword?)

(s/def :acc-text.nlg.semantic-graph.relation/to keyword?)

(s/def :acc-text.nlg.semantic-graph.relation/role
  (s/or :core (s/and keyword? #(or (= :verb %) (re-matches #"^ARG\d+$" (name %))))
        :non-core #{:segment :instance :modifier :child :item :statement :predicate :comparable :expression :entity
                    :input :definition :pointer}))

(s/def ::relations
  (s/coll-of
    (s/keys :req-un [:acc-text.nlg.semantic-graph.relation/from
                     :acc-text.nlg.semantic-graph.relation/to
                     :acc-text.nlg.semantic-graph.relation/role]
            :opt-un [::attributes])))

(s/def ::graph
  (s/keys :req [::relations ::concepts]))
