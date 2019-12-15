(ns acc-text.nlg.gf.grammar.impl
  (:require [acc-text.nlg.amr :as amr]
            [acc-text.nlg.semantic-graph :as sg]
            [acc-text.nlg.dictionary :as dict]
            [clojure.math.combinatorics :refer [permutations]]
            [clojure.string :as str]))

(defn concept->name [{:keys [id type]}]
  (str (->> (str/split (name type) #"-")
            (map str/capitalize)
            (str/join))
       (name id)))

(defn get-child-with-role [concepts relations role]
  (some (fn [[relation concept]]
          (when (= role (:role relation)) concept))
        (zipmap relations concepts)))

(defmulti build-function (fn [concept _ _ _] (:type concept)))

(defmethod build-function :default [concept children _ _]
  {:name   (concept->name concept)
   :params (map concept->name children)
   :body   (for [child-concept children]
             {:type  :function
              :value (concept->name child-concept)})
   :ret    [:s "Str"]})

(defmethod build-function :data [{value :value :as concept} _ _ _]
  {:name   (concept->name concept)
   :params []
   :body   [{:type  :data
             :value value}]
   :ret    [:s "Str"]})

(defmethod build-function :quote [{value :value :as concept} _ _ _]
  {:name   (concept->name concept)
   :params []
   :body   [{:type  :literal
             :value value}]
   :ret    [:s "Str"]})

(defmethod build-function :dictionary-item [{value :value :as concept} _ _ {dictionary :dictionary}]
  {:name   (concept->name concept)
   :params []
   :body   [(assoc (get dictionary value) :type :dict-item)]
   :ret    [:s "Str"]})

(defmethod build-function :modifier [concept children relations _]
  (let [child-concept (get-child-with-role children relations :child)
        modifier-concepts (remove #(= (:id child-concept) (:id %)) children)]
    {:name   (concept->name concept)
     :params (map concept->name children)
     :body   (cond-> (map (fn [modifier-concept]
                            {:type  :function
                             :value (concept->name modifier-concept)})
                          modifier-concepts)
                     (some? child-concept) (concat [{:type  :function
                                                     :value (concept->name child-concept)}]))
     :ret    [:s "Str"]}))

(defmethod build-function :amr [{value :value :as concept} children relations {amr :amr dictionary :dictionary}]
  (let [role-map (reduce (fn [m [{role :role {attr-name :name} :attributes} concept]]
                           (cond-> m
                                   (some? attr-name) (assoc (keyword attr-name) concept)
                                   (= :function role) (assoc :verb concept)))
                         {}
                         (zipmap relations children))]
    {:name   (concept->name concept)
     :params (map concept->name children)
     :body   (for [syntax (->> value (get amr) (::amr/frames) (map :syntax))]
               (for [{:keys [role implicit] :as item} syntax]
                 (merge
                   (dissoc item :role :implicit)
                   (cond
                     (some? implicit) (let [role (get role-map role)
                                            {relations :relations} (get dictionary (:value role))
                                            entry (first (get relations implicit))]
                                        (if (some? entry)
                                          (-> dictionary
                                              (get (dict/gen-id implicit entry))
                                              (assoc :type :dict-item))
                                          {:type :placeholder}))
                     (contains? role-map role) {:type  :function
                                                :value (concept->name (get role-map role))}))))
     :ret    [:s "Str"]}))

(defmethod build-function :shuffle [concept children _ _]
  {:name   (concept->name concept)
   :params (map concept->name children)
   :body   (for [permutation (filter seq (permutations children))]
             (for [child-concept permutation]
               {:type  :function
                :value (concept->name child-concept)}))
   :ret    [:s "Str"]})

(defmethod build-function :synonyms [concept children _ _]
  {:name   (concept->name concept)
   :params (map concept->name children)
   :body   (for [child-concept children]
             [{:type  :function
               :value (concept->name child-concept)}])
   :ret    [:s "Str"]})

(defmethod build-function :reference [concept children _ _]
  {:name   (concept->name concept)
   :params (map concept->name children)
   :body   (for [child-concept children]
             [{:type  :function
               :value (concept->name child-concept)}])
   :ret    [:s "Str"]})

(defn build-grammar [module instance {::sg/keys [concepts relations]} context]
  (let [context (update context :dictionary dict/flatten-dictionary)]
    #:acc-text.nlg.gf.grammar{:module   module
                              :instance instance
                              :flags    {:startcat (concept->name (first concepts))}
                              :syntax   (let [concept-map (zipmap (map :id concepts) concepts)
                                              relation-map (group-by :from relations)]
                                          (map (fn [{id :id :as concept}]
                                                 (let [relations (get relation-map id)
                                                       children (map (comp concept-map :to) relations)]
                                                   (build-function concept children relations context)))
                                               concepts))}))
