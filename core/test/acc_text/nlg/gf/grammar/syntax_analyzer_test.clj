(ns acc-text.nlg.gf.grammar.syntax-analyzer-test
  (:require [acc-text.nlg.gf.grammar.syntax-analyzer :as sut]
            [clojure.test :refer [is deftest]]))

(deftest predicate-selection
  (is (= "(copula Sg)" (sut/predicate :AUX)))
  (is (= nil (sut/predicate :NP))))

(deftest predicate-attachment
  (is (= [{:pos :NP :type :function :value "Data05" :role "Agent"}
          {:type :operator :value "++"}
          {:pos :AUX :predicate "(copula Sg)"}
          {:type :operator :value "++"}
          {:pos :NP :type :function :value "Data07" :role "co-Agent"}]
         (sut/add-predicates
           {:name   "Amr03"
            :params ["DictionaryItem04"
                     "Data05"
                     "Data07"]
            :body   [{:pos :NP :type :function :value "Data05" :role "Agent"}
                     {:type :operator :value "++"}
                     {:pos :AUX}
                     {:type :operator :value "++"}
                     {:pos :NP :type :function :value "Data07" :role "co-Agent"}]
            :ret    [:s "Str"]}))))

