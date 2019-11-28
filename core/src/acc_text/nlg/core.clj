(ns acc-text.nlg.core
  (:require [acc-text.nlg.gf.generator :as generator]
            [acc-text.nlg.gf.grammar :as grammar]
            [acc-text.nlg.gf.grammar.syntax-analyzer :as syn-analyzer]
            [acc-text.nlg.utils.nlp :as nlp]
            [clojure.string :as str]))

(defn realize [text placeholders]
  (when-not (str/blank? text)
    (reduce-kv (fn [s k v]
                 (let [pattern (re-pattern (format "(?i)\\{\\{%s\\}\\}" (name k)))]
                   (str/replace s pattern v)))
               text
               placeholders)))

(defn generate-text [semantic-graph context data]
  (->> (grammar/build :grammar :1 semantic-graph context)
       (syn-analyzer/add-predicates)
       (generator/generate)
       (map #(realize % data))
       (map nlp/process-sentence)
       (map nlp/annotate)))
