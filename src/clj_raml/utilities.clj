(ns clj-raml.utilities
  (:require [clj-yaml.core :as yaml]))

;; @todo: add here concatenation of files
;; parsing the YAML structure is the first
;; stage of the parser
(defn parse-yaml
  "Loads and parses YAML text"
  [str]
  (yaml/parse-string str))

(defn gen-yaml
  [data]
  (yaml/generate-string data :flow-style :flow))

;; Parser control

(defrecord Success [value])
(defrecord None [])
(defrecord Failure [token reason])
(defn success [x] (->Success x))
(defn none [] (->None))
(defn failure [reason token] (->Failure token reason ))
(defn success? [x] (instance? Success x))
(defn none? [x] (instance? None x))
(defn failure? [x] (instance? Failure x))
(defn flat-value [x]
  (let [v (:value x)]
    (if (and (map? v) (some (partial = :value) (keys v)))
      (recur v)
      v)))

(defn return [parsed context]
  {:parsed parsed, :context context})

;; combinators
(defn either-parser [pa pb]
  (fn [token context]
    (let [{:keys [parsed context] :as result} (pa token context)]
      (cond
        (success? parsed) result
        (failure? parsed) result
        :else (pb token context)))))

(defn map-parser [parser]
  (fn [tokens context]
    (reduce (fn [[context mapped] token]
              (let [{:keys [parsed context]} (parser token context)]
                [context (conj mapped parsed)]))
            [context []] tokens)))

(defn and-parser [pa & pbs]
  (fn [token context]
    (let [{:keys [parsed context] :as result} (pa token context)]
      (if (empty? pbs)
        result
        (if (or (none? parsed) (success? parsed))
          ((and-parser (rest pbs) (rest pbs)) token context)
          result)))))

(defn mandatory-parser [parser]
  (fn [token context]
    (let [{:keys [parsed context] :as result} (parser token context)]
      (if (or (success? parsed) (failure? context))
        result
        (failure (str "Mandatory parser " parser " returned nothing") token)))))

(defn assoc-parser [key parser]
  (fn [token context]
    (let [{:keys [parsed context] :as result} (parser token context)]
      (if (failure? parsed)
        result
        (if (success? parsed)
          (assoc context key (flat-value parsed))
          context)))))

;; Helper functions

(defn make-named-property-parser [property ctor]
  (fn [token context]
    (if  (and (map? token)
              (some (partial = property) (keys token)))
      (return (success (ctor (get token property)))
                context)
      (return (none)
              context))))
