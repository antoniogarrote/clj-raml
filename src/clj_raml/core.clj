(ns clj-raml.core
  (:require [clj-raml.types :as types]
            [clj-raml.utilities :refer :all]
            [clj-raml.tokens :refer :all]))

;; Parsers

(defprotocol RamlParser
  (parse [this token context]))


;; Description

(def title-parser (make-named-property-parser :title #(->Title %)))
(def version-parser (make-named-property-parser :version #(->Version %)))
(def description-parser (make-named-property-parser :description #(->Description %)))
(def base-uri-parser (make-named-property-parser :baseUri #(->BaseUri %)))

;; Named parameters

(let [inner-parser (make-named-property-parser :displayName #(->DisplayName %))]
  (defn display-name-parser [token context]
    (let [{:keys [parsed context] :as result} (inner-parser token context)]
      (if (success? parsed)
        (if (string? (flat-value parsed))
          result
          (return (failure token "displayName named parameter value must be a String")
                  context))
        result))))


(let [inner-parser (make-named-property-parser :enum #(->Enumeration %))]
  (defn enum-parser [token context]
    (let [{:keys [parsed context] :as result} (inner-parser token context)]
      (if (success? parsed)
        (if (seq? (flat-value parsed))
          result
          (return (failure token "enum must have an array as value") context))
        result))))

(let [inner-parser (make-named-property-parser :pattern #(->Pattern %))]
  (defn pattern-parser [token context]
    (let [{:keys [parsed context] :as result} (inner-parser token context)]
      (if (success? parsed)
        (try
          (return (success (->Pattern (re-pattern (flat-value parsed)))) context)
          (catch Throwable ex
            (return (failure token (str "Invalid regular expression in pattern:" parsed))
                    context)))
        result))))

;; URI Parameters

(defrecord UriParameter [name description type])

(comment
  (defn- scalar-uri-parameter-parser [token context]
    (if (and (map? token)
             (= 1 (count (keys token)))
             (some (partial = :type) (-> token vals first keys)))
      (let [uri-parameter-name (-> token keys first)
            uri-parameter-description (-> token vals first)]
        (and-parser
         (assoc-parser :type (mandatory-parser type-parser))
         (assoc-parser :description description-parser))))))
