(ns clj-raml.types
  (:require [clj-raml.utilities :refer :all]
            [clj-raml.tokens :refer :all]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [clj-time.coerce :as c]
            [schema.core :as s]
            [instaparse.core :as insta]))

(declare type-parser)

(def default-parser (make-named-property-parser :default #(->Default %)))
(def example-parser (make-named-property-parser :example #(->Example %)))
(def facets-parser (make-named-property-parser :facets #(->Facets %)))
(def facets-parser (make-named-property-parser :xml #(->Xml %)))

(defn examples-parser [token context]
  (if (and (map? token)
           (not= (nil? (:examples token))))
    (if (seq? (:examples token))
      (->> (:examples token)
           (map (fn [[name code]] [name (example-parser code)]))
           (into {})
           (->Examples))
      (return (failure "Examples must be a map of examples" token)))
    (return (none) context)))

;; Scalar types

(defn scalar-string-type-parser [token context]
  (if (and (map? token)
           (= "string" (:type token)))
    (return (success (->Type "string" [] [] s/Str {})) context)
    (return (none) context)))

(defn scalar-number-type-parser [token context]
  (if (and (map? token)
           (= "number" (:type token)))
    (return (success (->Type "number" [] [] s/Num {})) context)
    (return (none) context)))

(defn scalar-integer-type-parser [token context]
  (if (and (map? token)
           (= "integer" (:type token)))
    (return (success (->Type "integer" [] [] s/Int {})) context)
    (return (none) context)))

(defn scalar-boolean-type-parser [token context]
  (if (and (map? token)
           (= "boolean" (:type token)))
    (return (success (->Type "boolean" [] [] s/Bool {})) context)
    (return (none) context)))

(def date-only-formatter (f/formatter "yyyy-mm-dd"))
(def time-only-formatter (f/formatters :hour-minute-second))
(def time-only-fraction-formatter (f/formatters :hour-minute-second-fraction))
(def datetime-only-formatter (f/formatters :date-hour-minute-second))
(def datetime-only-fraction-formatter (f/formatters :date-hour-minute-second-fraction))
(def datetime-rfc-3339-formatter (f/formatters :date-time))
(def datetime-rfc-2616-formatter (f/formatter "EEE, dd MMM yyyy HH:mm:ss 'GMT'"))

(defn scalar-date-type-parser [token context]
  (if (and (map? token) (some (partial = :type) (keys token)))
    (condp = (if (map? token) (:type token) token)
      "date-only" (let [parsing-function #(c/to-date (f/parse date-only-formatter %))]
                    (return (success (->Type "date" [] [] s/Inst {:kind :date-only :parsing-function parsing-function})) context))
      "time-only" (let [parsing-function (fn [txt]
                                           (c/to-date
                                            (try
                                              (f/parse time-only-formatter txt)
                                              (catch Exception ex
                                                (f/parse time-only-fraction-formatter txt)))))]
                    (return (success (->Type "date"
                                             [] []
                                             s/Inst
                                             {:kind :time-only, :parsing-function parsing-function}))
                            context))
      "datetime-only" (let [parsing-function (fn [txt]
                                               (c/to-date
                                                (try
                                                  (f/parse datetime-only-formatter txt)
                                                  (catch Exception ex
                                                    (f/parse datetime-only-fraction-formatter txt)))))]
                        (return (success (->Type "date" [] [] s/Inst {:kind :datetime-only :parsing-function parsing-function}))
                                context))
      "datetime" (let [parsing-function (fn [txt]
                                          (c/to-date
                                           (if (= (:format token) "rfc2616")
                                             (f/parse datetime-rfc-2616-formatter txt)
                                             (f/parse datetime-rfc-3339-formatter txt))))]
                   (return (success (->Type "date" [] [] s/Inst {:kind :datetime :parsing-function parsing-function}))
                           context))
      (return (none) context))
    (return (none) context)))

(defn scalar-file-type-parser [token context]
  (if (or (= "file" token)
          (and (map? token)
               (= "file" (:type token))))
    (return (success (->Type "file" [] [] nil {})) context)
    (return (none) context)))

(extend-protocol s/Schema
  nil
  (spec [this]
    (schema.spec.leaf/leaf-spec (schema.spec.core/precondition this
                                                               #(nil? %)
                                                               #(list 'nil? %))))
  (explain [this]
    'nil))

(defn scalar-null-type-parser [token context]
  (if (or (and (map? token) (some (partial = :type) (keys token)))
          (nil? token))
    (let [value (if (map? token) (:type token) token)]
      (if (= nil value)
        (return (success (->Type "null" [] [] nil {})) context)
        (return (none) context)))))

(def scalar-type-parser (->> scalar-string-type-parser
                             (either-parser scalar-null-type-parser)
                             (either-parser scalar-boolean-type-parser)
                             (either-parser scalar-number-type-parser)
                             (either-parser scalar-integer-type-parser)
                             (either-parser scalar-date-type-parser)
                             (either-parser scalar-file-type-parser)))

;; Array type

(defn array-type-parser [token context]
  (if (and (map? token) (= "array" (:type token)))
    (let [type-parameter (if (instance? clj_raml.tokens.Type (:items token))
                           (success (:items token))
                           (let [parsed-type-parameter (type-parser (:items token) context)]
                             (cond
                               (failure? parsed-type-parameter) (return (failure "Error parsing type parameter" token)
                                                                 context)
                               (none? parsed-type-parameter)    (return (failure "No type parameter has been found" token))
                               :else                            (:parsed parsed-type-parameter))))]
      (cond
        (failure? type-parameter) (return (failure "Error parsing type parameter" token)
                                        context)
        (none? type-parameter) (return (failure "No type parameter has been found" token))
        :else
        (let [type-parameter (:value type-parameter)]
          (return (success (->Type "array" [] [] [(:schema type-parameter)] {:type-parameter type-parameter}))
                  context))
        ))
    (return (none) context)))

;; Object type

(defn compute-object-schema
  ([properties inheritance types]
   (let [base-schema (->> properties
                          (map (fn [[property parsed-type]]
                                 (if (-> parsed-type :extra :optional)
                                   [(s/optional-key (keyword (.replace (name property) "?" ""))) (:schema parsed-type)]
                                   [property (:schema parsed-type)])))
                          (into {}))]
     (if (empty? inheritance)
       base-schema
       (compute-object-schema properties inheritance types base-schema []))))
  ([properies inheritance types base-schema schema]

   (letfn [(merge-either-schemas [base-schema either-schemas acc]
             (if (empty? either-schemas)
               acc
               (let [first-either-schema (first either-schemas)]
                 (if (= schema.core.Either (class first-either-schema))
                   (recur base-schema (concat (:schemas first-either-schema) either-schemas) acc)
                   (recur base-schema (rest either-schemas) (conj acc (merge base-schema first-either-schema)))))))]

     (if (empty? inheritance)
       (apply s/either schema)
       (let [next-ancestor (first inheritance)
             rest-ancestors (rest inheritance)
             next-type (get types (keyword next-ancestor))]
         (if (nil? next-type)
           (throw (Exception. (str "Missing type information for base type " next-ancestor)))
           (let [next-type-schema (:schema next-type)
                 next-schema (if (= schema.core.Either (class next-type-schema))
                               ;; union, we merge in each v
                               (apply s/either (merge-either-schemas base-schema (:schemas next-type-schema) []))
                               (merge base-schema next-type-schema))]
             (recur properies rest-ancestors types base-schema (conj schema next-schema)))))))))


(defn object-type-parser [token {:keys [types] :as context}]
  (letfn [(check-optional [{:keys [extra] :as parsed-property} property-name property-description]
            (let [is-optional (or (.endsWith (name property-name) "?")
                                  (and (map? property-description)
                                       (= false (:required property-description))))]
              (assoc parsed-property :extra (assoc extra :optional is-optional))))]
    (if (not (nil? (:properties token)))
      (let [inheritance (filter #(not= "object" %) (flatten [(:type token)]))
            parsed-properties (->> (:properties token)
                                   (map (fn [[property type-description]]
                                          (let [parsed-property (type-parser type-description context)]
                                            (if (or (failure? (:parsed parsed-property))
                                                    (none? (:parsed parsed-property)))
                                              [property parsed-property]
                                              [property (check-optional (:value (:parsed parsed-property)) property type-description)]))))
                                   (into {}))
            failures (filter (fn [[key result]] (or
                                                 (failure? (:parsed result))
                                                 (none? (:parsed result))))
                             parsed-properties)]
        (if (> (count failures) 0)
          (return (failure (str "Error parsing object type property types "
                                (map first failures))
                           context))
          (let [schema (compute-object-schema parsed-properties inheritance types)]
            (return (success (->Type "object" [] [] schema {:property-types parsed-properties :inheritance inheritance}))
                    context))))
      (return (none) context))))

(defn union-type-parser [token context]
  (if (and (map? token)
           (= "union" (:type token)))
    (let [union-types (mapv #(type-parser % context) (:values token))]
      (if (some #(or (failure? (:value %)) (none? (:value %))) union-types)
        (return (failure "Error parsing union types" token))
        (let [union-schemas (map (comp :schema :value :parsed) union-types)
              union-types (map (comp :value :parsed) union-types)]
          (return (success (->Type "union" [] [] (apply s/either union-schemas) {:union-types union-types}))
                  context))))
    (return (none) context)))

(defn enumeration-type-parser [token context]
  (if (and (map? token)
           (not (nil? (:enum token))))
    (return (success (->Type "enum" [] [] (apply s/enum (:enum token)) {}))
            context)
    (return (none) context)))

(def raml-grammar "TYPE_EXPRESSION = TYPE_NAME | SCALAR_TYPE | <'('> <BS>  TYPE_EXPRESSION <BS> <')'> | ARRAY_TYPE | UNION_TYPE
                   SCALAR_TYPE = 'string' | 'number' | 'integer' | 'boolean' | 'date-only' | 'time-only' | 'datetime-only' | 'datetime' | 'file' | 'null'
                   ARRAY_TYPE = TYPE_EXPRESSION <'[]'>
                   TYPE_NAME = #\"\\w[\\w\\d]+\"
                   UNION_TYPE = TYPE_EXPRESSION <BS> (<'|'> <BS> TYPE_EXPRESSION)+
                   BS = #\"\\s*\"
                   ")

(def raml-type-grammar-analyser (insta/parser raml-grammar))


(defn ast->type [ast {:keys [types] :as context}]
  (let [type (filterv #(not= % :TYPE_EXPRESSION) ast)]
    (if (and (= 1 (count type))
             (vector? (first type)))
      (recur (first type) context)
      (do
        (condp = (first type)
          :UNION_TYPE {:type "union"
                       :values (mapv #(ast->type % context) (rest type))}
          :SCALAR_TYPE {:type (last type)}
          :ARRAY_TYPE {:type "array"
                       :items (ast->type (last type) context)}
          :TYPE_NAME (let [ref-type (get types (keyword (last type)))]
                       (if (nil? ref-type)
                         (throw (Exception. (str "Cannot find type reference " (last type))))
                         ref-type))
          (throw (Exception. (str "Cannot parse type expression AST " (mapv identity type)))))))))

(defn type-parser [token {:keys [types] :as context}]
  (if (nil? token)
    (return (success (->Type "string" [] [] s/Str {})) context)
    (if (string? token)
      (let [parsed-type (try
                          (raml-type-grammar-analyser token)
                          (catch Exception ex
                            (failure (str "Cannot parse type expression '" token "': " ex) token)))]
        (if (failure? parsed-type)
          (return parsed-type context)
          (recur (ast->type parsed-type context) context)))
      (let [parser (->> enumeration-type-parser
                        (either-parser
                         object-type-parser)
                        (either-parser
                         union-type-parser)
                        (either-parser
                         array-type-parser)
                        (either-parser
                         scalar-type-parser))]
        (parser token context)))))

(defn types-parser [token {:keys [types] :as context}]
  (let [result (reduce (fn [context [type-name type-description]]
                         (if (failure? context)
                           context
                           (if (nil? (get types type-name))
                             (let [{:keys [parsed]} (type-parser type-description {:types context})]
                               (if (success? parsed)
                                 (assoc context type-name (:value parsed))
                                 (failure (str "Error parsing type " type-name ": " (if (none? parsed)
                                                                                      "Not found"
                                                                                      (:reason parsed)))
                                          token)))
                             [(failure (str "Error, trying to re-define type " type-name) token) context])))
                       types
                       token)]
    (if (failure? result)
      (return result context)
      (return (success (->Types (vals result))) (assoc context :types result)))))

;; Wrapper for a set of Raml types
(defprotocol RamlTypeSystemProtocol
  (validate [this type data]))

(defn check-disallowed-keys [exception data]
  (let [errors (:error (.getData exception))]
    (if (and (map? errors)
             (every? #(= 'disallowed-key %) (vals errors)))
      data
      (throw exception))))

(defrecord RamlTypeSystem [types]
    RamlTypeSystemProtocol
    (validate [this type data]
      (let [found-type (get types type)]
        (if (nil? found-type)
          (throw (Exception. (str "Cannot find type with name " type)))
          (try (s/validate (:schema found-type) data)
               (catch clojure.lang.ExceptionInfo ex
                 (check-disallowed-keys ex data)))))))

;; An adapter for input-output data in a format against a type system
(defprotocol RamlTypeSytemAdapterProtocol
  (in [this data])
  (out [this data])
  (types [this]))

;; System constructor for a certain format
(defmulti ->raml (fn [format str] format))

(defmacro adapt
  "Adapts a system to provide input/output in a certain format"
  [raml adapter]
  `(~adapter ~raml))

(defn raml-types
  "Generates a map of type names and the associated parsed types"
  [types-map]
  (let [{:keys [parsed context]} (types-parser types-map {})]
    (if (success? parsed)
      (->RamlTypeSystem (:types context))
      (throw (Exception. (str "Error parsing token " (:token parsed) "\n Reason: " (:reason parsed)))))))

(defn compose-validator [raml-type-system]
  (apply s/either (map :schema (vals raml-type-system))))


;; YAML implementation

(defmethod ->raml :yaml [_ str]
  (->> (parse-yaml str)
       (raml-types)))

(defrecord YamlRamlAdapter [raml-type-system]
  RamlTypeSytemAdapterProtocol
  (in [this data]
    (let [data (parse-yaml data)]
      (s/validate (compose-validator (:types raml-type-system)) data)
      data))
  (out [this data]
    (s/validate (compose-validator (:types raml-type-system)) data)
    (gen-yaml data))
  (types [this] raml-type-system))

(defn yaml->raml [yaml-data]
  (adapt (->raml :yaml yaml-data) ->YamlRamlAdapter))
