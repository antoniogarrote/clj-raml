(ns clj-raml.types-test
  (:require [clojure.test :refer :all]
            [clj-raml.core :refer :all]
            [clj-raml.utilities :refer :all]
            [clj-raml.types :refer :all]
            [schema.core :as s]))


(deftest scalar-string-type-parser-test
  (let [examples [(:parsed (scalar-string-type-parser (parse-yaml "type: string") {:types {}}))]]
    (doseq [parsed examples]
      (is (success? parsed))
      (is (instance? clj_raml.tokens.Type (:value parsed)))
      (is (= (flat-value parsed) "string")))))

(deftest scalar-number-type-parser-test
  (let [examples [(:parsed (scalar-number-type-parser (parse-yaml "type: number") {:types {}}))]]
    (doseq [parsed examples]
      (is (success? parsed))
      (is (instance? clj_raml.tokens.Type (:value parsed)))
      (is (= (flat-value parsed) "number")))))

(deftest scalar-integer-type-parser-test
  (let [examples [(:parsed (scalar-integer-type-parser (parse-yaml "type: integer") {:types {}}))]]
    (doseq [parsed examples]
      (is (success? parsed))
      (is (instance? clj_raml.tokens.Type (:value parsed)))
      (is (= (flat-value parsed) "integer")))))

(deftest scalar-boolean-type-parser-test
  (let [examples [(:parsed (scalar-boolean-type-parser (parse-yaml "type: boolean") {:types {}}))]]
    (doseq [parsed examples]
      (is (success? parsed))
      (is (instance? clj_raml.tokens.Type (:value parsed)))
      (is (= (flat-value parsed) "boolean")))))

(deftest scalar-date-type-parser-test
  (let [parsed (:parsed (scalar-date-type-parser (parse-yaml "type: date-only\nexample: 2015-05-23") :ctx))]
    (is (success? parsed))
    (let [{value :value schema :schema {kind :kind parsing-function :parsing-function} :extra} (:value parsed)]
      (is (= value "date"))
      (is (= :date-only kind))
      (is (s/validate schema (parsing-function "2015-05-23")))))
  (let [parsed (:parsed (scalar-date-type-parser (parse-yaml "type: time-only\nexample: 12:30:00") :ctx))]
    (is (success? parsed))
    (let [{value :value schema :schema {kind :kind parsing-function :parsing-function} :extra} (:value parsed)]
      (is (= value "date"))
      (is (= :time-only kind))
      (is (s/validate schema (parsing-function "12:30:00")))))
  (let [parsed (:parsed (scalar-date-type-parser (parse-yaml "type: datetime-only\nexample: 2015-07-04T21:00:00") :ctx))]
    (is (success? parsed))
    (let [{value :value schema :schema {kind :kind parsing-function :parsing-function} :extra} (:value parsed)]
      (is (= value "date"))
      (is (= :datetime-only kind))
      (is (s/validate schema (parsing-function "2015-07-04T21:00:00")))))
  (let [parsed (:parsed (scalar-date-type-parser (parse-yaml "type: datetime\nexample: 2016-02-28T16:41:41.090Z\nformat: rfc3339") :ctx))]
    (is (success? parsed))
    (let [{value :value schema :schema {kind :kind parsing-function :parsing-function} :extra} (:value parsed)]
      (is (= value "date"))
      (is (= :datetime kind))
      (is (s/validate schema (parsing-function "2016-02-28T16:41:41.090Z")))))
  (let [parsed (:parsed (scalar-date-type-parser (parse-yaml "type: datetime\nexample: Sun, 28 Feb 2016 16:41:41 GMT\nformat: rfc2616") :ctx))]
    (is (success? parsed))
    (let [{value :value schema :schema {kind :kind parsing-function :parsing-function} :extra} (:value parsed)]
      (is (= value "date"))
      (is (= :datetime kind))
      (is (s/validate schema (parsing-function "Sun, 28 Feb 2016 16:41:41 GMT"))))))

(deftest scalar-file-type-parser-test
  (let [examples [(:parsed (scalar-file-type-parser (parse-yaml "type: file") {:types {}}))
                (:parsed (scalar-file-type-parser (parse-yaml "file") {:types {}}))]]
    (doseq [parsed examples]
      (is (success? parsed))
      (is (instance? clj_raml.tokens.Type (:value parsed)))
      (is (= (flat-value parsed) "file")))))


(deftest nil-validation
  (is (nil? (s/validate nil nil)))
  (is (thrown? Exception (s/validate nil 3))))

(deftest scalar-null-type-parser-test
  (let [parsed (:parsed (scalar-null-type-parser (parse-yaml "null") {:types {}}))]
    (is (success? parsed))
    (is (= "null" (flat-value parsed)))))

(deftest scalar-parser-test
  (is "string" (flat-value (:parsed (scalar-type-parser (parse-yaml "type: string") {:types {}}))))
  (is "number" (flat-value (:parsed (scalar-type-parser (parse-yaml "type: number") {:types {}}))))
  (is "date"  (flat-value (:parsed (scalar-type-parser (parse-yaml "type: datetime\nexample: Sun, 28 Feb 2016 16:41:41 GMT\nformat: rfc2616") {:types {}})))))

(deftest array-parser-test
  (let [parsed (:parsed (array-type-parser (parse-yaml "type: array\nitems: string\nminItems: 1\nuniqueItems: true") {:types {}}))]
    (is (= "array" (flat-value parsed)))
    (is (= "string" (-> parsed :value :extra :type-parameter :value)))
    (is (s/validate (:schema (:value parsed)) ["a" "b" "c"]))
    (is (thrown? Exception (s/validate (:schema (:value parsed)) [1 2 3])))))

(deftest object-parser-test
  (let [parsed (:parsed (object-type-parser (parse-yaml "type: object\nproperties:\n  name:\n    type: string\n  age:\n    type: integer") {:types {}}))]
    (is (= "object" (flat-value parsed)))
      (is (= ["string" "integer"] (->> parsed :value :extra :property-types vals (map :value))))
      (is (s/validate (:schema (:value parsed)) {:name "Lara" :age 12}))
    (is (thrown? Exception (s/validate {:name 12 :age "Seema"}))))
  (let [parsed (:parsed (object-type-parser (parse-yaml "type: object\nproperties:\n  name:\n    type: string\n  age?:\n    type: integer") {:types {}}))]
    (is (= "object" (flat-value parsed)))
    (is (= ["string" "integer"] (->> parsed :value :extra :property-types vals (map :value))))
    (is (s/validate (:schema (:value parsed)) {:name "Lara" :age 12}))
    (is (s/validate (:schema (:value parsed)) {:name "Lara"}))
    (is (thrown? Exception (s/validate {:name 12 :age "Seema"})))))

(deftest enumeration-type-parser-test
  (let [parsed (:parsed (enumeration-type-parser (parse-yaml "enum: [ low, medium, high ]") {:types {}}))]
    (is (= "enum" (flat-value parsed)))
    (is (s/validate (:schema (:value parsed)) "low"))
    (is (thrown? Exception (s/validate (:schema :value parsed) "other")))))

(deftest union-parser-test
  (let [parsed (:parsed (union-type-parser {:type "union" :values ["string", "integer"]} {:types {}}))]
    (is (= "union" (flat-value parsed)))
    (is (= ["string" "integer"] (->> parsed :value :extra :union-types (map :value))))
    (is (s/validate (:schema (:value parsed)) "string"))
    (is (s/validate (:schema (:value parsed)) 12))
    (is (thrown? Exception (s/validate (:schema (:value parsed)) true)))))


(deftest type-parser-test
  (let [parsed (:parsed (type-parser "string | integer | boolean" {}))]
    (is (= "union" (flat-value parsed)))
    (is (s/validate (:schema (:value parsed)) "string"))
    (is (s/validate (:schema (:value parsed)) 2))
    (is (s/validate (:schema (:value parsed)) true))
    (is (thrown? Exception (s/validate (:schema (:value parsed)) 3.0))))

  (let [parsed (:parsed (type-parser "string[]" {}))]
    (is (= "array" (flat-value parsed)))
    (is (s/validate (:schema (:value parsed)) ["string"]))
    (is (thrown? Exception (s/validate (:schema (:value parsed)) [2])))
    (is (thrown? Exception (s/validate (:schema (:value parsed)) [true])))
    (is (thrown? Exception (s/validate (:schema (:value parsed)) 3.0))))

  (let [parsed (:parsed (type-parser "(string|integer)[]" {}))]
    (is (= "array" (flat-value parsed)))

    (is (s/validate (:schema (:value parsed)) ["string"]))
    (is (s/validate (:schema (:value parsed)) [2]))
    (is (thrown? Exception (s/validate (:schema (:value parsed)) [true])))
    (is (thrown? Exception (s/validate (:schema (:value parsed)) 3.0))))

  (let [parsed (:parsed (type-parser "(string|integer)[] | boolean" {}))]
    (is (= "union" (flat-value parsed)))
    (is (s/validate (:schema (:value parsed)) ["string"]))
    (is (s/validate (:schema (:value parsed)) [2]))
    (is (s/validate (:schema (:value parsed)) true))
    (is (thrown? Exception (s/validate (:schema (:value parsed)) 3.0)))
    (is (thrown? Exception (s/validate (:schema (:value parsed)) [true]))))

  (let [parsed (:parsed (type-parser "(string|integer)[][][]" {}))]
    (is (= "array" (flat-value parsed)))
    (is (s/validate (:schema (:value parsed)) [[["a" "b"] ["c"]]]))
    (is (thrown? Exception (s/validate (:schema (:value parsed)) [["a" "b"] "c"])))
    (is (thrown? Exception (s/validate (:schema (:value parsed)) 3.0)))
    (is (thrown? Exception (s/validate (:schema (:value parsed)) [true])))))

(deftest parse-test
  (let [input-data (parse-yaml "Person:\nAnother:")
        parsed (raml-types input-data)
        {:keys [Person Another]} (:types parsed)]
    (is (= "string" (flat-value Person)))
    (is (= "string" (flat-value Another))))
  (let [raml (yaml->raml "User:\n  type: object\n  properties:\n    firstname: string\n    lastname:  string\n    age:       number")
        data {:firstname "Manuel" :lastname "Marin" :age 37}]
    (is (= (out raml data)
           "{firstname: Manuel, lastname: Marin, age: 37}\n"))
    (is (= data (->> data (out raml) (in raml)))))
  (let [input-data "Person:\n
  type: object\n
  properties:\n
    firstname: string\n
    lastname:  string\n
    title:    string\n
Manager:\n
  type: Person\n
  properties:\n
    reports: Person[]\n
    phone:  string\n
\n"
        raml (yaml->raml input-data)
        person {:firstname "Rafael" :lastname "GH" :title "Mr"}
        manager {:firstname "Francesca" :lastname "P" :title "Ms" :reports [] :phone "324234234"}]
    (is (validate (types raml) :Person person))
    (is (thrown? Exception (validate (types raml) :Manager person)))
    (is (validate (types raml) :Person manager))
    (is (validate (types raml) :Manager manager))))
(comment
  (def raml (raml-types {:Person
                         {:type "object"
                          :properties
                          {:name "string",
                           :age  "integer"}},
                         :Manager
                         {:type "Person"
                          :properties
                          {:level "integer"
                           :manages "Person[]"}}}))

  (def john {:name "John Smith" :age 23})
  (validate raml :Person john) ;; corrects
  (validate raml :Manager john) ;; throws a validation exception

  (def marta {:name "Marta Cantillo" :age 27 :level 1 :manages [john]})
  (validate raml :Manager marta) ;; corrects
  (validate raml :Person marta) ;; corrects


  (def input-data
"Person:\n
  type: object\n
  properties:\n
    name: string\n
    age:  integer\n
Manager:\n
  type: Person\n
  properties:\n
    manages: Person[]\n
    level:  integer\n
\n")

(def raml (yaml->raml input-data))

(def john
"
name: John Smith
age: '23'
")

(def carmine {:name "Carmine Donetti" :age 23})
(def marta {:name "Marta Cantillo" :age 27 :level 1 :manages [carmine]})
(println (out raml marta))

)
