# clj-raml

An Clojure implementation of [RAML 1.0 data types](https://github.com/raml-org/raml-spec/blob/master/versions/raml-10/raml-10.md/#raml-data-types) specification with support for data validation.

## Usage

In order to run any kind of validations you first need to create a RAML type system from a description of types.
This description can be created using the ```raml-types``` function that receives as an argument a Clojure's map of maps, where the top level keys are the type names:

```clojure
(require '[clj-raml.types :as t])

(def t/raml (raml-types {:Person
						 {:type "object"
						  :properties
						  {:name "string",
						   :age  "integer"}},
						:Manager
						{:type "Person"
						 :properties
						 {:level "integer"
						  :manages "Person[]"}}}))
```

Once the type system has been created, type validation can be performed over data instances:

```clojure
;; Validation of a Person
(def john {:name "John Smith" :age 23})
(validate raml :Person john) ;; correct
(validate raml :Manager john) ;; throws a validation exception

;; Validation of a Manager
(def marta {:name "Marta Cantillo" :age 27 :level 1 :manages [john]})
(validate raml :Manager marta) ;; correct
(validate raml :Person marta) ;; correct
```

To deal with formats other than EDN, a RAML adapter can be created defining the protocol ```RamlTypeSystemProtocol```.
This protocol wraps a RAML type system and provides methods to read and write data in a particular format after performing the data validation.

The library includes a YAML adapter:

```clojure
;; We create the adapter
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

;; We try to read some incoming YAML data
(def john
"
name: John Smith
age: 23
")
(in raml john) ; => {:firsname "John Smith" :age 23}

(def john
"
name: John Smith
age: '23'
")
(in raml john) ; validation exception

;; We try to generate YAML data

(def carmine {:name "Carmine Donetti" :age 23})
(def marta {:name "Marta Cantillo" :age 27 :level 1 :manages [carmine]})
(out raml marta) ;; =>
;; name: Marta Cantillo
;; age: 27
;; level: 1
;; manages:
;; - {name: Carmine Donetti, age: 23}
```

## Status

The current implementation supports:

- scalar types
- type expressions
- array types
- unions
- object types
- optional properties
- multiple inherintance

The main part of the specification that hasn't been implemented yet are facets.

The parsing of types does not support yet ahead declarations of types:

```yaml
types:
  Manager:
	type: Person
	properties:
	  ...
  Person:
	type: object
	properties:
	  ...
```

This declaration will fail because we're using the Person type before declaring it. In order to overcome this problem, just declare types in the usage order.


```yaml
types:
  Person:
	type: object
	properties:
  Manager:
	type: Person
	properties:
	  ...
```

## License

Copyright © 2016 Antonio Garrote
MIT LICENSE
