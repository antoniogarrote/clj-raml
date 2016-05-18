(ns clj-raml.tokens)

(defrecord Title [value])
(defrecord Version [value])
(defrecord Description [value])
(defrecord BaseUri [value])
(defrecord DisplayName [value])
(defrecord Enumeration [value])
(defrecord Pattern [value])
(defrecord Types [value])

(defrecord Uri [protocol fragment parameters])

;; Type declarations
(defrecord Default [value])
(defrecord Schema [value])
(defrecord Type [value facets examples schema extra])
(defrecord Example [value])
(defrecord Examples [value])
(defrecord Facets [value])
(defrecord Xml [value])
