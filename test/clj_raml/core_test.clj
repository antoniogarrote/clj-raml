(ns clj-raml.core-test
  (:require [clojure.test :refer :all]
            [clj-raml.utilities :refer :all]
            [clj-raml.core :refer :all]))

(defn test-parser [parser context {:keys [some-tokens none-tokens error-tokens]}]
  (when (some? some-tokens)
    (doseq [[token expected] some-tokens]
      (let [result (parser (parse-yaml token) context)]
        (is (success? (:parsed result)))
        (if (instance? java.util.regex.Pattern expected)
          (is (= (str expected) (str (flat-value (:parsed result)))))
          (is (= expected (flat-value (:parsed result))))))))
  (when (some? none-tokens)
    (doseq [token none-tokens]
      (is (= {:parsed (none) :context context}
             (parser (parse-yaml token) context)))))
  (when (some? error-tokens)
    (doseq [token error-tokens]
      (is (failure? (:parsed (parser (parse-yaml token) context)))))))


(deftest display-name-parser-test
  (test-parser display-name-parser :ctx
               {:some-tokens [["displayName: Public Gists" "Public Gists"]]
                :none-tokens ["1" "displayname: other"]
                :error-tokens ["displayName: 1"]}))

(deftest enum-parser-test
  (test-parser enum-parser :ctx
               {:some-tokens [["enum: [.json, .xml]" [".json" ".xml"]]]
                :none-tokens ["type: file", "displayName: 2"]
                :error-tokens ["enum: 123"]}))

(deftest pattern-parser-test
  (test-parser pattern-parser :ctx
               {:some-tokens [["pattern: ^[a-zA-Z0-9][-a-zA-Z0-9]*$" (re-pattern "^[a-zA-Z0-9][-a-zA-Z0-9]*$")]]
                :none-tokens ["displayName: 2", "enum: [.json]"]
                :error-tokens ["pattern: ^[a--\\]"]}))

(deftest title-parser-test
  (test-parser title-parser :ctxt
               {:some-tokens [["title: a title", "a title"]]}))
