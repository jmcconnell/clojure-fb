(ns com.ubermensch.clojure-fb
  (:require [clojure.http.resourcefully :as resourcefully]
            [com.ubermensch.configurator :as config])
  (:use [org.danlarkin (json :as json)]
        [clojure.contrib.test-is :only (with-test is testing)]
        [clojure.contrib.str-utils :only (re-sub)]))

(when (not *compile-files*)
  (config/register-config "fb.config"))

; TODO: make all this configurable
(def *facebook-server* "http://api.facebook.com/restserver.php")
(def *standard-params* { "v"            "1.0" ; version
                         "format"       "JSON"
                         "api_key"      (config/get ::api-key)
                         "secret_key"   (config/get ::secret-key) })

(with-test
  (defn pad [n s]
    (let [num-zeroes-needed (- n (count s))]
      (str (apply str (repeat num-zeroes-needed "0")) s)))

  (is (= "0001" (pad 4 "1")))
  (is (= "0001" (pad 4 "01")))
  (is (= "01" (pad 0 "01")))
  (is (= "1" (pad -1 "1"))))

(with-test
  (defn md5-sum [s]
    (let [md (java.security.MessageDigest/getInstance "MD5")]
      (pad 32
           (.toString
             (BigInteger. 1 (.digest md (.getBytes (str s))))
             16))))

  (is (= "2e5f9458bcd27e3c2b5908af0b91551a" (md5-sum "md5 test"))))

(with-test
  (defn- make-request-string [params private-key]
    (let [sorted-params (into (sorted-map) params)
          request-string (apply str
                                (map #(str (key %) "=" (val %)) sorted-params))
          request-string (str request-string private-key)]
      request-string))

  (testing "keys are sorted"
           (is (= "alpha=1beta=2clojure=3"
                  (make-request-string { "beta" "2" "clojure" "3" "alpha" "1" }
                                       nil))))

  (testing "private key is appended"
           (is (= "alpha=1beta=2clojure=3private-key"
                  (make-request-string { "beta" "2" "clojure" "3" "alpha" "1" }
                                       "private-key")))))

(with-test
  (defn- make-signature [params private-key]
    (let [request-string (make-request-string params private-key)]
      (md5-sum request-string)))

  (is (= "0d2d1a9a637f7c3ff2890ac699f645bb"
         (make-signature { "alpha" "1" "beta" "2" "clojure" "3" }
                         "private-key"))))

(defn- add-query-param [uri-builder k v]
  (.queryParam uri-builder k (to-array [v])))

(defn get-response [params]
  (let [params (merge *standard-params* params)
        params (assoc params "call_id" (str (System/currentTimeMillis)))]
    (resourcefully/post *facebook-server* {} params)))

(defn call [method params]
  (json/decode-from-str
    (first (:body-seq (get-response (assoc params "method" method))))))

(with-test
  (defn- scrub-parameter [param]
    (re-sub #"fb_sig_" "" param))

  (is (= "test" (scrub-parameter "fb_sig_test"))))

(with-test
  (defn- scrub-parameters [params]
    (reduce #(assoc %1 (scrub-parameter (key %2)) (first (val %2)))
            (sorted-map)
            params))
  
  (is (= { "test1" "val1" "test2" "val2" "fb_sig" "val3" }
         (scrub-parameters { "fb_sig_test1" (into-array '("val1"))
                             "fb_sig_test2" (into-array '("val2"))
                             "fb_sig" (into-array '("val3")) })))
  
  (testing "multiple values per key"
           (is (= { "test1" "val1" "test2" "val3" }
                  (scrub-parameters { "test1" (into-array '("val1" "val2"))
                                      "test2" (into-array '("val3")) })))))

(defn parse-request [req]
  (let [params (.getParameterMap req)
        sig-params (scrub-parameters
                     (filter #(re-matches #"^fb_sig_.*" (key %1)) params))
        params (scrub-parameters params)
        sig (params "fb_sig")]
    (assert (= sig (make-signature sig-params (config/get ::secret-key))))
    params))
