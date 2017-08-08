(ns ccoreo.core
  (:use ring.middleware.params
        ring.util.response
        ring.adapter.jetty))

(def big-int #(BigInteger/valueOf %))

(defn sigma
  "Sum of 'f across the range 's to 'e, inclusive."
  [s e f]
  (reduce + (map f (range s (inc e)))))

(defn fact
  "Returns the factorial of 'n."
  [n]
  (reduce * (range (big-int 1) (inc n))))

(def big-e
  "100,000 decimals of e estimated using factorial method out to 1/100"
  (subs (str
         (with-precision
           100000 (* 1.0M (sigma 0
                                 (big-int 100)
                                 #(/ 1 (fact %))))))
        2))

(defn find-nth-eprime-of-length
  "find `n`th occurrence of `length`-length prime number in expansion of e,
  prime numbers estimated to be probable with an error likelihood of
  (0.5)^100 = 7.888609052210118 x 10^-31"
  [n length]
  (loop [offset 0
         find-count n]
    (let [candidate (BigInteger. (subs big-e offset (+ offset length)))
          prime? (.isProbablePrime candidate 100)]
      (if prime?
        (if (= 1 find-count) candidate (recur (inc offset) (dec find-count)))
        (recur (inc offset) find-count)))))

(defn page [primes]
  (str "<html><body>"
       (if primes
         (str "found prime(s): " primes)
         (str "<form action=\"/\" method=\"POST\">"
              "occurrence: <input name='occurrence' type='number'>"
              "<br />"
              "length: <input name='length' type='number'>"
              "<br />"
              "<input type='submit'>"
              "</form>"))
       "</body></html>"))

(defn handler [req]
  (let [params (:params req)
        occurrence (get params "occurrence")
        length (get params "length")
        primes (if occurrence
                 (find-nth-eprime-of-length (Integer/parseInt occurrence)
                                            (Integer/parseInt length))
                 nil)]
    (-> (response (page primes))
        (content-type "text/html"))))

(def app
  (wrap-params handler))

(defn -main []
  (run-jetty app {:port 3000}))
