(ns alphabet-cipher.coder)

(defn rotate [x coll]
  (cond
    (> x (count coll)) (rotate (- x (count coll)) coll)
    (pos? x) (into (vec (drop x coll)) (take x coll))
    (neg? x) (rotate (+ (count coll) x) coll)
    :else coll))

(defn alphabet-list []
  (map char (range 97 123)))

(defn letter-position [x]
  (.indexOf (map char (range 97 123)) x))

(defn substition-chart []
  (let [al (alphabet-list)]
    (map #(rotate % al) (range 26))))

(defn keyword-for-message [keyword message]
  (let [num-repeats (inc (int (/ (count message) (count keyword))))
        repeated-keyword (apply str (repeat num-repeats keyword))]
    (take (count message) repeated-keyword)))

(defn encode-letter [message-letter keyword-letter]
  (let [chart (substition-chart)
        row (nth chart (letter-position message-letter))]
  (nth row (letter-position keyword-letter))))

(defn encode-loop [keyword message]
  (loop [kw (keyword-for-message keyword message)
         msg message
         acc []]
    (if (empty? msg)
      acc
      (recur (rest kw) (rest msg) (conj acc (encode-letter (first msg) (first kw)))))))

(defn encode [keyword message]
  (let [kw (encode-loop keyword message)]
    (apply str kw)))


(defn decode [keyword message]
  "decodeme")
