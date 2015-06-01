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

(defn position-letter [x]
  (char (+ 97 x)))

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

(defn decode-letter-loop [message-letter keyword-letter]
  (let [chart (substition-chart)]
    (loop [c chart
           counter 0]
      (let [lp (letter-position keyword-letter)
            row (first c)
            row-letter (nth row lp)]
        (if (= row-letter message-letter)
          counter
          (recur (rest c) (inc counter)))))))

(defn decode-letter [message-letter keyword-letter]
  (let [letter-number (decode-letter-loop message-letter keyword-letter)]
    (position-letter letter-number)))

(defn decode-loop [keyword message]
  (loop [kw keyword
         msg message
         acc []]
    (if (empty? msg)
      acc
      (recur (rest kw) (rest msg) (conj acc (decode-letter (first msg) (first kw)))))))

(defn decode [keyword message]
  (let [kw (keyword-for-message keyword message)
        msg (decode-loop kw message)]
    (apply str msg)))
