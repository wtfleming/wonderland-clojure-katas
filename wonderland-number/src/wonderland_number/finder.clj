(ns wonderland-number.finder)

(defn wonderland-number []
  (let [nums (range (int 100000) (int 1000000))
        digits (fn [x] (set (str x)))
        is-valid? (fn [x i] (= (digits x) (digits (* x i))))]
    (first (filter #(every? identity (map (partial is-valid? %) (range 2 7))) nums))))
