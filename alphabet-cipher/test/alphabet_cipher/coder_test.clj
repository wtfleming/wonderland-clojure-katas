(ns alphabet-cipher.coder-test
  (:require [clojure.test :refer :all]
            [alphabet-cipher.coder :refer :all]))

(deftest test-letter-position
  (testing "retrieve the correct position"
    (is (= 0 (letter-position \a)))
    (is (= 1 (letter-position \b)))
    (is (= 2 (letter-position \c)))))

(deftest test-alphabet-list
  (testing "creates a list with all letters of the alphabet"
    (is (= '(\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z)
           (alphabet-list)))))

(deftest test-keyword-for-message
  (testing "creates a properly length keyword"
    (is (= '(\s \c \o \n \e \s \s \c \o \n \e \s \s \c \o)
           (keyword-for-message "scones" "meetmebythetree")))))

(deftest test-encode-letter
  (testing "encodes a letter"
    (is (= \e (encode-letter \m \s)))))


(deftest test-encode
  (testing "can encode give a secret keyword"
    (is (= "hmkbxebpxpmyllyrxiiqtoltfgzzv"
           (encode "vigilance" "meetmeontuesdayeveningatseven")))
    (is (= "egsgqwtahuiljgs"
           (encode "scones" "meetmebythetree")))))

(deftest test-decode
  (testing "can decode an cyrpted message given a secret keyword"
    (is (= "meetmeontuesdayeveningatseven"
           (decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv")))
    (is (= "meetmebythetree"
           (decode "scones" "egsgqwtahuiljgs")))))
