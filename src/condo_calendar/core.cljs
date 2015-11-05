(ns condo-calendar.core
  (:require [cljs-time.core :as t]
            [cljs-time.periodic :as p]
            [cljs-time.format :as cf]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [om.next :as om :refer-macros [defui]]))

  (enable-console-print!)




  (def init-data {:people [{:color 'red' :name "Judy" :id 1}
                           {:color 'blue' :name "John" :id 2}
                           {:color 'green' :name "Jake" :id 3}
                           {:color 'yellow' :name "Susan" :id 4}
                           {:color 'none' :name "default" :id 0}]})


;; explore the alternative of generativg the months on the fly
;
; 1   2   3   4   5   6   7
; 8   9  10  11  12  13  14
; 15 16  17  18  19  20  21
; 22 23  24  25  26  27  28
; 29 30  31
;
;     1   2   3   4   5   6
; 7   8   9  10  11  12  13
; 14 15  16  17  18  19  30
; 21 21  23  24  25  26  27
; 28 29  30  31
;
;         1   2   3   4   5
; 6   7   8   9  10  11  12
; 13  14 15  16  17  18  19
; 20  21 22  23  24  25  26
; 27  28 29  30  31
;
;              1   2   3   4
; 5    6   7   8   9  10  11
; 12  13  14  15  16  17  18
; 19  20  21  22  23  24  25
; 26  27  28  29  30  31
;
;                  1   2   3
; 4    5   6   7   8   9  10
; 11  12  13  14  15  16  17
; 18  19  20  21  22  23  24
; 25  26  27  28  29  30  31
;
;                      1   2
;  3   4   5   6   7   8   9
; 10  11  12  13  14  15  16
; 17  18  19  20  21  22  23
; 24  25  26  27  28  29  30
; 31
;
;                          1
;  2   3   4   5   6   7   8
;  9  10  11  12  13  14  15
; 16  17  18  19  20  21  22
; 23  24  25  26  27  28  29
; 30  31
;

(defn sunday-of-first-week-of-month [year month]
  "return the sunday starting the week containing the first of the month"
  (let [first-of-month (t/local-date year month 1)]
    (t/minus first-of-month (t/days (mod (t/day-of-week first-of-month) 7)))))

(defn seq-of-days [year month]
  (map #(hash-map :date % :date-key (cf/unparse (cf/formatters :basic-date) %))
       (p/periodic-seq (sunday-of-first-week-of-month year month) (t/days 1))))

(defn six-week-containing-month [year month]
  "return the six week period starting with the week of the first
  of the month and continuing for six weeks."
  (take 6 (partition 7 (seq-of-days year month))))

(defmulti read om/dispatch)

(defmethod read :month/month-id
  [{:keys [state selector] :as env} key {:keys [month]}]
  (println "read calendar")
  (let [st @state]
    {:value (om/denormalize selector (get-in st [:calendar month]) st)}))

(defmethod read :day/day-id
  [{:keys [state selector] :as env} key ])

(defui Month
       static om/Ident
       (ident [this {:keys [date]}]
              [:month/month-id (cf/unparse (cf/formatter "yyyyMM") date)])
       static om/IQuery
       (query [this]
              '[:month/month-id om/get-query Day])
       static om/IQueryParams
       (params [this]
               (let [today (t/today)
                     year (t/year today)
                     month (t/month today)]
                 :month-begin (local-date year month 1))))

(defui Day
       static om/Ident
       (ident [this {:keys [date]}]
              [:day/day-id (cf/unparse (cf/formatters :basic-date) date)])
       static om/IQuery
       (query [this]
              '[:day/day-id :assigned-to])
       )
