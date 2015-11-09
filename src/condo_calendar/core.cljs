(ns condo-calendar.core
  (:require [cljs-time.core :as t]
            [cljs-time.periodic :as p]
            [cljs-time.format :as cf]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [om.next :as om :refer-macros [defui]]
            [om.dom :as dom]
            [goog.dom :as gdom]))

  (enable-console-print!)






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
  (let [first-of-month (t/date-time year month 1)]
    (t/minus first-of-month (t/days (mod (t/day-of-week first-of-month) 7)))))

(defn date-key [day]
  (cf/unparse (cf/formatters :basic-date) day))

(defn seq-of-days [year month]
  (map #(hash-map :date % :date-key (date-key %))
       (p/periodic-seq (sunday-of-first-week-of-month year month) (t/days 1))))

(defn six-weeks-containing-month [year month]
  "return the six week period starting with the week of the first
  of the month and continuing for six weeks."
  (take 6 (partition 7 (seq-of-days year month))))

(def init-data {:people [{:color 'red' :name "Judy" :id 1}
                         {:color 'blue' :name "John" :id 2}
                         {:color 'green' :name "Jake" :id 3}
                         {:color 'yellow' :name "Susan" :id 4}
                         {:color 'none' :name "default" :id 0}]
                :month-id (t/date-time 2015 11 1)
                :month (six-weeks-containing-month 2015 11)
                :days []})


(defmulti read om/dispatch)

(defmethod read :month/month-id
  [{:keys [state selector] :as env} key {:keys [month]}]
  (println "reading month ID" selector month)
  (let [st @state]
    (println "Month Id " (:month-id st))
    {:value (:month-id st)}))

(defmethod read :day/day-id
  [{:keys [state selector] :as env} key ]
  (println "read day" selector key)
  (let [st @state]
    {:value (get-in st [:days selector] {:name "available" :color 'none'})}))

(defmethod read :month/weeks
  [{:keys [state selector] :as env} key ]
  (println "read month" selector key)
  (let [st @state]
    {:value (get-in st [:month])}))

(defmulti mutate om/dispatch)

(defmethod mutate 'points/increment
  [{:keys [state]} _ {:keys [date owner]}]
  {:action
   (fn []
     (swap! state assoc-in
            [:days (date-key date)]
            owner))})


(defui Day
       static om/Ident
       (ident [this {:keys [date-key]}]
              (println "Day Ident " date-key this)
              [:day/day-id date-key])
       static om/IQuery
       (query [this]
              '[:name :color])
       Object
       (render [this]
               (println "rendering day " (om/props this))
               (let [day (:date (om/props this))]
                 (apply dom/div #js{:className "day"}
                        [(dom/div #js {:className "day-no"} (t/day day))
                         (dom/div #js {:className "name"} (get (om/props this) :name ""
                                                               ))]))))

(def day (om/factory Day {:keyfn :date-key}))

(defui Week
       Object
       (render [this]
               (println "rendering week " (om/props this))
               (apply dom/div #js {:className "week"}
                        (map day (om/props this)))))

(def week (om/factory Week {:keyfn #(:date-key (first %))}))

(defui Month-header
       static om/IQuery
       (query [this]
              '[:month-id])
       Object
       (render [this]
               (println "rendering month header" (om/props this))
               (let [month-id (om/props this)]
                 (dom/div #js {:className "month-header"} (cf/unparse (cf/formatter "MMMM YYYY") (t/date-time month-id))))))

(def month-header (om/factory Month-header))

(defui Month
       static om/IQuery
       (query [this]
              [:month/weeks {:month/month-id (om/get-query Month-header)} {:day/day-id (om/get-query Day)}])
       Object
       (render [this]
               (dom/div #js {:className "month"}
                      (month-header (:month/month-id (om/props this)))
                      (map week (:month/weeks (om/props this))))))

(def reconciler
  (om/reconciler
    {:state  (atom init-data)
     :parser (om/parser {:read read :mutate mutate})}))

(om/add-root! reconciler
              Month (gdom/getElement "app"))
