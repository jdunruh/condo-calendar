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
;  1  2   3   4   5   6   7
;  8  9  10  11  12  13  14
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
;  5   6   7   8   9  10  11
; 12  13  14  15  16  17  18
; 19  20  21  22  23  24  25
; 26  27  28  29  30  31
;
;                  1   2   3
;  4   5   6   7   8   9  10
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
  (map #(hash-map :date % :day/by-date (date-key %))
       (p/periodic-seq (sunday-of-first-week-of-month year month) (t/days 1))))

(defn six-weeks-containing-month [year month]
  "return the six week period starting with the week of the first
  of the month and continuing for six weeks."
  (take 6 (partition 7 (seq-of-days year month))))

(defn assign-person-to-day [state day person]
  "assign a person to a day by putting an entry into the days hash in the app state"
  (update state :days/by-date assoc day {:day/by-date day :person/by-id person}))

(def init-data {:people   [{:color 'red' :name "Judy" :id 1}
                           {:color 'blue' :name "John" :id 2}
                           {:color 'green' :name "Jake" :id 3}
                           {:color 'yellow' :name "Susan" :id 4}]
                :month-id (t/date-time 2015 11 1)
                :month    (six-weeks-containing-month 2015 11)
                :days/by-date     {}})

(defmulti read om/dispatch)

(defmethod read :month/month-id
  [{:keys [state selector] :as env} key {:keys [month]}]
  (println "reading month ID" selector month)
  (let [st @state]
    (println "Month Id " (:month-id st))
    {:value (:month-id st)}))

(defmethod read :day/day-id
  [{:keys [state selector] :as env} key]
  (println "read day" selector key)
  (let [st @state]
    (if-let [assignment (get-in st [:days/by-date selector])]
      {:value assignment}
      {:value {:name "available" :color 'white'}})))

(defmethod read :person/by-id
  [{:keys [state selector] :as env}] key [:keys selector]
  (println "read person " selector)
  (let [st @state]
    (if-let [person (get-in st [:person/by-id selector])]
      {:value (assoc person :date selector)}
      {:value {:name "available" :color "white" :date selector}})))

(defmethod read :month/weeks
  [{:keys [state selector] :as env} key]
  (println "read month" selector key)
  (let [st @state]
    {:value (get-in st [:month])}))

(defn add-assignment-to-calendar [state date assignee]
  (println "adding " assignee " to " date)
  (update state :days/by-date assoc date {:days/by-date date :person/by-id assignee}))


(defn remove-date-from-days [days date]
  (println "in remove " days date)
  (dissoc days date))

(defn release-day [state date]
  (update state :days/by-date remove-date-from-days date))

(defmulti mutate om/dispatch)

(defmethod mutate 'day/assign
  [{:keys [state]} _ {:keys [assignee date] :as params}]
  (if (not-any? #(= date (:day/by-date %)) (:days @state))
    {:value {:days/by-date [date]}
     :action
            (fn []
              (swap! state add-assignment-to-calendar date assignee))}
    {:value {:error (str "Attempt to assign a day that is already assigned " date)}}))

(defmethod mutate 'day/release
  [{:keys [state]} _ {:keys [assignee date] :as params}]
  (println "releasing " date " requested by " assignee)
  (if-let [day (get-in @state [:days/by-date date])]
    (if (= (:person/by-id day) assignee)
      {:value {:day/by-date [:name]}
       :action
              (fn []
                (swap! state release-day date))}
      {:value {:error "Cannot release this day - it is assigned to someone else"}})
    {:value {:error "Cananot release this day - it is not assigned"}}))

(defui Person
       static om/Ident
       (ident [this {:keys [person-id]}]
              [:person/by-id person-id])
       static om/IQuery
       (query [this]
              '[:color :name])
       Object
       (render [this]
               (let [name (get (om/props this) :name "")
                     color (get (om/props this) :color "transparent")]
                 (dom/div #js {:className (str "name " + color)} name))))

(def person (om/factory Person))


(defui Day
       static om/Ident
       (ident [this {:keys [day/day-id]}]
              (println "Day Ident " date-key this)
              [:day/by-date date-key])
       static om/IQuery
       (query [this]
              '[:day/day-id :date])
       Object
       (render [this]
               (println "rendering day " (om/props this))
               (let [day (:date (om/props this))
                     day-id (:day/day-id (om/props this))]
                 (dom/div #js{:className "day"}
                          (dom/div #js {:className "day-no"} (t/day day))
                          (person day-id)
                          ))))

(def day (om/factory Day {:keyfn :day/by-date}))

(defui Week
       Object
       (render [this]
               (println "rendering week " (om/props this))
               (apply dom/div #js {:className "week"}
                      (map day (om/props this)))))

(def week (om/factory Week {:keyfn #(:day/by-date (first %))}))

(defui Month-header
       static om/IQuery
       (query [this]
              '[:month/month-id])
       Object
       (render [this]
               (println "rendering month header" (om/props this))
               (let [month-id (om/props this)]
                 (dom/div #js {:className "month-header"}
                          (dom/button #js {:className "change-month"} (dom/i #js {:className "fa fa-chevron-left"}))
                          (cf/unparse (cf/formatter "MMMM YYYY") (t/date-time month-id))
                          (dom/button #js {:className "change-month"} (dom/i #js {:className "fa fa-chevron-right"}))))))


(def month-header (om/factory Month-header))

(defui Month-body
       static om/IQuery
       (query [this]
              '[:month/weeks])
       Object
       (render [this]
               (println "rendering body " (:month/weeks (om/props this)))
               (apply dom/div #js {:className "month-body"}
                      (map week (:month/weeks (om/props this))))))


       (def month-body (om/factory Month-body))

(defui Month
       static om/IQuery
       (query [this]
             [:month/weeks (om/get-query Month-header) (om/get-query Day)])
       Object
       (render [this]
               (println "rendering month " (keys (om/props this)))
               (println (:months/weeks (om/props this)))
               (println (om/props this))
               (dom/div #js {:className "month"}
                        (month-header (:month/month-id this))
                        (month-body (om/props this)))))

(def reconciler
  (om/reconciler
    {:state  (atom init-data)
     :parser (om/parser {:read read :mutate mutate})}))

(om/add-root! reconciler
             Month (gdom/getElement "app"))
