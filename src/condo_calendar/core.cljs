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






;; Here are the 7 possible month layouts - based on a 31 day month. Just stop at
;; 28, 29, or 30 for shorter months. All months can fit into 6 weeks
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
  (map #(hash-map :date % :days/by-date (date-key %))
       (p/periodic-seq (sunday-of-first-week-of-month year month) (t/days 1))))

(defn six-weeks-containing-month [year month]
  "return the six week period starting with the week of the first
  of the month and continuing for six weeks."
  (take 6 (partition 7 (seq-of-days year month))))

(def init-data
  (let [today (t/today)
        month (t/month today)
        year  (t/year today)]
                {:person/by-id {0 {:color  "red" :name "Judy" :id 0}
                               1 {:color "blue" :name "John" :id 1}
                               2 {:color "green" :name "Jake" :id 2}
                               3 {:color "yellow" :name "Susan" :id 3}}
                 :user 1
                 :month/month-id (t/date-time year month 1)
                 :month (six-weeks-containing-month year month)
                 :days/by-date {}
                 :current-user [:person/by-id 2]}))

(defn date-to-assignment [state date]
  (get-in state (get-in state [:days/by-date date] [nil]) {:name "available" :color "white"}))

(defn denormalize-week [state week]
  (map #(merge % (date-to-assignment state (:days/by-date %))) week))


(defn denormalize-month [state]
  (map #(denormalize-week state %) (:month state)))

(defmulti read om/dispatch)

(defmethod read :month/month-id
  [{:keys [state selector] :as env} key {:keys [month]}]
  (println "reading month ID" selector month)
  (let [st @state]
    (println "Month Id " (:month/month-id st))
    {:value (:month/month-id st)}))

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
    {:value (denormalize-month st)}))


(defn add-assignment-to-calendar [state date assignee]
  (println "adding " assignee " to " date)
  (update state :days/by-date assoc date [:person/by-id assignee]))


(defn remove-date-from-days [days date]
  (println "in remove " days date)
  (dissoc days date))

(defn release-day [state date]
  (println "release-day " state date)
  (update state :days/by-date remove-date-from-days date))

(defn next-month [current-month-start]
  "given a date-time, generate the date-time one month later"
  (t/plus current-month-start (t/months 1)))

(defn last-month [current-month-start]
  "given a date-time, generate the date-time one month earlier"
  (t/minus current-month-start (t/months 1)))


(defmulti mutate om/dispatch)

(defmethod mutate 'day/assign
  [{:keys [state]} _ {:keys [assignee date] :as params}]
  (if (not-any? #(= date (:day/by-date %)) (:days @state))
    {:value {:days/by-date date}
     :action
            (fn []
              (swap! state add-assignment-to-calendar (str date) assignee))}
    {:value {:error (str "Attempt to assign a day that is already assigned " date)}}))

(defmethod mutate 'day/release
  [{:keys [state]} _ {:keys [assignee date] :as params}]
  (if-let [day (get-in @state [:days/by-date date])]
    (if (= (second day) assignee)
      {:value {:days/by-date date}
       :action
              (fn []
                (swap! state release-day (str date)))}
      {:value {:error "Cannot release this day - it is assigned to someone else"}})
    {:value {:error "Cananot release this day - it is not assigned"}}))

(defmethod mutate 'month/next
  [{:keys [state]} _ _]
  (println "changing to next month")
  (let [st @state
        new-month (next-month (:month/month-id st))
        month (t/month new-month)
        year  (t/year new-month)]
    (println "in next month " new-month month year)
    {:value  {:month/month-id (date-key new-month)}
     :action (fn []
               (swap! state assoc :month/month-id new-month :month (six-weeks-containing-month year month)))}))

(defmethod mutate 'month/previous
  [{:keys [state]} _ _]
  (let [st @state
        new-month (last-month (:month/month-id st))
        month (t/month new-month)
        year  (t/year new-month)]
    {:value  {:month/month-id (date-key new-month)}
     :action (fn []
               (swap! state (assoc :month/month-id new-month :month (six-weeks-containing-month year month))))}))

;(defn assign-day [assignee date]
;  "assign a day to a user"
;  (om/transact! reconciler '[(day/release {:assignee 2 :date 20151112})]))
;
;(defn release-a-day [assignee date]
;  "release a day owner by a user"
;  (om.next/transact! reconciler '[(day/assign {:date 20151112 :assignee 2})]))

;; need to add click listeners, remove if owner by this user, none if owner by other user, assign if available
(defui Day
       static om/Ident
       (ident [this {:keys [day/by-date]}]
              (println "Day Ident " by-date this)
              [:days/by-date by-date])
       static om/IQuery
       (query [this]
              '[:day/day-id :date])
       Object
       (render [this]
               (println "rendering day " (om/props this))
               (let [day (:date (om/props this))
                     color (:color (om/props this))
                     name (:name (om/props this))]
                 (dom/div #js{:className (str "day " color)}
                          (dom/div #js {:className "day-no"} (t/day day))
                          (dom/span #js {:className "day-name"} name)
                          ))))

(def day (om/factory Day {:keyfn :days/by-date}))

(defui Week
       Object
       (render [this]
               (println "rendering week " (om/props this))
               (apply dom/div #js {:className "week"}
                      (map day (om/props this)))))

(def week (om/factory Week {:keyfn #(:days/by-date (first %))}))

(defui Month-header
       static om/IQuery
       (query [this]
              '[:month/month-id])
       static om/Ident
       (ident [this props]
              (println "month header ident " this props)
              [:month/month-id props])
       Object
       (render [this]
               (println "rendering month header" (om/props this))
               (let [month-id (om/props this)]
                 (dom/div #js {:className "month-header"}
                          (dom/button #js {:className "change-month"} (dom/i #js {:className "fa fa-chevron-left fa-2x"}))
                          (cf/unparse (cf/formatter "MMMM YYYY") (t/date-time month-id))
                          (dom/button #js {:className "change-month"} (dom/i #js {:className "fa fa-chevron-right fa-2x"}))))))


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
             [:month/weeks {:month/month-id (om/get-query Month-header)}])
       Object
       (render [this]
               (println "rendering month " (keys (om/props this)))
               (println (:months/weeks (om/props this)))
               (println "month ID " (:month/month-id (om/props this)))
               (println (om/props this))
               (dom/div #js {:className "month"}
                        (month-header (:month/month-id (om/props this)))
                        (month-body (om/props this)))))

(def reconciler
  (om/reconciler
    {:state  (atom init-data)
     :parser (om/parser {:read read :mutate mutate})}))

(om/add-root! reconciler
             Month (gdom/getElement "app"))
