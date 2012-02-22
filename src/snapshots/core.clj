(ns snapshots.core
  (:require [overtone.osc :as osc]
            [clojure.string :as str]))

(defrecord snapshots-server [server history])

(def binary-ops
  {"not=" not=
   "=" =})

(defn extract-value
  [val-name event]
  (case val-name
    "path" (:path event)))

(defn filter-drop-while
  [contents identifier op val]
  (let [op-fn (get binary-ops op)]
    (drop-while #(op-fn (extract-value identifier %) val) contents)))

(defn filter-take-while
  [contents identifier op val]
  (let [op-fn (get binary-ops op)]
    (take-while #(op-fn (extract-value identifier %) val) contents)))

(defn filter-drop
  [contents n]
  (let [n (Integer. n)]
    (drop n contents)))

(defn filter-filter
  [contents identifier op val]
  (let [op-fn (get binary-ops op)]
    (filter #(op-fn (extract-value identifier %) val) contents)))

(defn filter-contents
  [filter contents]
      (println "filter: " filter )
  (let [[cmd & args] (clojure.string/split filter #"\s+")]

      (case cmd
        "drop-while" (apply filter-drop-while contents args)
        "take-while" (apply filter-take-while contents args)
        "drop" (apply filter-drop contents args)
        "filter" (apply filter-filter contents args))))

(defn history-query
  [store-name query-id & filters]
  (let [bucket (get @storage* store-name)
        contents (if bucket @bucket [])]
    (loop [filters filters
           contents contents]
      (if-not (or (empty? contents) (empty? filters))
        (recur (rest filters) (filter-contents (first filters) contents))
        contents))))

(defn- history-store
  [storage*  store-name event-path & args]
  (let [event-info {:path event-path :args args :ts (now)}]
    (dosync
     (let [bucket* (get @storage* store-name)]
       (if bucket*
         (swap! bucket* conj event-info)
         (alter storage* assoc store-name (atom [event-info])))))))

(defn- history-clear
  [storage* store-name]
  (dosync
   (alter storage* dissoc store-name)))

(defn- handle-query
  [& args]
  (let [events (apply history-query args)]
    (pprint events)))

(defn- handle-fetch
  [& args]
  (let [events (apply history-fetch args)]
    (pprint events)))

(defn- handle-snapshot
  [& args]
  (let [events (apply snapshot args)]
    (pprint events)))

(defn mk-snapshots-server
  [port]
  (let [server (osc/osc-server port)
        storage* (ref {})])

  (osc-handle server "/store"
              (fn [{[store-name event-path & args] :args}]
                (apply history-store storage* store-name event-path args)))
  (osc-handle server "/clear"
              (fn [{[store-name] :args}]
                (history-clear storage* store-name)))
  (osc-handle server "/query"
              (fn [{[store-name query-id & args] :args}]
                (apply handle-query store-name query-id args)))
  (osc-handle server "/fetch"
              (fn [{[store-name query-id & args] :args}]
                (apply handle-fetch store-name query-id args)))
  (osc-handle server "/snapshot"
              (fn [{[store-name query-id & args] :args}]
                (apply handle-snapshot store-name query-id args)))
  (snapshots-server server storage*))
