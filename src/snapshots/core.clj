(ns snapshots.core
  (:use [overtone.core]
        [clojure.pprint])
  (:require [overtone.osc :as osc]
            [clojure.string :as str]))

(def storage* (ref {}))

(def history (osc/osc-server 5000))

(defn history-store
  [store-name event-path & args]
  (let [event-info {:path event-path :args args :ts (now)}]
    (dosync
     (let [bucket* (get @storage* store-name)]
       (if bucket*
         (swap! bucket* conj event-info)
         (alter storage* assoc store-name (atom [event-info])))))))

(defn history-clear
  [store-name]
  (dosync
   (alter storage* dissoc store-name)))

(def binary-ops
  {"not=" not=
   "=" =})

(defn extract-value
  [val-name event]
;;  (println "extracting " val-name " from: " event)
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



(defn handle-query
  [& args]
  (let [events (apply history-query args)]
    (pprint events)
;;    (play-back events)
    ))



(osc-rm-all-handlers history)
(osc-handle history "/store" (fn [{[store-name event-path & args] :args}] (apply history-store store-name event-path args)))
(osc-handle history "/clear" (fn [{[store-name] :args}] (history-clear store-name) ))
(osc-handle history "/query" (fn [{[store-name query-id & args] :args}] (apply handle-query store-name query-id args)))

(def c (osc/osc-client "localhost" 5000))

(osc-send c "/store" "/piano/jenny" "/note/on" 10 )

(osc-send c "/store" "/piano/sam" "/phrase/a/start")
(osc-send c "/store" "/piano/sam" "/phrase/a/stop")

(osc-send c "/store" "/piano/sam" "/phrase/b/start")

(osc-send c "/store" "/piano/sam" "/phrase/b/stop")

(osc-send c "/clear" "/piano/sam")

(osc-send c "/query" "/piano/sam" 42
          "drop-while path not= /phrase/b/start"
          "drop 1"
          "drop-while path not= /phrase/b/start"
          "take-while path not= /phrase/b/stop")

(osc-send c "/query" "/a" 42 "drop-while path not= /foo/start"
          "drop 1"
          "take-while path not= /foo/stop")

(play-back (history-query "/piano/sam" 42
;;                          "filter path = note/on"
                          ))


(play-back (history-query "/piano/sam" 42
;;                          "drop-while path not= /phrase/a/start"
;;                          "drop 1"
;;                          "take-while path not= /phrase/a/stop"
                          ))

(osc-send c "/query" "/a" 42 )

(osc-send c "/store" "/a" "/foo/start")
(osc-send c "/store" "/a" "/beans" 8)
(osc-send c "/store" "/a" "/foo/stop")


(count @(get @storage* "/piano/sam"))
@storage*


(def g (group))

(def notes* (atom {}))
(def defaults* (atom {}))


(defsynth foo [not 60 gate 1 vol 0.5 hp 100 lp 10000]
  (let [freq (midicps not)
        env (env-gen:kr (envelope [0 1 0] [0.01 1] :sine 1) gate :action FREE)
        snd (+ (sin-osc freq)
               (* 0.2 (saw freq))
               (* 0.2 (saw (* 4 freq)))
               (* 0.2 (saw (/ freq 2)))
               (* 0.1 (saw (/ freq 4)))
               (* 0.5 (sin (/ freq 4))))
        snd (lpf snd (lag lp))
        snd (hpf snd (lag hp))
        ]

    (out 0 (pan2 (* vol env snd)))))


(defn play-note
  [note vel]
  (let [id (foo :target g note :vol (/ vel 127) :hp (get @defaults* :hp 1000) :lp (get @defaults* :lp 1000))]
    (swap! notes* assoc note id)))

(defn release-note
  [note]
  (let [id (get @notes* note)]
    (ctl id :gate 0)))






(defn midi-handler [e ts]
  (let [note (:note e)
        vel (:vel e)]
    (if (= 0 vel)
      (do
        ;;send note off osc message to history storage
        (osc-send c "/store" "/piano/sam" "/note/off" note)
;;        (println "note off!")
        (release-note note))
      (do
        (play-note note vel)
;;        (println "note on!")
        (osc-send c "/store" "/piano/sam" "note/on" note vel)
        ;;send note on oscm essage to history storage
        ))))

(defn update-ctl-params
  [hp lp]
  (ctl g :hp hp :lp lp))

(defn handle-osc [{[a b] :args}]
  (let [hp (+ 10 (* a 2000))
        lp (+ 10 (* b 2000))]
    (swap! defaults* assoc :hp hp :lp lp)
    (update-ctl-params hp lp)))

(midi-in)
(def keyboard (midi-in "USB"))
(midi-handle-events keyboard #'midi-handler)
(volume 1)
(def a "#(> % 10)")

(defn play-back
  [events]
  (println "events: " events)
  (when-not (empty? events)
        (let [start-t (:ts (first events))
          cur-t   (now)
          t-diff (- cur-t start-t)]
      (dorun
       (map (fn [{path :path ts :ts [note vel] :args}]
              (when (= "note/on" path)
                (println "on")
                (at (+ ts t-diff) (play-note (+ 10 note) vel)))
              (when (= "/note/off" path)
                (println "off")
                (at (+ ts t-diff) (release-note (+ 10  note)))))
            events))))
  )

(defn play-filter
  [events]
  (println "events: " events)
  (when-not (empty? events)
        (let [start-t (:ts (first events))
          cur-t   (now)
          t-diff (- cur-t start-t)]
      (dorun
       (map (fn [{path :path ts :ts [note vel] :args}]
              (let [lp (double (/ note 100))
                    hp (double (/ vel 100))]
                (println "lp: " lp ", hp: " hp)
                (at (+ ts t-diff) (update-ctl-params lp hp))))
            events)))))

(let [n (now)]
  (dorun
     (for [i (range 2000)]
    (at (+ n (* 2 i)) (update-ctl-params (+ 10 (* 100 i)) 1000 )))))



(zero-conf-on)

(def o (osc-server 6660))

(osc-listen o (fn [msg] (println msg)))

(osc-handle o "/3/xy5" (fn [msg] (handle-osc msg)))
