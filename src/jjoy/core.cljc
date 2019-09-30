(ns jjoy.core
  (:require [clojure.edn :as edn]
            [jjoy.base :as base]
            [jjoy.dsl.template :as dsl.template]
            [jjoy.utils :as ut]))

(comment
  shuffle lang
  "abc-bc"
  "[a]-a"
  "..[..a]-..a"
  )

(def word base/word)

(def ^:dynamic *current-thread*)

(defn load-program
  ([json-program] (load-program {} json-program))
  ([more-vocabulary {:strs [vocabulary body]}]
   {:vocabulary (merge (ut/map-vals base/make-definition vocabulary)
                       more-vocabulary
                       base/lib
                       base/primitive
                       {(word "current-thread") (base/no-args-op *current-thread*)
                        (word "template") (base/binary-op [data template]
                                                          (dsl.template/run template data))})
    :body body}))

(defn jsonify+load
  ([form-program] (jsonify+load {} form-program))
  ([more-vocabulary form-program] (load-program more-vocabulary (base/jsonify form-program))))

(def YIELD-WORD (base/word "yield"))
(def RESUME-WORD (base/word "resume"))
(def SPAWN-WORD (base/word "spawn"))
(def KILL-WORD (base/word "kill"))
(def JOIN-WORD (base/word "join"))

(defn tick [{:keys [thread-id threads] :as state}]
  (if-let [{[term & p-stack] :p-stack
            :keys [stack joins]} (get threads thread-id)]
    (let [state' (assoc-in state [:threads thread-id :p-stack] p-stack)]
      (cond
        (nil? term)
        (let [state' (-> state'
                         (dissoc :thread-id)
                         (ut/dissoc-in [:threads thread-id]))]
          (if (seq joins)
            (reduce (fn [{:keys [threads] :as state} joined-thread-id]
                      (if (get threads joined-thread-id)
                        (do (prn "--CONTINUE JOINED THREAD" joined-thread-id
                                 {:prepend-stack stack})
                            (tick (-> state
                                      (update-in [:threads joined-thread-id :stack] #(concat stack %))
                                      (assoc :thread-id joined-thread-id))))
                        state)) state' joins)
            (do
              (prn "--THREAD FINISHED" thread-id {:result stack})
              (cond-> state' (seq? stack)
                      (assoc-in [:results thread-id] stack)))))

        (= YIELD-WORD term)
        state'

        (= SPAWN-WORD term)
        (let [[spawn-p-stack spawn-stack & stack] stack
              next-thread-id (:next-thread-id state)
              _ (prn "--SPAWN THREAD" next-thread-id {:stack spawn-stack
                                                      :p-stack spawn-p-stack})
              state' (-> state'
                         (assoc-in [:threads thread-id :stack] (cons next-thread-id stack))
                         (assoc-in [:threads next-thread-id] {:stack spawn-stack
                                                              :p-stack spawn-p-stack})
                         (assoc :thread-id next-thread-id
                                :next-thread-id (inc next-thread-id))
                         (tick))]
          (prn "--CONTINUE AFTER SPAWN" thread-id)
          (recur (assoc state' :thread-id thread-id)))

        (= RESUME-WORD term)
        (let [[resume-thread-id & stack] stack
              state' (-> state'
                         (assoc-in [:threads thread-id :stack] stack)
                         (assoc :thread-id resume-thread-id))]
          (recur (assoc (tick state')
                        :thread-id thread-id)))

        (= JOIN-WORD term)
        (let [[join-thread-id & stack] stack]
          (if-let [res (get-in state' [:results join-thread-id])]
            (do (prn "--JOIN TO FINISHED THREAD" thread-id {:join-thread join-thread-id
                                                            :prepend-stack res})
                (recur (-> state'
                           (assoc-in [:threads thread-id :stack] (concat res stack))
                           (ut/dissoc-in [:results join-thread-id]))))
            (do
              (prn "--JOIN TO THREAD" thread-id {:joined-to join-thread-id})
              (-> state'
                  (assoc-in [:threads thread-id :stack] stack)
                  (update-in [:threads join-thread-id :joins] conj thread-id)))))

        (= KILL-WORD term)
        (let [[to-kill-thread-id & stack] stack]
          (recur (-> state'
                     (ut/dissoc-in [:threads to-kill-thread-id])
                     (assoc-in [:threads thread-id :stack] stack))))

        :else (do
                #_(prn "--CALL BASE" {:thread thread-id
                                    :state (get-in state [:threads thread-id])})
                (recur
                 (binding [*current-thread* thread-id]
                   (update-in state [:threads thread-id] base/tick))))))
    (dissoc state :thread-id)))

(defn run
  ([program]
   (run () program))
  ([stack program]
   (let [state {:thread-id 0
                :next-thread-id 1
                :threads {0 {:stack stack :p-stack (:body program)}}}]
     (binding [base/*vocabulary* (:vocabulary program)]
       (tick state)))))

(defn dump-thread [{:keys [stack p-stack joins results]}]
  {"stack" stack
   "p-stack" p-stack
   "joins" joins
   "results" results})

;; if we introduce References GC should be here
(defn dump-state [{:keys [thread-id next-thread-id threads results]}]
  {"next-thread-id" next-thread-id
   "threads" (->> (for [[k v] threads]
                    [(str k) (dump-thread v)])
                  (into {}))
   "results" (->> (for [[k v] results]
                    [(str k) v])
                  (into {}))})

(defn load-thread [{:strs [stack p-stack joins]}]
  {:stack stack
   :p-stack p-stack
   :joins joins})

(defn load-state [{:strs [next-thread-id threads results]}]
  {:next-thread-id next-thread-id
   :threads (->> (for [[k v] threads]
                   [(edn/read-string k) (load-thread v)])
                 (into {}))
   :results (->> (for [[k v] results]
                   [(edn/read-string k) v])
                 (into {}))})

(defn running? [{:keys [threads]}]
  (< 0 (count threads)))

(defn unpark [program state thread-id prepend-stack]
  (if (get-in state [:threads thread-id])
    (let [state' (-> state
                     (update-in [:threads thread-id :stack] #(concat prepend-stack %))
                     (assoc :thread-id thread-id))]
      (binding [base/*vocabulary* (:vocabulary program)]
        (tick state')))
    (throw (ex-info "Unknown thread" {:type ::unknown-thread}))))

(comment
  get-vehicles = { url "http://samples-dev.ix-io.net/api/vehicles", method "get" }
  http-request "body" get
  get-user = "http://samples-dev.ix-io.net/api/users/"
  "ab-[ba]" shuffle str.join
  "url" swap { method "get" } assoc
  http-request "body" get

  get-vehicles {data {* {user-id ""}}} jolt.shift [get-user] map

  get-
  )
