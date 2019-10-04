(ns jjoy.core
  (:require [clojure.edn :as edn]
            [jjoy.base :as base]
            [jjoy.dsl.template :as dsl.template]
            [jjoy.utils :as ut]))

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
;; (def RESUME-WORD (base/word "resume"))
(def SPAWN-WORD (base/word "spawn"))
(def KILL-WORD (base/word "kill"))
(def CONSUME-WORD (base/word "consume"))

(declare tick)

(defn panicking! [state]
  (-> state
      (assoc-in [:threads (:thread-id state)]
                {:stack [(base/word "panic")]
                 :p-stack ()})))

(defn tick [{:keys [thread-id threads] :as state}]
  (if-let [{[term & p-stack] :p-stack
            :keys [stack consumer]} (get threads thread-id)]
    (let [state' (assoc-in state [:threads thread-id :p-stack] p-stack)]
      (cond
        (nil? term)
        (let [state' (-> state'
                         (dissoc :thread-id))]
          (if-let [{[consuming-threads & consumer-stack] :stack}
                   (and consumer
                        (get threads consumer))]
            (let [state' (reduce
                          (fn [state consuming-id]
                            (ut/dissoc-in state [:threads consuming-id :consumer]))
                          state' consuming-threads)]
              (prn "--CONTINUE consuming THREAD" consumer
                   {:prepend-value stack})
              (recur (-> state'
                         (ut/dissoc-in [:threads thread-id])
                         (assoc-in [:threads consumer :stack]
                                   (cons thread-id (cons stack consumer-stack)))
                         (assoc :thread-id consumer))))
            (do
              (prn "--THREAD FINISHED" thread-id {:result stack})
              state')))

        (= YIELD-WORD term)
        (dissoc state' :thread-id)

        (= SPAWN-WORD term)
        (let [[spawn-p-stack spawn-stack & stack] stack
              next-thread-id (:next-thread-id state)
              _ (prn "--SPAWN THREAD" next-thread-id {:stack spawn-stack
                                                      :p-stack spawn-p-stack})
              state' (-> state'
                         (assoc-in [:threads thread-id :stack] (cons next-thread-id stack))
                         (assoc-in [:threads next-thread-id] {:stack spawn-stack
                                                              :p-stack spawn-p-stack})
                         (assoc :next-thread-id (inc next-thread-id))
                         (tick))]
          (prn "--START SPAWNED THREAD" next-thread-id)
          (recur (assoc state' :thread-id next-thread-id)))

        ;; (= RESUME-WORD term)
        ;; (let [[resume-thread-id & stack] stack
        ;;       state' (-> state'
        ;;                  (assoc-in [:threads thread-id :stack] stack)
        ;;                  (assoc :thread-id resume-thread-id))]
        ;;   (recur (assoc (tick state')
        ;;                 :thread-id thread-id)))

        (= CONSUME-WORD term)
        (let [[consume-thread-ids & stack] stack]
          (prn "--CONSUME FROM THREADS" thread-id {:joined-to consume-thread-ids})
          (reduce (fn [state consume-id]
                    (let [thread (get-in state [:threads consume-id])]
                      (prn "---CONSUME FROM" consume-id thread)
                      (cond
                        (nil? thread)
                        (do (prn "Trying to consume not exists thread, panicking!" {:consume-id consume-id})
                            (reduced (tick (panicking! state))))

                        (:consumer thread)
                        (do (prn "Trying to consume thread with set consumer, panicking!" {:consume-id consume-id})
                            (reduced (tick (panicking! state))))

                        (seq (:p-stack thread))
                        (assoc-in state [:threads consume-id :consumer] thread-id)

                        :else
                        (reduced
                         (tick (-> state
                                   (assoc-in [:threads consume-id :consumer] thread-id)
                                   (assoc :thread-id consume-id)))))))
                  state'
                  consume-thread-ids))

        (= KILL-WORD term)
        (let [[to-kill-thread-id & stack] stack]
          (recur (-> state'
                     (assoc-in [:threads to-kill-thread-id] {:stack [(base/word "killed")]
                                                             :p-stack ()})
                     (assoc-in [:threads thread-id :stack] stack)
                     (assoc :thread-id to-kill-thread-id)
                     (tick)
                     (assoc :thread-id thread-id))))

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

(defn dump-thread [{:keys [stack p-stack listeners]}]
  {"stack" stack
   "p-stack" p-stack
   "listeners" listeners})

;; if we introduce References GC should be here
(defn dump-state [{:keys [thread-id next-thread-id threads results]}]
  {"next-thread-id" next-thread-id
   "threads" (->> (for [[k v] threads]
                    [(str k) (dump-thread v)])
                  (into {}))})

(defn load-thread [{:strs [stack p-stack listeners]}]
  {:stack stack
   :p-stack p-stack
   :listeners listeners})

(defn load-state [{:strs [next-thread-id threads]}]
  {:next-thread-id next-thread-id
   :threads (->> (for [[k v] threads]
                   [(edn/read-string k) (load-thread v)])
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
