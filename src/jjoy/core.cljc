(ns jjoy.core
  (:require [clojure.string :as str]
            [jjoy.lib :as lib]
            [jjoy.utils :as ut]
            [jjoy.base :as base]))

(comment
  shuffle lang
  "abc-bc"
  "[a]-a"
  "..[..a]-..a"
  )

(def word base/word)

(defn load-program
  ([json-program] (load-program {} json-program))
  ([more-vocabulary {:strs [vocabulary body]}]
   {:vocabulary (merge (ut/map-vals base/make-definition vocabulary)
                       more-vocabulary
                       base/lib
                       base/primitive)
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
  (prn "---TICK" state)
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
                        (tick (-> state
                                  (update-in [:threads joined-thread-id :stack] #(concat stack %))
                                  (assoc :thread-id joined-thread-id)))
                        state)) state' joins)
            (cond-> state' (seq? stack)
                    (assoc-in [:results thread-id] stack))))

        (= YIELD-WORD term)
        state'

        (= SPAWN-WORD term)
        (let [[spawn-p-stack spawn-stack & stack] stack
              next-thread-id (:next-thread-id state)
              state' (-> state'
                         (assoc-in [:threads thread-id :stack] (cons next-thread-id stack))
                         (assoc-in [:threads next-thread-id] {:stack spawn-stack
                                                              :p-stack spawn-p-stack})
                         (assoc :next-thread-id (inc next-thread-id)))]
          (recur (assoc (tick state')
                        :thread-id next-thread-id)))

        (= RESUME-WORD term)
        (let [[resume-thread-id & stack] stack
              state' (-> state'
                         (assoc-in [:threads thread-id :stack] stack)
                         (assoc :thread-id resume-thread-id))]
          (recur (assoc (tick state')
                        :thread-id thread-id)))

        (= JOIN-WORD term)
        (let [[join-thread-id & stack] stack]
          (-> state'
              (assoc-in [:threads thread-id :stack] stack)
              (update-in [:threads join-thread-id :joins] conj thread-id)))

        (= KILL-WORD term)
        (let [[to-kill-thread-id & stack] stack]
          (recur (-> state'
                     (ut/dissoc-in [:threads to-kill-thread-id])
                     (assoc-in [:threads thread-id :stack] stack))))

        :else (recur (update-in state [:threads thread-id] base/tick))))
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
