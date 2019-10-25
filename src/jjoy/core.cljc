(ns jjoy.core
  (:require [clojure.edn :as edn]
            [jjoy.dsl.template :as dsl.template]
            [jjoy.dsl.query :as dsl.query]
            [jjoy.dsl.shuffle :as dsl.shuffle]
            [jjoy.utils :as ut]))

(def word-prefix \•)

(defn word [n]
  (str word-prefix n))

(defn word? [s]
  (and (string? s)
       (= (first s) word-prefix)))

(defn unword [w]
  (subs w 1))

(def ^:dynamic *current-thread*)

(defn current-thread [] *current-thread*)

(defn no-implementation [& _]
  (assert false))

(def control-words
  {(word "yield") :yield
   (word "spawn") :spawn
   (word "kill") :kill
   (word "consume") :consume})

(defn control-word? [w] (contains? control-words w))

(defn resolve-control-word [w] (control-words w))

;; (def primitives
;;   {(word "current-thread") (base/no-args-op *current-thread*)
;;    (word "template") (base/binary-op [data template]
;;                                      (dsl.template/run template data))
;;    YIELD-WORD no-implementation
;;    SPAWN-WORD no-implementation
;;    KILL-WORD no-implementation
;;    CONSUME-WORD no-implementation})

(defn f-caller [f arity]
  (fn [{:keys [stack] :as s}]
    (assert (<= arity (count stack)))
    (let [[args stack'] (split-at arity stack)
          _ (prn "---F CALL" args stack)
          res (apply f (reverse args))]
      (assoc s :stack (cons res stack')))))

(defn get-def-container [definitions word]
  (if-let [def (get @definitions word)]
    def
    (let [x (volatile! nil)]
      (swap! definitions assoc word x)
      x)))

(defn get-exec [execs word]
  (if-let [exec (get @execs word)]
    exec
    (let [x (volatile! nil)]
      (swap! execs assoc word x)
      x)))

(defmulti special-form (fn [word definitions] word))
(defmethod special-form :default [word definitions]
  (let [def (get-def-container definitions word)]
    {:consume 0
     :implementation (fn [s] (@def s))}))

(defmethod special-form (word "query") [_ definitions]
  {:consume 1
   :implementation (fn [query]
                     (let [f (dsl.query/compile query)]
                       (f-caller f 1)))})

(defmethod special-form (word "shuffle") [_ definitions]
  {:consume 1
   :implementation (fn [spec]
                     (let [f (dsl.shuffle/compile spec {:referse? true})]
                       (fn [s] (update s :stack f))))})

(defmethod special-form (word "template") [_ definitions]
  {:consume 1
   :implementation (fn [spec]
                     (let [f (dsl.template/compile spec)]
                       (f-caller f 1)))})

(defmethod special-form (word "clj") [_ definitions]
  {:consume 2
   :implementation (fn [arity fn]
                     (let [f (eval (edn/read-string fn))]
                       (f-caller f arity)))})

(defprotocol Executable
  (exec [this state]))

(defprotocol Dumpable
  (dump [this]))

(defrecord Exec [form impl]
  Executable
  (exec [_ state] (impl state))
  Dumpable
  (dump [_] form))

(defn get-or-make-exec [k execs f]
  (if-let [v (get @execs k)]
    v
    (let [v (f)]
      (swap! execs assoc k v)
      v)))

(defn make-exec [word acc {:keys [definitions execs]}]
  (let [{:keys [consume implementation]} (special-form word definitions)]
    (cond
      (= 0 consume)
      (cons (get-or-make-exec word execs #(->Exec [word] implementation)) acc)

      (< (count acc) consume)
      (cons (get-or-make-exec [:dynamic word] execs
                              #(->Exec [word]
                                       (fn [{:keys [stack] :as s}]
                                         (assert (<= consume (count stack)))
                                         (let [[args stack'] (split-at consume stack)]
                                           ((implementation (reverse args)) (assoc s :stack stack'))))))
            acc)

      :else
      (let [[args acc'] (split-at consume acc)]
        (cons (get-or-make-exec [:with-args word args] execs
                                #(->Exec (cons word args) (apply implementation (reverse args))))
              acc')))))

(declare load-json)

(defn load-seq [acc [x & tail] memory]
  (if x
    (cond
      (control-word? x) (recur (cons (resolve-control-word x) acc) tail memory)
      (word? x) (recur (make-exec x acc memory) tail memory)
      :else (recur (cons (load-json x memory) acc) tail memory))
    (reverse acc)))

(defn load-json [form memory]
  (cond
    (map? form) (into {} (ut/map-vals #(load-json % memory) form))
    (sequential? form) (load-seq () form memory)
    (control-word? form) (resolve-control-word form)
    (word? form) (first (make-exec form () memory))

    :else form))

(declare dump-seq)
(defn dump-json [form]
  (cond
    (map? form) (into {} (ut/map-vals dump-json form))
    (sequential? form) (dump-seq form)
    (satisfies? Dumpable form) (first (dump form))
    :else form))

(defn dump-seq [seq]
  (mapcat (fn [v] (if (satisfies? Dumpable v)
                    (dump v)
                    [(dump-json v)]))
          seq))

(defmulti compile-definition (fn [s memory] (get s "type")))

(def ^:dynamic *memory*)

(defmethod compile-definition "instruction"
  [spec memory]
  (binding [*memory* memory]
    (let [sym (gensym)
          form (edn/read-string {:readers
                                 {'word
                                  (fn [s]
                                    `(let [w# (word ~s)
                                           spec# (special-form w# (:definitions ~sym))]
                                       (assert (= 0 (:consume spec#)))
                                       (get-or-make-exec w# (:execs ~sym) #(->Exec [w#] (:implementation spec#)))))}}
                                (get spec "fn"))
          f `(let [~sym *memory*]
               ~form)]
      (eval f))))

(defmethod compile-definition "clojure"
  [{:strs [fn arity]} _]
  (f-caller (eval (edn/read-string fn)) arity))

(defmethod compile-definition "words" [{:strs [body]} memory]
  (let [body' (load-seq () body memory)]
    (fn [s]
      (update s :r-stack #(concat body' %)))))

(defmethod compile-definition "ff" [{:strs [name arity]} _]
  (f-caller (resolve (symbol name)) arity))

(defn load-program
  [{:strs [definitions body]}]
  (let [memory {:definitions (atom {})
                :execs (atom {})}
        body' (load-seq () body memory)]
    (doseq [[name spec] definitions
            :let [def (get-def-container (:definitions memory) (word name))]]
      (vreset! def (compile-definition spec memory)))
    {:memory memory
     :body body'}))

(defn panicking! [state]
  (-> state
      (assoc-in [:threads (:thread-id state)]
                {:stack [(word "panic")]
                 :r-stack ()})))

(defn tick [{:keys [thread-id threads] :as state}]
  (prn "---TICK" state)
  (if-let [{[term & r-stack] :r-stack
            :keys [stack consumer]} (get threads thread-id)]
    (let [state' (assoc-in state [:threads thread-id :r-stack] r-stack)]
      (case term
        nil
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

        :yield
        (dissoc state' :thread-id)

        :spawn
        (let [[spawn-r-stack spawn-stack & stack] stack
              next-thread-id (:next-thread-id state)
              _ (prn "--SPAWN THREAD" next-thread-id {:stack spawn-stack
                                                      :r-stack spawn-r-stack})
              state' (-> state'
                         (assoc-in [:threads thread-id :stack] (cons next-thread-id stack))
                         (assoc-in [:threads next-thread-id] {:stack spawn-stack
                                                              :r-stack spawn-r-stack})
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

        :consume
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

                        (seq (:r-stack thread))
                        (assoc-in state [:threads consume-id :consumer] thread-id)

                        :else
                        (reduced
                         (tick (-> state
                                   (assoc-in [:threads consume-id :consumer] thread-id)
                                   (assoc :thread-id consume-id)))))))
                  state'
                  consume-thread-ids))

        :kill
        (let [[to-kill-thread-id & stack] stack]
          (recur (-> state'
                     (assoc-in [:threads to-kill-thread-id] {:stack [(word "killed")]
                                                             :r-stack ()})
                     (assoc-in [:threads thread-id :stack] stack)
                     (assoc :thread-id to-kill-thread-id)
                     (tick)
                     (assoc :thread-id thread-id))))

        (recur (if (satisfies? Executable term)
                 (binding [*current-thread* thread-id]
                   (update-in state' [:threads thread-id] #(exec term %)))
                 (update-in state' [:threads thread-id :stack]
                            #(cons term %))))))
    (dissoc state :thread-id)))

(defn run
  ([program]
   (run () program))
  ([stack program]
   (let [state {:thread-id 0
                :next-thread-id 1
                :threads {0 {:stack stack :r-stack (:body program)}}}]
     (tick state))))

(defn dump-thread [{:keys [stack r-stack]}]
  {"stack" (dump-seq stack)
   "r-stack" (dump-seq r-stack)})

;; if we introduce References GC should be here
(defn dump-state [{:keys [thread-id next-thread-id threads results]}]
  {"next-thread-id" next-thread-id
   "threads" (->> (for [[k v] threads]
                    [(str k) (dump-thread v)])
                  (into {}))})

(defn load-thread [memory {:strs [stack r-stack]}]
  {:stack (seq (load-seq () stack memory))
   :r-stack (seq (load-seq () r-stack memory))})

(defn load-state [{:keys [memory]} {:strs [next-thread-id threads]}]
  {:next-thread-id next-thread-id
   :threads (->> (for [[k v] threads]
                   [(edn/read-string k) (load-thread memory v)])
                 (into {}))})

(defn running? [{:keys [threads]}]
  (< 0 (count threads)))

(defn unpark [program state thread-id prepend-stack]
  (if (get-in state [:threads thread-id])
    (let [state' (-> state
                     (update-in [:threads thread-id :stack] #(concat prepend-stack %))
                     (assoc :thread-id thread-id))]
      (tick state'))
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
