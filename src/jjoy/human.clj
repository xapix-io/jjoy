(ns jjoy.human
  (:refer-clojure :exclude [read])
  (:require [clojure.tools.reader.edn :as edn]
            [clojure.tools.reader.reader-types :refer [indexing-push-back-reader]]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [jjoy.core :as jj]
            [jjoy.utils :as ut])
  (:import [java.io PushbackReader StringReader]))

(defprotocol FS
  (read-ns [this ns]))

(defn in-memory-fs [m]
  (reify FS
    (read-ns [_ ns] (get m ns))))

(defn read [s]
  (let [reader (indexing-push-back-reader s)]
    (letfn [(f []
              (let [v (edn/read {:eof ::end
                                 :source-position true
                                 :readers {'word (fn [x] {::word x})}}
                                reader)]
                (when-not (= ::end v)
                  (cons v (f)))))]
      (f))))

(def special-words  (->> jj/control-words
                         (keys)
                         (map (comp symbol jj/unword))
                         (set)))

(defn resolve-word [{:keys [env]} word]
  (let [s (or (get-in env [word :fully-qualified])
              (special-words word)
              (assert (contains? env word) (str "Unknown word " word)))]
    (jj/word (str s))))

(declare parse parse-seq)

(defmulti parser (fn [{:keys [state]} _term] state))

(defn syntax-error [& args]
  (throw (Exception. (str "Syntax error: " (pr-str args)))))

(defn parse-value [{:keys [_env] :as s} v]
  (cond
    (symbol? v) (if (:stringify-symbols s)
                  (str v)
                  (resolve-word s v))
    (map? v) (->> (for [[k v] v]
                    [(cond
                       (string? k) k
                       (symbol? k) (str k)
                       :else (syntax-error "Unsupported type of object field" k))
                     (parse-value s v)])
                  (into {}))
    (sequential? v) (:body (parse-seq s v))
    (or (string? v)
        (number? v)
        (boolean? v)
        (nil? v)) v
    :else (syntax-error "Unsupported value type" v)))

(defmethod parser :consume
  [s term]
  (cond
    (keyword? term) (assoc s :state term)
    :else (update s :body #(conj % (parse-value s
                                                term)))))

(defmethod parser :default
  [s term]
  (-> s
      (assoc :state :consume)
      (update :body #(conj % (parse-value (assoc s :stringify-symbols true) term) (jj/word (name (:state s)))))))

(defn load-lib [{:keys [fs libs-cache] :as _s} ns]
  (if-let [lib (get @libs-cache ns)]
    lib
    (let [content (read-ns fs ns)
          _ (assert content (str "Unknown lib by ns " ns))
          res (parse content
                     {:fs fs
                      :libs-cache libs-cache
                      :ns ns})]
      (swap! libs-cache assoc ns res)
      res)))

(defn parser-use [ctx spec]
  (let [[ns options]
        (cond
          (symbol? spec)
          [spec {"as" spec}]

          (sequential? spec)
          (let [[ns & args] spec]
            [ns
             (into {} (map (fn [[k v]] [(name k) v])
                           (partition 2 args)))]))

        {:keys [definitions] :as _lib} (load-lib ctx ns)
        mapping (cond->
                    {}
                  (get options "as")
                  (into
                   (for [[w _d] definitions]
                     [(symbol (name (get options "as")) (name w))
                      w]))

                  (get options "refer")
                  (into
                   (if (= (get options "refer") :all)
                     (for [[w _d] definitions]
                       [(symbol (name w)) w])
                     (for [w (get options "refer")
                           :let [target (symbol (name ns) (name w))]]
                       (do (assert (get definitions target) (str "Unknown word " w " in library " ns))
                           [w target])))))]
    (-> ctx
        (update :definitions merge definitions)
        (update :env merge (ut/map-vals (fn [w] {:type :library
                                                 :ns ns
                                                 :fully-qualified w})
                                        mapping))
        (assoc :state :consume))))

(defmethod parser :use
  [ctx specs]
  (reduce parser-use ctx specs))

(defn parse-seq [s seq]
  (reduce parser (assoc s
                        :body []
                        :state :consume)
          seq))

(defmethod parser :def
  [ctx expr]
  (cond
    (symbol? expr)
    (-> ctx
        (assoc-in [:def "name"] (name expr))
        (assoc-in [:def "meta"] (meta expr)))

    (string? expr)
    (assoc-in ctx [:def "doc"] expr)

    (map? expr)
    (update-in ctx [:def "options"] merge expr)

    (sequential? expr)
    (let [def (:def ctx)
          sym (symbol (name (:ns ctx)) (get def "name"))
          ctx' (-> ctx
                   (dissoc :def)
                   (assoc :state :consume)
                   (assoc :body [])
                   (assoc-in [:env (symbol (get def "name"))]
                             {:type :def
                              :fully-qualified sym
                              :source (get def "meta")}))
          ctx'' (parse-seq ctx' expr)
          def' {"type" "words"
                ;; FIXME do not allow imports / defs inside body def
                "body" (:body ctx'')}]
      (-> ctx''
          (assoc :env (:env ctx'))
          (assoc :body (:body ctx))
          (assoc-in [:definitions sym] def')))))

(defn instruction-body-words [ctx form]
  (if-let [w (and (map? form)
                  (::word form))]
    (tagged-literal 'word (jj/unword (resolve-word ctx w)))
    form))

(defmethod parser :definstruction-body
  [ctx expr]
  (let [def (:def ctx)
        sym (symbol (name (:ns ctx)) (get def "name"))
        ctx' (-> ctx
                 (dissoc :def)
                 (assoc :state :consume)
                 (assoc :body [])
                 (assoc-in [:env (symbol (get def "name"))]
                           {:type :instruction
                            :fully-qualified sym
                            :source (get def "meta")}))
        def' {"type" "instruction"
              "fn" (pr-str (list 'fn (get def "bindings")
                                 (walk/prewalk (partial instruction-body-words ctx) expr)))}]
    (-> ctx'
        (assoc-in [:definitions sym] def'))))

(defmethod parser :definstruction
  [ctx expr]
  (cond
    (symbol? expr)
    (-> ctx
        (assoc-in [:def "name"] (name expr))
        (assoc-in [:def "meta"] (meta expr)))

    (string? expr)
    (assoc-in ctx [:def "doc"] expr)

    (map? expr)
    (update-in ctx [:def "options"] merge expr)

    (vector? expr)
    (-> ctx
        (assoc :state :definstruction-body)
        (assoc-in [:def "bindings"] expr))))

(defmethod parser :defclj-body
  [ctx expr]
  (let [def (:def ctx)
        sym (symbol (name (:ns ctx)) (get def "name"))
        ctx' (-> ctx
                 (dissoc :def)
                 (assoc :state :consume)
                 (assoc :body [])
                 (assoc-in [:env (symbol (get def "name"))]
                           {:type :defclj
                            :fully-qualified sym
                            :source (get def "meta")}))
        bindings (get def "bindings")
        _ (assert (not (contains? (set bindings) '&)))
        def' {"type" "clojure"
              "arity" (count bindings)
              "fn" (pr-str (list 'fn bindings expr))}]
    (-> ctx'
        (assoc-in [:definitions sym] def'))))

(defmethod parser :defclj
  [ctx expr]
  (cond
    (symbol? expr)
    (-> ctx
        (assoc-in [:def "name"] (name expr))
        (assoc-in [:def "meta"] (meta expr)))

    (string? expr)
    (assoc-in ctx [:def "doc"] expr)

    (map? expr)
    (update-in ctx [:def "options"] merge expr)

    (vector? expr)
    (-> ctx
        (assoc :state :defclj-body)
        (assoc-in [:def "bindings"] expr))))

(defmethod parser :clj-body
  [ctx expr]
  (let [def (:def ctx)
        bindings (get def "bindings")
        _ (assert (not (contains? (set bindings) '&)))]
    (-> ctx
        (dissoc :def)
        (assoc :state :consume)
        (update :body conj (count bindings) (pr-str (list 'fn bindings expr)) (jj/word "clj")))))

(defmethod parser :clj
  [ctx expr]
  (-> ctx
      (assoc :state :clj-body)
      (assoc-in [:def "bindings"] expr)))

(defmethod parser :declare
  [ctx expr]
  (assert (symbol? expr))
  (let [sym (symbol (name (:ns ctx)) (name expr))]
    (-> ctx
        (assoc :state :consume)
        (assoc-in [:env expr]
                  {:type :def
                   :fully-qualified sym}))))

(defmethod parser :import
  [ctx specs]
  (let [import-symbol (fn [imported-name arity]
                        (symbol "jjoy.import"
                                (str (str/replace (str imported-name) #"/" ".")
                                     "-" arity)))
        defs (->> (map (fn [[[imported-name arity] _]]
                         [(import-symbol imported-name arity)
                          {"type" "ff"
                           "name" (str imported-name)
                           "arity" arity}])
                       specs)
                  (into {}))
        envs (->> (map (fn [[[imported-name arity] sym]]
                         (let [_ (assert (symbol? sym))]
                           [sym
                            {:type :import
                             :fully-qualified (import-symbol imported-name arity)
                             :source (meta sym)}]))
                       specs)
                  (into {}))]
    (-> ctx
        (assoc :state :consume)
        (update :definitions merge defs)
        (update :env merge envs))))

(defn analyze
  [body ctx] (parse-seq ctx body))

(defn parse
  ([body] (parse body {}))
  ([body opts]
   (-> body
       (read)
       (analyze (merge {:env {}
                        :ns 'main
                        :definitions {}
                        :libs-cache (atom {})}
                       opts)))))

(defn to-core [{:keys [body definitions]}]
  {"definitions" (ut/map-keys str definitions)
   "body" body})

#_(defn handle-import [ctx specs]
    (let [specs' (map (fn [[spec word]]
                        (let [_ (assert (base/word? word))
                              [_ alias fun arity] (re-find #"(.+?)/(.+)/(\d+)" spec)]
                          {"word" word
                           "imported-word" (jj/word (str "_imports/" alias "/" fun "/" arity))
                           "alias" alias
                           "function" fun
                           "arity" (edn/read-string arity)}))
                      specs)]
      (-> ctx
          (update :imports into specs')
          (update :env merge (->> (for [{:strs [word imported-word] :as x} specs']
                                    [word {:type :import
                                           :fully-qualified imported-word
                                           :import x}])
                                  (into {}))))))

(comment
  (-> (pr-str :def 'zwei-drei [2 3]
              :import '{[clojure.core/into 2] into}
              :definstruction 'identity '[s] '(do s {::word into})
              :defclj 'plus '[a b] '(+ a b)
              'zwei-drei 'plus)
      (parse))

  (-> (apply pr-str
             '(:declare map-step
                        :definstruction map-acc
                        [{[res & stack] :stack
                          [acc & r-stack] :r-stack}]
                        {:stack stack
                         :r-stack (concat [{::word map-step} (conj acc res)] r-stack)}

                        :definstruction map-step
                        [{:keys [stack]
                          [acc [x & source] p & r-stack] :r-stack}]
                        (if x
                          {:stack (cons x stack)
                           :r-stack (concat p [{::word map-acc} acc source p] r-stack)}
                          {:stack (cons acc stack)
                           :r-stack r-stack})

                        :definstruction map [{[p l & stack] :stack :keys [r-stack]}]
                        {:stack stack
                         :r-stack (concat [{::word map-step} [] l p] r-stack)}

                        :import
                        {[clojure.core/+ 2] +}

                        [1 2 3] [1 +] map))
      (parse))

  )
