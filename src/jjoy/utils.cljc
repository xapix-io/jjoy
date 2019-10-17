(ns jjoy.utils)

(defmacro for-map
  "Like 'for' for building maps. Same bindings except the body should have a
  key-expression and value-expression. If a key is repeated, the last
  value (according to \"for\" semantics) will be retained.
  (= (for-map [i (range 2) j (range 2)] [i j] (even? (+ i j)))
     {[0 0] true, [0 1] false, [1 0] false, [1 1] true})
  An optional symbol can be passed as a first argument, which will be
  bound to the transient map containing the entries produced so far."
  {:style/indent 2}
  ([seq-exprs key-expr val-expr]
   `(for-map ~(gensym "m") ~seq-exprs ~key-expr ~val-expr))
  ([m-sym seq-exprs key-expr val-expr]
   `(let [m-atom# (atom (transient {}))]
      (doseq ~seq-exprs
        (let [~m-sym @m-atom#]
          (reset! m-atom# (assoc! ~m-sym ~key-expr ~val-expr))))
      (persistent! @m-atom#))))

(defn map-vals
  "Build map k -> (f v) for [k v] in map, preserving the initial type"
  [f m]
  (cond
    (sorted? m)
    (reduce-kv (fn [out-m k v] (assoc out-m k (f v))) (sorted-map) m)
    (map? m)
    (persistent! (reduce-kv (fn [out-m k v] (assoc! out-m k (f v))) (transient {}) m))
    :else
    (for-map [[k v] m] k (f v))))

(defn map-keys
  "Build map (f k) -> v for [k v] in map m"
  [f m]
  (if (map? m)
    (persistent! (reduce-kv (fn [out-m k v] (assoc! out-m (f k) v)) (transient {}) m))
    (for-map [[k v] m] (f k) v)))

(defn dissoc-in
  "Dissociate this keyseq from m, removing any empty maps created as a result
   (including at the top-level)."
  [m [k & ks]]
  (when m
    (if-let [res (and ks (dissoc-in (get m k) ks))]
      (assoc m k res)
      (let [res (dissoc m k)]
        (when-not (empty? res)
          res)))))
;; Deep merge JSON-like data
(defn deep-merge [a b]
  (cond
    (and (map? a) (map? b)) (merge-with deep-merge a b)
    (and (sequential? a) (sequential? b)) (map deep-merge a b)
    :else b))
