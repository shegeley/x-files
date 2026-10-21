(require '[datomic.api :as d])

(let [[uri & values] *command-line-args*]
  (try
    (let [connection (d/connect uri)]
      (try
        (assert (= (set values)
                   (set (d/q '[:find [?value ...]
                               :where [_ :migration/value ?value]]
                             (d/db connection)))))
        (println "restored data is queryable:" uri)
        (finally (d/release connection))))
    (finally
      (d/shutdown true)
      (shutdown-agents))))
