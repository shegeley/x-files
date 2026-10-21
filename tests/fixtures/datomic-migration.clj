(require '[datomic.api :as d])

(let [[operation database & values] *command-line-args*
      uri (str "datomic:sql://" database
               "?jdbc:postgresql://localhost:5432/datomic?user=datomic")]
  (try
    (when (= operation "create")
      (assert (d/create-database uri)))
    (let [connection (d/connect uri)]
      (try
        (case operation
          "create"
          (do
            @(d/transact connection [{:db/ident :migration/value
                                     :db/valueType :db.type/string
                                     :db/cardinality :db.cardinality/one}])
            @(d/transact connection [{:migration/value "before"}]))
          "add"
          @(d/transact connection [{:migration/value "after"}])
          "check"
          (assert (= (set values)
                     (set (d/q '[:find [?value ...]
                                 :where [_ :migration/value ?value]]
                               (d/db connection))))))
        (finally (d/release connection))))
    (println "migration fixture passed:" operation database)
    (finally
      (d/shutdown true)
      (shutdown-agents))))
