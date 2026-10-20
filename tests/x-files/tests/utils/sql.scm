(define-module (x-files tests utils sql)
  #:use-module ((x-files utils sql) #:select (sql))
  #:use-module ((ares suitbl) #:select (define-suite is test)))

(define-suite (sql-select-insert-tests)
  ;; bt-presence's statements, so it can drop its local compiler unchanged
  (test "upsert with placeholders" ()
    (is (equal? (string-append "INSERT INTO devices (mac, name, vendor, device_class, first_seen, last_seen)"
                               " VALUES (?, ?, ?, ?, ?, ?)"
                               " ON CONFLICT(mac) DO UPDATE SET name = ?, vendor = ?, device_class = ?, last_seen = ?")
                (sql '(insert-into devices (mac name vendor device_class first_seen last_seen)
                       (values ? ? ? ? ? ?)
                       (on-conflict (mac) (do-update (name ?) (vendor ?) (device_class ?) (last_seen ?))))))))

  (test "select with where, order and limit" ()
    (is (equal? "SELECT seen_at, rssi, adapter FROM sightings WHERE mac = ? ORDER BY seen_at desc LIMIT 100"
                (sql '(select (seen_at rssi adapter) (from sightings)
                       (where (= mac ?)) (order-by seen_at desc) (limit 100)))))
    (is (equal? "SELECT mac, seen_at FROM sightings ORDER BY seen_at desc LIMIT 50"
                (sql '(select (mac seen_at) (from sightings) (order-by seen_at desc) (limit 50))))))

  (test "string literals are quoted and escaped" ()
    (is (equal? "SELECT x FROM t WHERE name = 'O''Brien'"
                (sql '(select (x) (from t) (where (= name "O'Brien")))))))

  (test "conjunctions, functions and subqueries" ()
    (is (equal? "SELECT max(ts) FROM t WHERE device = 'a' AND ts > (SELECT max(ts) FROM u WHERE device = 'a')"
                (sql '(select ((max ts)) (from t)
                       (where (and (= device "a")
                                   (> ts (select ((max ts)) (from u) (where (= device "a")))))))))))

  (test "insert ... format, and insert ... select into a table function" ()
    (is (equal? "INSERT INTO agents.llm_exchanges FORMAT JSONEachRow"
                (sql '(insert-into agents.llm_exchanges (format JSONEachRow)))))
    (is (equal? "INSERT INTO FUNCTION remote('10.0.0.1:9440', agents, t) (a, b) SELECT a, b FROM agents.t"
                (sql '(insert-into (function (remote "10.0.0.1:9440" agents t)) (a b)
                       (select (a b) (from agents.t))))))))

(define-suite (sql-ddl-tests)
  (test "create database" ()
    (is (equal? "CREATE DATABASE IF NOT EXISTS agents"
                (sql '(create-database if-not-exists agents))))
    (is (equal? "CREATE DATABASE agents" (sql '(create-database agents)))))

  (test "create table with types, codecs, engine, partition and order key" ()
    (is (equal? (string-append "CREATE TABLE IF NOT EXISTS agents.t"
                               " (id UUID, ts DateTime64(3, 'UTC'), agent LowCardinality(String),"
                               " headers Map(String, String), body String CODEC(ZSTD(6)))"
                               " ENGINE = ReplacingMergeTree(ts) PARTITION BY toYYYYMM(ts)"
                               " ORDER BY (device, ts, id)")
                (sql '(create-table if-not-exists agents.t
                       (columns (id UUID)
                                (ts (DateTime64 3 "UTC"))
                                (agent (LowCardinality String))
                                (headers (Map String String))
                                (body String (codec (ZSTD 6))))
                       (engine (ReplacingMergeTree ts))
                       (partition-by (toYYYYMM ts))
                       (order-by device ts id))))))

  (test "alter table add column" ()
    (is (equal? "ALTER TABLE agents.t ADD COLUMN IF NOT EXISTS first_byte_ms UInt32"
                (sql '(alter-table agents.t (add-column if-not-exists (first_byte_ms UInt32))))))
    (is (equal? "ALTER TABLE agents.t ADD COLUMN body String CODEC(ZSTD(6))"
                (sql '(alter-table agents.t (add-column (body String (codec (ZSTD 6))))))))))
