(define-module (x-files utils sql)
  #:use-module ((srfi srfi-13) #:select (string-join string-upcase))
  #:use-module ((srfi srfi-11) #:select (let-values))
  #:use-module ((ice-9 match)  #:select (match))
  #:export (sql))

;;; SQL as Scheme lists: write a statement as a quoted/quasiquoted list, get
;;; the SQL string with `sql'.  Keep each statement in its own top-level
;;; variable:
;;;
;;;   (define %sql-recent
;;;     (sql '(select (mac seen_at) (from sightings)
;;;            (where (= mac ?)) (order-by seen_at desc) (limit 50))))
;;;
;;; Expressions:
;;;   symbol                  identifier, as is (agents.llm_exchanges, *)
;;;   ?                       placeholder
;;;   "text"                  string literal, quoted
;;;   42                      number
;;;   (= a b), (> a b), ...   binary operator
;;;   (and e ...), (or e ...) conjunction / parenthesised disjunction
;;;   (select ...)            parenthesised subquery
;;;   (call f a ...), (f a ...)  function call or type: f(a, ...)
;;;
;;; Statements: select, insert-into (values / select / format), create-database,
;;; create-table, alter-table.  See the docstring of `sql' for their shapes.

(define %binary-operators '(= != <> < > <= >= like ilike in))

(define (identifier x)
  (cond ((symbol? x) (symbol->string x))
        ((string? x) x)
        (else (error "sql: bad identifier" x))))

(define (string-literal s)
  (string-append "'" (string-join (string-split s #\') "''") "'"))

(define (expression e)
  (match e
    ('? "?")
    ((? string? s) (string-literal s))
    ((? number? n) (number->string n))
    ((? symbol? s) (symbol->string s))
    (('select _ ...) (string-append "(" (sql e) ")"))
    (('and es ...) (string-join (map expression es) " AND "))
    (('or es ...) (string-append "(" (string-join (map expression es) " OR ") ")"))
    (((? (lambda (op) (memq op %binary-operators)) op) a b)
     (string-append (expression a) " " (string-upcase (symbol->string op)) " " (expression b)))
    (('call f args ...) (call f args))
    (((? symbol? f) args ...) (call f args))
    (_ (error "sql: bad expression" e))))

(define (call f args)
  (string-append (identifier f) "(" (string-join (map expression args) ", ") ")"))

(define (expressions es) (string-join (map expression es) ", "))

(define (target t)
  "A table name, or (function (remote ...)) for a table function."
  (match t
    (('function call) (string-append "FUNCTION " (expression call)))
    (_ (expression t))))

(define (clause c)
  (match c
    (('where e) (string-append " WHERE " (expression e)))
    (('order-by col dir) (string-append " ORDER BY " (expression col) " " (symbol->string dir)))
    (('limit n) (string-append " LIMIT " (number->string n)))))

(define (update u)
  (match u ((col val) (string-append (identifier col) " = " (expression val)))))

(define (insert-extra more)
  (match more
    (() "")
    ((('on-conflict conflict-cols ('do-update updates ...)))
     (string-append " ON CONFLICT(" (expressions conflict-cols) ") DO UPDATE SET "
                    (string-join (map update updates) ", ")))))

(define (column-definition column)
  "(name type) or (name type (codec c ...))."
  (match column
    ((name type) (string-append (identifier name) " " (expression type)))
    ((name type ('codec codecs ...))
     (string-append (identifier name) " " (expression type)
                    " CODEC(" (expressions codecs) ")"))))

(define (if-not-exists flags)
  (if (memq 'if-not-exists flags) "IF NOT EXISTS " ""))

(define (split-flags parts)
  "Leading flag symbols of PARTS, and the rest."
  (let loop ((parts parts) (flags '()))
    (if (and (pair? parts) (memq (car parts) '(if-not-exists)))
        (loop (cdr parts) (cons (car parts) flags))
        (values (reverse flags) parts))))

(define (table-option option)
  (match option
    (('engine e) (string-append " ENGINE = " (expression e)))
    (('partition-by e) (string-append " PARTITION BY " (expression e)))
    (('order-by cols ...) (string-append " ORDER BY (" (expressions cols) ")"))))

(define (alteration a)
  (match a
    (('add-column 'if-not-exists column)
     (string-append "ADD COLUMN IF NOT EXISTS " (column-definition column)))
    (('add-column column)
     (string-append "ADD COLUMN " (column-definition column)))))

(define (sql form)
  "Compile FORM, a Scheme list, into a SQL string.  Statements:

@example
(select (col-or-expr ...) (from source) clause ...)
  clause: (where e) | (order-by col asc|desc) | (limit n)
(insert-into target (col ...) (values e ...) [(on-conflict (col ...) (do-update (col e) ...))])
(insert-into target (col ...) (select ...))
(insert-into target (format Name))
(create-database [if-not-exists] name)
(create-table [if-not-exists] name (columns column ...) option ...)
  column: (name type) | (name type (codec c ...))
  option: (engine e) | (partition-by e) | (order-by col ...)
(alter-table name alteration)
  alteration: (add-column [if-not-exists] column)
@end example

A target is a table name or @code{(function (remote ...))}."
  (match form
    (('select cols ('from source) clauses ...)
     (string-append "SELECT " (expressions cols) " FROM " (target source)
                    (apply string-append (map clause clauses))))
    (('insert-into t ('format name))
     (string-append "INSERT INTO " (target t) " FORMAT " (identifier name)))
    (('insert-into t cols ('values vals ...) more ...)
     (string-append "INSERT INTO " (target t) " (" (expressions cols) ") VALUES ("
                    (expressions vals) ")" (insert-extra more)))
    (('insert-into t cols (and query ('select _ ...)))
     (string-append "INSERT INTO " (target t) " (" (expressions cols) ") " (sql query)))
    (('create-database rest ...)
     (let-values (((flags rest) (split-flags rest)))
       (match rest
         ((name)
          (string-append "CREATE DATABASE " (if-not-exists flags) (identifier name))))))
    (('create-table rest ...)
     (let-values (((flags rest) (split-flags rest)))
       (match rest
         ((name ('columns columns ...) options ...)
          (string-append "CREATE TABLE " (if-not-exists flags) (identifier name)
                         " (" (string-join (map column-definition columns) ", ") ")"
                         (apply string-append (map table-option options)))))))
    (('alter-table name a)
     (string-append "ALTER TABLE " (identifier name) " " (alteration a)))
    (_ (error "sql: unrecognized form" form))))
