#lang typed/racket
(require typed/rackunit)
;Progress Towards Goal: 100%

;Define-type for ExprC
(define-type ExprC (U numC idC stringC appC if?C lamC))
(struct Binding ([name : Symbol] [arg : Value]) #:transparent)
(struct idC ([s : Symbol]) #:transparent)
(struct stringC ([s : String]) #:transparent)
(struct if?C ([condition : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct appC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)
(struct numC ([n : Real]) #:transparent)
(struct lamC ([args : (Listof idC)] [body : ExprC]) #:transparent)


; environment related data
(define-type Env (Listof Binding))
(define mt-env '())

; extends an environment
(define (extend-env [binds : (Listof Binding)] [env : Env]) : Env
  (append binds env))


;all value types
(define-type Value (U boolV numV closV primOpV stringV voidV))
(struct boolV ([b : Boolean]) #:transparent)
(struct numV ([n : Real]) #:transparent)
(struct closV ([args : (Listof idC)] [body : ExprC] [env : Env]) #:transparent)
(struct primOpV ([op : Symbol]) #:transparent)
(struct stringV ([s : String]))
(struct voidV ([v : Void]))



;hash table for banned bindings
(define dni_table
  (hash '-> '->
        'fn 'fn
        'if 'if
        'given 'given
        '= '=
        ))

; looks up the binding of a value in the environment
(define (lookup [for : Symbol] [env : Env]) : Value
  (match env
    ['() (error 'lookup "name not found: ~e (VEBG)" for)]
    [(cons (Binding name val) r) (cond
                                   [(symbol=? for name) val]
                                   [else (lookup for r)])]))

;parse function takes in a S expression and returns an ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real?) (numC s)]
    [(? symbol?) #:when (not (hash-has-key? dni_table s)) (idC s)]
    [(? string?) (stringC s)]
    [(list 'if v t e) (if?C (parse v) (parse t) (parse e))]
    [(list 'given (list bindings ...) 'do body)
     (define parsedBindings (map (lambda ([binding : Sexp])
            (match binding
              [(list (? symbol? name) '= expr) #:when (not (hash-has-key? dni_table name)) (cons name (parse expr))]
              [other (error 'parse "Poorly formatted binding ~e (VEBG)" binding)])) bindings))

     (check-dupes (map (lambda ([binding : (cons Symbol ExprC)]) (match binding
                                                                   [(cons name expr) name])) parsedBindings))

     (define bindingNames (map (lambda ([binding : (cons Symbol ExprC)]) (match binding
                                                   [(cons name expr) (idC name)])) parsedBindings))

     (define bindingBodies (map (lambda ([binding : (cons Symbol ExprC)]) (match binding
                                                   [(cons name expr) expr])) parsedBindings))
     
     (appC (lamC bindingNames (parse body)) bindingBodies)]
    [(list 'fn (list args ...) '-> body)
     (check-dupes args)
     (lamC (map (lambda ([a : Sexp])
                  (if (symbol? a)
                      (idC a)
                      (error 'parse "Expected symbol, got ~a (VEBG)" a))) args)
           (parse body))]
    [(list fun args ...) #:when (not (hash-has-key? dni_table fun))     
     (appC (parse fun) (map parse args))]
    [other (error 'Parse "Invalid syntax: ~e (VEBG)" s)]
    ))

; interp function interprets the parsed program
(define (interp [a : ExprC] [env : Env]) : Value
  (match a
    [(numC n) (numV n)]
    [(stringC s) (stringV s)]
    [(idC s) (lookup s env)]
    [(lamC args body) (closV args body env)]
    [(if?C con then else) (define conVal (interp con env))
                          (match conVal
                            [(boolV b) (cond
                                         [(equal? conVal (boolV #t)) (interp then env)]
                                         [else (interp else env)])]
                            [other (error 'interp "condition not a boolean expression: ~e (VEBG)" conVal)])]
    [(appC fun args) (define fd (interp fun env))
                     (define evaluated-args (map (lambda ([a : ExprC]) (interp a env)) args))
                     (match fd
                       [(primOpV op) (opToMeaning op evaluated-args)]
                       [(closV param body clo-env)
                        (cond
                          [(equal? (length (closV-args fd))
                                   (length evaluated-args))
                           (define env2 (extend-env
                                         (map Binding
                                              (map idC-s (closV-args fd)) evaluated-args)
                                         clo-env))
                           (interp (closV-body fd) env2)]
                          [else (error 'interp "wrong number of arguments applied to function (VEBG)")])

                        ]
                       [other (error 'interp "poorly formatted function call ~e (VEBG)" fd)])
                     ]))


; converts a Value v to its string equivalent
(define (serialize [v : Value])
  (match v
    [(numV n) (format "~v" n)]
    [(boolV b) (cond
               [(equal? b #t) "true"]
               [else "false"])]
    [(closV a b e) "#<procedure>"]
    [(primOpV o) "#<primop>"]
    [(stringV s) (format "~e" s)]))


;opToMeaning function helper function for primOp interpretation
;opToMeaning function helper function for primOp interpretation
(define (opToMeaning [op : Symbol] [args : (Listof Value)]) : Value
(match op
  ['+
   (match args
     [(list (numV l) (numV r)) (numV (+ l r))]
     [other (error 'opToMeaning "invalid args ~e (VEBG)" op)])]

  ['*
   (match args
     [(list (numV l) (numV r)) (numV (* l r))]
     [other (error 'opToMeaning "invalid args ~e (VEBG)" op)])]

  ['-
   (match args
     [(list (numV l) (numV r)) (numV (- l r))]
     [other (error 'opToMeaning "invalid args ~e (VEBG)" op)])]

  ['/
   (match args
     [(list (numV l) (numV r))
      (cond [(equal? r 0) (error 'interp "divide by zero error ~e (VEBG)" op)]
            [else (numV (/ l r))])]
     [other (error 'opToMeaning "invalid args ~e (VEBG)" op)])]

  ['<=
   (match args
     [(list (numV l) (numV r)) (boolV (<= l r))]
     [other (error 'opToMeaning "invalid args ~e (VEBG)" op)])]

  ['equal?
   (match args
     [(list (numV l) (numV r))   (boolV (equal? l r))]
     [(list (stringV l) (stringV r)) (boolV (equal? l r))]
     [(list (boolV l) (boolV r))    (boolV (equal? l r))]
     [(list _ _)                    (boolV #f)]
     [other (error 'opToMeaning "invalid args ~e (VEBG)" op)])]

  ['substring
   (match args
     [(list (stringV s) (numV start) (numV stop))
      #:when (and (exact-nonnegative-integer? start) (exact-nonnegative-integer? stop))
      (stringV (substring s start stop))]
     [other (error 'opToMeaning "invalid args ~e (VEBG)" op)])]

  ['strlen
   (match args
     [(list (stringV s)) (numV (string-length s))]
     [other (error 'opToMeaning "invalid args ~e (VEBG)" op)])]

  ['println
   (match args
     [(list (stringV s)) (voidV (println s))]
     [other (error 'opToMeaning "invalid args ~e (VEBG)" op)])]

  ['error
   (match args
     [(list (stringV s)) (error 'user-error "~a" s)]
     [(list _)           (error 'user-error "~a" (serialize (first args)))]
     [other (error 'opToMeaning "invalid args ~e (VEBG)" op)])]

  ['read-num
   (match args
     ['()
      (print ">")
      (define inp (read-line))
      (define numVal (string->number (match inp
                                       [(? string?) inp]
                                       [other ""])))
      (numV (match numVal
              [(? real?) (real-part numVal)]
              [other (error 'opToMeaning "invalid args ~e (VEBG)" op)]))]
     [other (error 'opToMeaning "invalid args ~e (VEBG)" op)])]

  ['read-str
   (match args
     ['()
      (print ">")
      (define inp (read-line))
      (stringV (match inp
                 [(? string?) inp]
                 [other ""]))]
     [other (error 'opToMeaning "invalid args ~e (VEBG)" op)])]

  ['chain
   (match args
     [(list arg1 args ...) (chain-helper args)]
     [other (error 'opToMeaning "invalid args ~e (VEBG)" op)])]

  ['++
   (match args
     [(list args ...) (stringV (++-helper args))]
     [other (error 'opToMeaning "invalid args ~e (VEBG)" op)])]

  ['randomInt (match args
                [(list (numV max))#:when(exact-nonnegative-integer? max) (numV (random max))]
                [(list (numV min) (numV max))#:when(and (exact-nonnegative-integer? min)(exact-nonnegative-integer? max)) (numV (random min max))]
                [other (error 'opToMeaning "invalid args ~e (VEBG)" op)])]

  [other (error 'opToMeaning "invalid operation: ~e (VEBG)" op)]))

; a helper function to iterate through and return the concatentation of all strings in list
(define (++-helper [args : (Listof Value)]) : String
  (match args
    ['() ""]
    [(cons f r) (cond
                  [(stringV? f) (string-append (stringV-s f) (++-helper r))]
                  [(numV? f) (string-append (number->string (numV-n f)) (++-helper r))]
                  [else (error '++-helper "++ has poorly formatted arguments ~e (VEBG)" args)])]))

; a helper function to iterate through and return the value of the last argument of chain
(define (chain-helper [args : (Listof Value)]) : Value
  (match args
    ['() (error 'chain-helper "chain has no arguments (VEBG)" args)]
    [(cons f r) (cond
                  [(equal? r '()) f]
                  [else (chain-helper r)])]))

;helper function called in parse, checks if there are duplicate in args and raises an error if there are dupes
(define (check-dupes [args : (Listof Sexp)]) : Null
  (if (not (equal? (remove-duplicates args) args))
    (error 'check-dupes "bad syntax: duplicate args ~e (VEBG)" args) null))

;combines parser and interpreter
(define (top-interp [fun-sexps : Sexp]) : String
  (serialize (interp (parse fun-sexps) top-env)))

;all terms that belong to the topmost environment
(define top-env
  (list
   (Binding 'true (boolV #t))
   (Binding 'false (boolV #f))
   (Binding '+  (primOpV '+))
   (Binding '-  (primOpV '-))
   (Binding '*  (primOpV '*))
   (Binding '/  (primOpV '/))
   (Binding '<=  (primOpV '<=))
   (Binding 'equal?  (primOpV 'equal?))
   (Binding 'substring (primOpV 'substring))
   (Binding 'strlen (primOpV 'strlen))
   (Binding 'error (primOpV 'error))
   ))



;***************************************************TESTING**********************************************************;

; test addition primop
(check-equal? (top-interp '{+ 1 2}) "3")

; test division primop
(check-equal? (top-interp '{/ 6 2}) "3")

; check lambda
(check-equal? (top-interp '{{fn (x) -> {+ x 1}} 1}) "2")

; check less than or equal to
(check-equal? (top-interp '{if {<= 1 5} 1 5}) "1")

; check if bools are equal
(check-equal? (top-interp '{equal? true true}) "true")

; check if bools are not equal
(check-equal? (top-interp '{equal? false true}) "false")

; check if then
(check-equal? (top-interp '{{fn (func) -> {func 2}} {fn (y) -> {if {equal? y 2} {- y 3} {* y 8}}}}) "-1")

; check if else
(check-equal? (top-interp '{{fn (func) -> {func 3}} {fn (y) -> {if {equal? y 2} {- y 3} {* y 8}}}}) "24")

; check curried function 
(check-equal?
 (top-interp
  '{{fn (compose) ->
       {{fn (add1) ->
            {{fn (add2) ->
                 {add2 99}}
             {compose add1 add1}}}
        {fn (x) -> {+ x 1}}}}
    {fn (f g) ->
         {fn (x) ->
              {f {g x}}}}})
 "101")

;test strings for parser
(check-equal? (parse "hello") (stringC "hello"))

;test local variables (given-do format) for parser
(check-equal? (parse '{given {[z = {+ 9 14}] [y = 98]} do
                             {+ z y}}) (appC (lamC (list (idC 'z)
                                                         (idC 'y)) (appC (idC '+) (list (idC 'z) (idC 'y))))
                                             (list (appC (idC '+) (list (numC 9) (numC 14))) (numC 98))))
; check parse
(check-equal? (parse '{{fn (z y) -> {+ z y}} {+ 9 14} 98})
              (appC (lamC (list (idC 'z)
                                (idC 'y)) (appC (idC '+) (list (idC 'z) (idC 'y))))
                    (list (appC (idC '+) (list (numC 9) (numC 14))) (numC 98))))
; check parse failure
(check-exn (regexp (regexp-quote "parse: Poorly formatted binding '(99 = (+ 9 14)) (VEBG)"))
           (lambda () (parse '{given {[99 = {+ 9 14}] [y = 98]} do {+ z y}})))

;test local variables (given-do format) duplicate variable errors
(check-exn (regexp (regexp-quote "check-dupes: bad syntax: duplicate args '(x x) (VEBG)"))
           (lambda () (parse '{given {[x = 3] [x = 4]} do {+ x x}})))

;test invalid function call
(check-exn (regexp (regexp-quote "interp: poorly formatted function call #<stringV> (VEBG)"))
           (lambda () (top-interp '{"hello"})))

;test invalid parser call
(check-exn (regexp (regexp-quote "Parse: Invalid syntax: '() (VEBG)"))
           (lambda () (parse '{})))

;test out of scope for interp
(check-exn (regexp (regexp-quote "lookup: name not found: 'z (VEBG)"))
           (lambda () (top-interp '{+ z 4})))

;test if statement with not enough args or too many args
(check-exn (regexp (regexp-quote "Parse: Invalid syntax: '(if true 3) (VEBG)"))
           (lambda () (parse '{if true 3})))

; check too many args in if
(check-exn (regexp (regexp-quote "Parse: Invalid syntax: '(if true 3 4 5) (VEBG)"))
           (lambda () (parse '{if true 3 4 5})))

;test if statement with invalid condition
(check-exn (regexp (regexp-quote "interp: condition not a boolean expression: (numV 2) (VEBG)"))
           (lambda () (top-interp '{if 2 3 4})))

; check variable shadowing
(check-equal? (top-interp '{given {[x = {+ 1 1}] [y = 2]} do {given {[x = 8]} do {+ x y}}}) "10")

; check duplicate var usage in same level scope
(check-exn (regexp (regexp-quote "check-dupes: bad syntax: duplicate args '(x x) (VEBG)"))
           (lambda () (top-interp '{given {[x = 5] [x = 2]} do {+ x x}})))

; check too many args for opToMeaning
(check-exn (regexp (regexp-quote "opToMeaning: invalid args '+ (VEBG)"))
           (lambda () (top-interp '{+ 5 4 6})))

; check too many args to lambda function
(check-exn (regexp (regexp-quote "interp: wrong number of arguments applied to function (VEBG)"))
           (lambda () (top-interp '{{fn (x) -> {+ x 1}} 1 2})))

; check serialized lambda output
(check-equal? (top-interp '{fn (x) -> {+ x 1}}) "#<procedure>")
 
; check serialized primop output
(check-equal? (top-interp '+) "#<primop>")

; check serialize string
(check-equal? (top-interp '"hello") "\"hello\"")

; check divide by zero error
(check-exn (regexp (regexp-quote "interp: divide by zero error '/ (VEBG)"))
           (lambda () (top-interp '{/ 5 0})))

; check string comapare true
(check-equal? (top-interp '{equal? "hello" "hello"}) "true")

; check string compare false
(check-equal? (top-interp '{equal? "hello" "world"}) "false")

; check wrong op with string args
(check-exn (regexp (regexp-quote "opToMeaning: invalid operation: '+ (VEBG)"))
           (lambda () (top-interp '{+ "hello" "world"})))

; check wrong op with bool args
(check-exn (regexp (regexp-quote "opToMeaning: invalid operation: '+ (VEBG)"))
           (lambda () (top-interp '{+ true false})))

; check wrong op with different types
(check-exn (regexp (regexp-quote "opToMeaning: invalid operation: '+ (VEBG)"))
           (lambda () (top-interp '{+ "hello" true})))

; check different type equivalency
(check-equal? (top-interp '{equal? "hello" true}) "false")

; check lambda comapare equivalency 
(check-equal? (top-interp '{equal? {fn () -> 2} {fn () -> 2}}) "false")

; check wrong op with num args
(check-exn (regexp (regexp-quote "opToMeaning: invalid operation: 'strlen (VEBG)"))
           (lambda () (top-interp '{strlen 5 4})))

; check strlen function
(check-equal? (top-interp '{strlen "hello"}) "5")

; check substring function
(check-equal? (top-interp '{substring "hello" 0 2}) "\"he\"")

; check wrong op with string num num args 
(check-exn (regexp (regexp-quote "opToMeaning: invalid operation: '+ (VEBG)"))
           (lambda () (top-interp '{+ "hello" 0 2})))

; check error function
(check-exn (regexp (regexp-quote "user-error: bad"))
           (lambda () (top-interp '{if {equal? "bad" "bad"} {error "bad"} "bad"})))

; check error number input
(check-exn (regexp (regexp-quote "user-error: 5"))
           (lambda () (top-interp '{if {equal? 5 5} {error 5} 5})))

; check wrong op with string arg
(check-exn (regexp (regexp-quote "opToMeaning: invalid operation: '+ (VEBG)"))
           (lambda () (top-interp '{+ "hello"})))

; check that nums can't be args
(check-exn (regexp (regexp-quote "parse: Expected symbol, got 3 (VEBG)"))
           (lambda () (parse '(fn (3 4 5) -> 6))))

; check no body in do statement
(check-exn (regexp (regexp-quote "Parse: Invalid syntax: '(given ((x = 5)) do) (VEBG)"))
           (lambda () (parse '(given ((x = 5)) do))))

; check that = is invalid name
(check-exn (regexp (regexp-quote "Parse: Invalid syntax: '= (VEBG)"))
           (lambda () (parse '(if = 0 1))))

; check that given is an invalid name
(check-exn (regexp (regexp-quote "parse: Poorly formatted binding '(given = "") (VEBG)"))
           (lambda () (parse '(given ((given = "")) do "World"))))

; check that decimals can't be passed to substring
(check-exn (regexp (regexp-quote "opToMeaning: invalid operation: 'substring (VEBG)"))
           (lambda () (top-interp '(substring "hello world" 1.2 3))))


(top-interp '{given {[f = {fn (g n) -> {if {equal? n 0} 0 {+ 2 {g g {- n 1}}}}}]} do {f f 7}})

     