#lang typed/racket

(require typed/rackunit)

; Full project implemented

(struct NumC ([n : Real]) #:transparent)
(struct StrC ([s : String]) #:transparent)
(struct IdC ([name : Symbol]) #:transparent)
(struct IfC ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct LamC ([params : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct AppC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)

(define-type ExprC (U NumC StrC IdC IfC LamC AppC))

(struct NumV ([n : Real]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
(struct StrV ([s : String]) #:transparent)
(struct CloV ([params : (Listof Symbol)]
              [body : ExprC]
              [env : Environment]) #:transparent)
(struct PrimV ([name : Symbol]) #:transparent)

(define-type Value (U NumV BoolV StrV CloV PrimV))

(struct Binding ([name : Symbol] [val : Value]) #:transparent)

(define-type Environment (Listof Binding))

; reserved-id? determines whether a symbol is reserved syntax
(: reserved-id? (Symbol -> Boolean))
(define (reserved-id? s)
  (not (false? (member s '(if = given fn -> do)))))

(check-equal? (reserved-id? 'if) #t)
(check-equal? (reserved-id? 'x) #f)

; duplicate-symbols? determines whether a list of symbols contains duplicates
(: duplicate-symbols? ((Listof Symbol) -> Boolean))
(define (duplicate-symbols? xs)
  (cond
    [(empty? xs) #f]
    [(member (first xs) (rest xs)) #t]
    [else (duplicate-symbols? (rest xs))]))

(check-equal? (duplicate-symbols? '(x y z)) #f)
(check-equal? (duplicate-symbols? '(x y x)) #t)

; lookup finds the value bound to a symbol in an environment
(: lookup (Symbol Environment -> Value))
(define (lookup name env)
  (cond
    [(empty? env)
     (error 'lookup "VEBG: unbound identifier: ~v" name)]
    [(symbol=? name (Binding-name (first env)))
     (Binding-val (first env))]
    [else
     (lookup name (rest env))]))

(check-equal? (lookup 'x (list (Binding 'x (NumV 10)))) (NumV 10))
(check-exn #rx"VEBG" (lambda () (lookup 'x empty)))

; extend-env binds parameters to argument values and extends an environment
(: extend-env ((Listof Symbol) (Listof Value) Environment -> Environment))
(define (extend-env params vals env)
  (cond
    [(not (= (length params) (length vals)))
     (error 'extend-env
            "VEBG: wrong number of arguments; expected ~v but got ~v"
            (length params)
            (length vals))]
    [else
     (append
      (map (lambda ([p : Symbol] [v : Value]) : Binding
             (Binding p v))
           params
           vals)
      env)]))

(check-equal?
 (lookup 'a (extend-env '(a b) (list (NumV 1) (NumV 2)) empty))
 (NumV 1))

(check-exn
 #rx"VEBG"
 (lambda () (extend-env '(a b) (list (NumV 1)) empty)))

; serialize converts a VEBG4 value into its printed string form
(: serialize (Value -> String))
(define (serialize v)
  (match v
    [(NumV n) (~v n)]
    [(BoolV #t) "true"]
    [(BoolV #f) "false"]
    [(StrV s) (~v s)]
    [(CloV _ _ _) "#<procedure>"]
    [(PrimV _) "#<primop>"]))

(check-equal? (serialize (CloV (list 'x) (IdC 'x) empty)) "#<procedure>")
(check-equal? (serialize (NumV 34)) "34")
(check-equal? (serialize (BoolV #t)) "true")
(check-equal? (serialize (BoolV #f)) "false")
(check-equal? (serialize (StrV "hello")) "\"hello\"")
(check-equal? (serialize (PrimV '+)) "#<primop>")

; expect-num extracts a real number from a value or signals a type error
(: expect-num (Value Symbol -> Real))
(define (expect-num v prim-name)
  (match v
    [(NumV n) n]
    [else
     (error 'primop
            "VEBG: primitive ~v expected a number, got ~v"
            prim-name
            (serialize v))]))

(check-equal? (expect-num (NumV 3) '+) 3)
(check-exn #rx"VEBG" (lambda () (expect-num (BoolV #t) '+)))

; expect-str extracts a string from a value or signals a type error
(: expect-str (Value Symbol -> String))
(define (expect-str v prim-name)
  (match v
    [(StrV s) s]
    [else
     (error 'primop
            "VEBG: primitive ~v expected a string, got ~v"
            prim-name
            (serialize v))]))

(check-equal? (expect-str (StrV "abc") 'strlen) "abc")
(check-exn #rx"VEBG" (lambda () (expect-str (NumV 3) 'strlen)))

; real->natural-index converts a real number to a natural string index
(: real->natural-index (Real Symbol -> Natural))
(define (real->natural-index n prim-name)
  (cond
    [(and (integer? n) (>= n 0))
     (assert (inexact->exact n) exact-nonnegative-integer?)]
    [else
     (error 'primop
            "VEBG: primitive ~v expected a natural number index, got ~v"
            prim-name
            n)]))

(check-equal? (real->natural-index 3 'substring) 3)
(check-exn #rx"VEBG" (lambda () (real->natural-index -1 'substring)))
(check-exn #rx"VEBG" (lambda () (real->natural-index 1.5 'substring)))

; primitive-equal? implements VEBG4 equal? on non-function values
(: primitive-equal? (Value Value -> Boolean))
(define (primitive-equal? a b)
  (match* (a b)
    [((NumV x) (NumV y)) (= x y)]
    [((BoolV x) (BoolV y)) (equal? x y)]
    [((StrV x) (StrV y)) (equal? x y)]
    [(_ _) #f]))

(check-equal? (primitive-equal? (NumV 3) (NumV 3)) #t)
(check-equal? (primitive-equal? (StrV "a") (StrV "a")) #t)
(check-equal? (primitive-equal? (StrV "a") (NumV 3)) #f)
(check-equal? (primitive-equal? (PrimV '+) (PrimV '+)) #f)

; check-arity ensures a primitive received the expected number of arguments
(: check-arity (Symbol (Listof Value) Natural -> Void))
(define (check-arity name args expected)
  (unless (= (length args) expected)
    (error 'primop
           "VEBG: primitive ~v expected ~v arguments, got ~v"
           name
           expected
           (length args))))

(check-equal? (check-arity '+ (list (NumV 1) (NumV 2)) 2) (void))
(check-exn #rx"VEBG" (lambda () (check-arity '+ (list (NumV 1)) 2)))

; apply-prim applies a VEBG4 primitive operator to already-evaluated values
(: apply-prim (Symbol (Listof Value) -> Value))
(define (apply-prim name args)
  (case name
    [(+)
     (check-arity name args 2)
     (NumV (+ (expect-num (first args) name)
              (expect-num (second args) name)))]
    [(-)
     (check-arity name args 2)
     (NumV (- (expect-num (first args) name)
              (expect-num (second args) name)))]
    [(*)
     (check-arity name args 2)
     (NumV (* (expect-num (first args) name)
              (expect-num (second args) name)))]
    [(/)
     (check-arity name args 2)
     (define numerator : Real (expect-num (first args) name))
     (define denominator : Real (expect-num (second args) name))
     (if (= denominator 0)
         (error 'primop "VEBG: division by zero in /")
         (NumV (/ numerator denominator)))]
    [(<=)
     (check-arity name args 2)
     (BoolV (<= (expect-num (first args) name)
                (expect-num (second args) name)))]
    [(substring)
     (check-arity name args 3)
     (define s : String (expect-str (first args) name))
     (define start : Natural
       (real->natural-index (expect-num (second args) name) name))
     (define stop : Natural
       (real->natural-index (expect-num (third args) name) name))
     (cond
       [(> start (string-length s))
        (error 'primop
               "VEBG: substring start index ~v out of range for string ~v"
               start
               s)]
       [(> stop (string-length s))
        (error 'primop
               "VEBG: substring stop index ~v out of range for string ~v"
               stop
               s)]
       [(< stop start)
        (error 'primop
               "VEBG: substring stop index ~v is before start index ~v"
               stop
               start)]
       [else
        (StrV (substring s start stop))])]
    [(strlen)
     (check-arity name args 1)
     (NumV (string-length (expect-str (first args) name)))]
    [(equal?)
     (check-arity name args 2)
     (BoolV (primitive-equal? (first args) (second args)))]
    [(error)
     (check-arity name args 1)
     (error 'user-error
            "VEBG: user-error: ~a"
            (serialize (first args)))]
    [else
     (error 'primop "VEBG: unknown primitive operator: ~v" name)]))


(check-equal? (apply-prim '+ (list (NumV 2) (NumV 3))) (NumV 5))
(check-equal? (apply-prim '- (list (NumV 10) (NumV 4))) (NumV 6))
(check-equal? (apply-prim '* (list (NumV 5) (NumV 6))) (NumV 30))
(check-equal? (apply-prim '/ (list (NumV 10) (NumV 2))) (NumV 5))
(check-equal? (apply-prim '<= (list (NumV 2) (NumV 3))) (BoolV #t))
(check-equal? (apply-prim 'strlen (list (StrV "abc"))) (NumV 3))
(check-equal? (apply-prim 'substring (list (StrV "abcdef") (NumV 1) (NumV 4))) (StrV "bcd"))
(check-equal? (apply-prim 'equal? (list (NumV 3) (NumV 3))) (BoolV #t))
(check-equal? (apply-prim 'equal? (list (PrimV '+) (PrimV '+))) (BoolV #f))
(check-exn #rx"VEBG" (lambda () (apply-prim '/ (list (NumV 1) (NumV 0)))))
(check-exn #rx"VEBG" (lambda () (apply-prim '+ (list (StrV "x") (NumV 1)))))
(check-exn #rx"VEBG" (lambda () (apply-prim 'error (list (StrV "bad")))))
(check-exn
 #rx"VEBG: substring start index"
 (lambda ()
   (apply-prim 'substring
               (list (StrV "abc") (NumV 4) (NumV 4)))))
(check-exn
 #rx"VEBG: unknown primitive operator"
 (lambda ()
   (apply-prim 'not-a-prim
               (list (NumV 1)))))

(: top-env Environment)
(define top-env
  (list
   (Binding '+ (PrimV '+))
   (Binding '- (PrimV '-))
   (Binding '* (PrimV '*))
   (Binding '/ (PrimV '/))
   (Binding '<= (PrimV '<=))
   (Binding 'substring (PrimV 'substring))
   (Binding 'strlen (PrimV 'strlen))
   (Binding 'equal? (PrimV 'equal?))
   (Binding 'error (PrimV 'error))
   (Binding 'true (BoolV #t))
   (Binding 'false (BoolV #f))))

(check-equal? (lookup 'true top-env) (BoolV #t))
(check-equal? (lookup '+ top-env) (PrimV '+))

; interp evaluates a VEBG4 expression in the given environment
(: interp (ExprC Environment -> Value))
(define (interp e env)
  (match e
    [(NumC n) (NumV n)]
    [(StrC s) (StrV s)]
    [(IdC name) (lookup name env)]
    [(IfC test then else)
     (match (interp test env)
       [(BoolV #t) (interp then env)]
       [(BoolV #f) (interp else env)]
       [other
        (error 'interp
               "VEBG: if test did not evaluate to a boolean; got ~v in ~v"
               (serialize other)
               e)])]
    [(LamC params body)
     (CloV params body env)]
    [(AppC fun args)
     (define fun-val : Value (interp fun env))
     (define arg-vals : (Listof Value)
       (map (lambda ([a : ExprC]) : Value
              (interp a env))
            args))
     (match fun-val
       [(CloV params body closure-env)
        (interp body (extend-env params arg-vals closure-env))]
       [(PrimV name)
        (apply-prim name arg-vals)]
       [other
        (error 'interp
               "VEBG: attempted to apply a non-function value ~v in expression ~v"
               (serialize other)
               e)])]))

(check-equal? (interp (NumC 7) top-env) (NumV 7))
(check-equal? (interp (StrC "hi") top-env) (StrV "hi"))
(check-equal? (interp (IdC 'true) top-env) (BoolV #t))
(check-equal?
 (interp (IfC (IdC 'true) (NumC 1) (NumC 2)) top-env)
 (NumV 1))
(check-equal?
 (interp (AppC (IdC '+) (list (NumC 2) (NumC 5))) top-env)
 (NumV 7))
(check-equal?
 (interp (AppC (LamC (list 'x) (AppC (IdC '+) (list (IdC 'x) (NumC 1))))
               (list (NumC 9)))
         top-env)
 (NumV 10))
(check-exn
 #rx"VEBG"
 (lambda () (interp (IfC (NumC 0) (NumC 1) (NumC 2)) top-env)))
(check-exn
 #rx"VEBG"
 (lambda () (interp (AppC (NumC 1) (list (NumC 2))) top-env)))

; parse-params parses a parenthesized parameter list
(: parse-params (Sexp -> (Listof Symbol)))
(define (parse-params s)
  (cond
    [(list? s)
     (define params : (Listof Sexp) (cast s (Listof Sexp)))
     (unless (andmap symbol? params)
       (error 'parse "VEBG: function parameters must all be identifiers: ~v" s))
     (define syms : (Listof Symbol) (cast params (Listof Symbol)))
     (for ([p : Symbol syms])
       (when (reserved-id? p)
         (error 'parse "VEBG: reserved word cannot be used as parameter: ~v" p)))
     (when (duplicate-symbols? syms)
       (error 'parse "VEBG: duplicate parameter name in function: ~v" s))
     syms]
    [else
     (error 'parse "VEBG: expected function parameter list, got ~v" s)]))


(check-equal? (parse-params '(x y z)) '(x y z))
(check-exn #rx"VEBG"
 (lambda ()
   (parse-params '(x x))))
(check-exn #rx"VEBG"
 (lambda ()
   (parse-params '(if))))
(check-exn #rx"VEBG: function parameters must all be identifiers"
 (lambda ()
   (parse-params '(x 3 y))))
(check-exn #rx"VEBG: expected function parameter list"
 (lambda ()
   (parse-params 'x)))

; binding-name extracts the variable name from a parsed given binding
(: binding-name ((Pairof Symbol ExprC) -> Symbol))
(define (binding-name b)
  (car b))

(check-equal? (binding-name (cons 'x (NumC 1))) 'x)

; binding-rhs extracts the right-hand side expression from a parsed given binding
(: binding-rhs ((Pairof Symbol ExprC) -> ExprC))
(define (binding-rhs b)
  (cdr b))

(check-equal? (binding-rhs (cons 'x (NumC 1))) (NumC 1))

; parse converts a VEBG4 s-expression into an ExprC
(: parse (Sexp -> ExprC))
(define (parse s)
  (match s
    [(? real? n)
     (NumC n)]

    [(? string? str)
     (StrC str)]

    [(? symbol? id)
     (if (reserved-id? id)
         (error 'parse "VEBG: reserved word cannot appear as identifier: ~v" id)
         (IdC id))]

    [(list 'if test then else)
     (IfC (parse test) (parse then) (parse else))]

    [(list 'if _ _)
     (error 'parse "VEBG: if expression must have test, then, and else clauses: ~v" s)]

    [(list 'if _ _ _ _ ...)
     (error 'parse "VEBG: if expression has too many clauses: ~v" s)]

    [(list 'fn params '-> body)
     (LamC (parse-params params) (parse body))]

    [(list 'fn _ ...)
     (error 'parse "VEBG: malformed function expression: ~v" s)]

    [(list 'given bindings 'do body)
     (cond
       [(list? bindings)
        (define binding-sexps : (Listof Sexp)
          (cast bindings (Listof Sexp)))

        (define parsed-bindings : (Listof (Pairof Symbol ExprC))
          (map
           (lambda ([b : Sexp]) : (Pairof Symbol ExprC)
             (match b
               [(list (? symbol? id) '= rhs)
                (when (reserved-id? id)
                  (error 'parse
                         "VEBG: reserved word cannot be used as binding name: ~v"
                         id))
                (cons id (parse rhs))]
               [else
                (error 'parse "VEBG: invalid given binding: ~v" b)]))
           binding-sexps))

        (define names : (Listof Symbol)
          (map binding-name parsed-bindings))

        (define rhss : (Listof ExprC)
          (map binding-rhs parsed-bindings))

        (when (duplicate-symbols? names)
          (error 'parse "VEBG: duplicate variable name in given: ~v" s))

        ; Desugar:
        ; {given {[x = rhs] ...} do body}
        ; =>
        ; {{fn (x ...) -> body} rhs ...}
        (AppC (LamC names (parse body)) rhss)]
       [else
        (error 'parse "VEBG: expected given binding list, got ~v" bindings)])]

    [(list 'given _ ...)
     (error 'parse "VEBG: malformed given expression: ~v" s)]

    [(list fun args ...)
     (AppC (parse fun)
           (map (lambda ([a : Sexp]) : ExprC
                  (parse a))
                (cast args (Listof Sexp))))]

    [else
     (error 'parse "VEBG: invalid expression: ~v" s)]))

(check-equal? (parse 3) (NumC 3))
(check-equal? (parse "hello") (StrC "hello"))
(check-equal? (parse 'x) (IdC 'x))

(check-exn #rx"VEBG"
 (lambda ()
   (parse 'if)))

(check-equal?
 (parse '{if true 1 2})
 (IfC (IdC 'true) (NumC 1) (NumC 2)))

(check-equal?
 (parse '{fn (x y) -> {+ x y}})
 (LamC
  '(x y)
  (AppC (IdC '+) (list (IdC 'x) (IdC 'y)))))
(check-equal?
 (parse '{+ 1 2})
 (AppC (IdC '+) (list (NumC 1) (NumC 2))))
(check-equal?
 (parse '{given {[z = {+ 9 14}]
                 [y = 98]}
           do
           {+ z y}})
 (AppC
  (LamC
   '(z y)
   (AppC (IdC '+) (list (IdC 'z) (IdC 'y))))
  (list
   (AppC (IdC '+) (list (NumC 9) (NumC 14)))
   (NumC 98))))

(check-exn #rx"VEBG"
 (lambda ()
   (parse '{if true 1})))
(check-exn #rx"VEBG: if expression has too many clauses"
 (lambda ()
   (parse '{if true 1 2 3})))
(check-exn #rx"VEBG"
 (lambda ()
   (parse '{fn (x x) -> x})))
(check-exn #rx"VEBG: malformed function expression"
 (lambda ()
   (parse '{fn (x) x})))
(check-exn #rx"VEBG"
 (lambda ()
   (parse '{given {[x = 1] [x = 2]} do x})))
(check-exn #rx"VEBG"
 (lambda ()
   (parse '{given {[if = 1]} do if})))
(check-exn #rx"VEBG"
 (lambda ()
   (parse '{given {x = 1} do x})))
(check-exn #rx"VEBG: expected given binding list"
 (lambda ()
   (parse '{given x do x})))
(check-exn #rx"VEBG: malformed given expression"
 (lambda ()
   (parse '{given {[x = 1]} x})))
(check-exn #rx"VEBG: invalid expression"
 (lambda ()
   (parse #t)))

; top-interp parses, evaluates, and serializes a VEBG4 program
(: top-interp (Sexp -> String))
(define (top-interp s)
  (serialize (interp (parse s) top-env)))

(check-equal? (top-interp '34) "34")
(check-equal? (top-interp '"hello") "\"hello\"")
(check-equal? (top-interp 'true) "true")
(check-equal? (top-interp 'false) "false")

(check-equal? (top-interp '{+ 1 2}) "3")
(check-equal? (top-interp '{- 10 3}) "7")
(check-equal? (top-interp '{* 4 5}) "20")
(check-equal? (top-interp '{/ 20 4}) "5")
(check-equal? (top-interp '{<= 2 3}) "true")
(check-equal? (top-interp '{<= 3 2}) "false")

(check-equal? (top-interp '{strlen "abcde"}) "5")
(check-equal? (top-interp '{substring "abcdef" 1 4}) "\"bcd\"")
(check-equal? (top-interp '{equal? 3 3}) "true")
(check-equal? (top-interp '{equal? 3 4}) "false")
(check-equal? (top-interp '{equal? "hi" "hi"}) "true")
(check-equal? (top-interp '{equal? true false}) "false")
(check-equal? (top-interp '{equal? + +}) "false")

(check-equal?
 (top-interp '{if true 1 2}) "1")

(check-equal?
 (top-interp '{if {<= 10 5} 1 2}) "2")

(check-equal?
 (top-interp '{{fn (x) -> {+ x 1}} 41}) "42")

(check-equal?
 (top-interp '{{fn (f x) -> {f x}}
               {fn (y) -> {* y y}}
               9}) "81")

(check-equal?
 (top-interp '{given {[z = {+ 9 14}]
                      [y = 98]}
                do
                {+ z y}}) "121")

(check-equal?
 (top-interp '{given {[+ = {fn (x y) -> {- x y}}]}
                do
                {+ 10 3}}) "7")

(check-equal?
 (top-interp '{given {[x = 10]}
                do
                {given {[x = 20]}
                  do
                  x}}) "20")

(check-equal?
 (top-interp '{given {[x = 10]}
                do
                {{fn (y) -> {+ x y}} 5}}) "15")

(check-exn #rx"VEBG" (lambda () (top-interp '{+ 1})))
(check-exn #rx"VEBG" (lambda () (top-interp '{+ 1 "bad"})))
(check-exn #rx"VEBG" (lambda () (top-interp '{/ 1 0})))
(check-exn #rx"VEBG" (lambda () (top-interp '{if 0 1 2})))
(check-exn #rx"VEBG" (lambda () (top-interp '{1 2 3})))
(check-exn #rx"VEBG" (lambda () (top-interp '{{fn (x) -> x} 1 2})))
(check-exn #rx"VEBG" (lambda () (top-interp '{substring "abc" 2 1})))
(check-exn #rx"VEBG" (lambda () (top-interp '{substring "abc" 0 10})))
(check-exn #rx"VEBG" (lambda () (top-interp '{error "oops"})))