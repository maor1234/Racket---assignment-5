#lang pl 


   (define-type BIT = (U 0 1))
   (define-type Bit-List = (Listof BIT))


   (define-type
    RegE
    [Reg Bit-List]
    [And RegE RegE]
    [Or RegE RegE]
    [Shl RegE]
    [Id Symbol]
    [With Symbol RegE RegE]
    [Bool Boolean]
    [Geq RegE RegE]
    [Maj RegE]
    [If RegE RegE RegE]
    [Fun Symbol RegE]
    [Call RegE RegE])
   (: list->bit-list : (Listof Any) -> Bit-List)
   (define (list->bit-list lst)
     (cond
      [(null? lst) null]
      [(eq? (first lst) 1) (cons 1 (list->bit-list (rest lst)))]
      [else (cons 0 (list->bit-list (rest lst)))]))



   (: parse-sexprFROL : Sexpr -> RegE)
   (define (parse-sexprFROL sexpr)
     (match
      sexpr
      [(list 'reg-len '= (number: n) body)
       (if (<= n 0)
         (error 'parse-sexprFROL "Register length must be at least 1")
         (parse-sexpr-RegL body n))]
      [else (error 'parse-sexprFROL "bad syntax in ~s" sexpr)]))
   (: parse-sexpr-RegL : Sexpr Number -> RegE)
   (define (parse-sexpr-RegL sexpr reg-len)
     (match
      sexpr
      [(list (and a (or 1 0)) ...)
       (if (eq? (length a) reg-len)
         (Reg (list->bit-list a))
         (error 'parse-sexprFROL "wrong number of bits in ~s" a))]
      [(list 'and sexpr1 sexpr2)
       (And
        (parse-sexpr-RegL sexpr1 reg-len)
        (parse-sexpr-RegL sexpr2 reg-len))]
      [(list 'or sexpr1 sexpr2)
       (Or (parse-sexpr-RegL sexpr1 reg-len) (parse-sexpr-RegL sexpr2 reg-len))]
      [(list 'shl sexpr1) (Shl (parse-sexpr-RegL sexpr1 reg-len))]
      ['false (Bool #f)]
      ['true (Bool #t)]
      [(symbol: name) (Id name)]
      [(list 'with (list (symbol: name) named) body)
       (With
        name
        (parse-sexpr-RegL named reg-len)
        (parse-sexpr-RegL body reg-len))]
      [(list 'geq? sexpr1 sexpr2)
       (Geq
        (parse-sexpr-RegL sexpr1 reg-len)
        (parse-sexpr-RegL sexpr2 reg-len))]
      [(list 'maj? sexpr1) (Maj (parse-sexpr-RegL sexpr1 reg-len))]
      [(list 'if sexpr1 sexpr2 sexpr3)
       (If
        (parse-sexpr-RegL sexpr1 reg-len)
        (parse-sexpr-RegL sexpr2 reg-len)
        (parse-sexpr-RegL sexpr3 reg-len))]
      [(cons 'fun x)
       (match
        sexpr
        [(list 'fun (list (symbol: var)) body-exp)
         (Fun var (parse-sexpr-RegL body-exp reg-len))]
        [else (error 'parse-sexpr-RegL "bad `fun' syntax in ~s" sexpr)])]
      [(list 'call fun args)
       (Call (parse-sexpr-RegL fun reg-len) (parse-sexpr-RegL args reg-len))]
      [else (error 'parse-sexprFROL "bad syntax in ~s" sexpr)]))
   (: parse : String -> RegE)
   (define (parse str) (parse-sexprFROL (string->sexpr str)))


   (test (parse-sexpr-RegL (string->sexpr "{1 1 1 1}") 4) => (Reg '(1 1 1 1)))
   (test (parse-sexpr-RegL (string->sexpr "{0 0 0}") 3) => (Reg '(0 0 0)))
   (test (parse "{ reg-len =  4  {1 0 0 0}}") => (Reg '(1 0 0 0)))  
   (test (parse "{ reg-len =  1  {1}}") => (Reg '(1)))



   (define-type ENV [EmptyEnv] [Extend Symbol RES ENV])
   (define-type RES
    [RegV Bit-List]
    [boolean-res Boolean]
    [H-Fun Symbol RegE ENV])


   (: lookup : Symbol ENV -> RES)
   (define (lookup x env)
     (cases
      env
      [(EmptyEnv) (error 'lookup "no binding for ~s" x)]
      [(Extend id val rest-env) (if (eq? id x) val (lookup x rest-env))]))




   (: eval : RegE ENV -> RES)
   (define (eval expr env)
     (cases
      expr
      [(Reg reg) (RegV reg)]
      [(And l r) (reg-arith-op bit-and (eval l env) (eval r env))]
      [(Or l r) (reg-arith-op bit-or (eval l env) (eval r env))]
      [(Shl l) (RegV (shift-left (RegV->bit-list (eval l env))))]
      [(Id name) (lookup name env)]
      [(With bound-id named-expr bound-body)
       (eval (Call (Fun bound-id bound-body) named-expr) env)]
      [(Fun bound-id bound-body) (H-Fun bound-id bound-body env)]
      [(Bool b) (boolean-res b)]
      [(Geq l r)
       (boolean-res
        (geq-bitlists?
         (RegV->bit-list (eval l env))
         (RegV->bit-list (eval r env))))]
      [(Maj l) (boolean-res (majority? (RegV->bit-list (eval l env))))]
      [(If b l r)
       (if (cases (eval b env) [(boolean-res b) b] [else #t])
         (eval l env)
         (eval r env))]
      [(Call fun-expression arg-expression)
       (let ([fval (eval fun-expression env)])
         (cases
          fval
          [(H-Fun bound-id bound-body f-env)
           (eval bound-body (Extend bound-id (eval arg-expression env) f-env))]
          [else (error 'eval "`call' expects a function, got: ~s" fval)]))]))
   (: RegV->bit-list : RES -> Bit-List)
   (define (RegV->bit-list res)
     (cases
      res
      [(RegV bl) bl]
      [else
       (error
        RegV->bit-list
        "we have to return a Bit-list so we get here an error")]))
   (test (RegV->bit-list (RegV '(0 0))) => '(0 0))
   (test (RegV->bit-list (RegV '(0 1))) => '(0 1))
   (test (RegV->bit-list (RegV '(1 0))) => '(1 0))
   (test (RegV->bit-list (RegV '(1 1))) => '(1 1))
   (test (RegV->bit-list (RegV '(1 1 1 0))) => '(1 1 1 0))
   (: bit-and : BIT BIT -> BIT)
   (define (bit-and a b) (if (and (= a 1) (= b 1)) 1 0))
   (test (bit-and 0 0) => 0)
   (test (bit-and 0 1) => 0)
   (test (bit-and 1 0) => 0)
   (test (bit-and 1 1) => 1)
   (: bit-or : BIT BIT -> BIT)
   (define (bit-or a b) (if (or (= a 1) (= b 1)) 1 0))
   (test (bit-or 0 0) => 0)
   (test (bit-or 0 1) => 1)
   (test (bit-or 1 0) => 1)
   (test (bit-or 1 1) => 1)
   (: reg-arith-op : (BIT BIT -> BIT) RES RES -> RES)
   (define (reg-arith-op op reg1 reg2)
     (: bit-arith-op : Bit-List Bit-List -> Bit-List)
     (define (bit-arith-op bl1 bl2)
       (cond
        [(and (null? bl1) (null? bl2)) '()]
        [(or (null? bl1) (null? bl2)) (error 'bit-arith-op "different length")]
        [else
         (cons
          (op (first bl1) (first bl2))
          (bit-arith-op (rest bl1) (rest bl2)))]))
     (RegV (bit-arith-op (RegV->bit-list reg1) (RegV->bit-list reg2))))
   (test (reg-arith-op bit-and (RegV '(0 0)) (RegV '(0 0))) => (RegV '(0 0)))
   (test
    (reg-arith-op bit-and (RegV '(0 1 1 1)) (RegV '(1 0 1 1)))
    =>
    (RegV '(0 0 1 1)))
   (test
    (reg-arith-op bit-and (RegV '(1 1 1 1 1)) (RegV '(0 0 0 0 0)))
    =>
    (RegV '(0 0 0 0 0)))
   (test
    (reg-arith-op bit-and (RegV '(1)) (RegV '(0 0)))
    =error>
    "bit-arith-op: different length")
   (test
    (reg-arith-op bit-and (RegV '(1 1)) (RegV '(0 0 0)))
    =error>
    "bit-arith-op: different length")
   (test (reg-arith-op bit-or (RegV '(1)) (RegV '(0))) => (RegV '(1)))
   (test (reg-arith-op bit-or (RegV '(0 0)) (RegV '(0 0))) => (RegV '(0 0)))
   (test
    (reg-arith-op bit-or (RegV '(0 1 1 1)) (RegV '(1 0 1 1)))
    =>
    (RegV '(1 1 1 1)))
   (test
    (reg-arith-op bit-or (RegV '(1)) (RegV '(0 0)))
    =error>
    "bit-arith-op: different length")
   (test
    (reg-arith-op bit-or (RegV '(1 1)) (RegV '(0 0 0)))
    =error>
    "bit-arith-op: different length")



   (: majority? : Bit-List -> Boolean)
   (define (majority? bl)
     (: mag-help : Bit-List Integer Integer -> Boolean)
     (define (mag-help bl n1 n2)
       (cond
        [(null? bl) (if (or (> n1 n2) (= n1 n2)) true false)]
        [else
         (if (eq? (first bl) 1)
           (mag-help (rest bl) (+ n1 1) n2)
           (mag-help (rest bl) n1 (+ n2 1)))]))
     (mag-help bl 0 0))
   (test (majority? '(1)) => true)
   (test (majority? '(1 1)) => true)
   (test (majority? '(0)) => false)
   (test (majority? '(1 1)) => true)
   (test (majority? '(1 1 0 0 0 0 0 1 1 1 1 1 0 0 0)) => false)
   (test (majority? '(0 1 0 1)) => true)
   (test (majority? '(0 1 1 1 1 1 1)) => true)
   (test (majority? '()) => true)
   (test (majority? '(1 1 1 0 0 0)) => true)




 (: geq-bitlists? : Bit-List Bit-List -> Boolean)
   (define (geq-bitlists? bl1 bl2)
     (cond
      [(and (eq? bl1 null) (eq? bl2 null)) true]
      [(or (eq? bl1 null) (eq? bl2 null))
       (error 'geq-bitlists? "different length")]
      [(> (first bl1) (first bl2)) true]
      [(< (first bl1) (first bl2)) false]
      [else (geq-bitlists? (rest bl1) (rest bl2))]))


   (test (geq-bitlists? '(0 0 0) '(0 0 1)) => false)
   (test (geq-bitlists? '(0) '(0)) => true)
   (test (geq-bitlists? '(1 1 1) '(1 0 1)) => true)
   (test (geq-bitlists? '(1 1 1 0) '(1 1 1 1)) => false)
   (test (geq-bitlists? '(0 0 0 0 0) '(0 0 0 1 0)) => false)
   (test (geq-bitlists? '(1 1 1 1 1) '(1 0 1 1 0)) => true)
   (test (geq-bitlists? '(0 1 1 0 1) '(1 1 0 1 1)) => false)
   (test (geq-bitlists? '(1) '(1)) => true)
   (test (geq-bitlists? '() '(1)) =error> "geq-bitlists?: different length")
   (test (geq-bitlists? '(1 1 1 0 1) '(1 1 0 1 1)) => true)
   (test (geq-bitlists? '(1 1 1 1) '(1 1 1 0)) => true)


   (: shift-left : Bit-List -> Bit-List)
   (define (shift-left bl) (append (rest bl) (list (first bl))))
   (test (shift-left '(0 0)) => '(0 0))
   (test (shift-left '(0 1 1 1 1)) => '(1 1 1 1 0))
   (test (shift-left '(1 1 1)) => '(1 1 1))
   (test (shift-left '(1 0 0)) => '(0 0 1))
   (test (shift-left '(0 1)) => '(1 0))
   (test (shift-left '(0 1 0 1 0 1)) => '(1 0 1 0 1 0))
   (test (shift-left '(0 1 0)) => '(1 0 0))
   (test (shift-left '(0 0 0 0 0 0 0 1)) => '(0 0 0 0 0 0 1 0))
   (test (shift-left '(0 1 0 0 1 0 0 1)) => '(1 0 0 1 0 0 1 0))
   (test (shift-left '(1 1 1 0)) => '(1 1 0 1))
   (test (shift-left '(0 1 1 0)) => '(1 1 0 0))
   (test (shift-left '(0 0 0 0)) => '(0 0 0 0))

   (: run : String -> Bit-List)
   (define (run command) (RegV->bit-list (eval (parse command) (EmptyEnv))))


   (test (run "{ reg-len =  4  {1 0 0 0}}") => '(1 0 0 0))
   (test
    (run "{ reg-len =  3  {1 0 0 0}}")
    =error>
    "wrong number of bits in (1 0 0 0)")
   (test (run "{ reg-len =  5  {1 0 0 0 0}}") => '(1 0 0 0 0))
   (test (run "{ reg-len = 4  {shl {1 0 0 0}}}") => '(0 0 0 1))
   (test (run "{ reg-len = 4  {shl {0 1 0 0}}}") => '(1 0 0 0))
   (test (run "{ reg-len = 4  {shl {1 1 0 0}}}") => '(1 0 0 1))
   (test (run "{ reg-len = 3 {if {1 0 1} {1 0 0} {1 0 1}}}") => '(1 0 0))
   (test (run "{ reg-len = 2 {if {with {x false} x} {0 0} {1 1}}}") => '(1 1))
  