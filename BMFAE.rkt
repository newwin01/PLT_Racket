#lang plai

; type definition for abstract syntax tree of BMFAE
(define-type BMFAE
    [num    (n number?)]
    [add     (lhs BMFAE?) (rhs BMFAE?)]
    [sub     (lhs BMFAE?) (rhs BMFAE?)]
    [id      (name symbol?)]
    [fun      (param symbol?) (body BMFAE?)]
    [refun      (param symbol?) (body BMFAE?)]
    [newbox  (v BMFAE?)]
    [setbox  (bn BMFAE?) (v BMFAE?)]
    [openbox  (v BMFAE?)]
    [seqn  (ex1 BMFAE?) (ex2 BMFAE?)]
    [app     (ftn BMFAE?) (arg BMFAE?)]
    [setvar (name symbol?) (val-expr BMFAE?)]
  )
  
        
; parse : sexp -> BMFAE
(define (parse sexp)
   (match sexp
        [(? number?)                (num sexp)]
        [(list '+ l r)              (add (parse l) (parse r))]
        [(list '- l r)              (sub (parse l) (parse r))]
        [(list 'with (list i v) e)  (app (fun i (parse e)) (parse v))]
        [(? symbol?)                (id sexp)]
        [(list 'newbox v)           (newbox (parse v))]
        [(list 'setbox i v)         (setbox (parse i) (parse v))]
        [(list 'openbox i)          (openbox (parse i))]
        [(list 'seqn ex1 ex2)       (seqn (parse ex1) (parse ex2))]
        [(list 'fun (list p) b)                 (fun p (parse b))]
        [(list 'refun (list p) b)   (refun p (parse b))]
        [(list f a)                 (app (parse f) (parse a))]
        [(list 'setvar i v)         (setvar i (parse v))]
        [else                       (error 'parse "bad syntax: ~a" sexp)]))

(define-type BMFAE-Value
  [numV      (n number?)]
  [closureV  (param symbol?) (body BMFAE?) (ds DefrdSub?)]
  [refclosV  (param symbol?) (body BMFAE?) (ds DefrdSub?)]
  [boxV      (address integer?)])

(define (num-op op)
     (lambda (x y)
          (numV (op (numV-n x) (numV-n y)))))

(define num+ (num-op +))
(define num- (num-op -))
(define num* (num-op *))


(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (address integer?) (ds DefrdSub?)])

(define-type Store
  [mtSto]
  [aSto   (address integer?) (value BMFAE-Value?)
          (rest Store?)])

;lookup: symbol DefrdSub -> address
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub ()           (error 'lookup "free identifier")]
    [aSub  (i adr saved) (if(symbol=? i name)
                                adr
                                (lookup name saved))]))

;store-lookup address Store -> BMFAE-Value
(define (store-lookup address sto)
  (type-case Store sto
    [mtSto ()           (error 'store-lookup "No value at address")]
    [aSto  (location value rest-store)
                 (if(= location address)
                     value
                     (store-lookup address rest-store))]))

; malloc: Store -> Integer
(define (malloc st)
  (+ 1 (max-address st)))

; max-address: Store -> Integer
(define (max-address st)
  (type-case Store st
    [mtSto () 0]
    [aSto (n v st)
          (max n (max-address st))]))

(define-type Value*Store
  [v*s (value BMFAE-Value?) (store Store?)])

; interp: BMFAE DefrdSub -> Value*Store
(define (interp bmfae ds st)
  (type-case BMFAE bmfae
     [num (n)      (v*s (numV n) st)]
     [add (l r)    (interp-two l r ds st 
                                             (lambda (v1 v2 st1) (v*s (num+ v1 v2) st1)))]

     [sub (l r)    (interp-two l r ds st 
                                             (lambda (v1 v2 st1) (v*s (num- v1 v2) st1)))]

     [id  (s)    (v*s (store-lookup (lookup s ds) st) st)]
     [fun (p b)  (v*s (closureV p b ds) st)]
     [refun (p b) (v*s (refclosV p b ds) st)]
     [app (f a)  (type-case Value*Store (interp f ds st)
                     [v*s (f-value f-store)
                            (type-case BMFAE-Value f-value
                               [closureV (c-param c-body c-ds)
                                         (type-case Value*Store (interp a ds f-store)
                                           [v*s (a-value a-store)
                                                (local ([define new-address (malloc a-store)])
                                                  (interp (closureV-body f-value)
                                                          (aSub (closureV-param f-value)
                                                                new-address
                                                                (closureV-ds f-value))
                                                          (aSto new-address
                                                                a-value
                                                                a-store)))])]
                                                                                                                                           
                              [refclosV (rc-param rc-body rc-ds)
                                        (local ([define address (lookup (id-name a) ds)])
                                          (interp (refclosV-body f-value)
                                                  (aSub (refclosV-param f-value)
                                                        address
                                                        (refclosV-ds f-value)) f-store))] 
                                                                                                                                            
                              [else (error interp "trying to apply a number")]
                                     )])]
     [newbox (val)
            (type-case Value*Store (interp val ds st)
              [v*s (vl st1)
                   (local [(define a (malloc st1))]
                          (v*s (boxV a)
                               (aSto a vl st1)))])]
     [setbox (bx-expr val-expr)
                               (type-case Value*Store (interp bx-expr ds st)
                                 [v*s (bx-val st2)
                                      (type-case Value*Store (interp val-expr ds st2)
                                        [v*s (val st3)
                                             (v*s val
                                                  (aSto (boxV-address bx-val)
                                                        val
                                                        st3))])])]
     [openbox (bx-expr)
             (type-case Value*Store (interp bx-expr ds st)
               [v*s (bx-val st1)
                    (v*s (store-lookup (boxV-address bx-val)
                                       st1)
                         st1)])]
     [seqn (a b) ;(interp-two a b ds st (lambda (v1 v2 st1) (v*s v2 st1)))]
                (interp-two a b ds st (lambda (v1 v2 st1) (v*s v2 st1)))]
    [setvar (id val-expr)
                 (local [(define a (lookup id ds))]
                      (type-case Value*Store (interp val-expr ds st) 
                            [v*s (val st)
                                    (v*s val
                                             (aSto a
                                                        val
                                                        st))]))]
    ))

;interp-two
(define (interp-two expr1 expr2 ds st handle)
  (type-case Value*Store (interp expr1 ds st)
    [v*s (val1 st2)
         (type-case Value*Store (interp expr2 ds st2)
           [v*s (val2 st3)
                (handle val1 val2 st3)])]))


(define (run sexp ds st)
     (interp (parse sexp) ds st))

(run '{with {a 3} {setvar a 5}} (mtSub) (mtSto))

(run '{with {a 3} {seqn {{fun {x} {setvar x 5}} a} a}} (mtSub) (mtSto))

(run '{with {swap {fun {x}
            {fun {y}
              {with {z x}
                {seqn {setbox x (openbox y)}
                           {setbox y (openbox z)}}}}}}
           {with {a (newbox 10)}
                       {with {b (newbox 20)}
                         {seqn {{swap a} b}
                           (openbox b)}}}} (mtSub) (mtSto))

(run '{with {swap {fun {x}
            {refun {y}
              {with {z x}
                {seqn {setbox x (openbox y)}
                           {setbox y (openbox z)}}}}}}
           {with {a (newbox 10)}
                       {with {b (newbox 20)}
                         {seqn {{swap a} b}
                           (openbox b)}}}} (mtSub) (mtSto))

(run '{with {swap {fun {x}
                       {fun {y}
                            {with {z x}
                                  {seqn {setvar x y}
                                        {setvar y z}}}}}}
            {with {a 10}
                  {with {b 20}
                        {seqn {{swap a} b}
                              b}}}} (mtSub) (mtSto))

(run '{with {swap {refun {x}
                       {refun {y}
                            {with {z x}
                                  {seqn {setvar x y}
                                        {setvar y z}}}}}}
            {with {a 10}
                  {with {b 20}
                        {seqn {{swap a} b}
                              b}}}} (mtSub) (mtSto))

(parse '{with {swap {fun {x}
            {fun {y}
              {with {z x}
                {seqn {setbox x {openbox y}}
                           {setbox y {openbox z}}}}}}}
           {with {a {newbox 10}}
                       {with {b {newbox 20}}
                         {seqn {{swap a} b}
                           {openbox a}}}}})
(run ' {with {swap {refun {x} {refun {y} {with {z x}
                                               {seqn {setvar x y} {setvar y z}}}}}}
             {with {a 10} {with {b 20} {seqn {{swap a} b} b}}}} (mtSub) (mtSto))

(run '{openbox {newbox {+ 3 4}}} (mtSub) (mtSto))

(run '{with {a {newbox 3}} {seqn {{fun {x} {setbox x 5}} a} {openbox a}}} (mtSub) (mtSto))

(run '{with {a 3} {seqn {{fun {x} {setvar x 5}} a} a}} (mtSub) (mtSto))
