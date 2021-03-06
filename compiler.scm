(load "tests/tests-driver.scm")


;; for immediate
(define int_tag_width           2)
(define int_tag                 #b00)
(define bool_tag_width          6)
(define bool_tag                #b101111)
(define empty_list_tag_width    8)
(define empty_list_tag          #b00111111)
(define char_tag_width          8)
(define char_tag                #b00001111)

(define (mask w) 
    (ash #b11111111 (- w 8))
)

(define (empty-list? x) 
    (null? x)
)

(define (immediate? x) 
    (or (integer? x) (boolean? x) (empty-list? x) (char? x))
)

(define (emit-immediate x) 
    (cond
        ((integer? x)       (fxlogor int_tag (ash x int_tag_width)))
        ((boolean? x)       (fxlogor bool_tag (ash (if x 1 0) bool_tag_width)))
        ((empty-list? x)    (fxlogor empty_list_tag (ash 0 empty_list_tag_width)))
        ((char? x)          (fxlogor char_tag (ash (char->integer x) char_tag_width)))
        (else (error 'emit-immediate "unknow immediate"))
    )
)

;; for primitive
(define-syntax define-primitive
    (syntax-rules ()
        [(_ (prim-name si env arg* ...) b b* ...)
            (begin
                (putprop 'prim-name '*is-prim* #t)
                (putprop 'prim-name '*arg-count* (length '(arg* ...)))
                (putprop 'prim-name '*emitter* (lambda (si env arg* ...) b b* ...)))]))

(define (primitive? x)
    (and (symbol? x) (getprop x '*is-prim*))
)

(define (primitive-emitter x)
    (and (symbol? x) (getprop x '*emitter*))
)

(define (primcall? expr)
    (and (pair? expr) (primitive? (car expr)))
)

(define (emit-primcall si env expr)
    (let ([prim (car expr)] [args (cdr expr)])
        ;(check-primcall-args prim args)
        (apply (primitive-emitter prim) si env args)
    )
)

;; for conditional
(define unique-label
	(let ([count 0])
        (lambda ()
            (let ([L (format "L_~s" count)])
                (set! count (add1 count))
                L))))

(define (if? expr)
    (and (list? expr) (symbol? (car expr)) (symbol=? 'if (car expr)))
)

(define (emit-if si env expr)
    (let [(test-expr (cadr expr)) (conseq-expr (caddr expr)) (altern-expr (cadddr expr)) (alt-label (unique-label)) (end-label (unique-label))] 
        (emit-expr si env test-expr)
        (emit "     cmpl $47,   %eax") ;bool-#f
        (emit "     je   ~a" alt-label)
        (emit-expr si env conseq-expr)
        (emit "     jmp  ~a" end-label)
        (emit "~a:" alt-label)
        (emit-expr si env altern-expr)
        (emit "~a:" end-label)
    )
)

;; for local variable
;; (let ([var val] ...) <body>)
(define (let? expr)
    (and (list? expr) (let ([fst (car expr)]) 
        (and (symbol? fst) (symbol=? 'let fst))
    ))
)

(define (let*? expr)
    (and (list? expr) (let ([fst (car expr)]) 
        (and (symbol? fst) (symbol=? 'let* fst))
    ))
)

(define (let-bindings expr)
    (cadr expr)
)

(define (let-body expr)
    (caddr expr)
)

(define (lhs binding)
    (car binding)
)

(define (rhs binding)
    (cadr binding)
)

(define (make-env)
    (make-eq-hashtable)
)

(define (new-env env)
    (hashtable-copy env #t)
)

(define (env-set! env variable si)
    (eq-hashtable-set! env variable si)
)

(define (env-lookup env variable)
    (eq-hashtable-ref env variable 0)
)

(define (emit-let si env expr) 
    (let iter ([bindings (let-bindings expr)] [si si] [local-env (new-env env)]) 
        (cond
            [(null? bindings) (emit-expr si local-env (let-body expr))]
            [else
                (let ([b (car bindings)]) 
                    (emit-expr si env (rhs b))
                    (emit "movl %eax, ~s(%rsp)" si)
                    (env-set! local-env (lhs b) si)
                    (iter (cdr bindings) (- si wordsize) local-env)
                )
            ]
        )
    )
)

(define (emit-let* si env expr) 
    (let iter ([bindings (let-bindings expr)] [si si] [env (new-env env)]) 
        (cond
            [(null? bindings) (emit-expr si env (let-body expr))]
            [else
                (let* ([b (car bindings)] [variable (lhs b)] [exist-si (env-lookup env variable)]) 
                    (emit-expr si env (rhs b))
                    (cond
                        [(= exist-si 0) 
                            (emit "movl %eax, ~s(%rsp) # new binding" si)
                            (env-set! env variable si)
                            (iter (cdr bindings) (- si wordsize) env)
                        ]
                        [else
                            (emit "movl %eax, ~s(%rsp) # overwrite binding" exist-si)
                            (env-set! env variable exist-si)
                            (iter (cdr bindings) si env)
                        ]
                    )
                )
            ]
        )
    )
)

;; for variable
(define (variable? expr)
    (symbol? expr)
)

(define (emit-load-variable env expr)
    (let ((si (env-lookup env expr))) 
        (emit "movl ~s(%rsp), %eax" si)
    )
)


;; for letrec
(define (letrec? expr)
    (and (list? expr) (not (null? expr)) (symbol? (car expr)) (symbol=? (car expr) 'letrec))
)

(define (letrec-bindings expr)
    (cadr expr)
)

(define (letrec-body expr)
    (caddr expr)
)

(define (unique-labels lvars)
    ;; todo
)

(define (emit-letrec si env expr)
    (emit ";TODO")
    (let* ([bindings (letrec-bindings expr)]
           [lvars (map lhs bindings)]
           [lambdas (map rhs bindings)]
           [labels (unique-labels lvars)])

        (for-each (lambda ))        
    )
)

;; core
(define wordsize 4)
(define (emit-expr si env expr)
    ;(and (printf (format "??? ~s ~s\n" expr (symbol? expr)))
    (cond
        [(immediate? expr) (emit " movl $~s, %eax" (emit-immediate expr))]
        [(primcall? expr) (emit-primcall si env expr)]
        [(if? expr) (emit-if si env expr)]
        [(let? expr) (emit-let si env expr)] 
        [(let*? expr) (emit-let* si env expr)] 
        [(variable? expr) (emit-load-variable env expr)]
        [else (error 'emit (format "unknow expr ~s ~s ~s." (list? expr) (symbol? expr) (symbol=? 'if expr) ))]
    )
    ;)
)

(define (emit-scheme-entry si env expr)
    (emit " .text")
    (emit " .globl scheme_entry")
    (emit " .type scheme_entry, @function")
    (emit "scheme_entry:")
    (emit " movq %rsp, %rsi")
    (emit " movq %rdi, %rsp")
    (emit-expr si env expr)
    (emit " movq %rsi, %rsp")
    (emit " ret")
    (emit " .size scheme_entry, .-scheme_entry")
)

(define (emit-program x)
    (let ([si (- wordsize)] [env (make-env)])
        (if (letrec? x) 
            (emit-letrec si env x)
            (emit-scheme-entry si env x)
        )
    )

)
