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
        [(_ (prim-name arg* ...) b b* ...)
            (begin
                (putprop 'prim-name '*is-prim* #t)
                (putprop 'prim-name '*arg-count* (length '(arg* ...)))
                (putprop 'prim-name '*emitter* (lambda (arg* ...) b b* ...)))]))

(define (primitive? x)
    (and (symbol? x) (getprop x '*is-prim*))
)

(define (primitive-emitter x)
    (and (symbol? x) (getprop x '*emitter*))
)

(define (primcall? expr)
    (and (pair? expr) (primitive? (car expr)))
)

(define (emit-primcall expr)
    (let ([prim (car expr)] [args (cdr expr)])
        ;(check-primcall-args prim args)
        (apply (primitive-emitter prim) args)
    )
)

;; for conditional
(define (if? expr)
    (and (list? expr) (symbol? (car expr)) (symbol=? 'if (car expr)))
)

(define (emit-if expr)
    (let [(test-expr (car expr)) (conseq-expr (cadr expr)) (altern-expr (caddr expr))] 
        (body-todo)
    )
)

;; core
(define (emit-expr expr)
    (cond
        [(immediate? expr) (emit " movl $~s, %eax" (emit-immediate expr))]
        [(primcall? expr) (emit-primcall expr)]
        [(if? expr) (emit-if expr)]
        [else (error 'emit "unknow expr")]
    )
)

(define (emit-program x)
    (emit " .text")
    (emit " .globl scheme_entry")
    (emit " .type scheme_entry, @function")
    (emit "scheme_entry:")
    (emit-expr x)
    (emit " ret")
    (emit " .size scheme_entry, .-scheme_entry")
)
