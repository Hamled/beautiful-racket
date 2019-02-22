#lang br/quicklang

(define (read-syntax parth port)
  (define src-lines (port->lines port))
  (define src-datums (format-datums '~a src-lines))
  (define datum `(module funstacker-mod "funstacker.rkt"
                         (handle-args ,@src-datums)))
  (datum->syntax #f datum))

(provide read-syntax)

(define-macro (funstacker-module-begin HANDLE-ARGS-EXPR)
  #'(#%module-begin
     (println (first HANDLE-ARGS-EXPR))))

(provide (rename-out [funstacker-module-begin #%module-begin]))


(define (handle-args . args)
  (for/fold ([stack-acc empty])
            ([arg (in-list args)]
             #:unless (void? arg))
    (cond
      [(number? arg) (cons arg stack-acc)]
      [(eqv? funstacker-add arg) (cons (+ (first stack-acc) (second stack-acc)) (drop stack-acc 2))]
      [(eqv? funstacker-mul arg) (cons (* (first stack-acc) (second stack-acc)) (drop stack-acc 2))])))
(provide handle-args)

(define funstacker-add '+)
(define funstacker-mul '*)

(provide (rename-out [funstacker-add +]))
(provide (rename-out [funstacker-mul *]))
