#lang br/quicklang

(define (read-syntax parth port)
  (define src-lines (port->lines port))
  (define src-datums (format-datums '(handle ~a) src-lines))
  (define datum `(module stacker-mod "stacker.rkt" ,@src-datums))
  (datum->syntax #f datum))

(provide read-syntax)

(define-macro (stacker-module-begin HANDLE-EXPR ...)
  #'(#%module-begin
     HANDLE-EXPR ...
     (println (pop-stack!))))

(provide (rename-out [stacker-module-begin #%module-begin]))

(define stack empty)

(define (pop-stack!)
  (define arg (first stack))
  (set! stack (rest stack))
  arg)

(define (push-stack! arg)
  (set! stack (cons arg stack)))

(define (handle [arg #f])
  (cond
    [(number? arg) (push-stack! arg)]
    [(eqv? stacker-add arg) (push-stack! (+ (pop-stack!) (pop-stack!)))]
    [(eqv? stacker-mul arg) (push-stack! (* (pop-stack!) (pop-stack!)))]))

(provide handle)

(define stacker-add '+)
(define stacker-mul '*)

(provide (rename-out [stacker-add +]))
(provide (rename-out [stacker-mul *]))
