#lang racket

(define Stack
  (class object%
    (field (stack '()))
    (define/public (pop)
      (when (empty? stack) (error "Tried popping from empty stack"))
      (let ((top (car stack)))
        (set! stack (cdr stack))
        top))
    (define/public (push v)
      (set! stack (cons v stack)))
    (super-new)))