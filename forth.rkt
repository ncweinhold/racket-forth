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
    (define/public (display-stack)
      ;; Will have a nested function that takes a numeric argument
      ;; The numeric value will just be the index into the stack
      ;; e.g. 0 will represent the top of the stack
      (display "TODO"))
    (super-new)))

(define (pop-stack s)
  (send s pop))

(define (push-stack s v)
  (send s push v))

;; The environment for now will just be a global hash table...
(define environment (make-hash))

(define (add-to-env env k v)
  (hash-set! env k v))

;; Populate the global environment with some built in Forth operators / functions
(define (initialise-environment env s)
  (add-to-env env "+" (lambda () (push-stack s (+ (pop-stack s) (pop-stack s)))))
  (add-to-env env "-" (lambda () (push-stack s (- (pop-stack s) (pop-stack s)))))
  (add-to-env env "*" (lambda () (push-stack s (* (pop-stack s) (pop-stack s)))))
  (add-to-env env "/" (lambda () (push-stack s (/ (pop-stack s) (pop-stack s))))))