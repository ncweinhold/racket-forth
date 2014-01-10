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
    (define/public (peek)
      (car stack))
    (define/public (view)
      (unless (empty? stack)
        (fprintf (current-output-port) "Bottom of the stack is: ~a ~n" (last stack))
        (for ([item (reverse stack)])
          (fprintf (current-output-port) "Item is ~a ~n" item))))
    (define/public (is-empty?)
      (empty? stack))
    (define/public (clear)
      (set! stack '()))
    (super-new)))

(define (pop-stack s)
  (send s pop))

(define (push-stack s v)
  (send s push v))

(define (peek-stack s)
  (send s peek))

(define (display-stack s)
  (send s view))

(define (empty-stack? s)
  (send s is-empty?))

(define (clear-stack s)
  (send s clear))

(define stack (new Stack))
;; The environment for now will just be a global hash table...
(define environment (make-hash))

(define (add-to-env env k v)
  (hash-set! env k v))

;; Populate the global environment with some built in Forth operators / functions
(define (initialise-environment env s)
  (add-to-env env "+" (lambda () (push-stack s (+ (pop-stack s) (pop-stack s)))))
  (add-to-env env "-" (lambda () (push-stack s (- (pop-stack s) (pop-stack s)))))
  (add-to-env env "*" (lambda () (push-stack s (* (pop-stack s) (pop-stack s)))))
  (add-to-env env "/" (lambda () (push-stack s (/ (pop-stack s) (pop-stack s)))))
  (add-to-env env "ABS" (lambda () (push-stack s (abs (pop-stack s)))))
  (add-to-env env "MOD" (lambda () (push-stack s (modulo (pop-stack s) (pop-stack s)))))
  (add-to-env env "AND" (lambda () (push-stack s (bitwise-and (pop-stack s) (pop-stack s)))))
  (add-to-env env "OR" (lambda () (push-stack s (bitwise-ior (pop-stack s) (pop-stack s)))))
  (add-to-env env "XOR" (lambda () (push-stack s (bitwise-xor (pop-stack s) (pop-stack s)))))
  ;(add-to-env env "LSHIFT")
  ;(add-to-env env "RSHIFT")
  (add-to-env env "MIN" (lambda () (push-stack s (min (pop-stack s) (pop-stack s)))))
  (add-to-env env "MAX" (lambda () (push-stack s (max (pop-stack s) (pop-stack s)))))
  (add-to-env env "." (lambda () (fprintf (current-output-port) "~a~n" (pop-stack s))))
  (add-to-env env ".S" (lambda () (display-stack s)))
  (add-to-env env "DUP" (lambda () (push-stack s (peek-stack s))))
  (add-to-env env "?DUP" (lambda () 
                           (if (empty-stack? s)
                               (push-stack s 0)
                               (push-stack s (peek-stack s)))))
  (add-to-env env "DROP" (lambda () (pop-stack s)))
  (add-to-env env "SWAP" (lambda () 
                           (let ((top (pop-stack s))
                                 (next (pop-stack s)))
                             (push-stack s top)
                             (push-stack s next))))
  (add-to-env env "CLEAR" (lambda () (clear-stack s)))
  (add-to-env env "SPACES" (lambda () (display (make-string (pop-stack s) #\space))))
  (add-to-env env "EMIT" (lambda () (write-byte (pop-stack s))))
  (add-to-env env "BYE" (lambda () (exit))))

(define (get-word k)
  (hash-ref environment k))

(define (parse-token token)
  (if (string->number token)
      (push-stack stack (string->number token))
      (begin
        ((get-word (string-upcase token)))
        "ok")))

(define (parse-line line)
  (let ((tokens (string-split line)))
    (for ([token tokens])
      (parse-token token))))

;; Repl function
;; Read a line from the standard input.
;; Break the line up into whitespace delimited tokens
;; For each token, process it appropriately

(define (repl)
  (display ">>> ")
  (let ((line (read-line)))
    (parse-line line))
  (repl))

;; Main entry point

(define (main)
  (initialise-environment environment stack)
  (display "Racket-Forth interpreter. Copyright (C) 2014, Nick Weinhold\n")
  (display "Racket-Forth comes with ABSOLUTELY NO WARRANTY; Use at your own risk\n")
  (repl))

(main)