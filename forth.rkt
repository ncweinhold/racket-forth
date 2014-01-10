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
    (super-new)))

(define (pop-stack s)
  (send s pop))

(define (push-stack s v)
  (send s push v))

(define (peek-stack s)
  (send s peek))

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
  (add-to-env env "." (lambda () (fprintf (current-output-port) "~a~n" (pop-stack s))))
  (add-to-env env "SPACES" (lambda () (display (make-string (pop-stack s) #\space))))
  (add-to-env env "EMIT" (lambda () (write-byte (pop-stack s))))
  (add-to-env env "DUP" (lambda () (push-stack s (peek-stack s))))
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