#lang eopl

;; interpreter for the LET language.  The \commentboxes are the
;; latex code for inserting the rules into the code in the book.
;; These are too complicated to put here, see the text, sorry.

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

(define lcm
  (lambda (num1 num2)
    (lcm-helper num1 num2 1)))

(define lcm-helper
  (lambda (num1 num2 counter)
    (if (and (= (remainder counter num1) 0) (= (remainder counter num2) 0))
        counter
        (lcm-helper num1 num2 (+ counter 1)))))

;; value-of : Exp * Env -> ExpVal
;; Page: 71
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      (const-exp (num) (num-val num))

      (var-exp (var) (apply-env env var))
      
      (zero?-exp (exp1)
        ;; -----------------------
        ;; INSERT YOUR CODE HERE 
        ;; -----------------------
       (let ((val1 (value-of exp1 env)))
         (cond [(rational-val? val1)
                (let ((rat1 (expval->rational val1)))
                  (if (= (car rat1) 0)
                      (bool-val #t)
                      (bool-val #f)))]
               [(num-val? val1)
                (let ((num1 (expval->num val1)))
                  (if (= num1 0)
                      (bool-val #t)
                      (bool-val #f)))])))
        ;; -----------------------
      

      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))

      ;; -----------------------
      ;; INSERT YOUR CODE HERE 
      ;; -----------------------
      (str-exp (str) (str-val str))

      (rational-exp (num1 num2)
                            (if (= num2 0)
                                (eopl:error 'divide-by-zero-error "error")
                                (rational-val (cons num1 num2))))
      (op-exp (exp1 exp2 op)
              (let ((val1 (value-of exp1 env))
                     (val2 (value-of exp2 env)))
                  (cond
                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    [(and (rational-val? val1) (num-val? val2))
                     (let ((num2 (expval->num val2))
                       (rat1 (expval->rational val1)))
                       (cond
                         [(= op 1) (rational-val (cons (+ (car rat1) (* (cdr rat1) num2)) (cdr rat1)))]
                         [(= op 2) (rational-val (cons (* (car rat1) num2) (cdr rat1)))]
                         [(= op 3) (rational-val (cons (car rat1) (* (cdr rat1) num2)))]
                         [else (rational-val (cons (- (car rat1) (* (cdr rat1) num2)) (cdr rat1)))]))]
                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    [(and (rational-val? val2) (num-val? val1))
                     (let ((num1 (expval->num val1))
                       (rat2 (expval->rational val2)))
                       (cond
                         [(= op 1) (rational-val (cons (+ (car rat2) (* (cdr rat2) num1)) (cdr rat2)))]
                         [(= op 2) (rational-val (cons (* (car rat2) num1) (cdr rat2)))]
                         [(= op 3) (rational-val (cons (* (cdr rat2) num1) (car rat2)))]
                         [else (rational-val (cons (- (* (cdr rat2) num1) (car rat2)) (cdr rat2)))]))]
                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    [(and (num-val? val1) (num-val? val2))
                     (let ((num1 (expval->num val1))
                       (num2 (expval->num val2)))
                       (cond
                         [(= op 1) (num-val (+ num1 num2))]
                         [(= op 2) (num-val (* num1 num2))]
                         [(= op 3) (num-val (/ num1 num2))]
                         [else (num-val (- num1 num2))]))]
                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    [(and (rational-val? val1) (rational-val? val2))
                     (let ((rat1 (expval->rational val1))
                       (rat2 (expval->rational val2)))
                       (let ((lcmval (lcm (cdr rat1) (cdr rat2))))
                       (cond
                         [(= op 1) (rational-val (cons (+ (* (car rat1) (/ lcmval (cdr rat1))) (* (car rat2) (/ lcmval (cdr rat2)))) lcmval))]
                         [(= op 2) (rational-val (cons (* (car rat1) (car rat2)) (* (cdr rat1) (cdr rat2))))]
                         [(= op 3) (rational-val (cons (* (car rat1) (cdr rat2)) (* (cdr rat1) (car rat2))))]
                         [else (rational-val (cons (- (* (car rat1) (/ lcmval (cdr rat1))) (* (car rat2) (/ lcmval (cdr rat2)))) lcmval))])))]
                    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    [else (eopl:error 'syntax-error "error")])))

      (if-exp (cond1 exp1 conds exps else-exp)
                  (if (expval->bool (value-of cond1 env))
                       (num-val (expval->num (value-of exp1 env)))
                       (cond
                         [(null? conds)
                          (num-val (expval->num (value-of else-exp env)))]
                         [(null? (cdr conds))
                          (if (expval->bool (value-of (car conds) env))
                              (num-val (expval->num (value-of (car exps) env)))
                              (num-val (expval->num (value-of else-exp env))))]
                         [else (value-of (if-exp (car conds) (car exps) (cdr conds) (cdr exps) else-exp) env)])))
                    
                    

              

      ;; -----------------------

      )))

