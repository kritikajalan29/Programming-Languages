;;;;;;;;;;;;;;;;;;; COMP 105 CONTINUATIONS ASSIGNMENT ;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 2


;; (list-of? A? v) takes in a predicate function A? and an arbitrary value v. It
;; returns true if v is a list of values, each of which satisfies A? else 
;; returns false

;; laws 
;;   (list-of? A? v) == #f ; where v is NOT  a list 
;;   (list-of? A? '()) == #t 
;;   (list-of? A? (cons x xs)) == #f ; if x does not satisfy A? 
;;   (list-of? A? (cons x xs)) ==  (list-of? A? xs); if x satisfies A?    


(define list-of? (A? v)
    (if (null? v) 
        #t
        (if (atom? v)
            #f
            (if (A? (car v))
                (list-of? A? (cdr v))
                #f )))) 


        ;; UNIT TESTS
        (check-assert (list-of? boolean? '()))
        (check-assert (list-of? boolean? '(#t #f #f #f #t)))
        (check-assert (list-of? number? '(1 45678)))
        (check-assert (not(list-of? number? 'a)))
        (check-assert (not (list-of? boolean? '(34 #f #f #f #t))))
        (check-assert (list-of? symbol? '(a b)))
        (check-assert (not (list-of? symbol? (cons 'a 'b))))
        (check-assert (not (list-of? boolean? '( #f #f #f #t 34))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 3


;; (formula? f) takes in a formula f and returns a boolean which represents 
;; whether f is a valid boolean formula or not

;; laws:
;;   (formula f) == #t ; where f is a symbol 
;;   (formula (make-not arg)) == (formula? arg)
;;   (formula (make-or args)) == (list-of? formula? args) 
;;                                          ;where args is a list of formulas
;;   (formula (make-and args)) == (list-of? formula? args)
;;                                          ;where args is a list of formulas


(record not [arg])
(record or  [args])
(record and [args])

(define formula? (f) 
    (if (symbol? f)
        #t
        (if(not? f)
            (formula? (not-arg f))
            (if (or? f)
                (list-of? formula? (or-args f))
                (if (and? f)
                    (list-of? formula? (and-args f))
                    #f)))))

        ;; UNIT TESTS
        (check-assert (formula? 'x))
        (check-assert (formula? (make-not 'x)))
        (check-assert (not (formula? (make-not #t))))
        (check-assert (formula? (make-or '(x y z))))
        (check-assert (not (formula? (make-or '(x 32 z)))))
        (check-assert (formula? (make-and '(x y z))))
        (check-assert (formula? (make-and '(x y (make-or (a b (make-not c)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 4


;; (eval-formula f env) takes in a formula f and an association list env which 
;; represents an environment. It returns true if the formula f is satisfied in 
;; the environment, else it returns false. 

;; laws:
;;   (eval-formula f env) == (find f env) ; where f is a symbol 
;;   (eval-formula (make-not arg) env) == (not (eval-formula arg env))
;;   (eval-formula (make-or (cons arg args)) env) == 
;;                   (exists? (lambda (x) (eval-formula x env)) (cons arg args))
;;   (eval-formula (make-and (cons arg args)) env) == 
;;                   (all? (lambda (x) (eval-formula x env)) (cons arg args))
;;

(define eval-formula (f env)
    (if (symbol? f)
        (find f env)
        (if(not? f)
            (not (eval-formula (not-arg f) env))
            (if (or? f)
                (exists? (lambda (x) (eval-formula x env)) (or-args f))
                (all? (lambda (x) (eval-formula x env)) (and-args f))))))


        ;; UNIT TESTS
        (check-assert (eval-formula 'x '((x #t) (y #f))))
        (check-assert (not (eval-formula (make-not 'x) '((x #t) (y #f) ))))
        (check-assert (eval-formula (make-or '(x y)) '((x #t) (y #f))))
        (check-assert (not (eval-formula (make-and '(x y)) '((x #t) (y #f)))))
        (check-assert (eval-formula (make-and (list2 (make-not 'y) 'x)) 
                                                    '((x #t) (y #f) (z #f))))
        (check-assert (not (eval-formula (make-or (list3 (make-not 'x) 'y 'z)) 
                                                    '((x #t) (y #f) (z #f)))))
        (check-assert (eval-formula (make-not (make-or (list1 'z))) '((z #f))))
        (check-assert (eval-formula (make-and '()) '((x #t))))
        (check-assert (not (eval-formula (make-or '()) '((x #t)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 5

;; (solve-sat f fail succ) takes in a formula f, a failure continuation fail &
;; a success continuation succ and searches for a satisfying assignment—that is,
;; a mapping of Boolean variables to Boolean values that makes the formula f 
;; true.


(define solve-sat (f fail succ)
    (letrec(

;; (solve-formula f bool cur fail succeed) extends assignment of environment cur
;; to find an assignment that makes the single formula f equal to bool where 
;; bool is #t or #f using the continuations fail and succeed.

;; laws: 
;; (solve-formula x             bool cur fail succeed) == 
;;                  (solve-symbol x bool cur fail succeed); where x is a symbol
;; (solve-formula (make-not f)  bool cur fail succeed) == 
;;                              (solve-formula f  (not bool) cur fail succeed)
;; (solve-formula (make-or  fs) #t   cur fail succeed) == 
;;                                          (solve-any fs #t cur fail succeed)
;; (solve-formula (make-or  fs) #f   cur fail succeed) == 
;;                                          (solve-all fs #f cur fail succeed)
;; (solve-formula (make-and fs) #t   cur fail succeed) == 
;;                                          (solve-all fs #t cur fail succeed)
;; (solve-formula (make-and fs) #f   cur fail succeed) == 
;;                                          (solve-any fs #f cur fail succeed)

            [solve-formula 
                (lambda (f bool cur fail succeed) 
                    (if (symbol? f)
                        (solve-symbol f bool cur fail succeed)
                        (if(not? f)
                            (solve-formula (not-arg f) (not bool) cur fail 
                                                                    succeed)
                            (if (or? f)
                                (if (= bool #t)
                                    (solve-any (or-args f) #t cur fail succeed)
                                    (solve-all (or-args f) #f cur fail succeed))
                                (if (= bool #t)
                                    (solve-all (and-args f) #t cur fail succeed)
                                    (solve-any (and-args f) #f cur fail 
                                                                succeed))))))]

;; (solve-all fs bool cur fail succeed)  extends environment cur to find an 
;; assignment that makes every formula in the list fs equal to bool.

;; laws: 
;; (solve-all '()         bool cur fail succeed) == (succeed cur fail)
;; (solve-all (cons f fs) bool cur fail succeed) == (solve-formula f bool cur
;;              fail (lambda (env res) (solve-all fs bool env res succeed))

            [solve-all
                (lambda(f bool cur fail succeed)
                    (if (null? f)
                        (succeed cur fail)
                        (solve-formula (car f) bool cur fail 
                            (lambda (env res) (solve-all (cdr f) bool env 
                                                            res succeed)))))]

;; (solve-any fs bool cur fail succeed)  extends environment cur to find an 
;; assignment that makes any one formula in the list fs equal to bool.

;; laws: 
;; (solve-any '()         bool cur fail succeed) == (fail)
;; (solve-any (cons f fs) bool cur fail succeed) == (solve-formula f bool cur
;;            (lambda () (solve-any fs bool cur fail succeed) succeed)

            [solve-any
                (lambda(f bool cur fail succeed)
                    (if (null? f)
                        (fail)
                        (solve-formula (car f) bool cur 
                         (lambda () (solve-any (cdr f) bool cur fail succeed)) 
                                                                    succeed)))]

;; (solve-symbol x bool cur fail succeed)  binds symbol x to bool in environment
;; cur if it is not already present.

;; laws: 
;; (solve-symbol x bool cur fail succeed) == 
;;              (succeed (bind x bool cur) fail) ;where x is not bound in cur
;; (solve-symbol x bool cur fail succeed) == 
;;                                  (succeed cur fail) ;where x is bool in cur
;; (solve-symbol x bool cur fail succeed) == 
;;                                      (fail) ;where x is (not bool) in cur

            [solve-symbol
                (lambda (x bool cur fail succeed)
                    (if (null? (find x cur))
                        (succeed (bind x bool cur) fail)
                        (if (= bool (find x cur))
                            (succeed cur fail)
                            (fail))))])   
            (solve-formula f #t '() fail succ)))


        ;; UNIT TESTS
        (check-assert (function? solve-sat))            ; correct name
        (check-error  (solve-sat))                      ; not 0 arguments
        (check-error  (solve-sat 'x))                   ; not 1 argument
        (check-error  (solve-sat 'x (lambda () 'fail))) ; not 2 args
        (check-error  (solve-sat 'x (lambda () 'fail) (lambda (c r)'succeed)'z))

        (check-error (solve-sat 'x (lambda () 'fail) (lambda () 'succeed))) 
                            ; success continuation expects 2 arguments, not 0
        (check-error (solve-sat 'x (lambda () 'fail) (lambda (_) 'succeed)))
                            ; success continuation expects 2 arguments, not 1
        (check-error (solve-sat  ; failure continuation expects 0 args, not 1
                        (make-and (list2 'x (make-not 'x)))
                            (lambda (_) 'fail)
                            (lambda (_) 'succeed)))

        (check-expect   ; x can be solved
            (solve-sat 'x (lambda () 'fail)
                        (lambda (cur resume) 'succeed)) 'succeed)

        (check-expect   ; x is solved by '((x #t))
            (solve-sat 'x (lambda () 'fail)
                            (lambda (cur resume) (find 'x cur))) #t)

        (check-expect   ; (make-not 'x) can be solved
            (solve-sat (make-not 'x) (lambda () 'fail)
                    (lambda (cur resume) 'succeed)) 'succeed)

        (check-expect   ; (make-not 'x) is solved by '((x #f))
            (solve-sat (make-not 'x) (lambda () 'fail)
                        (lambda (cur resume) (find 'x cur))) #f)

        (check-expect   ; (make-and (list2 'x (make-not 'x))) cannot be solved
            (solve-sat (make-and (list2 'x (make-not 'x)))
                        (lambda () 'fail)
                        (lambda (cur resume) 'succeed))
            'fail)

        (check-expect   ; (make-and (list2 'x (make-not 'y))) cann be solved
            (solve-sat (make-and (list2 'x (make-not 'y)))
                        (lambda () 'fail)
                        (lambda (cur resume) 'succeed))
            'succeed)