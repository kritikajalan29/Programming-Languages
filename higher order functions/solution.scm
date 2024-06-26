;;;;;;;;;;;;;;;;;;; COMP 105 HOFS ASSIGNMENT ;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 2


;; (flip f) takes in a function f which takes in x and y. It returns the same 
;; function f except with the order of the arguments flipped, i.e., y x
;; instead of x y

;; laws 
;;   ((flip f) x y) == (f y x)
;;   


(define flip (f)
    (lambda (x y) (f y x))) 


        ;; UNIT TESTS
        (check-expect ((flip append) '(1 2 3) '(4 5 6)) '(4 5 6 1 2 3))
        (check-assert (not ((flip <) 3 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 3


;; (takewhile f? xs) takes in a predicate function f? and a list xs and returns 
;; the longest prefix of the list in which every element satisfies the predicate

;; laws:
;;   (takewhile f? '()) == '()
;;   (takewhile f? (cons x xs)) == '() ; where (f x) = #f
;;   (takewhile f? (cons x xs)) == (cons x (takewhile (f? xs)))
;;                                 ; where (f x) = #t
;;
;; [NOTE] function uses shorthand or

(define takewhile (f? xs)
    ( if (|| (null? xs) (not(f? (car xs))))
        '()
        (cons (car xs) (takewhile f? (cdr xs))))) 


        ;; UNIT TESTS
        (check-expect (takewhile even? '()) '())
        (define even? (x) (= (mod x 2) 0))
        (check-expect (takewhile even? '(2 4 6 7 8 10 12)) '(2 4 6))

;; (dropwhile f? xs) takes in a predicate function f? and a list xs and returns 
;; the longest prefix of the list in which every element satisfies the predicate

;; laws:
;;   (dropwhile f? '()) == '()
;;   (dropwhile f? (cons x xs)) == (cons x xs) ; where (f x) = #f
;;   (dropwhile f? (cons x xs)) == ((dropwhile (f? xs))) ; where (f x) = #t
;;
;; [NOTE] function uses shorthand or


(define dropwhile (f? xs)
    ( if (|| (null? xs) (not(f? (car xs))))
        xs
        (dropwhile f? (cdr xs)))) 


        ;; UNIT TESTS
        (check-expect (dropwhile even? '()) '())
        (check-expect (dropwhile even? '(5 4 6 7 8 10 12)) '(5 4 6 7 8 10 12))
        (check-expect (dropwhile even? '(2 4 6 7 8 10 12)) '(7 8 10 12))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 4


;; (ordered-by f?) takes a comparison function f? and returns a predicate 
;; function that tells if a list of values is completely ordered by the relation
;; in f?

;; laws 
;;   ((ordered-by f?) '()) == #t
;;   ((ordered-by f?) (cons x '())) == #t
;;   ((ordered-by f?) (cons x (cons y zs))) == #f ; if (f? x y) = #f
;;   ((ordered-by f?) (cons x (cons y zs))) == ((ordered-by f?) (cons y zs)) 
;;                                                  ;if (f? x y) = #t


(define ordered-by (f?)
    (lambda (xs) 
        (if (|| (null? xs) (null? (cdr xs)))
            #t
            (if (f? (car xs) (car (cdr xs)))
                ((ordered-by f?) (cdr xs))
                #f)))) 

        ;; UNIT TESTS
        (check-assert (function? ordered-by))
        (check-assert (function? (ordered-by <)))
        (check-error (ordered-by < '(1 2 3)))

        (check-assert ((ordered-by <) '()))
        (check-assert ((ordered-by <) '(1)))
        (check-assert ((ordered-by <) '(1 2 3)))
        (check-assert (not((ordered-by <) '(3 2 1))))
        (check-assert ((ordered-by >=) '(3 2 1)))
        (check-assert ((ordered-by >=) '(3 3 3)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 5


;; (max* xs) takes in a non-empty list of integers, xs and returns the 
;; maximum of the integers
   

(define max* (xs)
    (foldl (lambda (x y) (if (> x y) x y)) (car xs) (cdr xs))) 

        ;; UNIT TESTS
        (check-expect (max* '(1 2 3)) 3)
        (check-expect (max* '(3 2 1)) 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (sum xs) takes in a non-empty list of integers, xs and returns the sum
;; of the integers       

(define sum (xs)
    (foldl + (car xs) (cdr xs))) 

        ;; UNIT TESTS
        (check-expect (sum '(1 2 3)) 6)
        (check-expect (sum '(1 -2 3)) 2)
        (check-expect (sum '(1)) 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (product xs) takes in a non-empty list of integers, xs and returns the
;; product of the integers  

(define product (xs)
    (foldl * (car xs) (cdr xs))) 

        ;; UNIT TESTS
        (check-expect (product '(1 2 3)) 6)
        (check-expect (product '(-1 2 3)) -6)
        (check-expect (product '(1)) 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 6


;; (append xs ys) takes two lists xs and ys and returns the concatenation of 
;; both lists

(define append (xs ys)
    (foldr cons ys xs))

        ;; UNIT TESTS
        (check-expect (append '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
        (check-expect (append '() '()) '())
        (check-expect (append '() '(5)) '(5))
        (check-expect (append '(5) '()) '(5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (reverse xs) takes in a list xs and returns its reverse

(define reverse (xs)
    (foldl cons '() xs))

        ;; UNIT TESTS
        (check-expect (reverse '(1 2 3)) '(3 2 1))
        (check-expect (reverse '()) '())
        (check-expect (reverse '(5)) '(5))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 7


;; (map f xs) is a higher-order function that takes in a function f and a list
;; xs and applies function f to each element of xs

(define map(f xs)
    (foldr (lambda (x xs) (cons (f x) xs )) '() xs))

        ;; UNIT TESTS
        (check-expect (map ((curry +) 1) '(1 2 3)) '(2 3 4))
        (check-expect (map ((curry -) 1) '(1 2 3)) '(0 -1 -2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (filter f? xs) is a higher-order function that takes in a predicate function
;; f? and a list xs and returns a new list that only contains elements of the
;; original list that satisfy the predicate f?

(define filter (f? xs)
    (foldr (lambda (x xs)
            (if (f? x) 
                (cons x xs)
                xs))
            '() xs))

        ;; UNIT TESTS
        (check-expect (filter ((curry >) 20) '(1 2 3)) '(1 2 3))
        (check-expect (filter ((curry equal?) 10) '(8 10 30)) '(10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (exists? f? xs) is a higher-order function that takes in a predicate function
;; f? and a list xs and returns a boolean value that suggests whether an element
;; of the original list satisfies the predicate f?

(define exists? (f? xs)
    (foldr (lambda (x save_val)
            (if (f? x) 
                #t
                save_val))
            #f xs))

        ;; UNIT TESTS
        (check-assert (exists? even? '(1 2 3)))
        (check-assert (not (exists? even? '(1 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (all? f? xs) is a higher-order function that takes in a predicate function f?
;; and a list xs and returns a boolean value that suggests whether every element
;; in the original list satisfies the predicate f?

(define all? (f? xs)
    (foldr (lambda (x save_val)
            (if (not(f? x)) 
                #f
                save_val))
            #t xs))

        ;; UNIT TESTS
        (check-assert (all? even? '(2 4 6)))
        (check-assert (not (all? even? '(1 2 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 8

;; (member? x s) checks for membership of element x in set s
(define member? (x s) (s x))

;;; 8 a
;; (evens (x)) is a characteristic function representing the set of even 
;; numbers. It takes in an element x and checks whether the element is even.


(define evens (x)
    (if (number? x)
        (if (= (mod x 2) 0 ) 
            #t 
            #f)
        #f))

        ;; UNIT TESTS
        (check-assert (not (member? 'a evens)))
        (check-assert (member? 2 evens))
        (check-assert (not (member? 467 evens)))
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 8 b

;; (two-digits (x)) is a characteristic function representing the set of two- 
;; digit numbers. It takes in an element x and checks whether the element is 
;; a double digit number.        

(define two-digits (x)
    (if (number? x)
        (if (and (> x 9) (< x 100)) 
            #t 
            #f)
        #f))

        ;; UNIT TESTS
        (check-assert (not (member? 'a two-digits)))
        (check-assert (not(member? 4 two-digits)))
        (check-assert (member? 46 two-digits))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 8 c

;; properties for 8c:
;;
;;(member? x (add-element x s)) == #t
;;(member? x (add-element y s)) == (member? x s) ; where (not (equal? y x))
;;(member? x (union s1 s2))     == (or (member? x s1) (member? x s2))
;;(member? x (inter s1 s2))     == (and (member? x s1) (member? x s2))
;;(member? x (diff  s1 s2))     == (and (member? x s1) (not (member? x s2)))



;; (add-element (x s)) is a characteristic function representing a set. It takes
;; in an element x and a set s and returns a new set containg all elements in s
;; as well as x (if x is not already present in s).


(define add-element (x s)
    (if (s x)
        s
        (lambda (e) (or (equal? e x) (s e)))))

        ;; UNIT TESTS
        (check-assert (not (member? 5 (add-element 3 evens))))
        (check-assert (member? 3 (add-element 3 evens)))



;; (union (s1 s2)) is a characteristic function representing the union of two 
;; sets. It takes in two lists s1 and s2 and returns a function containing their
;; union. 


(define union (s1 s2)
    (lambda (x) (or (s1 x) (s2 x))))

;; UNIT TESTS
    (check-assert (not (member? 5 (union two-digits evens))))
    (check-assert  (member? 99 (union two-digits evens)))
    (check-assert  (member? 2 (union two-digits evens)))


;; (inter (s1 s2)) is a characteristic function representing the intersection of
;; two sets. It takes in two lists s1 and s2 and returns a function containing 
;; their intersection. 

(define inter (s1 s2)
    (lambda (x) (and (s1 x) (s2 x))))

        ;; UNIT TESTS
        (check-assert (not (member? 5 (inter two-digits evens))))
        (check-assert  (not (member? 27 (inter two-digits evens))))
        (check-assert  (member? 22 (inter two-digits evens)))

;; (diff (s1 s2)) is a characteristic function representing the difference of
;; two sets. It takes in two lists s1 and s2 and returns a function containing 
;; the difference of the two sets. 

(define diff (s1 s2)
    (lambda (x) (and (s1 x) (not (s2 x)))))

        ;; UNIT TESTS
        (check-assert (not (member? 5 (diff two-digits evens))))
        (check-assert  (not (member? 22 (diff two-digits evens))))
        (check-assert   (member? 2 (diff  evens two-digits)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;