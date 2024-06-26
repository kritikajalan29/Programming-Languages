;;;;;;;;;;;;;;;;;;; COMP 105 SCHEME ASSIGNMENT ;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 2


;; (contig-helper? xs ys) takes in two lists xs & ys and determines whether 
;; the first list is a contiguous subsequence of the second from the beginning 
;; of the list.

;; laws: 
;; (contig-helper? '() ys) == #t
;; (contig-helper? xs '()) == #f
;; (contig-helper? (cons x xs) (cons y ys)) == (contig-helper? xs ys)
;;                                                      ; when x = y
;; (contig-helper? (cons x xs) (cons y ys)) == #f ; when x != y



(define contig-helper? (xs ys)
    

    (if (null? xs)
        #t 
        (if (null? ys)
            #f
            (if (equal? (car xs) (car ys))
                (contig-helper? (cdr xs) (cdr ys))
                #f ))))

        ;; UNIT TESTS
        (check-assert (contig-helper? '(1 2) '(1 2 3)))
        (check-assert (not(contig-helper? '(1 2 3) '(a 1 2 b 1 2 3))))



;; (contig-sublist? xs ys) takes in two lists xs & ys and determines whether 
;; the first list is a contiguous subsequence of the second by returning a 
;; boolean value.

(define contig-sublist? (xs ys)
    (if (null? xs)
        #t
        (if (null? ys)
            #f
            (if (equal? (car xs) (car ys))
                (if(contig-helper? xs ys)
                    #t
                    (contig-sublist? xs (cdr ys)))
                (contig-sublist? xs (cdr ys))))))



        ;; UNIT TESTS
        (check-assert (contig-sublist? '() '()))
        (check-assert (not(contig-sublist? 23 '())))
        (check-assert (contig-sublist? '(1 2) '(1 2 3)))
        (check-assert (contig-sublist? '(1 2) '(a b 1 2 3)))
        (check-assert (not(contig-sublist? '(1 2) '(a 1 b 2 3))))
        (check-assert (contig-sublist? '(1 2 3) '(a 1 2 b 1 2 3)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 3


;; (flatten xs) consumes a list of S‐expressions xs and erases internal brackets
;; returning a flattened list.

;; laws:
;;   (flatten '()) == '()
;;   (flatten (cons x xs)) == (cons x (flatten xs)); 
;;                          where x is an atom and not an empty list
;;   (flatten (cons x xs)) == (append (flatten x) (flatten xs)); 
;;                          where x is an atom

(define flatten (xs)
    (if (null? xs)
        '()
        (if (&& (atom? (car xs)) (not(equal? (car xs) '())))
            (cons (car xs) (flatten (cdr xs)))
            (append (flatten (car xs)) (flatten (cdr xs)))))) 

        ;; UNIT TESTS
        (check-expect(flatten '()) '())
        (check-expect(flatten '(a)) '(a))
        (check-expect (flatten '((I Ching) (U Thant) (E Coli))) 
                                                '(I Ching U Thant E Coli))
        (check-expect(flatten '(((((a)))))) '(a))
        (check-expect(flatten '((a) () ((b c) d e))) '(a b c d e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 4


;; (take n xs) expects a natural number n & a list of values xs and returns 
;; a list containing the first n elements of xs.

;; laws:
;;   (take 0 xs) == '()
;;   (take n xs) == xs ; where n >= length of xs
;;   (take n (cons x xs)) == (cons x (take (- n 1) xs)) ; where n < length of xs



(define take (n xs)
    (if (or (null? xs) (= n 0))
        '()
        (cons (car xs) (take (- n 1) (cdr xs))))) 

        ;; UNIT TESTS
        (check-expect(take 3 '()) '())
        (check-expect(take 10 '(a b c)) '(a b c))
        (check-expect(take 2 '(a b c)) '(a b))
        (check-expect(take 0 '(a b c)) '())
        



;; (drop n xs)  expects a natural number n & a list of values xs and returns 
;; the list xs without the first n elements

;; laws:
;;   (drop 0 xs) == xs
;;   (drop n xs) == '(); where n >= length of xs
;;   (drop n (cons x xs)) == (drop (- n 1) xs) ; where n < length of xs


(define drop (n xs)
    (if(or (null? xs) (= n 0))
        xs
        (drop (- n 1) (cdr xs)))) 

        ;; UNIT TESTS
        (check-expect(drop 3 '()) '())
        (check-expect(drop 0 '(a b c)) '(a b c))
        (check-expect(drop 2 '(a b c)) '(c))
        (check-expect(drop 10 '(a b c)) '())
        



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 5


;; (zip xs ys) takes in two lists xs & ys and converts the pair of lists to a 
;; list of pairs by associating corresponding values in the two lists

;; laws:
;;   (zip '() '()) == '()
;;   (zip (cons x xs) (cons y ys)) == (cons (list2 x y) (zip xs ys))


(define zip (xs ys)
    (if (&& (null? xs) (null? ys))
        '()
        (cons (list2 (car xs) (car ys)) (zip (cdr xs) (cdr ys))))) 



        ;; UNIT TESTS
        (check-expect (zip '() '()) '())
        (check-expect (zip '(1) '(a)) '((1 a)))
        (check-expect (zip '(1 2 3) '(a b c)) '((1 a) (2 b) (3 c)))




;; (ListL ps) takes in a list of pairs ps and converts the list of pairs to a 
;; singular list containing just the left elements from every pair 

;; laws:
;;   (ListL '()) == '()
;;   (ListL (cons (cons x1 x2) ys)) == (cons x (listL ys)) ; 
;;
;;   [NOTE: in the second law, ps which is a list of pairs is broken down into 
;;   the first pair depicted by (cons x1 x2) and ys is the remainder of list]

(define listL (ps)
    ( if (null? ps)
        '()
        (cons (car(car ps)) (listL (cdr ps)))))


        ;; UNIT TESTS
        (check-expect (listL '()) '())
        (check-expect (listL  '((1 a) (2 b) (3 c))) '(1 2 3))
        (check-expect (listL '((1 a)))  '(1))


;; (ListR ps) takes in a list of pairs ps and converts the list of pairs to a 
;; singular list containing just the right elements from every pair 

;; laws:
;;   (ListL '()) == '()
;;   (ListL (cons (cons x1 x2) ys)) == (append x2 (listL ys))
;;
;;   [NOTE: in the second law, ps which is a list of pairs is broken down into 
;;   the first pair depicted by (cons x1 x2) and ys is the remainder of list]

(define listR (ps)
    ( if (null? ps)
        '()
        (append  (cdr(car ps)) (listR (cdr ps)))))


        ;; UNIT TESTS
        (check-expect (listR '()) '())
        (check-expect (listR  '((1 a) (2 b) (3 c))) '(a b c))
        (check-expect (listR '((1 a)))  '(a))


;; (unzip ps) takes in a list of pairs ps and converts the list of pairs to a 
;; pair of lists containing the separate elements

(define unzip (ps)
        (list2 (listL ps) (listR ps)))

        ;; UNIT TESTS
        (check-expect (unzip '()) '(() ()))
        (check-expect (unzip  '((1 a) (2 b) (3 c))) '((1 2 3) (a b c)))
        (check-expect (unzip '((1 a)))  '((1) (a)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 6


;; (arg-max f xs) expects a function f and a nonempty list xs. It returns an 
;; element x from the list xs suxh that (f x) is as large as possible

;; laws:
;;   (arg-max f (cons x '())) == x
;;   (arg-max f (cons x xs)) == x ; when ( (f x) > (f (arg-max f xs)) )
;;   (arg-max f (cons x xs)) == (arg-max f xs) ; when ( (f x) 
;;                                                      < (f (arg-max f xs)) )



(define arg-max (f xs)
    (if (null? (cdr xs))
        (car xs)
        (let ([max (arg-max f (cdr xs))])
            (if (> (f (car xs)) (f max ))
                (car xs)
                max))))

;; UNIT TESTS
        (check-expect '(10) '(10))
        (define square (a) (* a a))
        (check-expect (arg-max square '(5 4 3 2 1)) 5)
        (check-expect(arg-max car '((105 PL) (160 Algorithms) (170 Theory))) 
                                                                '(170 Theory))