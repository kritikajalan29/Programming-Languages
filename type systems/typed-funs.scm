;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;          DROP         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (drop n xs)  expects a natural number n & a list of values xs and returns 
;; the list xs without the first n elements

;; laws:
;;   (drop 0 xs) == xs
;;   (drop n xs) == '(); where n >= length of xs
;;   (drop n (cons x xs)) == (drop (- n 1) xs) ; where n < length of xs

(val drop
    (type-lambda ['a]
        (letrec [([drop-mono : (int (list 'a) -> (list 'a))]
                    (lambda ([n : int] [xs : (list 'a)])
                        (if (or ((@ null? 'a) xs) ((@ = int) n 0))
                            xs
                            (drop-mono (- n 1) ((@ cdr 'a) xs)))))] drop-mono)))

    ;; UNIT TEST
    (check-expect ((@ drop sym) 4 '(a b c d)) (@ '() sym))
    (check-expect ((@ drop bool) 0 '(#t #f #t)) '(#t #f #t))
    (check-expect ((@ drop int) 2 '(1 2 3 4 5)) '(3 4 5))
    (check-type drop (forall ['a] (int (list 'a) -> (list 'a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       TAKEWHILE      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (takewhile f? xs) takes in a predicate function f? and a list xs and returns
;; the longest prefix of the list in which every element satisfies the predicate

;; laws:
;;   (takewhile f? '()) == '()
;;   (takewhile f? (cons x xs)) == '() ; where (not (f x))
;;   (takewhile f? (cons x xs)) == (cons x (takewhile (f? xs))) ; where (f x)
;;   


(val takewhile 
    (type-lambda ['a]
        (letrec [([takewhile-mono : (('a -> bool) (list 'a) -> (list 'a))]
                    (lambda ([f? : ('a -> bool)] [xs : (list 'a)])
                        (if  ((@ null? 'a) xs) 
                            (@ '() 'a)
                            (if (not(f? ((@ car 'a) xs)))
                                (@ '() 'a)
                                ((@ cons 'a) ((@ car 'a) xs) (takewhile-mono f?
                                ((@ cdr 'a) xs)))))))] takewhile-mono )))
                        
                            
                         
    ;; UNIT TEST               
    (define bool even? ([x : int]) ((@ = int) (mod x 2) 0))
    (check-expect ((@ takewhile int) even? (@ '() int)) (@ '() int))
    (check-expect ((@ takewhile int) even? '(2 4 6 7 8 10 12)) '(2 4 6))
    (check-expect ((@ takewhile int) (lambda ([x : int]) ((@ = int) x 2)) 
                                                '(2 2 2 2 3)) '(2 2 2 2))
    (check-type takewhile (forall ['a] (('a -> bool) (list 'a) -> (list 'a))))
