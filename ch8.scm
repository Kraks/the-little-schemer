(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; Note
;; = is for number
;; eq? is for atom and number
;; equal? is list, atom and number

(define rember-f
  (lambda (test? a l)
    (cond ((null? l) '())
          ((test? a (car l)) (cdr l))
          (else (cons (car l) (rember-f test? a (cdr l)))))))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-salad (eq?-c 'salad))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond ((null? l) '())
            ((test? (car l) a) (cdr l))
            (else (cons (car l) ((rember-f test?) a (cdr l))))))))

(define rember-eq? (rember-f eq?))
(define rember-equal? (rember-f equal?))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond ((null? l) '())
            ((test? (car l) old)
             (cons new (cons old (cdr l))))
            (else (cons (car l) ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond ((null? l) '())
            ((test? (car l) old)
             (cons old (cons new (cdr l))))
            (else (cons (car l) ((insertR-f test?) new old (cdr l))))))))

;; My implementation of insert-g
(define insert-left
  (lambda (new old l)
    (cons new (cons old l))))
(define insert-right
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (test?)
    (lambda (insert)
      (lambda (new old l)
        (cond ((null? l) '())
              ((test? (car l) old)
               (insert new old (cdr l)))
              (else (cons (car l)
                          (((insert-g test?) insert) new old (cdr l)))))))))

(((insert-g equal?) insert-left) 'a 'b '(c d e b))
(((insert-g equal?) insert-right) 'a 'b '(c d e b))
;; My implementation of insert-g end

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond ((null? l) '())
            ((eq? (car l) old)
             (seq new old (cdr l)))
            (else (cons (car l) ((insert-g seq) new old (cdr l))))))))

(define insertL (insert-g seqL))
(define insertR (insert-g seqR))

(define insertL (insert-g 
                 (lambda (new old l)
                   (cons new (cons old l)))))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst (insert-g seqS))

(define seqrem
  (lambda (new old l) l))

(define yyy
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

;; value in ch6, evaluate prefix expression
(define ^
  (lambda (n m)
    (expt n m)))
(define operator 
  (lambda (aexp)
    (car aexp)))
(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))
(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define value
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
          ((eq? (operator nexp) '+)
           (+ (value (1st-sub-exp nexp))
              (value (2nd-sub-exp nexp))))
          ((eq? (operator nexp) '*)
           (* (value (1st-sub-exp nexp))
              (value (2nd-sub-exp nexp))))
          (else 
           (^ (value (1st-sub-exp nexp))
              (value (2nd-sub-exp nexp)))))))

(define atom-to-function
  (lambda (x)
    (cond ((eq? x '+) +)
          ((eq? x '*) *)
          (else ^))))

(define value
  (lambda (nexp)
    (cond ((atom? nexp) nexp)
          (else ((atom-to-function (operator nexp))
                 (value (1st-sub-exp nexp))
                 (value (2nd-sub-exp nexp)))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond ((null? lat) '())
            ((test? (car lat) a)
             ((multirember-f test?) a (cdr lat)))
            (else (cons (car lat)
                        ((multirember-f test?) a (cdr lat))))))))

(define multirember-eq?
  (multirember-f eq?))

(define multiremberT
  (lambda (test? lat)
    (cond ((null? lat) '())
          ((test? (car lat))
           (multiremberT test? (cdr lat)))
          (else (cons (car lat)
                      (multiremberT test? (cdr lat)))))))

(multiremberT (lambda (x) (eq? x 'a)) '(b c d a x a))

;; It looks at every atom of the lat to see whether it is
;; eq? to a. Those atoms that are not are collected in one
;; list ls1; the others for which the answer is true are collected
;; in a second list ls2. Finally, it determines the value os
;; (f ls1 ls2)

;; col is collector, sometimes called a continuation

(define multirember&co
  (lambda (a lat col)
    (cond ((null? lat)
           (col '() '()))
          ((eq? (car lat) a)
           (multirember&co a (cdr lat)
                           (lambda (newlat seen)
                             (col newlat (cons (car lat) seen)))))
          (else
           (multirember&co a (cdr lat)
                           (lambda (newlat seen)
                             (col (cons (car lat) newlat) seen)))))))

(define a-friend
  (lambda (x y)
    (null? y)))

;; expand as follows
(multirember&co 'a '(a) a-friend)
(multirember&co 'a '() (lambda (newlat seen) (a-friend newlat (cons (car '(a)) seen))))
((lambda (newlat seen) (a-friend newlat (cons (car '(a)) seen))) '() '())

;; expand as follows
(multirember&co 'a '(b a) a-friend)
(multirember&co 'a '(a) (lambda (newlat seen) (a-friend (cons (car '(b a)) newlat) seen)))
(multirember&co 'a '() (lambda (newlat seen) 
                         ((lambda (newlat seen) (a-friend (cons (car '(b a)) newlat) seen))
                          newlat
                          (cons (car '(a)) seen))))
((lambda (newlat seen) 
   ((lambda (newlat seen) (a-friend (cons (car '(b a)) newlat) seen))
    newlat
    (cons (car '(a)) seen)))
 '() '())

;; numbers except a
(multirember&co 'a '(a b c d a b a) (lambda (x y) (length x)))

;; display x
(multirember&co 'a '(a b c d a b a) (lambda (x y)
                                      (display (cons "x:" x))))

;; display y
(multirember&co 'a '(a b c d a b a) (lambda (x y)
                                      (display (cons "y:" y))))