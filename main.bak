#lang racket



(define atom?
(lambda (x)
  (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t )
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
                  (rember a (cdr lat)))))))



(define firsts
  (lambda (l)
    (cond 
      ((null? l) (quote()))
      (else (cons (car (car l)) (firsts (cdr l))))
      )))



(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) old)
               (cons old
                     (cons new (cdr lat))))
              (else (cons (car lat)
                          (insertR new old (cdr lat)))))))))



(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((eq? (car lat) old)
               (cons new (cdr lat)))
              (else (cons (car lat)
                          (subst new old
                                 (cdr lat)))))))))
                


(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((or(eq? (car lat) o1) (eq? (car lat) o2))
               (cons new (cdr lat)))
              (else (cons (car lat)
                          (subst2 new o1 o2
                                  (cdr lat)))))))))


(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a)
       (multirember a (cdr lat)))
      (else
       (cons
        (car lat)
        (multirember a
                     (cdr lat)))))))



(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else
       (cond
         ((eq? (car lat) old)
          (cons old
                (cons new
                      (multiinsertR new old
                                    (cdr lat)))))
         (else
          (cons (car lat)
                (multiinsertR new old
                              (cdr lat)))))))))


(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else
       (cond
         ((eq? (car lat) old)
          (cons new
                (cons old
                      (multiinsertL new old
                                    (cdr lat)))))
         (else
          (cons (car lat)
                (multiinsertL new old
                              (cdr lat)))))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      (else (cond
              ((eq? (car lat) old)
               (cons new
                     (multisubst new old
                                 (cdr lat))))
              (else (cons (car lat)
                          (multisubst new old
                                 (cdr lat)))))))))

; NUMBERS GAME

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define o+
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else
       (add1 (o+ a (sub1 b)))))))

; esim:
; 3 + 3
; = 1 + (3 + 2)
; = 1 + (1 + (3 + 1))
; = 1 + (1 + (1 + (3 + 0)))
; = 6    



(define o-
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else
       (o- (sub1 a) (sub1 b))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else
       (o+ (car tup) (addtup (cdr tup)))))))

(define x
  (lambda (a b)
    (cond
      ((zero? b) 0)
      (else
       (+ a (x a (sub1 b)))))))
; esim:
; 2 x 3
; = 2 + (2 x 2) 
; = 2 + (2 + (2 x 1))
; = 2 + (2 + (2 + (2 x 0)))
; = 6

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
       (cons
        (o+ (car tup1) (car tup2))
        (tup+ (cdr tup1) (cdr tup2)))))))

(define o>
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else
       (o> (sub1 n) (sub1 m))))))


(define o<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else
       (o< (sub1 n) (sub1 m))))))

(define o=
  (lambda (n m)
    (cond
      ((o> n m) #f)
      ((o< n m) #f)
      (else #t))))


(define **
  (lambda (a b)
    (cond
      ((zero? b) 1)
      (else
       (x a (** a (sub1 b)))))))



(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else
       (add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else
       (pick (sub1 n) (cdr lat))))))




(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      ((number? (car lat)) (no-nums (cdr lat)))
      (else
       (cons
        (car lat)
        (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else
       (all-nums (cdr lat))))))

(define eqan
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else
       (cond
         ((eqan (car lat) a) (add1 (occur a (cdr lat))))
         (else
          (occur a (cdr lat))))))))

(define one?
  (lambda (n)
    (= n 1)))

(define rempick
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else
       (cons
        (car lat)
        (rempick (sub1 n) (cdr lat)))))))


(define rember*
  (lambda (a l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond 
         ((eq? a (car l))
          (rember* a (cdr l)))
         (else
          (cons (car l)
                (rember* a (cdr l))))))
      (else
       (cons (rember* a (car l))
             (rember* a (cdr l)))))))
      

(define insertR*
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((atom? (car lat))
       (cond 
         ((eq? old (car lat))
          (cons old
                (cons new
                      (insertR* new old (cdr lat)))))
         (else
          (cons (car lat)
                (insertR* new old (cdr lat))))))
       (else
        (cons (insertR* new old (car lat))
              (insertR* new old (cdr lat)))))))


(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (add1 (occur* a (cdr l))))
         (else
          (occur* a (cdr l)))))
      (else
       (o+ (occur* a (car l))
          (occur* a (cdr l)))))))



(define subst*
  (lambda (new old l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new
           (subst* new old (cdr l))))
         (else
          (cons (car l)
           (subst* new old (cdr l))))))
      (else
       (cons (subst* new old (car l))
        (subst* new old (cdr l)))))))


(define insertL*
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((atom? (car lat))
       (cond 
         ((eq? old (car lat))
          (cons new
                (cons old
                      (insertL* new old (cdr lat)))))
         (else
          (cons (car lat)
                (insertL* new old (cdr lat))))))
       (else
        (cons (insertL* new old (car lat))
              (insertL* new old (cdr lat)))))))

;(insertL* "kissa" 1 '((1 (4 1 (6 (2 1 2 )) 4 1 1 1)) 1 1 1 1 3 (3) (3 (1 0))))
      
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or
             (eq? (car lat) a)
             (member? a (cdr lat)))))))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (or
        (eq? (car l) a)
        (member* a (cdr l))))
      (else (or
             (member* a (car l))
             (member* a (cdr l)))))))

       
;(leftmost '(((4 1 (6 (2 1 2 )) 4 1 1 1)) 1 1 1 1 3 (3) (3 (1 0 5))))

(+ 2 2)