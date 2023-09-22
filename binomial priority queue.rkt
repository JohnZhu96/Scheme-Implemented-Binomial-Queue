#lang R5RS

; This procedure, not to be used, marks where in the code you have to fill stuff in...

(define ***fill-in-your-code-here*** '())

;;;; Beginning...

(define (ints from to)
  (if (> from to)
      '()
      (cons from (ints (+ 1 from) to))))

; a tail recursive reverse

(define (reverse L)
  (rev L '()))

(define (rev L S)
  (if (null? L)
      S
      (rev (cdr L) (cons (car L) S))))

;;;; Exercise 3 melding two trees
;This procedure runs in constant time
(define (meld a b)
  (cond ((not (pair? a)) (if (> a b)
                             (cons a (cons b '()))
                             (cons b (cons a '()))))
        (else (if (> (car a) (car b))
                  (cons (car a) (cons b (cdr a)))
                  (cons (car b) (cons a (cdr b)))))))



;;;; Exercise 4  evenmeld
;This prcedure runs in linear time 
(define (evenmeld L)
  (if (null? L)
      '()
      (cons (meld (car L) (cadr L)) (evenmeld (cddr L)))))

;;;; Exercise 5 trees
; This procedure runs in linear time beacuse function evenmeld runs in linear time, and this procedure
; basically just uses evenmeld recursively. So it runs in linear time. 
(define (trees L)
  (if (null? L)
      '()
      (if (even? (length L))
          (cons '() (trees (evenmeld L)))
          (cons (car L) (trees (evenmeld (cdr L)))))))

(define (length L)
  (define (1+ x) (+ 1 x))
  (if (null? L)
      0
      (1+ (length(cdr L)))))

; If there are n elements in the queue, the number of trees depends on how many time we use evenmeld
; procedure. If we consider the worst case, that n is the exponential of 2, then the worst case of number
; of trees is log n, which means that the number of trees could be represented as O(log n)
(define (queue L)  (reverse (trees L)))


;;;; binary numbers

(define (binary n)
  (if (= n 0)
      (list 0)
      (bin n)))

(define (bin n)
  (if (= n 0)
      '()
      (if (even? n)
          (cons 0 (bin (/ n 2)))
          (cons 1 (bin (/ (- n 1) 2))))))

(define (decimal bs)
  (if (null? bs) 0 (+ (car bs) (* 2 (decimal (cdr bs))))))


;;;; Exercise 6 increment

(define (increment B)
  (if (null? B)
      (cons 1 '())
      (if (= (car B) 0)
          (cons 1 (cdr B))
          (cons 0 (increment (cdr B))))))

;;;; Exercise 7  add

(define (plus a b)
  (if (< (length a) (length b))
      (add a b 0)
      (add b a 0)))

(define (add S L c)
  (if (= c 0)
      (if (null? S)
          L
          (if (= (car S) 0)
              (cons (car L) (add (cdr S) (cdr L) 0))
              ; (car S)= 1
              (if (= (car L) 0)
                  (cons 1 (add (cdr S) (cdr L) 0))
                  ; (cdr L)= 1
                  (cons 0 (add (cdr S) (cdr L) 1)))))
      ; c= 1
      (if (null? S)
          (increment L)
          (if (= (car S) 0)
              (add S (increment L) 0)
              ; (car S)= 1
              (if (= (car L) 0)
                  (cons 0 (add (cdr S) (cdr L) 1))
                  ; (car L)= 1
                  (cons 1 (add (cdr S) (cdr L) 1)))))))

(define (check a b)
  (let ((as (binary a))
        (bs (binary b)))
    (let ((cs (plus as bs)))
      (write (list a '+ b '= (+ a b))) (newline)
      (write (list 'as '= as)) (newline)
      (write (list 'bs '= bs)) (newline)
      (write (list 'cs '= cs '=_10 (decimal cs))) (newline)
      cs)))


;;;; Exercise 8  max-queue
;This procedure runs in O(logn) since it loops through every subtree of the binomial tree
(define (max-queue Q)
  (define (max-iter Q element max)
    (if (null? Q)
        max
        (if (null? element)
            (max-iter (cdr Q) (cdr Q) max)
            (if (pair? element)
                (max-iter Q (car element) max)
                (if (> element max)
                    (max-iter (cdr Q) (cdr Q) element)
                    (max-iter (cdr Q) (cdr Q) max))))))
  (if (null? Q)
      '()
      (max-iter Q (car Q) 0)))


;;;; Exercise 9  insert
;This procedure runs in O(logn)since it loops through every subtree of the binomial tree
(define (insert x Q)
  (define (insert-helper x Q)
    (if (null? Q)
      (cons x '())
      (if (null? (car Q))
          (cons x (cdr Q))
          (cons'() (insert-helper (meld (car Q) x) (cdr Q))))))
  (reverse (insert-helper x (reverse Q))))



;;;; Exercise 10 find
;This procedure runs in O(logn) since it loops through every subtree of the binomial tree
(define (find v Q)
  (define (find-iter v element Q)
    (if (null? Q)
        '()
        (if (pair? element)
            (find-iter v (car element) Q)
            (if (equal? v element)
                (car Q)
                (find-iter v (cdr Q) (cdr Q))))))
  (if (null? Q)
      '()
      (if (not (pair? Q))
          '()
          (find-iter v (car Q) Q))))


;;;; Exercise 11 remove
;This procedure runs in O(logn) since it loops through every subtree of the binomial tree
(define (remove v Q)
  (define(remove-iter v element Q)
    (if (null? Q)
        '()
        (if (null? element)
            Q
            (if (equal? element (car Q))
                (cons '() (cdr Q))
                (cons (car Q) (remove-iter v element (cdr Q)))))))
  (define (null-check Q)
    (if (null? Q )
        '()
        (if (not (pair? Q))
            '()
            (if (not (null?(car Q)))
                Q
                (null-check (cdr Q))))))
  (null-check(remove-iter v (find v Q) Q)))


;;;; Exercise 12 merge

(define (merge Q1 Q2)
  (define (merge-helper Q1 Q2 c)
    (if (null? c)
        (if (null? Q1)
            '()
            (if (null? (car Q1))
                (cons (car Q2) (merge-helper (cdr Q1) (cdr Q2) '()))
                (if (null? (car Q2))
                    (cons (car Q1) (merge-helper (cdr Q1) (cdr Q2) '()))
                    (cons '() (merge-helper (cdr Q1) (cdr Q2) (meld (car Q1) (car Q2)))))))
        ; c is not null
        (if (null? Q1)
            (cons c '())
            (if (null? (car Q1))
                (if (null? (car Q2))
                    (cons c (merge-helper (cdr Q1) (cdr Q2) '()))
                    (cons '() (merge-helper (cdr Q1) (cdr Q2) (meld (car Q2) c))))
                (if (null? (car Q2))
                    (cons '() (merge-helper (cdr Q1) (cdr Q2) (meld (car Q1) c)))
                    (cons c (merge-helper (cdr Q1) (cdr Q2) (meld (car Q1) (car Q2)))))))))
  ;This function balance the length of two queues
  (define (balance q1 q2)
    (if (< (length q1) (length q2))
          (balance (cons '() q1) q2)
          q1))
  (reverse (merge-helper (reverse (balance Q1 Q2)) (reverse (balance Q2 Q1)) '())))

;;;; Exercise 13 remove-max

(define (remove-max Q)
      (merge (remove (max-queue Q) (find (max-queue Q) Q)) (remove (max-queue Q) Q)))


(define (test Q)
   (write Q)
   (newline)
   (if (null? Q)
       '()
       (test (remove-max Q))))
