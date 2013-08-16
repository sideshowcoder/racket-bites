#lang racket

(define (qsort lst)
  (match lst
         ['() '()]
         [(cons pivot rst)
          (append
            (qsort (filter (lambda (e) (< e pivot)) rst))
            (list pivot)
            (qsort (filter (lambda (e) (> e pivot)) rst)))]))

(define (msort lst)
  (match lst
         [(or '() (list _)) lst]
         [_ (let-values ([(left right) (split-at lst (quotient (length lst) 2))])
              (merge (msort left) (msort right)))]))

(define (merge left right)
  (cond [(empty? left) right]
        [(empty? right) left]
        [(match* (left right)
                 [((list* l ls) (list* r rs))
                  (cond [(<= l r) (cons l (merge ls right))]
                        [         (cons r (merge left rs))])])]))


(require rackunit)

(check-equal? (qsort '()) '() "'() is sorted")
(check-equal? (qsort '(1)) '(1) "'(1) is sorted")
(check-equal? (qsort '(2 1)) '(1 2))
(check-equal? (qsort '(12 9 2321 2 23 123 1 4 5 6 99))
              '(1 2 4 5 6 9 12 23 99 123 2321))

(check-equal? (msort '()) '() "'() is sorted")
(check-equal? (msort '(1)) '(1) "'(1) is sorted")
(check-equal? (msort '(2 1)) '(1 2))
(check-equal? (msort '(12 9 2321 2 23 123 1 4 5 6 99))
              '(1 2 4 5 6 9 12 23 99 123 2321))
