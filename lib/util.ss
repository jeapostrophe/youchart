#lang scheme/base
(require (lib "contract.ss")
         (lib "etc.ss")
         (lib "list.ss")
         (only-in (lib "list.ss" "srfi" "1")
                  take))
(provide (all-defined-out))

(define (max* . x)
  (apply max (filter number? x)))
(define (min* . x)
  (apply min (filter number? x)))

(define list-head take)    
(define (list-splice l k e)    
  (append (list-head l k)
          (list* e (list-tail l (add1 k)))))

(define (list-len/c k c)
  (apply list/c (build-list k (lambda _ c))))