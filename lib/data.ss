#lang scheme/base
(require (lib "date.ss")
         (lib "list.ss")
         (lib "etc.ss")
         (lib "contract.ss")
         (lib "plt-match.ss"))

(define-struct a-date (year month day) #:mutable)
(define a-date-week-day
  (match-lambda
    [(struct a-date (y m d))
     (date-week-day (seconds->date (find-seconds 0 0 0 d m y)))]))
(define add1-a-date
  (match-lambda
    [(struct a-date (y m d))
     (define s0 (find-seconds 0 0 0 d m y))
     (define s0+1 (+ s0 (* 60 60 24)))
     (define d1 (seconds->date s0+1))
     (make-a-date (date-year d1)
                  (date-month d1)
                  (date-day d1))]))
(define (add-to-a-date a-date n)
  (foldl (lambda (e a)
           (add1-a-date a))
         a-date
         (build-list n identity)))
(define read-a-date
  (match-lambda
    [(list ys ms ds)
     (make-a-date ys ms ds)]))
(define write-a-date
  (match-lambda
    [(struct a-date (y m d))
     (list y m d)]))
(provide/contract
 [a-date-week-day (a-date? . -> . (integer-in 0 6))]
 [add1-a-date (a-date? . -> . a-date?)]
 [add-to-a-date (a-date? integer? . -> . a-date?)]
 [read-a-date (list? . -> . a-date?)]
 [write-a-date (a-date? . -> . list?)]
 [struct a-date ([year integer?]
                 [month (integer-in 1 21)]
                 [day (integer-in 1 31)])])

(define sleep-time/c (real-in 3 24))
(provide/contract
 [sleep-time/c contract?])

(define temp/c (integer-in 0 200))
(define real-temp/c (real-in 90.0 110.0))
(define (temp->real t)
  (+ 90.0 (/ t 10)))
(define (real->temp r)
  (inexact->exact (round (* (- r 90.0) 10))))
(provide/contract
 [temp/c contract?]
 [temp->real (temp/c . -> . real-temp/c)]
 [real->temp (real-temp/c . -> . temp/c)])

(define other-flags (list 'pregnancy-test 'intercourse 'fake))
(define other-flag/c (apply symbols other-flags))
(define thumbs (list 'thumb 'not-thumb))
(define thumbs/c (apply symbols thumbs))
(define cervical-fluids (list 'egg-white 'creamy 'sticky 'period 'spotting 'dry))
(define cervical-fluid/c (apply symbols cervical-fluids))
(define cervix-positions (list 'firm 'medium 'soft))
(define cervix-position/c (apply symbols cervix-positions))
(define flag/c (or/c thumbs/c cervix-position/c cervical-fluid/c other-flag/c string?))

; XXX only one of fluids, positions, etc
(define (valid-flags? fs)
  (and (list? fs)
       (let loop ([fs fs]
                  [seen empty])
         (if (empty? fs)
             #t
             (let ([f (first fs)])
               (if (or (not (contract-first-order-passes? flag/c f))
                       (member f seen))
                   #f
                   (loop (rest fs)
                         (if (or (eq? f 'thumb) (eq? f 'not-thumb))
                             (list* 'thumb 'not-thumb seen)
                             (list* f seen)))))))))
(provide/contract
 [valid-flags? (any/c . -> . boolean?)])

(define-struct point (sleep-time waking-temp flags) #:mutable)
(define point-flag?
  (match-lambda*
    [(list f (struct point (st wt fs)))
     (and (member f fs) #t)]))
(define point-notes
  (match-lambda
    [(struct point (st wt fs))
     (match (filter string? fs)
       [(list)
        #f]
       [(list notes)
        notes])]))
(define read-point
  (match-lambda
    [(list sts wts (list fs ...))
     (make-point sts wts fs)]))
(define write-point
  (match-lambda
    [(struct point (st wt fs))
     (list st wt fs)]))
(provide/contract
 [read-point (list? . -> . point?)]
 [write-point (point? . -> . list?)]
 [thumbs (listof thumbs/c)]
 [other-flags (listof other-flag/c)]
 [cervical-fluids (listof cervical-fluid/c)]
 [cervix-positions (listof cervix-position/c)]
 [point-notes (point? . -> . (or/c false/c string?))]
 [point-flag? (flag/c point? . -> . boolean?)]
 [struct point ([sleep-time (or/c false/c sleep-time/c)]
                [waking-temp (or/c false/c temp/c)]
                [flags valid-flags?])])

(define-struct cycle (start-date ovu points) #:mutable)
(define read-cycle
  (match-lambda
    [(list ds os (list ps ...))
     (make-cycle (read-a-date ds)
                 os
                 (map read-point ps))]))
(define write-cycle
  (match-lambda
    [(struct cycle (d os ps))
     (list (write-a-date d)
           os
           (map write-point ps))]))
(provide/contract
 [read-cycle (list? . -> . cycle?)]
 [write-cycle (cycle? . -> . list?)]
 [struct cycle ([start-date a-date?]
                [ovu (or/c false/c (cons/c integer? (listof integer?)))]
                [points (listof point?)])])

; XXX test
(define (empty-cycle/today)
  (define td (seconds->date (current-seconds)))
  (define today
    (make-a-date (date-year td)
                 (date-month td)
                 (date-day td)))
  (make-cycle today #f empty))
(provide/contract
 [empty-cycle/today (-> cycle?)])