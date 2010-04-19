#lang scheme/base
(require (lib "plt-match.ss")
         (lib "contract.ss")
         (lib "etc.ss")
         (lib "list.ss")
         (lib "struct.ss")
         "data.ss"
         "util.ss")

(define cycle-end-date
  (match-lambda
    [(struct cycle (start-date _ ps))
     (add-to-a-date start-date (length ps))]))
(provide/contract
 [cycle-end-date (cycle? . -> . a-date?)])

;; Each 30 minutes adds 0.1 degree (or subtracts if below) - normalize at 8hr
(define point-norm-temp
  (match-lambda
    [(struct point (st wt fs))
     (if wt
         (inexact->exact 
          (round
           (+ wt (* 2 (- 8 st)))))
         #f)]))
(provide/contract
 [point-norm-temp (point? . -> . (or/c false/c temp/c))])

;; Bar: Max of normalized temps of size points
(define (bar oi opis ps)
  (apply max
         (map (lambda (opi) (point-norm-temp (list-ref ps (sub1 opi))))
              opis)))
(provide/contract
 [bar (integer? (list-len/c 6 integer?) (listof point?) . -> . integer?)])

;; Thumb: If an "ovulation" is followed by a drop in 0.3 degrees, then it wasn't an ov.
(define (thumb? oi opis ps)
  (define the-bar (bar oi opis ps))
  (define look-ps
    (list-tail ps oi))
  (let/ec esc
    (define count (box 0))
    (for-each (lambda (pi p)
                (define nt (point-norm-temp p))
                (when (and nt (nt . <= . the-bar))
                  (set-box! count (add1 (unbox count)))
                  (when ((unbox count) . >= . 2)
                    (esc (+ oi pi)))))
              (build-list (length look-ps) add1)
              look-ps)
    #f))
(provide/contract
 [thumb? (integer? (list-len/c 6 integer?) (listof point?) . -> . (or/c false/c integer?))])

;; Ovulation: a point 0.3 degrees higher than the last six indicates
(define (find-ovulation ps)
  (let/ec esc
    (foldl
     (match-lambda*
       [(list ip 
              (and p (struct point (st _ _)))
              (vector l 6ps))
        (define nwt (point-norm-temp p))
        (cond
          [(or (not nwt)
               (point-flag? 'thumb p))
           (vector l 6ps)]
          [(l . < . 6)
           (vector (add1 l) (list* (cons ip nwt) 6ps))]            
          [((- nwt 3) . >= . (apply max (map cdr 6ps)))
           (esc ip (map car 6ps))]
          [else
           (vector l (list* (cons ip nwt) (reverse (rest (reverse 6ps)))))])])
     (vector 0 empty)
     (build-list (length ps) add1)
     ps)
    (values #f empty)))
(provide/contract
 [find-ovulation (((listof point?))
                  . ->* . ((or/c false/c integer?) 
                           (or/c (list-len/c 6 integer?)
                                 (list/c))))])

; This is O(n^2), could be better
(define (thumb-annotate ps)
  (define-values (o ops) (find-ovulation ps))
  (cond
    [(not o)
     ps]
    [(point-flag? 'not-thumb (list-ref ps (sub1 o)))
     ps]      
    [(thumb? o ops ps)
     => (lambda (t?)
          (if t?
              (let* ([o- (sub1 o)]
                     [op (list-ref ps o-)])
                (thumb-annotate 
                 (list-splice 
                  ps o-
                  (copy-struct point op
                               [point-flags (list* 'thumb (point-flags op))]))))
              ps))]
    [else
     ps]))
(provide/contract
 [thumb-annotate ((listof point?) . -> . (listof point?))])

(define proc-cycle
  (match-lambda
    [(and c (struct cycle (start ovu ua_ps)))
     (if ovu
         c
         (let ()
           (define ps (thumb-annotate ua_ps))
           (define-values (o ops) (find-ovulation ps))
           (if o
               (make-cycle start (list* o ops) ps)
               (make-cycle start #f ps))))]))
(provide/contract
 [proc-cycle (cycle? . -> . cycle?)])

(define (extend-list l e i d-t)
  (define len (length l))
  (build-list (max len (add1 i))
              (lambda (an-i)
                (cond [(= an-i i)
                       e]
                      [(an-i . < . len)
                       (list-ref l an-i)]
                      [else
                       (d-t)]))))
(provide/contract
 [extend-list (list? any/c integer? (-> any/c) . -> . list?)])

(define (update-cycle/point cyc pt i)
  (match cyc
    [(struct cycle (start ovu ua_ps))
     (define len (length ua_ps))
     ; If it is a new point, a period, and ovulation already occurred,
     ; start a new cycle
     (if (and (i . >= . len)
              (point-flag? 'period pt)
              ovu)
         (cons 'new 
               (make-cycle (add-to-a-date (cycle-end-date cyc) (add1 (- len i)))
                           #f
                           (list pt)))
         (cons 'update
               (proc-cycle
                (make-cycle start #f
                            (extend-list ua_ps pt i
                                         (lambda () (make-point #f #f empty)))))))]))
(provide/contract
 [update-cycle/point (cycle? point? integer? . -> . (cons/c (symbols 'update 'new) cycle?))])

(define print-cycle 
  (match-lambda
    [(struct cycle (start ovu ps))
     (define-values (o ops)
       (if ovu 
           (values (first ovu) (rest ovu))
           (values #f empty)))
     (printf "Cycle Starting: ~S~n" (write-a-date start))
     (foldl 
      (match-lambda*
        [(list (and p (struct point (st wt fs)))
               (vector i d))
         (define nwt (point-norm-temp p))
         (when (and o (= i o))
           (printf "\t-----------------~n"))
         (printf "\t~a. ~a ~a ~a~a [~a]\t~S~n"
                 i (a-date-day d) 
                 (a-date-week-day d)
                 (if wt (temp->real wt) "")
                 (if (member i ops) "*" "")
                 (if nwt (temp->real nwt) "")
                 fs)
         (vector (add1 i) (add1-a-date d))])
      (vector 1 start)
      ps)
     (void)]))
(provide/contract
 [print-cycle (cycle? . -> . void)])