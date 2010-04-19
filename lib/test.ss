#lang scheme/base
(require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
         (lib "etc.ss")
         (lib "list.ss")
         "data.ss"
         "model.ss"
         "util.ss"
         "render.ss"
         "view.ss"
         "xml.ss"
         "db.ss")
(provide tests)

(define test-db (build-path (this-expression-source-directory) 'up "db"))
(define (test-data i) 
  (with-input-from-file (build-path test-db "test" (format "chart~a.ss" i))
    read))

(define-syntax vwrap
  (syntax-rules ()
    [(_ e)
     (call-with-values
      (lambda () e)
      (lambda x x))]))

(define tests
  (test-suite
   "YouChart Tests"
   
   (test-suite
    "util.ss"
    (test-equal? "max*" (max* 1 2 3 #f 4) 4)
    (test-equal? "min*" (min* 1 2 3 #f 4) 1)
    (test-equal? "list-splice" (list-splice (list 1 2 3) 1 4) (list 1 4 3)))
   
   (test-suite
    "xml.ss"
    (test-suite
     "js-id"
     (test-equal? "foo-bar" (js-id 'foo-bar) 'foo_bar)
     (test-equal? "foo_bar" (js-id 'foo_bar) 'foo_bar)
     (test-equal? "dna" (js-id 'dna) 'dna)))
   
   (test-suite
    "data.ss"
    
    (test-suite
     "valid-flags?"
     (test-true "simple" (valid-flags? '(egg-white)))
     (test-false "duplicate" (valid-flags? '(egg-white egg-white)))
     (test-false "thumb and not-thumb" (valid-flags? '(thumb not-thumb))))
    
    (test-suite
     "a-date-week-day"
     (test-equal? "`(2007 06 23)" (a-date-week-day (read-a-date `(2007 06 23))) 6)
     (test-equal? "`(2007 05 31)" (a-date-week-day (read-a-date `(2007 05 31))) 4)
     (test-equal? "`(2006 12 31)" (a-date-week-day (read-a-date `(2006 12 31))) 0)
     (test-equal? "`(2008 2 28)" (a-date-week-day (read-a-date `(2008 2 28))) 4))
    
    (test-suite
     "add1-a-date"
     (test-equal? "normal"
                  (write-a-date (add1-a-date (read-a-date `(2007 06 23))))
                  `(2007 06 24))
     (test-equal? "month end"
                  (write-a-date (add1-a-date (read-a-date `(2007 05 31))))
                  `(2007 06 1))
     (test-equal? "year end"
                  (write-a-date (add1-a-date (read-a-date `(2006 12 31))))
                  `(2007 01 01))
     (test-equal? "leap year (pt 1)"
                  (write-a-date (add1-a-date (read-a-date `(2008 2 28))))
                  `(2008 2 29))
     (test-equal? "leap year (pt 2)"
                  (write-a-date (add1-a-date (read-a-date `(2008 2 29))))
                  `(2008 3 1)))
    
    (test-suite
     "add-to-a-date"
     (test-equal? "1"
                  (write-a-date (add-to-a-date (read-a-date `(2007 06 23)) 1))
                  `(2007 06 24))
     (test-equal? "5"
                  (write-a-date (add-to-a-date (read-a-date `(2007 06 23)) 5))
                  `(2007 06 28))
     (test-equal? "leap year"
                  (write-a-date (add-to-a-date (read-a-date `(2008 2 29)) 5))
                  `(2008 3 5)))
    
    (test-suite
     "temp->real and real->temp"
     (test-exn "temp->real neg" exn:fail:contract? (lambda () (temp->real -1)))
     (test-exn "temp->real too-high" exn:fail:contract? (lambda () (temp->real 300)))
     (test-equal? "0 -> 90.0" (temp->real 0) 90.0)
     (test-equal? "10 -> 91.0" (temp->real 10) 91.0)
     (test-equal? "35 -> 93.5" (temp->real 35) 93.5)
     (test-exn "real->temp too-low" exn:fail:contract? (lambda () (real->temp 88.0)))
     (test-exn "real->temp too-high" exn:fail:contract? (lambda () (real->temp 120.0)))
     (test-equal? "90.0 -> 0" (real->temp 90.0) 0)
     (test-equal? "91.0 -> 10" (real->temp 91.0) 10)
     (test-equal? "93.5 -> 35" (real->temp 93.5) 35)
     (test-equal? "93.533 -> 35" (real->temp 93.533) 35))
    
    (test-suite
     "points"
     (test-false "thumb (f)" (point-flag? 'thumb (make-point 8 5 (list))))
     (test-true "thumb" (point-flag? 'thumb (make-point 8 5 (list 'thumb)))))
    
    (test-suite
     "notes"
     (test-false "none" (point-notes (make-point 8 5 (list))))
     (test-equal? "some" (point-notes (make-point 8 5 (list 'thumb "Notes")))
                  "Notes"))
    
    (test-suite
     "read-cycle & write-cycle"
     (test-not-exn "Test data 0 is readable"
                   (lambda () (read-cycle (test-data 0))))
     (test-equal? "Identity"
                  (write-cycle (read-cycle (test-data 0)))
                  (test-data 0))
     (test-not-exn "Test data 1 is readable"
                   (lambda () (read-cycle (test-data 1))))
     (test-not-exn "Test data 2 is readable"
                   (lambda () (read-cycle (test-data 2))))))
   
   (test-suite
    "model.ss"
    
    (test-suite
     "cycle-end-date"
     (test-equal? "simple"
                  (write-a-date 
                   (cycle-end-date
                    (read-cycle
                     '([2007 05 05] 
                       (35 34 33 32 31 30 29)
                       ((8 77 (period))
                        (8 75 (period))
                        (8 71 (period))
                        (8 72 (period))
                        (8 69 (period))
                        (8 71 (period))
                        (8 69 (period))
                        (8 70 ())
                        (8 72 ())
                        (8 70 ())
                        (8 70 ())
                        (8 70 ())
                        (8 69 ())
                        (8 69 ())
                        (#f #f ())
                        (8 69 ())
                        (8 72 ())
                        (8 71 ())
                        (8 67 (spotting))
                        (8 73 (spotting))
                        (8 65 (spotting))
                        (8 75 (spotting))
                        (8 75 (spotting))
                        (8 78 (spotting))
                        (8 75 (spotting))
                        (8 66 (spotting))
                        (8 73 (spotting))
                        (8 63 (spotting))
                        (8 70 (spotting))
                        (8 70 ())
                        (8 63 (creamy))
                        (8 68 (creamy))
                        (8 67 (egg-white))
                        (8 67 (egg-white))
                        (8 74 (egg-white))
                        (8 77 ())
                        (8 74 ())
                        (8 77 ())
                        (8 77 ())
                        (8 76 ()))))))
                  '[2007 06 14]))
    
    (test-suite
     "point-norm-temp"
     (test-equal? "no temp" (point-norm-temp (make-point 8 #f '())) #f)
     (test-equal? "no normalization" (point-norm-temp (make-point 8 70 '())) 70)
     (test-equal? "below" (point-norm-temp (make-point 7.5 69 '())) 70)
     (test-equal? "above" (point-norm-temp (make-point 8.5 71 '())) 70))
    
    (test-suite
     "bar"
     (test-equal? "does not apply"
                  (bar 7
                       (list 6 5 4 3 2 1)
                       (list (make-point 8 70 '()) ;1
                             (make-point 8 70 '()) ;2
                             (make-point 8 70 '()) ;3
                             (make-point 8 70 '()) ;4
                             (make-point 8 70 '()) ;5
                             (make-point 8 70 '()) ;6
                             (make-point 8 76 '()) ;7
                             (make-point 8 73 '())))
                  70)
     (test-equal? "simple"
                  (bar 7
                       (list 6 5 4 3 2 1)
                       (list (make-point 8 70 '()) ;1
                             (make-point 8 70 '()) ;2
                             (make-point 8 74 '()) ;3
                             (make-point 8 70 '()) ;4
                             (make-point 8 76 '()) ;5
                             (make-point 8 70 '()) ;6
                             (make-point 8 76 '(thumb)) ;7
                             (make-point 8 70 '()) ;8
                             (make-point 8 70 '()) ;9
                             (make-point 8 73 '())))
                  76))
    
    (test-suite
     "thumb?"
     (test-equal? "does not apply"
                  (thumb? 7
                          (list 6 5 4 3 2 1)
                          (list (make-point 8 70 '()) ;1
                                (make-point 8 70 '()) ;2
                                (make-point 8 70 '()) ;3
                                (make-point 8 70 '()) ;4
                                (make-point 8 70 '()) ;5
                                (make-point 8 70 '()) ;6
                                (make-point 8 76 '()) ;7
                                (make-point 8 73 '())))
                  #f)
     (test-equal? "simple"
                  (thumb? 7
                          (list 6 5 4 3 2 1)
                          (list (make-point 8 70 '()) ;1
                                (make-point 8 70 '()) ;2
                                (make-point 8 70 '()) ;3
                                (make-point 8 70 '()) ;4
                                (make-point 8 70 '()) ;5
                                (make-point 8 70 '()) ;6
                                (make-point 8 76 '(thumb)) ;7
                                (make-point 8 70 '()) ;8
                                (make-point 8 70 '()) ;9
                                (make-point 8 73 '())))
                  9))
    
    (test-suite
     "find-ovulation"
     (test-equal? "not enough data"
                  (vwrap
                   (find-ovulation (list (make-point 8 70 '())
                                         (make-point 8 70 '())
                                         (make-point 8 70 '()))))
                  (list #f empty))
     (test-equal? "simple"
                  (vwrap
                   (find-ovulation (list (make-point 8 70 '()) ;1
                                         (make-point 8 70 '()) ;2
                                         (make-point 8 70 '()) ;3
                                         (make-point 8 70 '()) ;4
                                         (make-point 8 70 '()) ;5
                                         (make-point 8 70 '()) ;6
                                         (make-point 8 73 '()))))
                  (list 7 (list 6 5 4 3 2 1)))
     (test-equal? "normalized"
                  (vwrap
                   (find-ovulation (list (make-point 8 70 '()) ;1
                                         (make-point 8 70 '()) ;2
                                         (make-point 8 70 '()) ;3
                                         (make-point 8 70 '()) ;4
                                         (make-point 8 70 '()) ;5
                                         (make-point 8 70 '()) ;6
                                         (make-point 7 71 '()))))
                  (list 7 (list 6 5 4 3 2 1)))
     (test-equal? "too much data"
                  (vwrap
                   (find-ovulation (list (make-point 8 70 '()) ;1
                                         (make-point 8 70 '()) ;2
                                         (make-point 8 70 '()) ;3
                                         (make-point 8 70 '()) ;4
                                         (make-point 8 70 '()) ;5
                                         (make-point 8 70 '()) ;6
                                         (make-point 8 73 '()) ;7
                                         (make-point 8 76 '()))))
                  (list 7 (list 6 5 4 3 2 1)))
     (test-equal? "thumb"
                  (vwrap
                   (find-ovulation (list (make-point 8 70 '()) ;1
                                         (make-point 8 70 '()) ;2
                                         (make-point 8 70 '()) ;3
                                         (make-point 8 70 '()) ;4
                                         (make-point 8 70 '()) ;5
                                         (make-point 8 70 '()) ;6
                                         (make-point 8 76 '(thumb)) ;7
                                         (make-point 8 70 '()) ;8
                                         (make-point 8 70 '()) ;9
                                         (make-point 8 73 '()))))
                  (list 10 (list 9 8 6 5 4 3))))
    
    (test-suite
     "thumb-annotate"
     (let ([ex (list (make-point 8 70 '()) ;1
                     (make-point 8 70 '()) ;2
                     (make-point 8 70 '()) ;3
                     (make-point 8 70 '()) ;4
                     (make-point 8 70 '()) ;5
                     (make-point 8 70 '()) ;6
                     (make-point 8 76 '()) ;7
                     (make-point 8 73 '()))])
       (test-equal? "does not apply"
                    (thumb-annotate ex)
                    ex))
     (test-equal? "applies"
                  (point-flags 
                   (list-ref (thumb-annotate (list (make-point 8 70 '()) ;1
                                                   (make-point 8 70 '()) ;2
                                                   (make-point 8 70 '()) ;3
                                                   (make-point 8 70 '()) ;4
                                                   (make-point 8 70 '()) ;5
                                                   (make-point 8 70 '()) ;6
                                                   (make-point 8 76 '()) ;7
                                                   (make-point 8 70 '()) ;8
                                                   (make-point 8 70 '()) ;9
                                                   (make-point 8 73 '())))
                             6))
                  '(thumb))
     (let ([ex (list (make-point 8 70 '()) ;1
                     (make-point 8 70 '()) ;2
                     (make-point 8 70 '()) ;3
                     (make-point 8 70 '()) ;4
                     (make-point 8 70 '()) ;5
                     (make-point 8 70 '()) ;6
                     (make-point 8 76 '(not-thumb)) ;7
                     (make-point 8 70 '()) ;8
                     (make-point 8 70 '()) ;9
                     (make-point 8 73 '()))])
       (test-equal? "applies but not-thumb"
                    (thumb-annotate ex)
                    ex)))
    
    (test-suite
     "proc-cycle"
     (test-equal? "test-data 0 => 1"
                  (write-cycle (proc-cycle (read-cycle (test-data 0))))
                  (test-data 1))
     (test-equal? "test-data 1 => 1"
                  (write-cycle (proc-cycle (read-cycle (test-data 1))))
                  (test-data 1)))
    
    (test-suite
     "extend-list"
     (test-equal? "simple" 
                  (extend-list empty 0 0 (lambda () #f))
                  (list 0))
     (test-equal? "carry"
                  (extend-list (list 0) 1 1 (lambda () #f))
                  (list 0 1))
     (test-equal? "replace"
                  (extend-list (list 0 #f) 1 1 (lambda () #f))
                  (list 0 1))
     (test-equal? "extend"
                  (extend-list empty 4 4 (lambda () #f))
                  (list #f #f #f #f 4))
     (test-equal? "extend and carry"
                  (extend-list (list 0) 4 4 (lambda () #f))
                  (list 0 #f #f #f 4)))
    
    (let ()
      (define (test-update-cycle/point cyc pt i)
        (define ans (update-cycle/point cyc pt i))
        (cons (car ans) (write-cycle (cdr ans))))
      (test-suite
       "update-cycle/point"
       (test-equal? "thumb"
                    (test-update-cycle/point 
                     (read-cycle
                      '([2007 05 05] 
                        #f
                        ((8 77 (period))
                         (8 75 (period))
                         (8 71 (period))
                         (8 72 (period))
                         (8 69 (period))
                         (8 71 (period))
                         (8 69 (period))
                         (8 70 ())
                         (8 72 ())
                         (8 70 ())
                         (8 70 ())
                         (8 70 ())
                         (8 69 ())
                         (8 69 ())
                         (#f #f ())
                         (8 69 ())
                         (8 72 ())
                         (8 71 ())
                         (8 67 (spotting))
                         (8 73 (spotting))
                         (8 65 (spotting))
                         (8 75 (spotting))
                         (8 75 (spotting))
                         (8 78 (spotting))
                         (8 75 (spotting)))))
                     (read-point '(8 66 (spotting)))
                     25)
                    (cons 'update
                          '([2007 05 05] 
                            #f
                            ((8 77 (period))
                             (8 75 (period))
                             (8 71 (period))
                             (8 72 (period))
                             (8 69 (period))
                             (8 71 (period))
                             (8 69 (period))
                             (8 70 ())
                             (8 72 ())
                             (8 70 ())
                             (8 70 ())
                             (8 70 ())
                             (8 69 ())
                             (8 69 ())
                             (#f #f ())
                             (8 69 ())
                             (8 72 ())
                             (8 71 ())
                             (8 67 (spotting))
                             (8 73 (spotting))
                             (8 65 (spotting))
                             (8 75 (spotting))
                             (8 75 (spotting))
                             (8 78 (thumb spotting))
                             (8 75 (spotting))
                             (8 66 (spotting))))))
       (test-equal? "ovulation"
                    (test-update-cycle/point 
                     (read-cycle
                      '([2007 05 05] 
                        #f
                        ((8 77 (period))
                         (8 75 (period))
                         (8 71 (period))
                         (8 72 (period))
                         (8 69 (period))
                         (8 71 (period))
                         (8 69 (period))
                         (8 70 ())
                         (8 72 ())
                         (8 70 ())
                         (8 70 ())
                         (8 70 ())
                         (8 69 ())
                         (8 69 ())
                         (#f #f ())
                         (8 69 ())
                         (8 72 ())
                         (8 71 ())
                         (8 67 (spotting))
                         (8 73 (spotting))
                         (8 65 (spotting))
                         (8 75 (spotting))
                         (8 75 (spotting))
                         (8 78 (thumb spotting))
                         (8 75 (spotting))
                         (8 66 (spotting))
                         (8 73 (spotting))
                         (8 63 (spotting))
                         (8 70 (spotting))
                         (8 70 ())
                         (8 63 (creamy))
                         (8 68 (creamy))
                         (8 67 (egg-white))
                         (8 67 (egg-white)))))
                     (read-point '(8 74 (egg-white)))
                     34)
                    (cons 'update
                          '([2007 05 05] 
                            (35 34 33 32 31 30 29)
                            ((8 77 (period))
                             (8 75 (period))
                             (8 71 (period))
                             (8 72 (period))
                             (8 69 (period))
                             (8 71 (period))
                             (8 69 (period))
                             (8 70 ())
                             (8 72 ())
                             (8 70 ())
                             (8 70 ())
                             (8 70 ())
                             (8 69 ())
                             (8 69 ())
                             (#f #f ())
                             (8 69 ())
                             (8 72 ())
                             (8 71 ())
                             (8 67 (spotting))
                             (8 73 (spotting))
                             (8 65 (spotting))
                             (8 75 (spotting))
                             (8 75 (spotting))
                             (8 78 (thumb spotting))
                             (8 75 (spotting))
                             (8 66 (spotting))
                             (8 73 (spotting))
                             (8 63 (spotting))
                             (8 70 (spotting))
                             (8 70 ())
                             (8 63 (creamy))
                             (8 68 (creamy))
                             (8 67 (egg-white))
                             (8 67 (egg-white))
                             (8 74 (egg-white))))))
       (test-equal? "period"
                    (test-update-cycle/point 
                     (read-cycle
                      '([2007 05 05] 
                        (35 34 33 32 31 30 29)
                        ((8 77 (period))
                         (8 75 (period))
                         (8 71 (period))
                         (8 72 (period))
                         (8 69 (period))
                         (8 71 (period))
                         (8 69 (period))
                         (8 70 ())
                         (8 72 ())
                         (8 70 ())
                         (8 70 ())
                         (8 70 ())
                         (8 69 ())
                         (8 69 ())
                         (#f #f ())
                         (8 69 ())
                         (8 72 ())
                         (8 71 ())
                         (8 67 (spotting))
                         (8 73 (spotting))
                         (8 65 (spotting))
                         (8 75 (spotting))
                         (8 75 (spotting))
                         (8 78 (spotting))
                         (8 75 (spotting))
                         (8 66 (spotting))
                         (8 73 (spotting))
                         (8 63 (spotting))
                         (8 70 (spotting))
                         (8 70 ())
                         (8 63 (creamy))
                         (8 68 (creamy))
                         (8 67 (egg-white))
                         (8 67 (egg-white))
                         (8 74 (egg-white))
                         (8 77 ())
                         (8 74 ())
                         (8 77 ())
                         (8 77 ())
                         (8 76 ()))))
                     (read-point '(8 77 (period)))
                     40)
                    (cons 'new
                          '([2007 06 15] 
                            #f
                            ((8 77 (period))))))))
    
    (test-suite
     "print-cycle"
     (test-not-exn "test-data 0" (lambda () (print-cycle (read-cycle (test-data 0)))))
     (test-not-exn "test-data 1" (lambda () (print-cycle (read-cycle (test-data 1)))))))
   
   (test-suite
    "view.ss")
   
   (test-suite
    "render.ss"
    (test-suite
     "cycle->svg"
     (test-not-exn 
      "empty" 
      (lambda () 
        (cycle->svg (make-cycle (read-a-date '(2007 02 01)) #f empty)
                    (lambda _ "#"))))))
   
   (test-suite
    "db.ss"
    (test-suite
     "read-user-cycle"
     (test-equal? "test-data 0" 
                  (write-cycle (read-user-cycle test-db "test" 0))
                  (write-cycle (read-cycle (test-data 0)))))
    ; XXX
    (test-suite
     "write-user-cycle")      
    (test-suite
     "user-cycles"
     (test-equal? "test" (user-cycles test-db "test") (list 0 1 2 3)))
    (test-suite
     "user-cycle-exists?"
     (test-true "0" (user-cycle-exists? test-db "test" 0))
     (test-true "3" (user-cycle-exists? test-db "test" 3))
     (test-false "4" (user-cycle-exists? test-db "test" 4)))
    (test-suite
     "user-latest-cycle"
     (test-equal? "test" (user-latest-cycle test-db "test") 3)))))