#lang scheme/base
(require (lib "list.ss")
         (lib "etc.ss")
         (lib "plt-match.ss")
         (lib "contract.ss")
         (lib "xml.ss" "xml")
         (lib "date.ss")
         "xml.ss"
         "model.ss"
         "data.ss"
         "util.ss")

(define a-date->string
  (match-lambda
    [(struct a-date (y m d))
     (parameterize ([date-display-format 'american])
       (date->string (seconds->date (find-seconds 0 0 0 d m y))))]))
(provide/contract
 [a-date->string (a-date? . -> . string?)])

; Cycle to SVG rendering
(define (point-id i p) (format "point:~a" i))
(define (point-back-id i p) (format "point:~a:back" i))
(define (point-color p)
  (define wt (point-waking-temp p))
  (cond
    [(point-flag? 'fake p) "lime"]
    [(point-flag? 'egg-white p) "mintcream"]
    [(point-flag? 'creamy p) "lightyellow"]
    [(point-flag? 'sticky p) "mistyrose"]
    [(point-flag? 'period p) "crimson"]
    [(point-flag? 'spotting p) "chocolate"]
    [(point-flag? 'dry p) "cornflowerblue"]
    [else "black"]))
(define (point-radius p)
  (cond
    [(point-flag? 'firm p) 0.25]
    [(point-flag? 'medium p) 0.35]
    [(point-flag? 'soft p) 0.45]
    [else 0.35]))

; XXX test
(define (last-temp ps)
  (define ns (filter number? (map point-waking-temp ps)))
  (if (empty? ns)
      #f
      (list-ref ns (sub1 (length ns)))))

(define on-click-js/c
  (string? integer? point? boolean? . -> . string?))
; XXX Show normalized?
(define (cycle->svg cyc
                    #:point-onclick-js point-onclick-js
                    #:add-fake-point? [add-fake-point? #t])
  (define start (cycle-start-date cyc))
  (define ovu (cycle-ovu cyc))
  (define rps (cycle-points cyc))
  (define ps 
    (if add-fake-point?
        (append rps (list (make-point 8 (last-temp rps) '(fake))))
        rps))
  (define-values (o ops)
    (if ovu 
        (values (first ovu) (rest ovu))
        (values #f empty)))       
  (define max-wt (with-handlers ([exn:fail? (lambda _ 6)]) (+ 3 (apply max* (map point-waking-temp ps)))))
  (define min-wt (with-handlers ([exn:fail? (lambda _ 0)]) (+ -3 (apply min* (map point-waking-temp ps)))))
  (define max-y (- max-wt min-wt))
  (define num-of-pts (max 40 (length ps)))
  (define axis-width 0.1)
  (define in-between-pts 1)
  (define size-y (+ axis-width max-y))
  (define size-x (+ axis-width (* in-between-pts (add1 num-of-pts))))
  (define (point-cx i)
    (* in-between-pts (add1 i)))
  (define (point-cy wt)
    (- max-y (- wt min-wt)))
  (define line+points-v
    (foldl (match-lambda*
             [(list i (and p (struct point (_ wt fs)))
                    (vector last-wt pt-ls halo-ls line-ls))
              (define wt* (or wt last-wt))
              (define id (point-id i p))
              (define back-id (point-back-id i p))
              (define pt-back-color "red")
              (define pt-radius (point-radius p))
              (define pt-color (point-color p))                     
              (define ovu? (and o (= (sub1 o) i)))
              (define the-js (point-onclick-js (point-back-id i p) i p ovu?))
              (define cx (point-cx i))
              (define cy (point-cy wt*))
              (define the-halo
                `(circle ([id ,back-id]
                          [cx ,(number->string cx)]
                          [cy ,(number->string cy)]
                          [r ,(number->string (* 1.5 pt-radius))]
                          [fill ,pt-back-color]
                          [stroke "none"]
                          [visibility "hidden"])))
              (define the-pt
                `(a ([xlink:href ,(format "javascript:~a" the-js)])
                    ,(cond
                       [(point-flag? 'thumb p)                              
                        `(g ([id ,id])
                            (g ([transform ,(format "rotate(45 ~a ~a)" cx cy)])
                               (rect ([x ,(number->string (- cx pt-radius))]
                                      [y ,(number->string (- cy (/ pt-radius 4)))]
                                      [width ,(number->string (* 2 pt-radius))]
                                      [height ,(number->string (/ pt-radius 2))]
                                      [fill ,pt-color]
                                      [stroke "none"]))
                               (rect ([x ,(number->string (- cx (/ pt-radius 4)))]
                                      [y ,(number->string (- cy pt-radius))]
                                      [width ,(number->string (/ pt-radius 2))]
                                      [height ,(number->string (* 2 pt-radius))]
                                      [fill ,pt-color]
                                      [stroke "none"])))
                            (circle ([cx ,(number->string cx)]
                                     [cy ,(number->string cy)]
                                     [r ,(number->string pt-radius)]
                                     [fill "none"]
                                     [stroke ,pt-color])))]
                       [ovu?
                        `(ellipse ([id ,id]
                                   [cx ,(number->string cx)]
                                   [cy ,(number->string cy)]
                                   [rx ,(number->string pt-radius)]
                                   [ry ,(number->string (* 1.5 pt-radius))]
                                   [fill ,pt-color]
                                   [stroke "none"]))]
                       [else
                        `(circle ([id ,id]
                                  [cx ,(number->string cx)]
                                  [cy ,(number->string cy)]
                                  [r ,(number->string pt-radius)]
                                  [fill ,pt-color]
                                  [stroke "none"]))])))
              (define the-line
                `(line ([x1 ,(number->string (point-cx (sub1 i)))]
                        [y1 ,(number->string (point-cy last-wt))]
                        [x2 ,(number->string cx)]
                        [y2 ,(number->string cy)])))
              (vector wt*
                      (if (not pt-ls)
                          (list the-pt)
                          (list* the-pt pt-ls))
                      (if (not halo-ls)
                          (list the-halo)
                          (list* the-halo halo-ls))
                      (if (not line-ls)
                          (list)
                          (list* the-line line-ls)))])
           (vector 0 #f #f #f)
           (build-list (length ps) identity)
           ps))
  (define-values (last-wt the-pts the-halos the-line)
    (apply values (vector->list line+points-v)))
  (define width-px 728)
  (define scale-x (/ width-px size-x))
  (define height-px (* scale-x size-y))
  (define scale-y (/ height-px size-y))
  (define the-svg
    `(svg ([id "chart"]
           [xmlns "http://www.w3.org/2000/svg"]
           [version "1.1"]
           [baseProfile "full"]
           ; XXX Try to get width 100%
           [height ,(format "~apx" height-px)]
           [xmlns:xlink "http://www.w3.org/1999/xlink"])
          (g ([id "cycle"]
              [transform ,(format "scale(~a ~a)" scale-x scale-y)])
             (g ([id "background"])
                (rect ([x "0"] 
                       [y "0"]
                       [fill "powderblue"]
                       [width ,(number->string size-x)]
                       [height ,(number->string size-y)]))
                (g ([id "vert-lines"])
                   ,@(map (lambda (i)
                            (define cx (point-cx (* 7 i)))
                            `(g (line ([x1 ,(number->string cx)]
                                       [y1 ,(number->string 0)]
                                       [x2 ,(number->string cx)]
                                       [y2 ,(number->string 0.5)]
                                       [stroke "white"]
                                       [fill "white"]
                                       [stroke-width "0.05"]))
                                (line ([x1 ,(number->string cx)]
                                       [y1 ,(number->string (- size-y 0.5))]
                                       [x2 ,(number->string cx)]
                                       [y2 ,(number->string size-y)]
                                       [stroke "white"]
                                       [fill "white"]
                                       [stroke-width "0.05"]))))
                          (build-list (quotient num-of-pts 7) add1)))
                (g ([id "horiz-lines"])
                   ,@(let ([n 6])
                       (map (lambda (i)
                              (define wt-i
                                (+ min-wt -1
                                   (inexact->exact
                                    (round
                                     (exact->inexact 
                                      (* (- max-wt min-wt)
                                         (/ i n)))))))
                              (define ly (point-cy wt-i))
                              `(g #;"XXX Text is not rendered in Firefox, and the size is wrong in Safari"
                                  #;(text ([x ,(number->string 0)]
                                           [y ,(number->string ly)]
                                           [fill "white"])
                                          ,(number->string (temp->real wt-i)))
                                  (line ([stroke "white"]
                                         [stroke-dasharray "0.05"]
                                         [stroke-width "0.05"]
                                         [x1 ,(number->string 0)]
                                         [y1 ,(number->string ly)]
                                         [x2 ,(number->string size-x)]
                                         [y2 ,(number->string ly)]))))
                            (build-list n add1)))))
             (g ([id "ovuline"])
                ,@(if o
                      `((line ([id "ovu-line"] 
                               [x1 ,(number->string (point-cx (- o 1.5)))]
                               [y1 ,(number->string 0)]
                               [x2 ,(number->string (point-cx (- o 1.5)))]
                               [y2 ,(number->string size-y)]
                               [fill "none"]
                               [stroke "yellow"]
                               [stroke-width "0.1"])))
                      empty))
             (g ([id "line+points"]
                 [fill "none"]
                 [stroke "black"]
                 [stroke-width "0.05"])
                ,@the-line
                ,@the-halos
                ,@the-pts)
             (g ([id "bar"])
                ,@(if o
                      (let ()
                        (define b (bar o ops ps))
                        (define b-y (point-cy (+ b .5)))
                        `((line ([id "bar-line"] 
                                 [x1 ,(number->string 0)]
                                 [y1 ,(number->string b-y)]
                                 [x2 ,(number->string size-x)]
                                 [y2 ,(number->string b-y)]
                                 [fill "none"]
                                 [stroke "purple"]
                                 [stroke-dasharray "0.5"]
                                 [stroke-width "0.05"]))))
                      empty)))))
  the-svg)
(provide
 cycle->svg)