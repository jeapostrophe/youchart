#lang scheme/base
(require (lib "list.ss")
         (lib "etc.ss")
         (lib "plt-match.ss")
         (lib "contract.ss")
         (lib "url.ss" "net")
         (lib "xml.ss" "xml")
         (lib "request-structs.ss" "web-server" "private")
         "xml.ss"
         "model.ss"
         "data.ss"
         "util.ss"
         "render.ss")

(define (template #:center [center empty]
                  #:right [right empty])
  `(html ([xmlns "http://www.w3.org/1999/xhtml"]
          [xml:lang "en"]
          [lang "en"])
         (head (meta ([http-equiv "Content-Type"]
                      [content "application/xhtml+xml; charset=UTF-8"]))
               (link ([rel "stylesheet"] [type "text/css"] [href "style.css"] [media "all"]))
               (title "YouChart"))
         (body
          (div ([id "header"]) 
               "YouChart"
               (div ([style "float: right; padding-right: 1em;"])
                    (a ([href "http://astore.amazon.com/jaymccarthy-20"]) "Store")))
          (div ([id "container"])
               (div ([id "center"] [class "column"])
                    ,@center
                    (div ([id "cfoot"]) (img ([src "leaderboard.gif"]))))
               #;(div ([id "left"] [class "column"]) nbsp)
               (div ([id "right"] [class "column"])
                    ,@right))
          (div ([id "footer-wrapper"])
               (div ([id "footer"])
                    "Copyright 2007 Jay and Libby McCarthy")))))

(define (login-page
         #:login-url login-url)
  (template #:center
            `((form ([method "post"]
                     [action ,(url->string login-url)])
                    "Username:" (input ([type "text"] [name "user"] [value ""])) (br)
                    "Password:" (input ([type "password"] [name "pass"] [value ""]))
                    (input ([type "submit"] [value "Login"]))))))
(provide login-page)

(define (login-request->values req)
  (define bs (request-bindings/raw req))
  (define (g i) (bytes->string/utf-8 (binding:form-value (bindings-assq i bs))))
  (values (g #"user") (g #"pass")))
(provide/contract
 [login-request->values ((request?) . ->* . (string? string?))])

(define (cycle->xhtml cycle
                      #:prev-cycle-url [prev-cycle-url #f]
                      #:next-cycle-url [next-cycle-url #f]
                      #:new-cycle-url [new-cycle-url #f]
                      #:save-point-url save-point-url)
  (define current-day-n (length (cycle-points cycle)))
  (define (point-onclick-js point-back-id i pt ovu?)
    (define wt (point-waking-temp pt))    
    (define st (point-sleep-time pt))
    (define n (point-notes pt))
    (define date (a-date->string (add-to-a-date (cycle-start-date cycle) i)))
    (define (select-help l)
      (let* ([seen? (box #f)]
             [known
              (map (lambda (i) (if (point-flag? i pt) (begin (set-box! seen? #t) #t) #f))
                   l)])
        (list* (not (unbox seen?)) known)))
    (javascript/string
     `(point_onclick 
       ,point-back-id
       ,i ,date
       ,@(select-help cervical-fluids)
       ,@(select-help cervix-positions)
       ,@(select-help thumbs)
       ,@(map (lambda (i) (point-flag? i pt))
              other-flags)
       ,ovu?
       ,(if n n "")
       ,(if wt (number->string (temp->real wt)) "")
       ,(if st (number->string st) ""))))
  (template #:center
            `((div ([id "chead"] [style "text-align: center"]) 
                   (span
                    ,(a-date->string (cycle-start-date cycle))
                    " --- "
                    ,(a-date->string (cycle-end-date cycle))
                    " (Day " ,(format "~a" current-day-n) ")")
                   (div ([style "float: left"])
                        ,(if prev-cycle-url `(a ([href ,(url->string prev-cycle-url)]) "Prev") 'nbsp))
                   (div ([style "float: right"])
                        ,(cond
                           [next-cycle-url `(a ([href ,(url->string next-cycle-url)]) "Next")]
                           [new-cycle-url `(strong (a ([href ,(url->string new-cycle-url)]) "New Cycle"))]
                           [else 'nbsp])))
              ,(cycle->svg cycle 
                           #:add-fake-point? (not (and next-cycle-url #t))
                           #:point-onclick-js point-onclick-js))
            #:right
            `((script ([type "text/javascript"])
                      ,(javascript
                        `(var [SELECTED ""]))
                      ,(javascript
                        `(function hideSelected ()
                                   (if SELECTED
                                       ((field ((field document getElementById) SELECTED) setAttribute) "visibility" "hidden"))))
                      ,(javascript 
                        (let ()
                          (define (select-help f l)
                            (map (lambda (i)
                                   `(= (field ((field document getElementById) ,(format f i)) selected)
                                       ,(js-id i)))
                                 l))
                          `(function point_onclick (backid
                                                    id date
                                                    unknown_cf ,@(map js-id cervical-fluids)
                                                    unknown_cp ,@(map js-id cervix-positions)
                                                    unknown_th ,@(map js-id thumbs)
                                                    ,@(map js-id other-flags)
                                                    ovulation notes wt st)
                                     ((field ((field document getElementById) "point-form") reset))
                                     (= SELECTED backid)
                                     ((field ((field document getElementById) backid) setAttribute) "visibility" "visible")
                                     ,@(select-help "point:fluid:~a" (list* 'unknown-cf cervical-fluids))
                                     ,@(select-help "point:position:~a" (list* 'unknown-cp cervix-positions))
                                     ,@(select-help "point:thumb:~a" (list* 'unknown-th thumbs))
                                     ,@(map (lambda (i)
                                              `(= (field ((field document getElementById) ,(format "point:~a" i)) checked) 
                                                  ,(js-id i)))
                                            other-flags)
                                     (= (field ((field document getElementById) "point:date") innerHTML) date)
                                     (= (field ((field document getElementById) "point:id") value) id)
                                     (= (field ((field document getElementById) "point:ovulation") checked) ovulation)                        
                                     (= (field ((field document getElementById) "point:notes") value) notes)
                                     (= (field ((field document getElementById) "point:waking-temp") value) wt)
                                     (= (field ((field document getElementById) "point:sleep-time") value) st)))))
              (form ([id "point-form"]
                     [method "post"]
                     [action ,(url->string save-point-url)]
                     [onreset "hideSelected();"])
                    (input ([type "hidden"] [name "id"] [id "point:id"] [value ""]))
                    (table ([style "text-align: center;"])
                           (tr (td ([colspan "2"]) (span ([id "point:date"] [style "font-size: smaller;"]) nbsp)))
                           (tr (td ([colspan "2"]) (input ([type "text"] [size "4"] [name "st"] [id "point:sleep-time"] [value ""])) " hrs"))
                           (tr (td ([colspan "2"]) (input ([type "text"] [size "4"] [name "wt"] [id "point:waking-temp"] [value ""])) deg))
                           ; XXX Single drop-down works in nightly safari (change once mainline)
                           (tr (td ([colspan "2"])
                                   (select ([id "point:fluid"] [name "fluid"] [multiple "true"])
                                           (option ([id "point:fluid:unknown-cf"] [selected "true"]) "Unknown")
                                           (option ([id "point:fluid:egg-white"]) "Egg White")
                                           (option ([id "point:fluid:creamy"]) "Creamy")
                                           (option ([id "point:fluid:sticky"]) "Sticky")
                                           (option ([id "point:fluid:dry"]) "Dry")
                                           (option ([id "point:fluid:period"]) "Period")
                                           (option ([id "point:fluid:spotting"]) "Spotting"))))                             
                           (tr (td ([colspan "2"])
                                   (select ([id "point:position"] [name "position"] [multiple "true"])
                                           (option ([id "point:position:unknown-cp"] [selected "true"]) "Unknown")
                                           (option ([id "point:position:firm"]) "Firm")
                                           (option ([id "point:position:medium"]) "Medium")
                                           (option ([id "point:position:soft"]) "Soft"))))
                           (tr (td "Thumb:")
                               (td (select ([id "point:thumb"] [name "thumb"] [multiple "true"])
                                           (option ([id "point:thumb:unknown-th"] [value "auto"] [selected "true"]) "Maybe")
                                           (option ([id "point:thumb:thumb"] [value "thumb"]) "Yes")
                                           (option ([id "point:thumb:not-thumb"] [value "not-thumb"]) "No"))))
                           (tr (td ([colspan "2"])
                                   (input ([type "submit"] [value "Commit"])) " "
                                   (input ([id "point-form:reset"] 
                                           [type "reset"]
                                           [value "Cancel"]))))
                           (tr (td "Ovulation") (td (input ([id "point:ovulation"] [name "flag:ovu"] [type "checkbox"] [disabled "true"]))))
                           (tr (td "Preg. Test?") (td (input ([id "point:pregnancy-test"] [name "flag:pt"] [type "checkbox"]))))
                           (tr (td "Intercourse") (td (input ([id "point:intercourse"] [name "flag:i"] [type "checkbox"]))))
                           (input ([type "hidden"] [id "point:fake"] [name "flag:fake"]))
                           (tr (td ([colspan "2"]) "Notes"))
                           (tr (td ([colspan "2"]) (textarea ([id "point:notes"] [name "notes"] [rows "5"] [cols "10"])))))))))
(provide
 cycle->xhtml)

; XXX Add up/down buttons for temp/time
; XXX Sleep buttons move by .5
; XXX Delete cycle
; XXX Better error message
(define (save-request->point req)
  (define bs (request-bindings/raw req))
  (define (c i) (string->number (bytes->string/utf-8 (binding:form-value (bindings-assq i bs)))))
  (define id (c #"id"))
  (define sleep-time (c #"st"))
  (define temp (real->temp (c #"wt")))
  (define fluid-flag
    (let ([f (binding:form-value (bindings-assq #"fluid" bs))])
      (cond [(bytes=? f #"Egg White") 'egg-white]
            [(bytes=? f #"Creamy") 'creamy]
            [(bytes=? f #"Sticky") 'sticky]
            [(bytes=? f #"Dry") 'dry]
            [(bytes=? f #"Period") 'period]
            [(bytes=? f #"Spotting") 'spotting]
            [(bytes=? f #"Unknown") #f]
            [else (error 'save-request->point "No fluid given")])))
  (define position-flag
    (let ([p (binding:form-value (bindings-assq #"position" bs))])
      (cond [(bytes=? p #"Unknown") #f]
            [(bytes=? p #"Firm") 'firm]
            [(bytes=? p #"Medium") 'medium]
            [(bytes=? p #"Soft") 'soft]
            [else (error 'save-request->point "No position given")])))
  (define thumb-flag
    (let ([p (binding:form-value (bindings-assq #"thumb" bs))])
      (cond [(bytes=? p #"auto") #f]
            [(bytes=? p #"thumb") 'thumb]
            [(bytes=? p #"not-thumb") 'not-thumb]
            [else (error 'save-request->point "No thumb given")])))
  (define (flag? i)
    (with-handlers ([exn:fail? (lambda (e) #f)])
      (define f (binding:form-value (bindings-assq i bs)))
      (cond [(bytes=? f #"on") #t]
            [else #f])))
  (define pregnancy-test? (flag? #"flag:pt"))
  (define intercourse? (flag? #"flag:i"))
  (define notes (bytes->string/utf-8 (binding:form-value (bindings-assq #"notes" bs))))
  (values (make-point 
           sleep-time temp
           (append (list notes)
                   (if position-flag (list position-flag) empty)
                   (if fluid-flag (list fluid-flag) empty)
                   (if pregnancy-test? (list 'pregnancy-test) empty)
                   (if intercourse? (list 'intercourse) empty)
                   (if thumb-flag (list thumb-flag) empty)))
          id))
(provide/contract
 [save-request->point ((request?) . ->* . (point? integer?))])