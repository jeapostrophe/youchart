#lang web-server
(require (lib "etc.ss")
         (lib "plt-match.ss")
         "../lib/view.ss"
         "../lib/test.ss"
         "../lib/data.ss"
         "../lib/model.ss"
         "../lib/db.ss"
         "../lib/xml.ss")
(provide start)

(define current-db (make-web-parameter (build-path (this-expression-source-directory) 'up "db")))
(define current-user (make-web-parameter #f))

(define (show-user/latest-cycle)
  (show-user
   (user-latest-cycle
    (current-db) (current-user))))

(define (show-user cycle-n)
  (define (current-cycle)
    (read-user-cycle 
     (current-db) (current-user)
     cycle-n))
  (redirect/get)
  (send/suspend/dispatch
   (lambda (embed/url)
     (xhtml-response 
      (cycle->xhtml
       (current-cycle)
       #:prev-cycle-url
       (if (user-cycle-exists? (current-db) (current-user) (sub1 cycle-n))
           (embed/url (lambda (req) (show-user (sub1 cycle-n))))
           #f)
       #:next-cycle-url
       (if (user-cycle-exists? (current-db) (current-user) (add1 cycle-n))
           (embed/url (lambda (req) (show-user (add1 cycle-n))))
           #f)
       #:new-cycle-url
       (embed/url 
        (lambda (req)
          (write-user-cycle (current-db) (current-user)
                            (add1 (user-latest-cycle
                                   (current-db) (current-user)))
                            (empty-cycle/today))
          (show-user/latest-cycle)))
       #:save-point-url
       (embed/url
        (lambda (req)
          (define-values (point i) (save-request->point req))
          (match (update-cycle/point (current-cycle) point i)
            [`(update . ,new-cycle)
             (write-user-cycle (current-db) (current-user) cycle-n
                               new-cycle)
             (show-user cycle-n)]
            [`(new . ,new-cycle)
             (define new-cycle-n (add1 cycle-n))
             (unless (user-cycle-exists? (current-db) (current-user) new-cycle-n)
               (write-user-cycle (current-db) (current-user) new-cycle-n
                                 new-cycle))
             (show-user new-cycle-n)]))))))))

(define (show-login)
  (redirect/get)
  (send/suspend/dispatch
   (lambda (embed/url)
     (xhtml-response
      (login-page
       #:login-url
       (embed/url
        (lambda (req)
          (define-values (user password) (login-request->values req))
          (if (user-exists? (current-db) user)
              (if (user-password=? (current-db) user password)
                  (web-parameterize 
                   ([current-user user])
                   (show-user/latest-cycle))
                  (show-login))
              (begin (create-user! (current-db) user password)
                     (web-parameterize 
                      ([current-user user])
                      (show-user/latest-cycle)))))))))))

(define (start req)
  (show-login))