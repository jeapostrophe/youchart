#lang scheme/base
(require (lib "contract.ss")
         (lib "etc.ss")
         (lib "list.ss")
         (only-in (lib "file.ss")
                  make-directory*)
         (only-in (lib "1.ss" "srfi")
                  filter-map)
         (planet "read.ss" ("ryanc" "scripting.plt" 1 1))
         "../lib/data.ss")

(define (user-path db user)
  (build-path db user))

; XXX test
(define (user-exists? db user)
  (file-exists? (user-password-path db user)))
(provide/contract
 [user-exists? (path? string? . -> . boolean?)])

; XXX test
(define (create-user! db user password)
  (make-directory* (user-path db user))
  (set-user-password! db user password)
  (write-user-cycle db user 0 (empty-cycle/today)))
(provide/contract
 [create-user! (path? string? string? . -> . void)])

(define (user-password-path db user)
  (build-path (user-path db user) "password"))
(define (user-password=? db user password)    
  (string=? (with-input-from-file (user-password-path db user) (lambda () (read)))
            password))
(define (set-user-password! db user new-password)
  (with-output-to-file
      (user-password-path db user)
    (lambda ()
      (write new-password))
    #:exists 'truncate/replace))
(provide/contract
 [user-password=? (path? string? string? . -> . boolean?)]
 [set-user-password! (path? string? string? . -> . void)])

(define (user-cycle-path db user cycle-n)
  (build-path (user-path db user) (format "chart~a.ss" cycle-n)))
(define (read-user-cycle db user cycle-n)
  (read-cycle (first (read-all/file (user-cycle-path db user cycle-n)))))
(define (write-user-cycle db user cycle-n cycle)
  (with-output-to-file
      (user-cycle-path db user cycle-n)
    (lambda ()
      (write (write-cycle cycle)))
    #:exists 'truncate/replace))  
(define (user-cycles db user)
  (map (compose string->number bytes->string/utf-8 second)
       (filter-map (lambda (p)
                     (regexp-match #"^chart([0-9]+).ss$" (path->bytes p)))
                   (directory-list (user-path db user)))))
(define (user-cycle-exists? db user n)
  (file-exists? (user-cycle-path db user n)))  
(define (user-latest-cycle db user)
  (apply max (user-cycles db user)))

(provide/contract
 [read-user-cycle (path? string? integer? . -> . cycle?)]
 [write-user-cycle (path? string? integer? cycle? . -> . void)]
 [user-cycles (path? string? . -> . (listof integer?))]
 [user-cycle-exists? (path? string? integer? . -> . boolean?)]
 [user-latest-cycle (path? string? . -> . integer?)])