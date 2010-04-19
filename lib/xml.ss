#lang scheme/base
(require (lib "list.ss")
         (lib "contract.ss")
         (lib "xml.ss" "xml")
         (lib "private/response-structs.ss" "web-server")
         (planet "syntax/sexp.ss" ("dherman" "javascript.plt" 5))
         (planet "syntax/pretty-print.ss" ("dherman" "javascript.plt" 5)))

(define svg? xexpr?)

(provide/contract
 [js-id (symbol? . -> . symbol?)]
 [javascript/string (list? . -> . string?)]
 [javascript (list? . -> . xexpr?)]
 [svg? (any/c . -> . boolean?)]
 [xhtml-response (xexpr? . -> . response/full?)]
 [write-svg! (svg? . -> . void)]
 [write-xhtml! (xexpr? . -> . void)])

(define (js-id s)
  (string->symbol
   (regexp-replace* "-" (symbol->string s) "_")))

(define (javascript/string s)
  (pretty-format
   (sexp->SourceElement
    s)))

(define (javascript s)
  (make-cdata 
   #f #f
   (javascript/string s)))

(define (xhtml-response xe)
  (define ob (open-output-bytes))
  (parameterize ([current-output-port ob])
    (write-xhtml! xe))
  (make-response/full
   200 "Okay" (current-seconds)
   #"application/xhtml+xml"
   empty
   (list (get-output-bytes ob))))

(define (write-svg! xe)
  (write-xml
   (make-document 
    (make-prolog 
     empty 
     (make-document-type 
      'svg
      (make-external-dtd/public 
       "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"
       "-//W3C//DTD SVG 1.1//EN" )
      #f))
    (xexpr->xml xe)
    empty)))

(define (write-xhtml! xe)
  (parameterize ([empty-tag-shorthand 'never])
    (write-xml
     (make-document 
      (make-prolog 
       empty 
       (make-document-type 
        'html
        (make-external-dtd/public 
         "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"
         "-//W3C//DTD XHTML 1.0 Strict//EN")
        #f))
      (xexpr->xml xe)
      empty))))