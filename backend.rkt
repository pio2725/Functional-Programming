#lang racket

(require web-server/servlet)
(require web-server/servlet/web)
(provide read-number)
(provide display-result)
(provide read-user-input)


; List of String -> number
(define (read-number prompt)
  (let ([num (string->number
              (bytes->string/utf-8
               (binding:form-value
                (bindings-assq
                 #"number"
                 (request-bindings/raw
                  (send/suspend
                   (λ (k-url)
                     (response/xexpr
                      `(html
                        (body
                         ;(p ,prompt)
                         (unquote-splicing (map (lambda (q) `(p ,q)) prompt))
                         (form ([method "POST"]
                                [action ,k-url])
                               (input ([type "text"]
                                       [name "number"]
                                       [size "8"]))
                               (input ([type "submit"]
                                       [value "Submit"])))))))))))))])
    (redirect/get)
    (if num
      num
      (read-number prompt))))

; string -> string
(define (read-user-input prompt)
  (let ([num ;(string->number
              (bytes->string/utf-8
               (binding:form-value
                (bindings-assq
                 #"string"
                 (request-bindings/raw
                  (send/suspend
                   (λ (k-url)
                     (response/xexpr
                      `(html
                        (body
                         (p ,prompt)
                         (form ([method "POST"]
                                [action ,k-url])
                               (input ([type "text"]
                                       [name "string"]))
                               (input ([type "submit"]
                                       [value "Submit"]))))))))))))])
    (redirect/get)
    (if num
      num
      (read-user-input prompt))))

; number -> response
(define (display-result str-list)
  (response/xexpr
   `(html
     (body
      (unquote-splicing (map (lambda (q) `(p ,q)) str-list))))))


