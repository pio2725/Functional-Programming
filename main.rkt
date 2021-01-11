#lang web-server/insta

(require "backend.rkt")
(require "survey.rkt")

(require web-server/http/xexpr
         web-server/servlet/web)

(define (start req)
  (display-result (start-survey)))
