#lang setup/infotab
(define name "porter-stemmer: a packaging of the porter-stemmer implementation from Programming Praxis")
(define categories '(datastructures))
(define can-be-loaded-with 'all)
(define required-core-version "5.1.1")
(define version "1.2")
(define repositories '("4.x"))
(define scribblings '(("manual.scrbl")))
(define primary-file "main.rkt")
(define blurb 
  '("This implementation comes from Programming Praxis (http://programmingpraxis.com/2009/09/08/porter-stemming/)."))
(define release-notes
  '((p "Added minimal documentation.")))