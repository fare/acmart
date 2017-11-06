#lang info
(define collection "scribble")
(define deps '("base" "scribble-lib" "at-exp-lib" "sha"))
(define build-deps '("racket-doc" "scribble-doc"))

(define pkg-desc "Port of the acmart 2016 style to Scribble")
(define pkg-authors '(fare leif))

;(define scribblings '(("acmart-fare.scrbl" () ("Scribble Libraries"))))
(define version "0.2017.1")
