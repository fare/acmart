#lang racket/base
; -*- Scheme -*-

;; Provides helpers for downloading acmart style files
;; (mostly taken from the lipics package)

(require
 file/unzip
 net/url
 racket/file
 racket/port
 racket/system
 sha)

(provide acmart-cls-path
         acmart-bst-path
         download-acmart-files)

(define acmart-url
  (let ((base (format "https://raw.githubusercontent.com/~a/~a/~a/"
                      "borisveytsman" "acmart"
                      "d21923a0301a8f741cb8e8911d069e340acee76a")))
    (λ (x) (format "~a~a" base x))))

(define acmart-ins-url (acmart-url "acmart.ins"))
(define acmart-ins-sha256
  "adb36b225c6686bc7c2d491682a17a515025a5bee594535d9dcf5d5f4a436065")
(define acmart-dtx-url (acmart-url "acmart.dtx"))
(define acmart-dtx-sha256
  "f87a03c0af8c92fcdc07e1ab71204b00ef3dce5d8ca4a090854a02c9e6cca90e")
(define acmart-bst-url (acmart-url "ACM-Reference-Format.bst"))
(define acmart-bst-sha256
  "7334f30e2e4356fad8acefad68b107bc7e9d08f5cbc95dc67402ab83e9af6a76")
(define acmart-cls-sha256
  "ec78c7c4244eb6dd444ea5e0943652b2598b85dcbf79cea11ad32aa6c66eaafa")

(define (file-sha256 path)
  (with-input-from-file path
    (λ () (sha256 (port->bytes (current-input-port))))))

(define (file-exists-and-of-hash? path hash)
  (and (file-exists? path)
       (equal? hash (bytes->hex-string (file-sha256 path)))))

(define acmart-base-path
  (build-path (find-system-path 'addon-dir) "acmart-style-files"))
(define (acmart-path p) (build-path acmart-base-path p))

(define acmart-ins-path (acmart-path "acmart.ins"))
(define acmart-dtx-path (acmart-path "acmart.dtx"))
(define acmart-cls-path (acmart-path "acmart.cls"))
(define acmart-bst-path (acmart-path "ACM-Reference-Format.bst"))

(define (download-file url path)
  (call-with-output-file path #:exists 'replace
    (λ (out)
      (call/input-url (string->url url) get-pure-port
                      (λ (in) (copy-port in out))))))

(define (check-hash path expected-hash description)
  (unless (file-exists? path)
    (error 'check-hash "file ~a doesn't exist" path))
  (let ((actual-hash (bytes->hex-string (file-sha256 path))))
    (unless (equal? expected-hash actual-hash)
      (error 'check-hash "file ~a ~a doesn't have expected SHA256 ~a but ~a"
             path (apply format description) expected-hash actual-hash))))

(define (download-file-check-hash url path hash)
  (download-file url path)
  (check-hash path hash `("as downloaded from ~a" ,url)))

;; Download acmart class file to the add-on directory
(define (download-acmart-files)
  (unless (and (file-exists-and-of-hash? acmart-cls-path acmart-cls-sha256)
               (file-exists-and-of-hash? acmart-bst-path acmart-bst-sha256))
    (displayln (format "Downloading class file via ~a" acmart-url))
    (make-directory* acmart-base-path)
    (download-file-check-hash acmart-ins-url acmart-ins-path acmart-ins-sha256)
    (download-file-check-hash acmart-dtx-url acmart-dtx-path acmart-dtx-sha256)
    (download-file-check-hash acmart-bst-url acmart-bst-path acmart-bst-sha256)
    ;; Invoke the Makefile in that directory...
    (parameterize ([current-directory acmart-base-path])
      (system "pdflatex acmart.ins" #:set-pwd? #t))
    (check-hash acmart-cls-path acmart-cls-sha256
                '("as compiled from source"))
    (for-each delete-file
              (list acmart-ins-path
                    acmart-dtx-path
                    (acmart-path "acmart.log")))))
