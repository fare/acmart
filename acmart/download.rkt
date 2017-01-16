#lang racket/base
; -*- Scheme -*-

;; Provides helpers for downloading acmart style files
;; (mostly taken from the lipics package)

(require sha
         file/unzip
         net/url
         racket/file
         racket/port
         racket/system)

(provide acmart-class-path
         acm-bibtex-style-path
         download-acmart-files)

(define acmart-url  "http://www.acm.org/binaries/content/assets/publications/consolidated-tex-template/acmart.zip")
;;(define acmart-url  "file:///home/fare/Downloads/acmart.zip")
(define acmart-hash "364212a42f31941a5946352df3cf2ae73582b725b03d997e43e462e4c320c897")

(define acmart-base-path
  (build-path (find-system-path 'addon-dir) "acmart-style-files"))
(define acmart-class-path (build-path acmart-base-path "acmart/acmart.cls"))
(define acm-bibtex-style-path (build-path acmart-base-path "acmart/ACM-Reference-Format.bst"))

;; Download acmart class file to the add-on directory
(define (download-acmart-files)
  (unless (and (file-exists? acmart-class-path)
               (file-exists? acm-bibtex-style-path))
    (define tmp (make-temporary-file))
    (displayln (format "Downloading class file via ~a" acmart-url))
    (define out (open-output-file tmp #:exists 'truncate))
    (call/input-url (string->url acmart-url)
                    get-pure-port
                    (λ (in) (copy-port in out)))
    (close-output-port out)
    (define hash
      (with-input-from-file tmp
        (λ () (sha256 (port->bytes (current-input-port))))))
    (unless (equal? (bytes->hex-string hash) acmart-hash)
      (raise-arguments-error 'scribble/acmart
                             "Invalid SHA256 hash for acmart tarball"
                             "expected" acmart-hash
                             "given" (bytes->hex-string hash)))
    ;; Don't make the directory until we have a valid download
    (make-directory* acmart-base-path)
    (unzip tmp (make-filesystem-entry-reader #:dest acmart-base-path))
    ;; Invoke the Makefile in that directory...
    (parameterize ([current-directory (build-path acmart-base-path "acmart")])
      (system "make" #:set-pwd? #t))))
