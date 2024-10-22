;; cl-libre-translate.asd
;; Copyright (c) 2024 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(asdf:defsystem #:cl-libre-translate
  :description "A Common Lisp client for the Libre Translate API for translating between human languages.  See https://github.com/LibreTranslate/LibreTranslate to setup the Libre Translate service."
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license  "ISC"
  :version "1.0.0"
  :serial t

  :depends-on (#:alexandria
               #:st-json
               #:dexador
               #:uiop)

  :components ((:file "package")
               (:file "main")
               (:file "cl-libre-translate"))

  :in-order-to ((test-op (test-op
                          cl-libre-translate.test))))
