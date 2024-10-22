;; package.lisp
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

(in-package :cl-user)
(defpackage :cl-libre-translate.test
  (:use :cl
        :fiveam
        :alexandria
        :cl-libre-translate
        :st-json))

(in-package :cl-libre-translate.test)

(def-suite :cl-libre-translate)
(in-suite :cl-libre-translate)

(test maybe-add-api-key-to-empty-list
      (let* ((lt::*api-key* "foobar")
             (results (lt::maybe-add-api-key nil)))
        ;; Should return (list (cons "api-key" "foobar"))
        (is-true (find "api-key" results
                       :test #'string=
                       :key #'car))
        (is-true (string= "foobar" (assoc-value results "api-key" :test #'string=)))
        (is-true (= 1 (length results)))))


(test maybe-add-api-key-to-a-list
  (let* ((lt::*api-key* "foobar")
         (results (lt::maybe-add-api-key (list (cons "abc" "def")))))
    ;; Should return (list (cons "api-key" "foobar"))
    (is-true (find "api-key" results
                   :test #'string=
                   :key #'car))
    (is-true (find "abc" results
                   :test #'string=
                   :key #'car))
    (is-true (string= "foobar" (assoc-value results "api-key" :test #'string=)))
    (is-true (string= "def" (assoc-value results "abc" :test #'string=)))
    (is-true (= 2 (length results)))))

(test maybe-add-api-key-to-empty-json
      (let* ((lt::*api-key* "foobar")
             (results (lt::maybe-add-api-key (jso))))
        ;; Should return (list (cons "api-key" "foobar"))
        (is-true (getjso "api-key" results))
        (is-true (string= "foobar" (getjso "api-key" results)))))

(test maybe-add-api-key-to-json
      (let* ((lt::*api-key* "foobar")
             (results (lt::maybe-add-api-key (jso"abc" "def"))))
        ;; Should return (list (cons "api-key" "foobar"))
        (is-true (getjso "api-key" results))
        (is-true (getjso "abc" results))
        (is-true (string= "foobar" (getjso "api-key" results)))
        (is-true (string= "def" (getjso "abc" results)))))
