;; cl-libre-translate.lisp
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

(in-package :cl-libre-translate)

(defparameter *verbose-http-stream* nil
  "Stream to log HTTP traffic to.  t to use stdout, nil to disable.")


(defparameter *config-file* (asdf:system-relative-pathname :cl-libre-translate ".config")
  "Name of an (optional) config file containing an API key and Libre Translate URL.~
All fields are optional and will override the defaults.~
Allowed keys are \"default-source\", \"default-target\", \"url\" and \"api-key\".~
Should be JSON like {\"url\": \"http://libretranslate.com\", \"api-key\": \"....\"}")

(defparameter *libre-translate-url* (quri:make-uri :scheme "http"
                                                       :host "localhost"
                                                       :port 5001)
  "The URI of Libre Translate host that will be used.")

(defparameter *api-key* nil
  "The API key to use for the connection.")

(defparameter *cache* (make-hash-table :size 100 :test 'equal)
  "A cache to avoid Libre Translate API requests.  Set to nil to disable.")

(defparameter *default-source* "auto"
  "The default source language.")

(defparameter *default-target* "es"
  "The default target language.")

(defun load-config ()
  "Read *config-file*, if it exists, and populate *libre-translate-url* and~
 and *api-key* parameters if the \"url\" and \"api-key\" fields are present."
  (when (uiop:file-exists-p *config-file*)

    (let ((config (with-input-from-file (ins *config-file*)
                    (read-json ins))))

      (when-let (api-key (getjso "api-key" config))
        (setf *api-key* api-key))

      (when-let (url-string (getjso "url" config))
        (setf *libre-translate-url* (quri:uri url-string)))

      (when-let (default-target (getjso "default-target" config))
        (setf *default-target* default-target))

      (when-let (default-source (getjso "default-source" config))
        (setf *default-source* default-source)))))

(load-config)

(defun maybe-add-api-key (content)
  "If *api-key* is non-nil, add the API key to the request content.~
content can be JSON or an alist."
  (if (and *api-key*
           (> (length *api-key*) 0))
      (let ((ak (list (cons "api-key" *api-key*))))
        (typecase content
          (jso (jso-from-alist (concatenate 'list
                                            (jso-to-alist content)
                                            ak)))
          (t (concatenate'list ak content))))
      content))

(defun api-req (api-path &key
                           (method :get)
                           content)
  "Low level function to make an API request."

  (let* ((the-url (quri:make-uri :defaults *libre-translate-url*
                                 :path api-path))
         (content-type-headers (typecase content
                                 (jso '(("content-type" . "application/json")
                                        ("accept" . "application/json")))
                                 (t '(("accept" . "application/json")))))
         (api-key-content (maybe-add-api-key content))
         (real-content (typecase api-key-content
                         (jso (write-json-to-string api-key-content))
                         (t api-key-content)))
         (raw-result (handler-bind
                         ;; Don't raise a condition - read the error from the JSON content body.
                         ((dex:http-request-failed 'dex:ignore-and-continue))
                       (dex:request the-url
                                    :method method
                                    :content real-content
                                    :headers content-type-headers
                                    :verbose *verbose-http-stream*)))

         (json-result (read-json-from-string raw-result)))

    (when-let (error-text
               (and (eq (type-of json-result) 'jso )
                    (getjso "error" json-result)))
      (error error-text))

    json-result))


(defun languages ()
  "Return the list of supported languages."
  (jso-from-alist (mapcar (lambda (js)
                            (cons (getjso "code" js) js))
                          (api-req "languages"))))

(defun show-languages (&optional (stream t))
  (flet
      ((format-language (code description)
         (let ((name-code (format nil
                                  "~a (~a)"
                                  (getjso "name" description)
                                  code)))
           (format stream "~a~%" name-code)
           name-code)))
    (declare (inline format-language languages))
    (mapjso #'format-language
            (languages))))

(defun describe-language (which-language)
  (let ((all-languages (languages)))
    (getjso which-language all-languages)))


(defparameter *language-count* 20
  "The number of supported languages.")

(defun translate (text &key
                         (source *default-source*)
                         (target *default-target*)
                         (alternatives 3)
                         (format "text"))
  "Translate text from the source language to the target language."
  (api-req "translate" :method :post
                       :content (jso "q" text
                                     "source" source
                                     "target" target
                                     "alternatives" alternatives
                                     "format" format)))

(defun cached-translate (text &key
                         (source *default-source*)
                         (target *default-target*)
                         (format "text"))
  "Like translate, but returns one result, and uses a local cache to avoid API lookups."
  (let ((text-hash (gethash text *cache* (make-hash-table :test 'equal :size *language-count*))))
    (let ((translated (gethash target text-hash)))
      (when translated
        (return-from cached-translate translated))
      (let ((translated-text (getjso "translatedText" (translate text
                                                :alternatives 0
                                                :source source
                                                :target target
                                                :format format))))
        (setf (gethash target text-hash) translated-text)
        (setf (gethash text *cache*) text-hash)
        translated-text))))

(defun detect (text)
  "Detect the language of text."
  (api-req "detect"
           :method :post
           :content (jso "q" text)))

(defun translate-file (file-name &key
                                   (output-file-name)
                                   (source *default-source*)
                                   (target *default-target*))
  "Translate a file.  If output-file-name is given, save the results to it."
  (let* ((results (api-req "translate_file"
                           :method :post
                           :content (list (cons "file" (probe-file file-name))
                                          (cons "source" source)
                                          (cons "target" target))))
         (download-url (getjso "translatedFileUrl" results)))
    (when output-file-name
      (dex:fetch download-url output-file-name))
    results))

(defun frontend-settings ()
  (api-req "frontend/settings"))

(defun suggest (text suggested source target)
  (api-req "suggest"
           :method :post
           :content (jso "q" text
                         "s" suggested
                         "source" source
                         "target" target)))
