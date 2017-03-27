;;;; callbook.lisp

(in-package #:callbook)

;;; "callbook" goes here. Hacks and glory await!

(defun lookup-call (call)
  (json:decode-json-from-string
   (babel:octets-to-string
    (drakma:http-request (concatenate 'string "https://callook.info/" call "/json") :method :get))))
