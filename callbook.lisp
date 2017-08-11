;;;; callbook.lisp

(in-package #:callbook)

;;; "callbook" goes here. Hacks and glory await!

(defparameter *timestamp* 0)
(defparameter *session* nil)

(defmacro cdr-assoc (name alist)
  "Replaces '(cdr (assoc name alist))' because it's used a bajillion
times when doing API stuff."
  `(cdr (assoc ,name ,alist :test #'equal)))

;; This defines a ham. Subclassed from 2d-point in the aviation
;; formulary.
(defclass ham (af:2d-point)
  ((call :accessor call
	 :initarg :call
	 :initform nil)
   (name :accessor name
	 :initarg :name
	 :initform nil)
   (lic-class :accessor lic-class
	      :initarg :lic-class
	      :initform nil)
   (grid :accessor grid
	 :initarg :grid
	 :initform nil)
   (email :accessor email
	  :initarg :email
	  :initform nil)
   (street :accessor street
	   :initarg :street
	   :initform nil)
   (city :accessor city
	 :initarg :city
	 :initform nil)
   (country :accessor country
	    :initarg :country
	    :initform nil)))

(defun make-ham-from-callbook (thing)
  "Create a ham object from a callbook lookup."
  (when (equal "VALID" (cdr-assoc :status thing))
    (let ((lat-thing (cdr-assoc :latitude (cdr-assoc :location thing)))
	  (lon-thing (cdr-assoc :longitude (cdr-assoc :location thing))))
      (make-instance 'ham
		     :call (cdr-assoc :callsign (cdr-assoc :current thing))
		     :name (cdr-assoc :name thing)
		     :lic-class (cdr-assoc :oper-class (cdr-assoc :current thing))
		     :grid (cdr-assoc :gridsquare (cdr-assoc :location thing))
		     :lat (unless (equal lat-thing "")
			    (with-input-from-string
				(in lat-thing)
			      (read-number:read-float in)))
		     :lon (unless
			      (equal lon-thing "")
			    (with-input-from-string
				(in lon-thing)
			      (read-number:read-float in)))
		     :street (cdr-assoc :line-1 (cdr-assoc :address thing))
		     :city (cdr-assoc :line-2 (cdr-assoc :address thing))
		     :country "United States"))))

(defun make-ham-from-hamqth (thing)
  "Create a ham object from a hamqth lookup."
  (make-instance 'ham
		 :call (string-upcase (cdr (assoc "callsign" thing :test 'equal)))
		 :name (cdr (assoc "adr_name" thing :test 'equal))
		 :grid (cdr (assoc "grid" thing :test 'equal))
		 :lat (with-input-from-string
			  (in (cdr (assoc "latitude" thing :test 'equal)))
			(read-number:read-float in))
		 :lon (with-input-from-string
			  (in (cdr (assoc "longitude" thing :test 'equal)))
			(read-number:read-float in))
		 :email (cdr (assoc "email" thing :test 'equal))
		 :street (cdr (assoc "adr_street1" thing :test 'equal))
		 :city (concatenate 'string
				    (cdr (assoc "adr_city" thing :test 'equal)) " "
				    (cdr (assoc "us_state" thing :test 'equal)) " "
				    (cdr (assoc "adr_zip" thing :test 'equal)))
		 :country (cdr (assoc "adr_country" thing :test 'equal))))

(defun callbook-lookup (call)
  "Look up a callsign on callbook.info using their API."
  (make-ham-from-callbook
   (json:decode-json-from-string
    (babel:octets-to-string
     (drakma:http-request
      (concatenate 'string "https://callook.info/" call "/json") :method :get)))))

(defun get-session-id (login passwd)
  "Given a login and a passwd, return either a hamqth session id or
nil."
  (let ((session (third
		  (third
		   (xmls:parse
		    (babel:octets-to-string
		     (drakma:http-request
		      (concatenate 'string
				   "https://www.hamqth.com/xml.php?u="
				   login "&p=" passwd)
		      :method :get))
		    :compress-whitespace t)))))
    (if (equal "session_id" (first (first session)))
	(third session)
	nil)))

(defun hamqth-lookup (login passwd call)
  "Look up a callsign on hamqth.com using their API. If there's
already a session key that's less than an hour old (I padded this by
30 seconds just to be safe), then call the API, else grab and store a
new session key, then call the API."
  (let ((now (local-time:timestamp-to-unix (local-time:now))))
    (unless (and *session*
		 (<= (- now *timestamp*) 3570))
      (setf *timestamp* now)
      (setf *session* (get-session-id login passwd)))
    (when *session*
      (make-ham-from-hamqth
       (mapcar
	(lambda (n)
	  (cons (first (first n)) (third n)))
	(rest
	 (rest
	  (third
	   (xmls:parse
	    (babel:octets-to-string
	     (drakma:http-request
	      (concatenate 'string
			   "https://www.hamqth.com/xml.php?id="
			   *session*
			   "&callsign="
			   call
			   "&prg=callbook")
	      :method :get))
	    :compress-whitespace t)))))))))
