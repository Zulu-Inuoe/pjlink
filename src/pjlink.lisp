;;;; PJLink base communication and types

(in-package #:pjlink)

;;;
;;; The structure of a PJLink command:
;;;  +--------------+---------+---------------+--------------------+----------------+
;;;  | Header+Class |   Body  |   Separator   |        Param       |    Terminator  |
;;;  +--------------+---------+---------------+--------------------+----------------+
;;;  |    2 bytes   | 4 bytes | 1 byte (0x20) | 128 bytes or less  |  1 byte (0x0D) |
;;;  +--------------+---------+---------------+--------------------+----------------+
;;;
;;; Example command (in ASCII)
;;;  `%1POWR 1
;;;

;;;
;;; The structure of a PJLink response:
;;;  +--------------+---------+---------------+--------------------+----------------+
;;;  | Header+Class |   Body  |   Separator   |        Param       |    Terminator  |
;;;  +--------------+---------+---------------+--------------------+----------------+
;;;  |    2 bytes   | 4 bytes | 1 byte (0x3D) | 128 bytes or less  |  1 byte (0x0D) |
;;;  +--------------+---------+---------------+--------------------+----------------+
;;;
;;; Example response (in ASCII)
;;;
;;;  `%1POWR=OK
;;;

(defconstant +pjlink-port+ 4352
  "Default PJLink port.")

(deftype hostname ()
  "A valid hostname for a projector or local inteface."
  '(or
    string ;;IPv4 or IPv6
    (or (vector t 4) (array (unsigned-byte 8) (4))) ;;IPv4
    (or (vector t 16) (array (unsigned-byte 8) (16))) ;;IPv6
    integer ;;IPv4
    null))

(defgeneric host (obj)
  (:documentation "Get the `hostname' designated by `obj'")
  (:method (obj)
    obj))

(defgeneric port (obj)
  (:documentation "Get the port designated by `obj'")
  (:method (obj)
    (declare (ignore obj))
    +pjlink-port+))

(defgeneric password (obj)
  (:documentation "Get the password designated by `obj'.
nil if no password is to be used.")
  (:method (obj)
    (declare (ignore obj))
    nil))

(defgeneric local-host (obj)
  (:documentation "Get the `local-host' designated by `obj'.")
  (:method (obj)
    (declare (ignore obj))
    nil))

(defgeneric local-port (obj)
  (:documentation "Get the `local-port' designated by `obj'.")
  (:method (obj)
    (declare (ignore obj))
    nil))

(defclass pjlink-config ()
  ((%host
    :type hostname
    :initarg :host
    :initform "localhost"
    :accessor host)
   (%port
    :type integer
    :initarg :port
    :initform +pjlink-port+
    :accessor port)
   (%password
    :type (or null sequence)
    :initarg :password
    :initform nil
    :accessor password)
   (%local-host
    :type hostname
    :initarg :local-host
    :initform nil
    :accessor local-host)
   (%local-port
    :type (or null integer)
    :initarg :local-port
    :initform nil
    :accessor local-port))
  (:documentation
   "Holds configuration for connecting with a PJLink projector
  host - host to connect to
  port - port to connect to
  password to use (unused if authentication disabled)
  local-host - Local interface to use
  local-port - Local port to use"))

(define-condition projector-command-error (error)
  ((%host
    :initarg :host
    :reader projector-command-error-host)
   (%class
    :initarg :class
    :reader projector-command-error-class)
   (%command
    :initarg :command
    :reader projector-command-error-command)))

(define-condition authorization-error (projector-command-error)
  ()
  (:report (lambda (c stream)
             (format stream "Autorization on '~A' failed" (projector-command-error-host c)))))

(define-condition undefined-command-error (projector-command-error)
  ()
  (:report (lambda (c stream)
             (format stream "Host '~A' does not support class '~A' command '~A'"
                     (projector-command-error-host c)
                     (projector-command-error-class c)
                     (projector-command-error-command c)))))

(define-condition out-of-parameter-error (projector-command-error)
  ((%parameter
    :initarg :parameter
    :reader out-of-parameter-error-parameter))
  (:report (lambda (c stream)
             (format stream "'~A' is out of bounds on '~A' for class '~A' command '~A'"
                     (out-of-parameter-error-parameter c)
                     (projector-command-error-host c)
                     (projector-command-error-class c)
                     (projector-command-error-command c)))))

(define-condition unavailable-time-error (projector-command-error)
  ()
  (:report (lambda (c stream)
             (format stream "Host '~A' cannot execute class '~A' command '~A' at this time."
                     (projector-command-error-host c)
                     (projector-command-error-class c)
                     (projector-command-error-command c)))))

(define-condition projector-display-error (projector-command-error)
  ()
  (:report (lambda (c stream)
             (format stream "Host '~A' failed to execute class '~A' command '~A'."
                     (projector-command-error-host c)
                     (projector-command-error-class c)
                     (projector-command-error-command c)))))

(defun %nibble->hex (nibble)
  "Convert a nibble into its hex char."
  (check-type nibble (unsigned-byte 4))
  (char "0123456789abcdef" nibble))

(defun %md5->hex-str (md5)
  "Convert a 16-octet md5 hash into a 32-char hex-encoded string."
  (loop
    :with hex-digest := (make-string 32)
    :for nidx :from 0 :below 16
    :for hidx :from 0 :below 32 :by 2
    :do
       (setf (char hex-digest hidx) (%nibble->hex (ash (logand #xF0 (aref md5 nidx)) -4))
             (char hex-digest (1+ hidx)) (%nibble->hex (logand #x0F (aref md5 nidx))))
    :finally
       (return hex-digest)))

(defun %encrypt-password (connection-response password plen)
  "Create a PJLink authentication digest from `connection-response' and `password'
`connection-response' should be a sequence like
PJLINK 1 <SEED>
And password a sequence of characters length 32 or less."
  (let ((buffer (make-string 40)))
    (declare (dynamic-extent buffer))
    (replace buffer connection-response :start2 9 :end2 17)
    (replace buffer password :start1 8 :end2 plen)
    (%md5->hex-str (md5:md5sum-string buffer :end (+ 8 plen)))))

(defun %verify-connect-response (response rlen)
  "Verifies the initial connection response.
Returns nil if the response does not match a proper pjlink connection response."
  (and (>= rlen 8)
       (string-equal response "PJLINK " :end1 7)
       (or
        ;; Authentication disabled
        (and (char= (char response 7) #\0))
        ;; Authentication enabled. Expect seed
        (and (char= (char response 7) #\1)
             (char= (char response 8) #\Space)
             (= rlen 17)))))

(defun %create-digest (connection-response password)
  "Creates a digest string from a connection response and a password.
Empty string on a non-authenticated response.
Otherwise calculates the response by prepending the seed from the connection response to the password.

`connection-response` should be a string containing a PJLink connection response
`password` should be a password designator"
  ;;Check whether authentication is enabled or not
  (ecase (char connection-response 7)
    (#\0 "") ; No authentication. Empty digest
    (#\1
     (let ((plen (length password)))
       (unless (<= plen 32)
         (error "Password length too long (~D)" plen))
       ;;Authentication
       (%encrypt-password connection-response password plen)))))

(defun %read-pjlink-command-line (buffer stream)
  "Reads a pjlink command-line (delimited by #\Return) from `stream` into `buffer`"
  (loop
    :for b := (read-char stream)
    :for i :from 0 :below (length buffer)
    ;; #\Return not included
    :until (char= b #\Return)
    :do (setf (char buffer i) b)
    :finally
       (return i)))

(defun %write-command (stream digest class command &rest params)
  "Writes a pjlink command to `stream' using `digest', `class', `command', and `params'
eg.
  %1CLSS ?<Return>"
  (declare (type stream stream)
           (type (or (string 0) (string 32)) digest)
           (type (integer 1 2) class)
           (type (string 4) command))
  (let ((buf (make-string 168))
        (len 0))
    (declare (dynamic-extent buf))
    (declare (type (integer 0 168) len))
    ;;Prepend the digest (if any)
    (replace buf digest)
    (incf len (length digest))

    ;;Add %1
    (setf (char buf (1- (incf len))) #\%
          (char buf (1- (incf len))) (code-char (+ (char-code #\0) class)))

    ;;Add command type
    (replace buf command :start1 len)
    (incf len (length command))

    ;;Space
    (setf (char buf (1- (incf len))) #\Space)

    ;;Add params
    (dolist (p params)
      (replace buf p :start1 len)
      (incf len (length p)))

    ;;End with #\Return
    (setf (char buf (1- (incf len))) #\Return)
    (write-sequence buf stream :end len)
    (finish-output stream)))

(defmacro %with-response-buffer (buffer &body body)
  `(let ((,buffer (make-string 136)))
     (declare (dynamic-extent ,buffer))
     ,@body))

(defun %validate-get-result (class command response len)
  (and (>= len 7)
       (char= (char response 0) #\%)
       (char= (char response 1) (code-char (+ (char-code #\0) class)))
       (string-equal response command :start1 2 :end1 6)
       (char= (char response 6) #\=)
       (- len 7)))

(defun %pjlink-get (host stream digest class command args)
  "Conducts a `get` command on `stream`, and returns the result string
uses
 `digest` as the authorization digest
 `class` as the command class
 `command` as the command name


This will issue a query such as
  %1POWR ?

Then given a result of
  %1POWR=0
returns the string \"0\""
  (%write-command stream digest class command "?" args)
  (%with-response-buffer in
    ;;Get the response params
    (let* ((rlen (%read-pjlink-command-line in stream))
           (param-len (%validate-get-result class command in rlen)))
      (unless param-len
        (if (string-equal in "PJLINK ERRA" :end1 rlen)
            (error 'authorization-error :host host :class class :command command)
            (error "Bad get response: '~A'" (subseq in 0 rlen))))
      (when (and (= param-len 4) (string-equal in "ERR" :start1 7 :end1 10))
        (ecase (char in 10)
          (#\1 (error 'undefined-command-error :host host :class class :command command))
          (#\2 (error 'out-of-parameter-error :host host :class class :command command :parameter args))
          (#\3 (error 'unavailable-time-error :host host :class class :command command))
          (#\4 (error 'projector-display-error :host host :class class :command command))))
      (subseq in 7 (+ 7 param-len)))))

(defun %pjlink-set (host stream digest class command params)
  "Conducts a `set` command on `stream`, and returns the result string
uses
 `digest` as the authorization digest
 `class` as the command class
 `command` as the command name
 `params` as the string params to send


This will issue a query such as
  %1POWR 1

Then given a result of
  %1POWR=OK
will return no values.
Will error on error responses such as ERR1, ERRA, ERR3 etc."
  (%write-command stream digest class command params)
  (%with-response-buffer in
    ;;Get the response params
    (let* ((rlen (%read-pjlink-command-line in stream))
           (param-len (%validate-get-result class command in rlen)))
      (unless param-len
        (if (string-equal in "PJLINK ERRA" :end1 rlen)
            (error 'authorization-error :host host :class class :command command)
            (error "Bad set response: '~A'" (subseq in 0 rlen))))
      (when (and (= param-len 4) (string-equal in "ERR" :start1 7 :end1 10))
        (ecase (char in 10)
          (#\1 (error 'undefined-command-error :host host :class class :command command))
          (#\2 (error 'out-of-parameter-error :host host :class class :command command :parameter params))
          (#\3 (error 'unavailable-time-error :host host :class class :command command))
          (#\4 (error 'projector-display-error :host host :class class :command command))))
      (unless (and (= param-len 2) (string-equal in "OK" :start1 7 :end1 9))
        (error "set '~A' failed with response '~A'" command (subseq in 7 (+ 7 param-len))))
      (values))))

(defun %read-line-and-generate-digest (stream password)
  ;;Read the initial connection line and figure out the digest to use
  (let ((buffer (make-string 18)))
    (declare (dynamic-extent buffer))
    (let ((rlen (%read-pjlink-command-line buffer stream)))
      (unless (%verify-connect-response buffer rlen)
        (error "Invalid response '~A'" (subseq buffer 0 rlen)))
      (%create-digest buffer password))))

(defmacro %with-pjlink-connection ((stream-var digest-var)
                                   (host
                                    &key
                                      (port +pjlink-port+)
                                      (password nil)
                                      (local-host nil)
                                      (local-port nil))
                                   &body body)
  (with-gensyms (host-sym port-sym password-sym local-host-sym local-port-sym
                          socket)
    `(let* ((,host-sym ,host)
            (,port-sym ,port)
            (,password-sym ,password)
            (,local-host-sym ,local-host)
            (,local-port-sym ,local-port)
            (,socket (usocket:socket-connect ,host-sym ,port-sym
                                             :element-type 'character
                                             :local-host ,local-host-sym
                                             :local-port ,local-port-sym)))
       (unwind-protect
            (let* ((,stream-var (usocket:socket-stream ,socket))
                   (,digest-var (%read-line-and-generate-digest ,stream-var ,password-sym)))
              ,@body)
         (usocket:socket-close ,socket)))))

(defmacro %defpjlink-get (name (class command) (&whole input-transform &optional input-args &body transform-body) (result-var) &body body)
  (with-gensyms (host-info port password local-host local-port host args stream digest)
    (multiple-value-bind (body decl doc)
        (parse-body body :documentation t)
      `(defun ,name (,@input-args ,host-info
                     &key
                       ((:port ,port) (port ,host-info))
                       ((:password ,password) (password ,host-info))
                       ((:local-host ,local-host) (local-host ,host-info))
                       ((:local-port ,local-port) (local-port ,host-info))
                     &aux
                       (,host (host ,host-info))
                       (,args ,(if input-transform `(progn ,@transform-body) "")))
         ,doc
         (let ((,result-var
                 (%with-pjlink-connection (,stream ,digest)
                     (,host :password ,password :port ,port :local-host ,local-host :local-port ,local-port)
                   (%pjlink-get ,host ,stream ,digest ,class ,command ,args))))
           ,@decl
           ,@body)))))

(defmacro %defpjlink-set (name (class command) args &body body)
  (with-gensyms (host-info port password local-host local-port host stream digest)
    (multiple-value-bind (body decl doc)
        (parse-body body :documentation t)
      `(defun ,name (,@args
                     ,host-info
                     &key
                       ((:port ,port) (port ,host-info))
                       ((:password ,password) (password ,host-info))
                       ((:local-host ,local-host) (local-host ,host-info))
                       ((:local-port ,local-port) (local-port ,host-info))
                     &aux
                       (,host (host ,host-info)))
         ,doc
         ,@decl
         (%with-pjlink-connection (,stream ,digest)
             (,host :password ,password :port ,port :local-host ,local-host :local-port ,local-port)
           (%pjlink-set ,host ,stream ,digest ,class ,command (progn ,@body)))
         (values)))))
