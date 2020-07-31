;;;; PJLink base communication and types

(in-package #:pjlink)

(defconstant +default-port+ 4352
  "Default PJLink port.")

(defconstant +max-password-length+ 32
  "Max number of characters in a PJLink password.")

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
    "Use `obj' as a `hostname'."
    (check-type obj hostname)
    obj))

(defgeneric port (obj)
  (:documentation "Get the port designated by `obj'")
  (:method (obj)
    "Use the default PJLink port."
    (declare (ignore obj))
    +default-port+))

(defgeneric password (obj)
  (:documentation "Get the password designated by `obj'.
nil if no password is to be used.")
  (:method (obj)
    "Provide no password"
    (declare (ignore obj))
    nil))

(defgeneric local-host (obj)
  (:documentation "Get the `local-host' designated by `obj'.")
  (:method (obj)
    "No specific local-host"
    (declare (ignore obj))
    nil))

(defgeneric local-port (obj)
  (:documentation "Get the `local-port' designated by `obj'.")
  (:method (obj)
    "No specific local-port"
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
    :initform +default-port+
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

(declaim (inline %chars->string))
(defun %chars->string (&rest characters)
  (declare (dynamic-extent characters))
  (coerce characters 'string))

(define-compiler-macro %chars->string (&rest characters)
  `(coerce (list ,@characters) 'string))

(defun %nibble->hex (nibble)
  "Convert a nibble into its hex char."
  (check-type nibble (unsigned-byte 4))
  (char "0123456789abcdef" nibble))

(defun %class->char (class)
  "Convert a class number to its character representation."
  (check-type class (integer 1 9))
  (char "0123456789" class))

(defun %read-pjlink-command-line (buffer stream)
  "Reads a pjlink command-line (delimited by #\Return) from `stream` into `buffer`
 Returns the number of characters read, before encountering #\Return"
  (loop
    :for b := (read-char stream)
    :for i :from 0 :below (length buffer)
    ;; #\Return not included
    :until (char= b #\Return)
    :do (setf (char buffer i) b)
    :finally (return i)))

(defun %md5->hex-str (md5)
  "Convert a 16-octet md5 hash into a 32-char hex-encoded string."
  (loop
    :with hex-digest := (make-string 32)
    :for nidx :from 0 :below 16
    :for hidx :from 0 :below 32 :by 2
    :for byte := (aref md5 nidx)
    :do (setf (char hex-digest (+ hidx 0)) (%nibble->hex (ldb (byte 4 4) byte))
              (char hex-digest (+ hidx 1)) (%nibble->hex (ldb (byte 4 0) byte)))
    :finally (return hex-digest)))

(defconstant +seed-length+ 8
  "The number of random characters we receive from an authentication line, to be used as salt in our MD5 hash.")

(defun %encrypt-password (seed password &optional (seed-start 0))
  "Create a PJLink authentication digest from `seed' and `password'
 `seed-start' indicates where reading from `seed' should start.
And password a sequence of characters length 32 or less."
  (let ((seed-end (+ seed-start +seed-length+))
        (plen (length password)))
    (unless (<= seed-end (length seed))
      (error "Seed buffer too short (~D). Should be at least ~D." (length seed) +seed-length+))
    (unless (<= plen +max-password-length+)
      (error "Password length too long (~D). Max password length is ~D." plen +max-password-length+))
    (let ((buffer (make-string (+ +seed-length+ plen))))
      (declare (dynamic-extent buffer))
      (replace buffer seed :start2 seed-start :end2 seed-end)
      (replace buffer password :start1 +seed-length+ :end2 plen)
      (%md5->hex-str (md5:md5sum-string buffer :end (+ +seed-length+ plen))))))

(defun %verify-connection-response-and-generate-digest (response password &optional (rlen (length response)))
  "Verifies that `response' is a valid connection response, and generates a digest from it and `password'
 `rlen' is the number of characters in `response' we should read."
  (or
   ;; Authentication disabled
   (and (string-equal response "PJLINK 0" :end1 rlen)
        ;; Empty digest
        "")
   ;; Authentication enabled.
   ;; Eg "PJLINK 1 12345678"
   (and (= rlen #.(+ #2=#.(length #1="PJLINK 1 ") +seed-length+))
        (string-equal response #1# :end1 #2#)
        ;; Create digest
        (%encrypt-password response password #2#))
   (error "Invalid connection response '~A'" (subseq response 0 rlen))))

(defconstant +max-connection-response-length+ 18
  "Max length of an initial connection response.
 This is one with authentication enabled:
  PJLINK 1 01234567<CR>")

(defun %auth-handshake (stream password)
  "Read the initial connection line and output a digest, if necessary."
  (let* ((buffer (make-string +max-connection-response-length+))
         (rlen (%read-pjlink-command-line buffer stream)))
    (declare (dynamic-extent buffer))
    (let ((digest (%verify-connection-response-and-generate-digest buffer password rlen)))
      (unless (zerop (length digest))
        (write-sequence digest stream))))
  (values))

(defmacro %with-pjlink-connection ((stream-var)
                                   (host
                                    &key
                                      (port +default-port+)
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
            (let ((,stream-var (usocket:socket-stream ,socket)))
              (%auth-handshake ,stream-var ,password-sym)
              ,@body)
         (usocket:socket-close ,socket)))))

(defconstant +max-command-line-length+ (+ 1 1 4 1 128 1)
  "The max length of a PJLink command line:
 Header(1)  Class(1)  Command(4)  Separator(1)  Param(128)  CR(1)")

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

(defun %valid-command-response-p (class command response &optional (rlen (length response)))
  "Returns true if the response `response' is a valid response string, given `class' and `command'
 Given `class' = 1, and `command' = \"POWR\", a valid response would be
  %1POWR="
  (and (>= rlen 7)
       (char= (char response 0) #\%)
       (char= (char response 1) (%class->char class))
       (string-equal response command :start1 2 :end1 6)
       (char= (char response 6) #\=)))

(defun %read-response (host stream class command param)
  "Reads and checks a PJLink response from `stream', and returns the result."
  (let* ((response (make-string +max-command-line-length+))
         (rlen (%read-pjlink-command-line response stream)))
    (declare (dynamic-extent response))
    (when (string-equal response "PJLINK ERRA" :end1 rlen)
      (error 'authorization-error :host host :class class :command command))
    (unless (%valid-command-response-p class command response rlen)
      (error "Bad response: '~A'" (subseq response 0 rlen)))
    (let* ((header-len #.(length "%1XXXX="))
           (err-len #.(length "ERR"))
           (param-len (- rlen header-len)))
      ;; If the response is an error string
      (when (and (= param-len #.(length "ERRX"))
                 (string-equal response "ERR" :start1 header-len :end1 (+ header-len err-len)))
        (case (char response (+ header-len err-len))
          (#\1 (error 'undefined-command-error :host host :class class :command command))
          (#\2 (error 'out-of-parameter-error :host host :class class :command command :parameter param))
          (#\3 (error 'unavailable-time-error :host host :class class :command command))
          (#\4 (error 'projector-display-error :host host :class class :command command))))
      (subseq response header-len rlen))))

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


(defun %write-command (stream class command &rest params)
  "Writes a PJLink command to `stream' using `class', `command', and `params'
 eg.
  %1CLSS ?<Return>"
  (declare (dynamic-extent params))
  (check-type command (string 4))
  (let ((buf (make-string +max-command-line-length+))
        (len 0))
    (declare (dynamic-extent buf))

    ;;Add %
    (setf (char buf len) #\%)
    (incf len)

    ;; Add class
    (setf (char buf len) (%class->char class))
    (incf len)

    ;;Add command type
    (replace buf command :start1 len)
    (incf len (length command))

    ;;Space
    (setf (char buf len) #\Space)
    (incf len)

    ;;Add params
    (dolist (p params)
      (let ((plen (length p)))
        (when (> (+ plen len) (1- +max-command-line-length+))
          (error "Parameter length too long"))
        (replace buf p :start1 len)
        (incf len plen)))

    ;;End with #\Return
    (setf (char buf len) #\Return)
    (incf len)

    (write-sequence buf stream :end len)
    (finish-output stream)))

(defun %pjlink-get (host stream class command param)
  "Conducts a `get` command on `stream`, and returns the result string
uses
 `class' as the command class
 `command' as the command name
 `param' as the command parameter, if any

This will issue a query such as
  %1POWR ?

Then given a result of
  %1POWR=0
returns the string \"0\""
  (%write-command stream class command "?" param)
  (%read-response host stream class command param))

(defun %pjlink-set (host stream class command param)
  "Conducts a `set` command on `stream`, and returns the result string
uses
 `class' as the command class
 `command' as the command name
 `param' as the command parameter, if any

This will issue a query such as
  %1POWR 1

Then given a result of
  %1POWR=OK
will return no values.
Will error on error responses such as ERR1, ERRA, ERR3 etc."
  (%write-command stream class command param)
  (let ((response (%read-response host stream class command param)))
    (unless (string-equal response "OK")
      (error "set '~A' failed with response '~A'" command response)))
  (values))

(defmacro %defpjlink-get (name (class command) (&whole input-transform &optional input-args &body transform-body) (result-var) &body body)
  "Helper macro to create a GET function.
 `name' is the name of the resulting function
 `class' indicates the class number for the command - eg 1
 `command' is the string designating the command - eg \"POWR\"
 `input-args' and `transform-body' are used to receive input arguments and transform them into string PJLink parameters
  eg. ((input-type input-number)
        (%input2->string input-type input-number))
  arguments defined in `input-args' always preceed the host and key args
 `result-var' is the variable holding the result of the get
 `body' is the body of the function, which processes the result."
  (with-gensyms (args stream)
    (multiple-value-bind (body decl doc)
        (parse-body body :documentation t)
      `(defun ,name (,@input-args host-info &key port password local-host local-port)
         ,@(ensure-list doc)
         (let* ((host (host host-info))
                (port (or port (port host-info)))
                (password (or password (password host-info)))
                (local-host (or local-host (local-host host-info)))
                (local-port (or local-port (local-port host-info)))
                (,args ,(if input-transform `(progn ,@transform-body) ""))
                (,result-var
                  (%with-pjlink-connection (,stream)
                      (host :password password :port port :local-host local-host :local-port local-port)
                    (%pjlink-get host ,stream ,class ,command ,args))))
           ,@decl
           ,@body)))))

(defmacro %defpjlink-set (name (class command) args &body body)
  "Helper macro to create a SET function.
 `name' is the name of the resulting function
 `class' indicates the class number for the command - eg 1
 `command' is the string designating the command - eg \"POWR\"
 `args' is a list of additional arguments to the function. These always preceed the host and key arguments.
 `body' is the body of the function, which generates the string PJLink parameter to send."
  (with-gensyms (stream)
    (multiple-value-bind (body decl doc)
        (parse-body body :documentation t)
      `(defun ,name (,@args host-info &key port password local-host local-port)
         ,@(ensure-list doc)
         ,@decl
         (let ((host (host host-info))
               (port (or port (port host-info)))
               (password (or password (password host-info)))
               (local-host (or local-host (local-host host-info)))
               (local-port (or local-port (local-port host-info))))
           (%with-pjlink-connection (,stream)
               (host :password password :port port :local-host local-host :local-port local-port)
             (%pjlink-set host ,stream ,class ,command (progn ,@body))))
         (values)))))
