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
    :type hostname
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
  (ecase nibble
    (0 #\0)
    (1 #\1)
    (2 #\2)
    (3 #\3)
    (4 #\4)
    (5 #\5)
    (6 #\6)
    (7 #\7)
    (8 #\8)
    (9 #\9)
    (10 #\a)
    (11 #\b)
    (12 #\c)
    (13 #\d)
    (14 #\e)
    (15 #\f)))

(defun %md5->hex-str (md5)
  "Convert a 16-octet md5 hash into a 32-char hex-encoded string."
  (loop
    :with hex-digest := (make-array 32 :element-type 'character)
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
  (let ((buffer (make-array 40 :element-type 'character)))
    (declare (dynamic-extent buffer))
    (replace buffer connection-response :start2 9 :end2 17)
    (replace buffer password :start1 8 :end2 plen)
    (values (%md5->hex-str (md5:md5sum-string buffer :end (+ 8 plen))))))

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
             (>= rlen 17)))))

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

(defmacro %with-command-buffer ((buffer digest class command &rest params) &body body)
  "Create a buffer pre-filled with a PJLink command using `digest`, `class`, `command` and `params`
eg.
  %1CLSS ?<Return>"
  (with-gensyms (digest-sym class-sym command-sym idx)
    `(let ((,digest-sym ,digest)
           (,class-sym ,class)
           (,command-sym ,command)
           (,buffer (make-array 168 :element-type 'character))
           (,idx 0))
       (declare (dynamic-extent ,buffer))
       ;;Prepend the digest (if any)
       (replace ,buffer ,digest-sym)
       (incf ,idx (length ,digest-sym))

       ;;Add %1
       (setf (char ,buffer (1- (incf ,idx))) #\%
             (char ,buffer (1- (incf ,idx))) (code-char (+ (char-code #\0) ,class-sym)))

       ;;Add command type
       (replace ,buffer ,command-sym :start1 ,idx)
       (incf ,idx (length ,command-sym))

       ;;Space
       (setf (char ,buffer (1- (incf ,idx))) #\Space)

       ;;Add params
       ,@(with-gensyms (params-sym)
           (loop
             :for p :in params
             :collecting
             `(let ((,params-sym ,p))
                (replace ,buffer ,params-sym :start1 ,idx)
                (incf ,idx (length ,params-sym)))))

       ;;End with #\Return
       (setf (char ,buffer (1- (incf ,idx))) #\Return)
       (locally ,@body))))

(defmacro %with-response-buffer (buffer &body body)
  `(let ((,buffer (make-array 136 :element-type 'character)))
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
  (%with-command-buffer (out digest class command "?" args)
    (write-sequence out stream)
    (finish-output stream))
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
  (%with-command-buffer (out digest class command params)
    (write-sequence out stream)
    (finish-output stream))
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
  (let ((buffer (make-array 18 :element-type 'character)))
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
  (with-gensyms (input-transform-fn config host port password local-host local-port stream digest)
    (multiple-value-bind (body decl doc)
        (parse-body body :documentation t)
      `(progn
         (flet ((,input-transform-fn (,@input-args)
                  ,@(if input-transform
                        transform-body
                        '(""))))
           (defgeneric ,name (,@input-args ,config &key &allow-other-keys)
             (:documentation ,doc))
           (defmethod ,name (,@input-args ,host
                             &key
                               ((:port ,port) +pjlink-port+)
                               ((:password ,password) nil)
                               ((:local-host ,local-host) nil)
                               ((:local-port ,local-port) nil))
             ,doc
             (let ((,result-var
                     (%with-pjlink-connection (,stream ,digest)
                         (,host :password ,password :port ,port :local-host ,local-host :local-port ,local-port)
                       (%pjlink-get ,host ,stream ,digest ,class ,command (,input-transform-fn ,@input-args)))))
               ,@decl
               ,@body))
           (defmethod ,name (,@input-args (,config pjlink-config) &key)
             ,doc
             (let ((,result-var
                     (%with-pjlink-connection (,stream ,digest)
                         ((host ,config) :password (password ,config) :port (port ,config) :local-host (local-host ,config) :local-port (local-port ,config))
                       (%pjlink-get (host ,config) ,stream ,digest ,class ,command (,input-transform-fn ,@input-args)))))
               ,@decl
               ,@body)))))))

(defmacro %defpjlink-set (name (class command) args &body body)
  (with-gensyms (config host port password local-host local-port stream digest)
    (multiple-value-bind (body decl doc)
        (parse-body body :documentation t)
      `(progn
         (defgeneric ,name (,@args ,config &key &allow-other-keys))
         (defmethod ,name (,@args
                           ,host
                           &key
                             ((:port ,port) +pjlink-port+)
                             ((:password ,password) nil)
                             ((:local-host ,local-host) nil)
                             ((:local-port ,local-port) nil))
           ,doc
           ,@decl
           (%with-pjlink-connection (,stream ,digest)
               (,host :password ,password :port ,port :local-host ,local-host :local-port ,local-port)
             (%pjlink-set ,host ,stream ,digest ,class ,command (progn ,@body)))
           (values))
         (defmethod ,name (,@args (,config pjlink-config) &key)
           ,doc
           ,@decl
           (%with-pjlink-connection (,stream ,digest)
               ((host ,config) :password (password ,config) :port (port ,config) :local-host (local-host ,config) :local-port (local-port ,config))
             (%pjlink-set (host ,config) ,stream ,digest ,class ,command (progn ,@body)))
           (values))))))
