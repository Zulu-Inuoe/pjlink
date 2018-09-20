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
    :initform #(127 0 0 1)
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

(deftype power-status ()
  "Power status of a projector.
see `power-on', `power-off', and `get-power-status'"
  '(member :standby :lamp-on :cooling :warm-up))

(deftype input-type ()
  "An input type for a projector.
Note that a projector may have several inputs of the same type, identified by an `input-number`
see `get-input', `set-input', and `get-inputs'"
  '(member :rgb :video :digital :storage :network))

(defclass projector-input ()
  ((%type
    :type input-type
    :initarg :type
    :initform (error "Must supply type")
    :reader input-type)
   (%number
    :type (integer 1 9)
    :initarg :number
    :initform (error "Must supply number")
    :reader input-number))
  (:documentation
   "An available input for a projector.
Note that projectors may have multiple inputs of the same type, differentiated by number.
Note that projector input numbers start at 1, not 0."))

(deftype av-mute-status ()
  "Status of the audio-video mute setting on a projector.
Audio-video mute will cease output of audio or video, without powering off the projector.
see `get-av-mute'
see `set-av-mute'"
  '(member :vm-on :am-on :avm-on :avm-off))

(deftype error-status ()
  "Status of a component of a projector.
see `get-error-status'
see `projector-status'"
  '(member :ok :warning :error))

(defclass projector-status ()
  ((%fan-status
    :type error-status
    :initarg :fan
    :initform (error "Must supply fan status")
    :reader fan-status)
   (%lamp-status
    :type error-status
    :initarg :lamp
    :initform (error "Must supply lamp status")
    :reader lamp-status)
   (%temperature-status
    :type error-status
    :initarg :temperature
    :initform (error "Must supply temperature status")
    :reader temperature-status)
   (%cover-open-status
    :type error-status
    :initarg :cover-open
    :initform (error "Must supply cover-open status")
    :reader cover-open-status)
   (%filter-status
    :type error-status
    :initarg :filter
    :initform (error "Must supply filter status")
    :reader filter-status)
   (%other-status
    :type error-status
    :initarg :other
    :initform (error "Must supply other status")
    :reader other-status))
  (:documentation
   "The status of projector components."))

(defclass projector-lamp ()
  ((%number
    :type (integer 0 8)
    :initarg :number
    :initform (error "Must supply number")
    :reader lamp-number)
   (%hours
    :type (integer 0 99999)
    :initarg :hours
    :initform (error "Must supply hours")
    :reader lamp-hours)
   (%is-on
    :type boolean
    :initarg :is-on
    :initform (error "Must supply is-on")
    :reader lamp-is-on)))

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

(defmacro %with-command-buffer ((buffer digest class command params) &body body)
    "Create a buffer pre-filled with a PJLink command using `digest`, `class`, `command` and `params`
eg.
  %1CLSS ?<Return>"
  (with-gensyms (digest-sym class-sym command-sym params-sym idx)
    `(let ((,digest-sym ,digest)
           (,class-sym ,class)
           (,command-sym ,command)
           (,params-sym ,params)
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
       (replace ,buffer ,params-sym :start1 ,idx)
       (incf ,idx (length ,params-sym))

       ;;End with #\Return
       (setf (char ,buffer (1- (incf ,idx))) #\Return)
       (locally ,@body))))

(defmacro %with-response-buffer (buffer &body body)
  `(let ((,buffer (make-array 136 :element-type 'character)))
     (declare (dynamic-extent ,buffer))
     ,@body))

(defun %validate-get-result (class command response len)
  (and (>= len 8)
       (char= (char response 0) #\%)
       (char= (char response 1) (code-char (+ (char-code #\0) class)))
       (string-equal response command :start1 2 :end1 6)
       (char= (char response 6) #\=)
       (- len 7)))

(defun %powr->sym (input-val)
  (ecase input-val
    (#\0 :standby)
    (#\1 :lamp-on)
    (#\2 :cooling)
    (#\3 :warm-up)))

(defun %input->sym (input-val)
  (ecase input-val
    (#\1 :rgb)
    (#\2 :video)
    (#\3 :digital)
    (#\4 :storage)
    (#\5 :network)))

(defun %input->string (input-type input-number)
  (unless (<= 1 input-number 9)
    (error "Invalid input number '~A'" input-number))
  (format nil "~A~D"
          (ecase input-type
            (:rgb #\1)
            (:video #\2)
            (:digital #\3)
            (:storage #\4)
            (:network #\5))
          input-number))

(defun %avmt->sym (response)
  (eswitch (response :test #'string-equal)
    ("11" :vm-on)
    ("21" :am-on)
    ("31" :avm-on)
    ("30" :avm-off)))

(defun %avmt->string (avmt)
  (ecase avmt
    (:vm-on "11")
    (:am-on "21")
    (:avm-on "31")
    (:avm-off "30")))

(defun %erst->sym (erst)
  (ecase erst
    (#\0 :ok)
    (#\1 :warning)
    (#\2 :error)))

(defun %lamp-str->lamp-infos (lamps-str)
  "Parses a lamp string into a list of `lamp-info`'s
`lamps-str` should be a string where each lamp is represented by

  <Hours> SPC <OnOrOff>

Additional lamps are separated by spaces.

eg
  8262 1 13451 1 198 0"
  (loop
    :with idx := 0
    :while (< idx (length lamps-str))
    :for lamp-number :from 0 :by 1
    :for valid := (or (zerop idx)
                      (char= (char lamps-str (1- (incf idx))) #\Space)
                      (error "Malformed lamp string: '~A'" lamps-str))
    :for hours :=
               (loop
                 :with start := idx
                 :until (char= (char lamps-str (1- (incf idx))) #\Space)
                 :finally
                    (return (parse-integer lamps-str :start start :end (1- idx) :radix 10)))
    :for is-on := (ecase (char lamps-str (1- (incf idx)))
                    (#\0 nil)
                    (#\1 t))
    :collecting (make-instance 'projector-lamp :number lamp-number :hours hours :is-on is-on)))

(defun %inst-str->input-infos (inst-str)
  "Parses a inst string into a list of `input-info`'s
`inst-str` should be a string where each input is represented by

  <Type><Number>

Additional inputs are separated by spaces.

eg
  11 2Z 3E"
  (loop
    :with idx := 0
    :while (< idx (length inst-str))
    :for valid := (or (zerop idx)
                      (char= (char inst-str (1- (incf idx))) #\Space)
                      (error "Malformed inst string: '~A'" inst-str))
    :for type := (%input->sym (char inst-str (1- (incf idx))))
    :for number := (parse-integer inst-str :start (1- (incf idx)) :end idx :radix 36)
    :collecting (make-instance 'projector-input :type type :number number)))

(defun %pjlink-get (stream digest class command)
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
  (%with-command-buffer (out digest class command "?")
    (write-sequence out stream)
    (finish-output stream))
  (%with-response-buffer in
    ;;Get the response params
    (let* ((rlen (%read-pjlink-command-line in stream))
           (param-len (%validate-get-result class command in rlen)))
      (unless param-len
        (if (string-equal in "PJLINK ERRA" :end1 rlen)
            (error "Authentication error")
            (error "Bad get response: '~A'" (subseq in 0 rlen))))
      (when (and (= param-len 4) (string-equal in "ERR" :start1 7 :end1 10))
        (error
         (format nil "get '~A'(~D): ~A" command class
                 (ecase (char in 10)
                   (#\1 "Undefined command")
                   (#\2 "Out of parameter")
                   (#\3 "Unavailable time")
                   (#\4 "Projector/Display failure")))))
      (subseq in 7 (+ 7 param-len)))))

(defun %pjlink-set (stream digest class command params)
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
            (error "Authentication error")
            (error "Bad set response: '~A'" (subseq in 0 rlen))))
      (when (and (= param-len 4) (string-equal in "ERR" :start1 7 :end1 10))
        (error
         (format nil "set '~A'(~D) '~A': ~A" command class params
                 (ecase (char in 10)
                   (#\1 "Undefined command")
                   (#\2 "Out of parameter")
                   (#\3 "Unavailable time")
                   (#\4 "Projector/Display failure")))))
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

(defmacro %defpjlink-get (name (class command) (result-var) &body body)
  (with-gensyms (config host port password local-host local-port stream digest)
    (multiple-value-bind (body decl doc)
        (parse-body body :documentation t)
      `(progn
         (defgeneric ,name (,config &key &allow-other-keys))
         (defmethod ,name (,host
                           &key
                             ((:port ,port) +pjlink-port+)
                             ((:password ,password) nil)
                             ((:local-host ,local-host) nil)
                             ((:local-port ,local-port) nil))
           ,doc
           (let ((,result-var
                   (%with-pjlink-connection (,stream ,digest)
                       (,host :password ,password :port ,port :local-host ,local-host :local-port ,local-port)
                     (%pjlink-get ,stream ,digest ,class ,command))))
             ,@decl
             ,@body))
         (defmethod ,name ((,config pjlink-config) &key)
           ,doc
           (let ((,result-var
                   (%with-pjlink-connection (,stream ,digest)
                       ((host ,config) :password (password ,config) :port (port ,config) :local-host (local-host ,config) :local-port (local-port ,config))
                     (%pjlink-get ,stream ,digest ,class ,command))))
             ,@decl
             ,@body))))))

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
             (%pjlink-set ,stream ,digest ,class ,command (progn ,@body)))
           (values))
         (defmethod ,name (,@args (,config pjlink-config) &key)
           ,doc
           ,@decl
           (%with-pjlink-connection (,stream ,digest)
               ((host ,config) :password (password ,config) :port (port ,config) :local-host (local-host ,config) :local-port (local-port ,config))
             (%pjlink-set ,stream ,digest ,class ,command (progn ,@body)))
           (values))))))

(%defpjlink-set power-on (1 "POWR") ()
  "Instruct the projector to power on."
  "1")

(%defpjlink-set power-off (1 "POWR") ()
  "Instruct the projector to power off."
   "0")

(%defpjlink-get get-power-status (1 "POWR") (result)
  "Query the `power-status' of the projector.
see `set-port-on', `set-power-off'"
  (%powr->sym (char result 0)))

(%defpjlink-set set-input (1 "INPT") (input-type input-number)
  "Sets the input to the given `input-type' and `input-number'
see `set-input*', `get-input'"
  (%input->string input-type input-number))

(%defpjlink-set set-input* (1 "INPT") (input-info)
  "As `set-input' but using a `projector-input' object instead."
  (%input->string (input-type input-info) (input-number input-info)))

(%defpjlink-get get-input (1 "INPT") (result)
  "Query the current `projector-input' on the projector."
  (make-instance
     'projector-input
     :type (%input->sym (char result 0))
     :number (parse-integer result :start 1 :end 2 :radix 10)))

(%defpjlink-set set-av-mute (1 "AVMT") (avmt)
  "Set the `av-mute-status' on the projector.
see `get-av-mute'"
  (%avmt->string avmt))

(%defpjlink-get get-av-mute (1 "AVMT") (result)
  "Query the current `av-mute-status' of the projector.
see `set-av-mute'"
  (%avmt->sym result))

(%defpjlink-get get-error-status (1 "ERST") (result)
  "Query the `projector-status' projector."
  (make-instance
     'projector-status
     :fan (%erst->sym (char result 0))
     :lamp (%erst->sym (char result 1))
     :temperature (%erst->sym (char result 2))
     :cover-open (%erst->sym (char result 3))
     :filter (%erst->sym (char result 4))
     :other (%erst->sym (char result 5))))

(%defpjlink-get get-lamps (1 "LAMP") (result)
  "Query the available `projector-lamp's on the projector as a list."
  (%lamp-str->lamp-infos result))

(%defpjlink-get get-inputs (1 "INST") (result)
  "Query the available `projector-input's on the projector as a list."
  (%inst-str->input-infos result))

(%defpjlink-get get-projector-name (1 "NAME") (result)
  "Query the projector's name."
  result)

(%defpjlink-get get-manufacturer-name (1 "INF1") (result)
  "Query the projector's manufacturer name."
  result)

(%defpjlink-get get-product-name (1 "INF2") (result)
  "Query the projector's product name."
  result)

(%defpjlink-get get-other-info (1 "INFO") (result)
  "Query other information about the projector."
  result)

(%defpjlink-get get-pjlink-class (1 "CLSS") (result)
  "Query the pjlink class of the projector."
  (values (parse-integer result :radix 36)))
