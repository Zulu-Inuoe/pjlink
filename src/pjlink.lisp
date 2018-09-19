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

(defconstant +pjlink-port+ 4352)

(defclass %pjlink-connection ()
  ((%socket
    :type usocket:socket
    :initarg :socket
    :initform (error "Must supply socket")
    :reader %socket)
   (%digest
    :type (or null (simple-array character 32))
    :initarg :digest
    :initform (error "Must supply digest")
    :reader %digest)
   (%class
    :type integer
    :initarg :class
    :initform (error "Must supply class")
    :reader %class)))

(defclass %error-status ()
  ((%fan-status
    :type keyword
    :initarg :fan
    :initform (error "Must supply fan status")
    :accessor fan-status)
   (%lamp-status
    :type keyword
    :initarg :lamp
    :initform (error "Must supply lamp status")
    :accessor lamp-status)
   (%temperature-status
    :type keyword
    :initarg :temperature
    :initform (error "Must supply temperature status")
    :accessor temperature-status)
   (%cover-open-status
    :type keyword
    :initarg :cover-open
    :initform (error "Must supply cover-open status")
    :accessor cover-open-status)
   (%filter-status
    :type keyword
    :initarg :filter
    :initform (error "Must supply filter status")
    :accessor filter-status)
   (%other-status
    :type keyword
    :initarg :other
    :initform (error "Must supply other status")
    :accessor other-status)))

(defclass %lamp-info ()
  ((%number
    :type integer
    :initarg :number
    :initform (error "Must supply number")
    :accessor lamp-number)
   (%hours
    :type integer
    :initarg :hours
    :initform (error "Must supply hours")
    :accessor hours)
   (%is-on
    :type boolean
    :initarg :is-on
    :initform (error "Must supply is-on")
    :accessor is-on)))

(defclass %input-info ()
  ((%type
    :type keyword
    :initarg :type
    :initform (error "Must supply type")
    :accessor input-type)
   (%number
    :type integer
    :initarg :number
    :initform (error "Must supply number")
    :accessor input-number)))

(defun %encrypt-password (password seed &key (seed-start 0))
  "Create the authentication response given `password` and `seed`.
`password` should be a string length 32 or less
`seed` should be a string, length 8
`seed-start` denotes where to offset `seed`. 8 characters will be read from that offset."
  (unless (<= (length password) 32)
    (error "Password length too long"))
  (let ((buffer (make-array 40 :element-type 'character)))
    (declare (dynamic-extent buffer))
    (replace buffer seed :start2 seed-start :end2 (+ seed-start 8))
    (replace buffer password :start1 8)
    (let ((md5 (md5:md5sum-string buffer :end (+ 8 (length password)))))
      ;;We need to convert the md5 into a hex string
      (with-output-to-string (str)
        (loop
          :for b :across md5
          :do (format str "~(~2,'0X~)" b))))))

(defun %coerce-password (password
                         &aux
                           (password (if (functionp password) (funcall password) password)))
  "Coerces a password into a string`
`password` can be a string, or a function of no arguments which returns a string."
  (etypecase password
    (string
     (unless (<= (length password) 32)
       (error "Password length too long"))
     password)))

(defun %verify-connect-response (response)
  "Verifies the initial connection response.
Returns nil if the response does not match a proper pjlink connection response."
  (and (>= (length response) 8)
       (string-equal response "PJLINK " :end1 7)
       (or
        ;; Authentication disabled
        (and (char= (char response 7) #\0))
        ;; Authentication enabled. Expect seed
        (and (char= (char response 7) #\1)
             (char= (char response 8) #\Space)
             (>= (length response) 17)))))

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
     ;;Authentication
     (%encrypt-password (%coerce-password password) connection-response :seed-start 9))))

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

(defmacro %with-command-buffer ((buffer connection class command params) &body body)
  "Create a buffer pre-filled with a PJLink command using `class`, `command` and `params`
eg.
  %1CLSS ?<Return>

When authentication is enabled, also prepends the `connection`'s digest string."
  `(%with-command-buffer-impl (,buffer (%digest ,connection) ,class ,command ,params)
     ,@body))

(defmacro %with-command-buffer-impl ((buffer digest class command params) &body body)
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
             (char ,buffer (1- (incf ,idx))) ,class-sym)

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
       (char= (char response 1) class)
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
                      (error "Malformed lamp string"))
    :for hours :=
               (loop
                 :with start := idx
                 :until (char= (char lamps-str (1- (incf idx))) #\Space)
                 :finally
                    (return (parse-integer lamps-str :start start :end (1- idx) :radix 10)))
    :for is-on := (ecase (char lamps-str (1- (incf idx)))
                    (#\0 nil)
                    (#\1 t))
    :collecting (make-instance '%lamp-info :number lamp-number :hours hours :is-on is-on)))

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
                      (error "Malformed inst string"))
    :for type := (%input->sym (char inst-str (1- (incf idx))))
    :for number := (parse-integer inst-str :start (1- (incf idx)) :end idx :radix 36)
    :collecting (make-instance '%input-info :type type :number number)))

(defun %pjlink-get-impl (stream digest class command)
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
  (%with-command-buffer-impl (out digest class command "?")
    (write-sequence out stream)
    (finish-output stream))
  (%with-response-buffer in
    ;;Get the response params
    (let* ((rlen (%read-pjlink-command-line in stream))
           (param-len (%validate-get-result class command in rlen)))
      (unless (null param-len)
        (when (and (= param-len 4) (string-equal in "ERR" :start1 7 :end1 10))
          (error
           (ecase (char in 10)
             (#\1 "Undefined command")
             (#\3 "Unavailable time")
             (#\4 "Projector/Display failure")
             ((#\a #\A) "Authentication error"))))
        (subseq in 7 (+ 7 param-len))))))

(defun %pjlink-clss?-impl (stream digest)
  "Lower level CLSS query used during initialization to verify authentication and device class."
  (let ((result (%pjlink-get-impl stream digest #\1 "CLSS")))
    (values (parse-integer result :radix 36))))

(defun %pjlink-get (connection class command
                    &aux (stream (usocket:socket-stream (%socket connection))))
  "See `%pjlink-get-impl`. This is a convenience function taking in a connection instead."
  (%pjlink-get-impl stream (%digest connection) class command))

(defun %pjlink-set (connection class command params
                    &aux
                      (stream (usocket:socket-stream (%socket connection)))
                      (digest (%digest connection)))
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
  (%with-command-buffer-impl (out digest class command params)
    (write-sequence out stream)
    (finish-output stream))
  (%with-response-buffer in
    ;;Get the response params
    (let* ((rlen (%read-pjlink-command-line in stream))
           (param-len (%validate-get-result class command in rlen)))
      (unless param-len
        (error "Bad set response: ~A" (subseq in rlen)))
      (when (and (= param-len 4) (string-equal in "ERR" :start1 7 :end1 10))
        (error
         (ecase (char in 10)
           (#\1 "Undefined command")
           (#\3 "Unavailable time")
           (#\4 "Projector/Display failure")
           ((#\a #\A) "Authentication error"))))
      (unless (and (= param-len 2) (string-equal in "OK" :start1 7 :end1 9))
        (error "Set '~A' failed with response '~A'" command (subseq in 7 (+ 7 param-len))))
      (values))))

(defun pjlink-connect (host
                       &key
                         (port +pjlink-port+)
                         (password nil)
                         (local-host nil)
                         (local-port nil))
  "Open a PJLink connection to `host`.
  `port` specifies the port to use. defaults to pjlink port 4352
  `password` will be used if the host requires authentication
  `local-host` allows specifying a different local host to use (interface device)
  `local-port` allows specifying a specific local port to use"
  (let* ((socket (usocket:socket-connect host port
                                         :element-type 'character
                                         :local-host local-host
                                         :local-port local-port))
         (buffer (make-array 18 :element-type 'character))
         (stream (usocket:socket-stream socket)))
    (declare (dynamic-extent buffer))
    (%read-pjlink-command-line buffer stream)
    (unless (%verify-connect-response buffer)
      (error "Invalid response ~A" buffer))

    (let* ((digest (%create-digest buffer password))
           (class (%pjlink-clss?-impl stream digest)))
      (make-instance '%pjlink-connection :socket socket :digest digest :class class))))

(defun pjlink-powr (connection power-on)
  (%pjlink-set connection #\1 "POWR" (if power-on "1" "0")))

(defun pjlink-powr? (connection)
  (let ((result (%pjlink-get connection #\1 "POWR")))
    (%powr->sym (char result 0))))

(defun pjlink-inpt (connection input-type input-number)
  (let ((input-str (%input->string input-type input-number)))
    (%pjlink-set connection #\1 "INPT" input-str)))

(defun pjlink-inpt? (connection)
  (let ((result (%pjlink-get connection #\1 "INPT")))
    (values (%input->sym (char result 0))
            (parse-integer result :start 1 :end 2 :radix 10))))

(defun pjlink-avmt (connection avmt)
  (%pjlink-set connection #\1 "AVMT" (%avmt->string avmt)))

(defun pjlink-avmt? (connection)
  (let ((result (%pjlink-get connection #\1 "AVMT")))
    (%avmt->sym result)))

(defun pjlink-erst? (connection)
  (let ((result (%pjlink-get connection #\1 "ERST")))
    (make-instance
     '%error-status
     :fan (%erst->sym (char result 0))
     :lamp (%erst->sym (char result 1))
     :temperature (%erst->sym (char result 2))
     :cover-open (%erst->sym (char result 3))
     :filter (%erst->sym (char result 4))
     :other (%erst->sym (char result 5)))))

(defun pjlink-lamp? (connection)
  ;;Each lamp is returned as a pair of "<hour> SPC <status>" ordered by lamp number
  (%lamp-str->lamp-infos (%pjlink-get connection #\1 "LAMP")))

(defun pjlink-inst? (connection)
  (%inst-str->input-infos (%pjlink-get connection #\1 "INST")))

(defun pjlink-name? (connection)
  (%pjlink-get connection #\1 "NAME"))

(defun pjlink-inf1? (connection)
  (%pjlink-get connection #\1 "INF1"))

(defun pjlink-inf2? (connection)
  (%pjlink-get connection #\1 "INF2"))

(defun pjlink-info? (connection)
  (%pjlink-get connection #\1 "INFO"))

(defun pjlink-clss? (connection)
  (let ((result (%pjlink-get connection #\1 "CLSS")))
    (values (parse-integer result :radix 16))))
