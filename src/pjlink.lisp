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
;;;  `%1POWR 1`
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
;;;  `%1POWR=OK`
;;;

(defconstant +pjlink-port+ 4352)

(defclass %pjlink-connection ()
  ((%socket
    :type usocket:socket
    :initarg :socket
    :initform (error "Must supply socket")
    :reader %socket)
   (%digest
    :type (or null (simple-array (unsigned-byte 8) 32))
    :initarg :digest
    :initform (error "Must supply digest")
    :reader %digest)))

(defun %encrypt-password (password seed &key (seed-start 0))
  "Create the authentication response given `password` and `seed`.
Both `password` and `seed` should be octet sequences (vector (unsigned-byte 8))
`password` can be 32 or fewer octets
`seed` must be 4 octets."
  (let ((digest (make-array 40 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent digest))
    (replace digest seed :start2 seed-start :end2 (+ seed-start 8))
    (replace digest password :start1 8)
    (let ((md5 (md5:md5sum-sequence digest :end (+ 8 (length password)))))
      (loop
        :with ret := (make-array 32 :element-type '(unsigned-byte 8))
        :for i :from 0 :below 16
        :for hex-d := (format nil "~(~2,'0X~)" (aref md5 i))
        :do (setf (aref ret (+ (* i 2) 0)) (char-code (char hex-d 0))
                  (aref ret (+ (* i 2) 1)) (char-code (char hex-d 1)))
        :finally
           (return ret)))))

(defun %coerce-password (password
                         &key
                           (external-format :utf-8)
                           (start 0)
                           (end nil end-sup-p)
                         &aux
                           (password (if (functionp password) (funcall password) password)))
  "Coerces a password into a `(vector (unsigned-byte 8))`
`password` can be a string, decoded according to `external-format`
or an (array (unsigned-byte 8)) with maximum length 32."
  (etypecase password
    (string
     (let ((ret (flexi-streams:string-to-octets password :external-format external-format :start start :end (if end-sup-p end (length password)))))
       (unless (<= (length ret) 32)
         (error "Encoded password length too long"))
       ret))
    ((vector (unsigned-byte 8) 32)
     password)
    ((array (unsigned-byte 8))
     (unless (<= (length password) 32)
       (error "Password length too long"))
     password)))

(defun %verify-connect-response (response)
  (and (>= (length response) 8)
       (= (aref response 0) (char-code #\P))
       (= (aref response 1) (char-code #\J))
       (= (aref response 2) (char-code #\L))
       (= (aref response 3) (char-code #\I))
       (= (aref response 4) (char-code #\N))
       (= (aref response 5) (char-code #\K))
       (= (aref response 6) (char-code #\Space))
       (or (and (= (aref response 7) (char-code #\0))
                (>= (length response) 9)
                (= (aref response 8) (char-code #\Return)))
           (and (= (aref response 7) (char-code #\1))
                (= (aref response 8) (char-code #\Space))
                (>= (length response) 18)
                (= (aref response 17) (char-code #\Return))))))

(defun %authentication-enabled (response)
  (= (aref response 7) (char-code #\1)))

(defun %create-digest (response password)
  ;;Check whether authentication is enabled or not
  (switch ((aref response 7))
    ((char-code #\0)
     ;;No authentication. Empty digest
     (make-array 0 :element-type '(unsigned-byte 8)))
    ((char-code #\1)
     ;;Authentication
     (%encrypt-password (%coerce-password password) response :seed-start 9))))

(defun %read-pjlink-command-line (buffer stream)
  "Reads a pjlink command-line (delimited by 0x13) from `stream` into `buffer`"
  (loop
    :for b := (read-byte stream)
    :for i :from 0 :below (length buffer)
    :do (setf (aref buffer i) b)
    :until (= b (char-code #\return))))

(defmacro %with-command-buffer ((buffer connection command params) &body body)
  (with-gensyms (conn command-sym params-sym idx)
    `(let* ((,conn ,connection)
            (,command-sym ,command)
            (,params-sym ,params)
            (,buffer (make-array 168 :element-type '(unsigned-byte 8)))
            (,idx 0))
       (declare (dynamic-extent ,command-sym ,params-sym ,buffer))
       (replace ,buffer (%digest ,conn))
       (incf ,idx (length (%digest ,conn)))
       (setf (aref ,buffer (1- (incf ,idx))) (char-code #\%)
             (aref ,buffer (1- (incf ,idx))) (char-code #\1))
       (etypecase ,command-sym
         (string
          ,(with-gensyms (octets)
             `(let ((,octets (flexi-streams:string-to-octets ,command-sym :external-format :utf-8)))
                (replace ,buffer ,octets :start1 ,idx)
                (incf ,idx (length ,octets))))))
       (setf (aref ,buffer (1- (incf ,idx))) (char-code #\Space))
       (etypecase ,params-sym
         (string
          ,(with-gensyms (octets)
             `(let ((,octets (flexi-streams:string-to-octets ,params-sym :external-format :utf-8)))
                (replace ,buffer ,octets :start1 ,idx)
                (incf ,idx (length ,octets))))))
       (setf (aref ,buffer (1- (incf ,idx))) (char-code #\Return))
       (locally ,@body))))

(defmacro %with-response-buffer (buffer &body body)
  `(let* ((,buffer (make-array 136 :element-type '(unsigned-byte 8))))
     (declare (dynamic-extent ,buffer))
     ,@body))

(defun %pjlink-print (buffer &optional (stream *standard-output*))
  (let ((str (flexi-streams:octets-to-string buffer :external-format :utf-8)))
    (loop
      :for c :across str
      :until (char= c #\Return)
      :do (write-char c stream))
    (terpri stream)
    (values)))

(defun %pjlink-clss? (connection &aux (stream (usocket:socket-stream (%socket connection))))
  (%with-command-buffer (out connection "CLSS" "?")
    (%pjlink-print out)
    (write-sequence out stream)
    (finish-output stream)
    (%with-response-buffer in
      (%read-pjlink-command-line in stream)
      (%pjlink-print in))
    0))

(defun %pjlink-connect (host
                        &key
                          (port +pjlink-port+)
                          (password nil)
                          (local-host nil)
                          (local-port nil))
  (let* ((socket (usocket:socket-connect host port
                                         :element-type '(unsigned-byte 8)
                                         :local-host local-host
                                         :local-port local-port))
         (stream (usocket:socket-stream socket))
         (buffer (make-array 18 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent buffer))
    (%read-pjlink-command-line buffer stream)
    (unless (%verify-connect-response buffer)
      (error "Invalid response ~A" buffer))

    (let ((digest (%create-digest buffer password)))
      (make-instance 'pjlink-connection :socket socket :digest digest))))
