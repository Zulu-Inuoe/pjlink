;;;; Class 2 support types and commands

(in-package #:pjlink)

(deftype input-type2 ()
  "An input type for a class2 projector, adding `:internal'
Note that a projector may have several inputs of the same type, identified by an `input-number`

see `set-projector-input2'"
  '(or input-type (eql :internal)))

(deftype input-number2 ()
  "An input number for class 2 projectors, expanding the range of input numbers to [1, 35].

see `set-projector-input2'"
  '(integer 1 35))

(deftype projector-input2 ()
  "A cons of (`input-type2' . `input-number2')

eg.
'(:internal . 30)

see `get-input2'
see `set-input2*'
see `get-inputs2'"
  'cons)

(deftype projector-resolution ()
  "Resolution of a projector, as:
  a cons of (<horz-resolution> . <vert-resolution>)
  :no-signal if no signal is available
  :unknown-signal if the signal is unknown

see `get-resolution'
see `get-recommended-resolution'"
  '(or cons (member :no-signal :unknown-signal)))

(deftype mac-address ()
  "A MAC address as a vector of 6 octets.

see `search-projectors'"
  '(vector (unsigned-byte 8) 6))

(deftype status-event ()
  "A keyword denoting a notification event type from the projector.

see `make-status-listener'"
  '(member :lkup :erst :powr :inpt))

(deftype status-handler ()
  "A function-designator for a function of three arguments:
remote-host: a `hostname' representing the origin of the status update
event-type: a `status-event' representing the status notification
arg: an object representing the event status, depending on `event-type':

  :lkup - the `mac-address' of the projector
  :erst - `projector-status'
  :powr - `power-status'
  :inpt - `projector-input2'

see `make-status-listener'"
  '(or symbol function))

(defun %input2->sym (input-val)
  (ecase input-val
    (#\1 :rgb)
    (#\2 :video)
    (#\3 :digital)
    (#\4 :storage)
    (#\5 :network)
    (#\6 :internal)))

(defun %input2->string (input-type2 input-number2)
  (check-type input-type2 input-type2)
  (check-type input-number2 input-number2)
  (format nil "~C~C"
          (ecase input-type2
            (:rgb #\1)
            (:video #\2)
            (:digital #\3)
            (:storage #\4)
            (:network #\5)
            (:internal #\6))
          (char "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" input-number2)))

(defun %inst-str2->input-infos (inst-str)
  "Parses a inst string into a list of `input-info`'s
`inst-str` should be a string where each input is represented by

  <Type><Number>

Additional inputs are separated by spaces.

eg
  11 2Z 3E 61"
  (loop
    :with idx := 0
    :while (< idx (length inst-str))
    :for valid := (or (zerop idx)
                      (char= (char inst-str (1- (incf idx))) #\Space)
                      (error "Malformed inst string: '~A'" inst-str))
    :for type := (%input2->sym (char inst-str (1- (incf idx))))
    :for number := (parse-integer inst-str :start (1- (incf idx)) :end idx :radix 36)
    :collecting (cons type number)))

(defun %res-str->resolution (res-str)
  "Parses a resolution string of the form

<horz>x<vert>

eg
  1920x1080."
  (let ((x-pos (position #\x res-str)))
    (unless x-pos
      (error "Malformed resolution string: '~A'" res-str))
    (let ((horz (parse-integer res-str :end x-pos))
          (vert (parse-integer res-str :start (1+ x-pos))))
      (cons horz vert))))

;;; Class 2 commands

(%defpjlink-set set-input2 (2 "INPT") (input-type input-number)
  "Sets the input to the given `input-type2' and `input-number2'
see `set-input2*'
see `get-input2'"
  (%input2->string input-type input-number))

(%defpjlink-set set-input2* (2 "INPT") (input-info)
  "As `set-input2' but using a `projector-input' or `projector-input2' object instead."
  (%input2->string (car input-info) (cdr input-info)))

(%defpjlink-get get-input2 (2 "INPT") nil (result)
  (cons
   (%input2->sym (char result 0))
   (parse-integer result :start 1 :end 2 :radix 36)))

(%defpjlink-get get-inputs2 (2 "INST") nil (result)
  "Query the available `projector-input2's on the projector as a list."
  (%inst-str2->input-infos result))

(%defpjlink-get get-serial-number (2 "SNUM") nil (result)
  "Get the serial number of the projector.
nil if not available."
  (unless (zerop (length result))
    result))

(%defpjlink-get get-software-version (2 "SVER") nil (result)
  "Get the software version of the projector.
nil if not available."
  (unless (zerop (length result))
    result))

(%defpjlink-get get-input-name (2 "INNM")
    ((input-type input-number)
      (%input2->string input-type input-number))
  (result)
  "Get the input name of the given input type and number
nil if not available."
  (unless (zerop (length result))
    result))

(%defpjlink-get get-input-name* (2 "INNM")
    ((input-info)
      (%input2->string (car input-info) (cdr input-info)))
  (result)
  "As `get-input-name' but using a `projector-input' or `projector-input2' object instead.
nil if not available."
  (unless (zerop (length result))
    result))

(%defpjlink-get get-resolution (2 "IRES") nil (result)
  "Get the current resolution of the active input.
:no-signal if no signal is available
:unknown-signal if there is an unknown signal active"
  (case (char result 0)
    (#\- :no-signal)
    (#\* :unknown-signal)
    (t (%res-str->resolution result))))

(%defpjlink-get get-recommended-resolution (2 "RRES") nil (result)
  "Get the recommended `resolution' for the projector."
  (unless (zerop (length result))
    (%res-str->resolution result)))

(%defpjlink-get get-filter-usage-time (2 "FILT") nil (result)
  "Get the filter usage time of the projector."
  (values (parse-integer result)))

(%defpjlink-get get-lamp-model (2 "RLMP") nil (result)
  "Get the lamp replacement model numbers.
nil if no replacement model numbers are available."
  (unless (zerop (length result))
    (split-sequence:split-sequence #\Space result :remove-empty-subseqs t)))

(%defpjlink-get get-filter-model (2 "RFIL") nil (result)
  "Get the filter replacement model numbers.
nil if no replacement model numbers are available."
  (unless (zerop (length result))
    (split-sequence:split-sequence #\Space result :remove-empty-subseqs t)))

(%defpjlink-set increase-speaker (2 "SVOL") ()
  "Increment the speaker volume by one level."
  "1")

(%defpjlink-set decrease-speaker (2 "SVOL") ()
  "Decrement the speaker volume by one level."
  "0")

(%defpjlink-set increase-microphone (2 "MVOL") ()
  "Increment the microphone volume by one level."
  "1")

(%defpjlink-set decrease-microphone (2 "MVOL") ()
  "Decrement the microphone volume by one level."
  "0")

(%defpjlink-set freeze-screen (2 "FREZ") ()
  "Freeze the screen."
  "1")

(%defpjlink-set unfreeze-screen (2 "FREZ") ()
  "Freeze the screen."
  "0")

(%defpjlink-get get-freeze-status (2 "FREZ") nil (result)
  "Query the current freeze status of the projector.
true if freeze is ON
false if freeze is OFF"
  (ecase (char result 0)
    (#\0 nil)
    (#\1 t)))

;;; Search protocol

(defun %validate-search-ack (response len)
  (and (= len 25)
       (string-equal response "%2ACKN=" :end1 7)))

(defun %parse-mac-address (buf &key (start 0))
  (let ((mac (make-array 6 :element-type '(unsigned-byte 8))))
    (setf (aref mac 0) (parse-integer buf :start (+ start 0) :end (+ start 2) :radix 16)
          (aref mac 1) (parse-integer buf :start (+ start 3) :end (+ start 5) :radix 16)
          (aref mac 2) (parse-integer buf :start (+ start 6) :end (+ start 8) :radix 16)
          (aref mac 3) (parse-integer buf :start (+ start 9) :end (+ start 11) :radix 16)
          (aref mac 4) (parse-integer buf :start (+ start 12) :end (+ start 14) :radix 16)
          (aref mac 5) (parse-integer buf :start (+ start 15) :end (+ start 17) :radix 16))
    mac))

(defun %parse-search-ack (response)
  (%parse-mac-address response :start 7))

(defun %normalize-hostname (host)
  "Returns a normalized octet vector representation of host"
  (car (usocket:get-hosts-by-name (usocket::host-to-hostname host))))

(defun %find-broadcast-interface (interface-address)
  "Find an IP interface matching `interface-address' with broadcast capabilities
nil if no such interface is available."
  (find interface-address
        (ip-interfaces:get-ip-interfaces-by-flags '(:iff_broadcast))
        :key #'ip-interfaces:ip-interface-address
        :test #'equalp))

(defun %calculate-broadcast-addr (address subnet)
  "Calculate a broadcast address vector from an ipv4 address and subnet."
  (let ((broadcast-addr (make-array 4 :element-type '(unsigned-byte 8))))
    (setf (aref broadcast-addr 0) (logand 255 (logorc2 (aref address 0) (aref subnet 0)))
          (aref broadcast-addr 1) (logand 255 (logorc2 (aref address 1) (aref subnet 1)))
          (aref broadcast-addr 2) (logand 255 (logorc2 (aref address 2) (aref subnet 2)))
          (aref broadcast-addr 3) (logand 255 (logorc2 (aref address 3) (aref subnet 3))))
    broadcast-addr))

(defun %calculate-broadcast-addr* (interface)
  "Calculates a broadcast address vector from an `ip-interfaces::ip-interface'"
  ;;NOTE: ip-interfaces has a ip-interfaces:ip-interface-broadcast-address
  ;;      but that doesn't seen to be getting calculated correctly
  (let ((address (ip-interfaces:ip-interface-address interface))
        (subnet (ip-interfaces:ip-interface-netmask interface)))
    (%calculate-broadcast-addr address subnet)))

(defun %address-and-broadcast (local-host
                               &aux
                                 (normalized (and local-host (%normalize-hostname local-host)))
                                 (ipv6 (and normalized (= (length normalized) 16))))
  "Returns the address and broadcast address to use for the interface `local-host'
Finds a suitable broadcast interface matching `local-host' and returns its address.
When `local-host' is nil, the general broadcast address is used"
  (when ipv6
    (error "ipv6 not supported"))

  (let ((interface (and normalized (%find-broadcast-interface normalized))))
    (when (and normalized (null interface))
      (error "no such interface: ~A" local-host))

    (if interface
        (values (ip-interfaces:ip-interface-address interface) (%calculate-broadcast-addr* interface))
        (values nil #(255 255 255 255)))))

(defun search-projectors (&key local-host (port +default-port+))
  "Performs a PJLink broadcast search.
Returns a list of `(`hostname' . `mac-address')` pairs representing each projector that responded.

`local-host' indicates the local interface to use for the search. if nil will
use the general broadcast address instead.
`port' is the port used for the SRCH operation."
  (multiple-value-bind (address broadcast-address)
      (%address-and-broadcast local-host)
    (usocket:with-connected-socket (socket
                                    (usocket:socket-connect nil nil
                                                            :protocol :datagram
                                                            :element-type 'character
                                                            :local-host address
                                                            :local-port port))
      (setf (usocket:socket-option socket :broadcast) t)
      (let ((buf #.(format nil "%2SRCH~C" #\Return)))
        (usocket:socket-send socket buf (length buf) :host broadcast-address :port port))

      (let ((buf (make-string 40)))
        (declare (dynamic-extent buf))
        (loop
          :for remaining-time := 30
          :if (multiple-value-bind (ready time-left)
                  (usocket:wait-for-input socket :timeout remaining-time :ready-only t)
                (when (and (not ready) (null time-left))
                  (loop-finish))

                (setf remaining-time (or time-left 0))

                (when ready
                  (multiple-value-bind (buf len remote-host remote-port)
                      (usocket:socket-receive socket buf (length buf) :element-type 'character)
                    (declare (ignore remote-port))
                    (when (%validate-search-ack buf len)
                      (let ((mac-address (%parse-search-ack buf)))
                        (cons remote-host mac-address))))))
            :collect :it)))))

(defclass status-listener ()
  ((%lock
    :type bt:lock
    :initform (bt:make-lock))
   (%address
    :type hostname
    :initarg :address
    :initform (error "Must specify address"))
   (%port
    :type integer
    :initarg :port
    :initform (error "Must specify port"))
   (%handlers-ptr
    :type cons
    :initform (cons nil nil))
   (%end-thread-fn-ptr
    :type cons
    :initform (cons nil nil))))

(defun make-status-listener (&key handlers local-host (port +default-port+))
  "Creates a projector status listener listening on `local-host'.
`handlers' is either a `status-handler' handler or a list of `status-handler's
  these handlers will be initially registered as `add-handler'
if `handlers' is non-nil, the listener will be started, as per `start-listener'"
  (let* ((handlers (ensure-list handlers))
         (listener
            (make-instance 'status-listener :address local-host :port port)))
    (when handlers
      (dolist (h handlers)
        (add-handler listener h))
      (start-listener listener))
    listener))

(defun add-handler (status-listener handler)
  "Add a `status-handler' to `status-listener'."
  (with-slots (%lock %handlers-ptr)
      status-listener
    (bt:with-lock-held (%lock)
      (push handler (car %handlers-ptr))))
  (values))

(defun remove-handler (status-listener handler)
  "Remove a `status-handler' from a `status-listener'."
  (with-slots (%lock %handlers-ptr)
      status-listener
    (bt:with-lock-held (%lock)
      (setf (car %handlers-ptr) (remove handler (car %handlers-ptr)))))
  (values))

(defun %notify-handlers (handlers remote-host event-type args)
  (dolist (h handlers)
    (handler-case
        (funcall h remote-host event-type args)
      (error (e)
        (declare (ignore e))
        (values)))))

(defun %parse-lkup-status (buf len)
  (cond
    ((= len 25)
     (values :lkup (%parse-mac-address buf :start 7)))
    (t
     (values nil nil))))

(defun %parse-erst-status (buf len)
  (cond
    ((= len 14)
     (values :erst
             (list
              (cons :fan (%erst->sym (char buf 7)))
              (cons :lamp (%erst->sym (char buf 8)))
              (cons :temperature (%erst->sym (char buf 9)))
              (cons :cover-open (%erst->sym (char buf 10)))
              (cons :filter (%erst->sym (char buf 11)))
              (cons :other (%erst->sym (char buf 12))))))
    (t
     (values nil nil))))

(defun %parse-powr-status (buf len)
  (cond
    ((= len 9)
     (values :powr (%powr->sym (char buf 7))))
    (t
     (values nil nil))))

(defun %parse-inpt-status (buf len)
  (cond
    ((= len 10)
     (values :inpt
             (cons
              (%input2->sym (char buf 7))
              (parse-integer buf :start 8 :end 9 :radix 36))))
    (t
     (values nil nil))))

(defun %validate-and-parse-status (buf len)
  (when (>= len 8)
    (switch (buf :test (lambda (a b)
                         (string-equal a b :end1 7)))
      ("%2LKUP="
       (%parse-lkup-status buf len))
      ("%2ERST="
       (%parse-erst-status buf len))
      ("%2POWR="
       (%parse-powr-status buf len))
      ("%2INPT="
       (%parse-inpt-status buf len))
      (t
       (values nil nil)))))

(defun %make-listener-thread (running-ptr handlers-ptr socket)
  (bt:make-thread
   (lambda ()
     (let ((buf (make-string 256)))
       (declare (dynamic-extent buf))
       (loop
         :while (car running-ptr)
         :do
            (handler-case
                (when-let ((ready (usocket:wait-for-input socket :ready-only t)))
                  (multiple-value-bind (buf len remote-host remote-port)
                      (usocket:socket-receive socket buf (length buf) :element-type 'character)
                    (declare (ignore remote-port))
                    (multiple-value-bind (event-type args)
                        (%validate-and-parse-status buf len)
                      (when event-type
                        (%notify-handlers (car handlers-ptr) remote-host event-type args)))))
              (error (e)
                (declare (ignore e))
                (values))))))))

(defun %make-end-thread-fn (running-ptr thread socket address port)
  (lambda ()
    (setf (car running-ptr) nil)
    ;;hacky hack hack hack to get the wait to finish
    (usocket:socket-send socket "0" 1 :host (or address "localhost") :port port)
    (when (bt:thread-alive-p thread)
      (bt:join-thread thread))
    (usocket:socket-close socket)))

(defun %make-finalizer (running-ptr socket)
  (lambda ()
    (when (car running-ptr)
      (setf (car running-ptr) nil)
      (usocket:socket-close socket))))

(defun start-listener (status-listener)
  "Start a `status-listener' if it is not already started.
This will cause incoming status notifications to alert any registered handlers."
  (bt:with-lock-held ((slot-value status-listener '%lock))
    (let ((end-thread-fn-ptr (slot-value status-listener '%end-thread-fn-ptr)))
      (unless (car end-thread-fn-ptr)
        (let* ((address (slot-value status-listener '%address))
               (port (slot-value status-listener '%port))
               (socket (usocket:socket-connect nil nil :protocol :datagram :element-type 'character :local-host address :local-port port))
               (thread nil)
               (running-ptr (cons t nil))
               (handlers-ptr (slot-value status-listener '%handlers-ptr))
               (ok nil))
          (unwind-protect
               (progn
                 (setf (car end-thread-fn-ptr) (%make-end-thread-fn running-ptr thread socket address port))
                 (setf thread (%make-listener-thread running-ptr handlers-ptr socket))
                 (setf ok t))
            (unless ok
              (setf (car end-thread-fn-ptr) nil)
              (usocket:socket-close socket)))
          (trivial-garbage:finalize status-listener (%make-finalizer running-ptr socket))))))
  status-listener)

(defun stop-listener (status-listener)
  "Stop a `status-listener' if it is not already stopped.
This will cease listening for status notification updates."
  (with-slots (%lock %end-thread-fn-ptr)
      status-listener
    (bt:with-lock-held (%lock)
      (when (car %end-thread-fn-ptr)
        (trivial-garbage:cancel-finalization status-listener)
        (funcall (car %end-thread-fn-ptr))
        (setf (car %end-thread-fn-ptr) nil))))
  (values))
