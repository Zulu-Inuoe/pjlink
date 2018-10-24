;;;; Class 2 support types and commands

(in-package #:pjlink)

(deftype input-type2 ()
  "An input type for a class2 projector, adding `:internal'
Note that a projector may have several inputs of the same type, identified by an `input-number`
see `input-type'
see `get-input2', `set-input2', and `get-inputs2'"
  '(or input-type (eql :internal)))

(deftype mac-address ()
  "A MAC address as a vector of 6 octets."
  '(vector (unsigned-byte 8) 6))

(defun %input2->sym (input-val)
  (ecase input-val
    (#\1 :rgb)
    (#\2 :video)
    (#\3 :digital)
    (#\4 :storage)
    (#\5 :network)
    (#\6 :internal)))

(defun %input2->string (input-type input-number)
  (unless (<= 1 input-number 35)
    (error "Invalid input number '~A'" input-number))
  (format nil "~A~D"
          (ecase input-type
            (:rgb #\1)
            (:video #\2)
            (:digital #\3)
            (:storage #\4)
            (:network #\5)
            (:internal #\6))
          input-number))

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
  "Sets the input to the given `input-type2' and `input-number'
see `input-type2'
see `set-input2*', `get-input2'"
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
  "Get the recommended resolution for the projector."
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

(defun %validate-search-ack (class response len)
  (and (= len 25)
       (char= (char response 0) #\%)
       (char= (char response 1) (code-char (+ (char-code #\0) class)))
       (string-equal response "ACKN=" :start1 2 :end1 7)))

(defun %parse-search-ack (response)
  (let ((mac (make-array 6 :element-type '(unsigned-byte 8))))
    (setf (aref mac 0) (parse-integer response :start 7 :end 9 :radix 16)
          (aref mac 1) (parse-integer response :start 10 :end 12 :radix 16)
          (aref mac 2) (parse-integer response :start 13 :end 15 :radix 16)
          (aref mac 3) (parse-integer response :start 16 :end 18 :radix 16)
          (aref mac 4) (parse-integer response :start 19 :end 21 :radix 16)
          (aref mac 5) (parse-integer response :start 22 :end 24 :radix 16))
    mac))

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
    (setf (aref broadcast-addr 0) (+ 256 (logorc2 (aref address 0) (aref subnet 0)))
          (aref broadcast-addr 1) (+ 256 (logorc2 (aref address 1) (aref subnet 1)))
          (aref broadcast-addr 2) (+ 256 (logorc2 (aref address 2) (aref subnet 2)))
          (aref broadcast-addr 3) (+ 256 (logorc2 (aref address 3) (aref subnet 3))))
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

(defun search-projectors (&key local-host (port +pjlink-port+))
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
      (let ((buf (make-array 7 :element-type 'character :initial-contents #(#\% #\2 #\S #\R #\C #\H #\Return))))
        (declare (dynamic-extent buf))
        (usocket:socket-send socket buf (length buf) :host broadcast-address :port port))

      (let ((buf (make-array 40 :element-type 'character)))
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
                    (when (%validate-search-ack 2 buf len)
                      (let ((mac-address (%parse-search-ack buf)))
                        (cons remote-host mac-address))))))
            :collect :it)))))
