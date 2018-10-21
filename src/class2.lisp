;;;; Class 2 support types and commands

(in-package #:pjlink)

(deftype input-type2 ()
  "An input type for a class2 projector, adding `:internal'
Note that a projector may have several inputs of the same type, identified by an `input-number`
see `input-type'
see `get-input2', `set-input2', and `get-inputs2'"
  '(or input-type :internal))

(defclass projector-input2 ()
  ((%type
    :type input-type
    :initarg :type
    :initform (error "Must supply type")
    :reader input-type)
   (%number
    :type (integer 1 35)
    :initarg :number
    :initform (error "Must supply number")
    :reader input-number))
  (:documentation
   "An available input for a class 2 projector.
Note that projectors may have multiple inputs of the same type, differentiated by number.
Note that projector input numbers start at 1, not 0."))

(defmethod print-object ((object projector-input2) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S ~A" (input-type object) (input-number object))))

(defclass projector-resolution ()
  ((%horz
    :type integer
    :initarg :horz
    :initform (error "Must supply horz")
    :reader horz-resolution)
   (%vert
    :type integer
    :initarg :vert
    :initform (error "Must supply vert")
    :reader vert-resolution)))

(defmethod print-object ((object projector-resolution) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~Ax~A" (horz-resolution object) (vert-resolution object))))

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
    :collecting (make-instance 'projector-input2 :type type :number number)))

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
      (make-instance 'projector-resolution :horz horz :vert vert))))

;;; Class 2 commands
(%defpjlink-set set-input2 (2 "INPT") (input-type input-number)
  "Sets the input to the given `input-type2' and `input-number'
see `input-type2'
see `set-input2*', `get-input2'"
  (%input2->string input-type input-number))

(%defpjlink-set set-input2* (2 "INPT") (input-info)
  "As `set-input2' but using a `projector-input' or `projector-input2' object instead."
  (%input2->string (input-type input-info) (input-number input-info)))

(%defpjlink-get get-input2 (2 "INPT") nil (result)
  (make-instance
   'projector-input2
   :type (%input2->sym (char result 0))
   :number (parse-integer result :start 1 :end 2 :radix 36)))

(%defpjlink-get get-inputs2 (2 "INST") nil (result)
  "Query the available `projector-input2's on the projector as a list."
  (%inst-str2->input-infos result))

(%defpjlink-get get-serial-number (2 "SNUM") nil (result)
  "Get the serial number of the projector.
nil if not available."
  (unless (zerop (length result))
    result))

(%defpjlink-get get-serial-number (2 "SVER") nil (result)
  "Get the software version of the projector.
nil if not available."
  (unless (zerop (length result))
    result))

(%defpjlink-get get-input-name (2 "INNM")
    ((input-type input-number)
      (%input2->string input-type input-number))
  (result)
  "Get the software version of the projector.
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