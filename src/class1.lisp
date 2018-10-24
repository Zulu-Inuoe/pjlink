;;;; Class 1 support types and commands

(in-package #:pjlink)

(deftype power-status ()
  "Power status of a projector.
see `power-on', `power-off', and `get-power-status'"
  '(member :standby :lamp-on :cooling :warm-up))

(deftype input-type ()
  "An input type for a projector.
Note that a projector may have several inputs of the same type, identified by an `input-number`
see `get-input', `set-input', and `get-inputs'"
  '(member :rgb :video :digital :storage :network))

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
  '(member nil :warning :error))

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
    (#\0 nil)
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
    :collecting (cons hours is-on)))

(defun %inst-str->input-infos (inst-str)
  "Parses a inst string into a list of `input-info`'s
`inst-str` should be a string where each input is represented by

  <Type><Number>

Additional inputs are separated by spaces.

eg
  11 45 51"
  (loop
    :with idx := 0
    :while (< idx (length inst-str))
    :for valid := (or (zerop idx)
                      (char= (char inst-str (1- (incf idx))) #\Space)
                      (error "Malformed inst string: '~A'" inst-str))
    :for type := (%input->sym (char inst-str (1- (incf idx))))
    :for number := (parse-integer inst-str :start (1- (incf idx)) :end idx :radix 10)
    :collecting (cons type number)))

;;; Class 1 commands

(%defpjlink-set power-on (1 "POWR") ()
  "Instruct the projector to power on."
  "1")

(%defpjlink-set power-off (1 "POWR") ()
  "Instruct the projector to power off."
   "0")

(%defpjlink-get get-power-status (1 "POWR") nil (result)
  "Query the `power-status' of the projector.
see `set-port-on', `set-power-off'"
  (%powr->sym (char result 0)))

(%defpjlink-set set-input (1 "INPT") (input-type input-number)
  "Sets the input to the given `input-type' and `input-number'
see `set-input*', `get-input'"
  (%input->string input-type input-number))

(%defpjlink-set set-input* (1 "INPT") (input-info)
  "As `set-input' but using a `projector-input' object instead."
  (%input->string (car input-info) (cdr input-info)))

(%defpjlink-get get-input (1 "INPT") nil (result)
  "Query the current `projector-input' on the projector."
  (cons
   (%input->sym (char result 0))
   (parse-integer result :start 1 :end 2 :radix 10)))

(%defpjlink-set set-av-mute (1 "AVMT") (avmt)
  "Set the `av-mute-status' on the projector.
see `get-av-mute'"
  (%avmt->string avmt))

(%defpjlink-get get-av-mute (1 "AVMT") nil (result)
  "Query the current `av-mute-status' of the projector.
see `set-av-mute'"
  (%avmt->sym result))

(%defpjlink-get get-error-status (1 "ERST") nil (result)
  "Query the `projector-status' projector."
  (list
   (cons :fan (%erst->sym (char result 0)))
   (cons :lamp (%erst->sym (char result 1)))
   (cons :temperature (%erst->sym (char result 2)))
   (cons :cover-open (%erst->sym (char result 3)))
   (cons :filter (%erst->sym (char result 4)))
   (cons :other (%erst->sym (char result 5)))))

(%defpjlink-get get-lamps (1 "LAMP") nil (result)
  "Query the available `projector-lamp's on the projector as a list."
  (%lamp-str->lamp-infos result))

(%defpjlink-get get-inputs (1 "INST") nil (result)
  "Query the available `projector-input's on the projector as a list."
  (%inst-str->input-infos result))

(%defpjlink-get get-projector-name (1 "NAME") nil (result)
  "Query the projector's name.
nil if not available."
  (unless (zerop (length result))
    result))

(%defpjlink-get get-manufacturer-name (1 "INF1") nil (result)
  "Query the projector's manufacturer name.
nil if not available."
  (unless (zerop (length result))
    result))

(%defpjlink-get get-product-name (1 "INF2") nil (result)
  "Query the projector's product name.
nil if not available."
  (unless (zerop (length result))
    result))

(%defpjlink-get get-other-info (1 "INFO") nil (result)
  "Query other information about the projector.
nil if not available."
  (unless (zerop (length result))
    result))

(%defpjlink-get get-pjlink-class (1 "CLSS") nil (result)
  "Query the pjlink class of the projector."
  (values (parse-integer result :radix 36)))