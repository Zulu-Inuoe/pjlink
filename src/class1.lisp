;;;; Class 1 support types and commands

(in-package #:pjlink)

(deftype power-status ()
  "Power status of a projector.

see `power-on'
see `power-off'
see `get-power-status'"
  '(member :standby :lamp-on :cooling :warm-up))

(deftype input-type ()
  "An input type for a projector.
Note that a projector may have several inputs of the same type, identified by an `input-number'

see `set-input'"
  '(member :rgb :video :digital :storage :network))

(deftype input-number ()
  "Input number for a projector

see `set-input'"
  '(integer 1 9))

(deftype projector-input ()
  "A cons of (`input-type' . `input-number')
eg.
'(:digital . 5)

see `get-input'
see `set-input'
see `set-input*'
see `get-inputs'"
  '(cons input-type input-number))

(defun projector-input (input-type input-number)
  "Create a `projector-input' from `input-type' and `input-number'"
  (check-type input-type input-type)
  (check-type input-number input-number)
  (cons input-type input-number))

(defun input-type (projector-input)
  "Get the `input-type' part of `projector-input'"
  (check-type projector-input projector-input)
  (car projector-input))

(defun input-number (projector-input)
  "Get the `input-number' part of `projector-input'"
  (check-type projector-input projector-input)
  (cdr projector-input))

(deftype lamp-hours ()
  "Number of hours a lamp has been on."
  `(integer 0 99999))

(deftype lamp-status ()
  "A cons of (`lamp-hours' . on-p)
 eg.
 '(500 . nil)

see `get-lamps'"
  `(cons lamp-hours (member nil t)))

(defun lamp-status (lamp-hours lamp-on-p)
  "Create a `lamp-status' from `lamp-hours' and `on-p'"
  (check-type lamp-hours lamp-hours)
  (cons lamp-hours (and lamp-on-p t)))

(defun lamp-hours (lamp-status)
  "Get the `lamp-hours' part of `lamp-status'"
  (check-type lamp-status lamp-status)
  (car lamp-status))

(defun lamp-on-p (lamp-status)
  "Get the `lamp-on-p' part of `lamp-status'"
  (check-type lamp-status lamp-status)
  (cdr lamp-status))

(deftype av-mute-status ()
  "Status of the audio-video mute setting on a projector.
Audio-video mute will cease output of audio or video, without powering off the projector.

see `get-av-mute'
see `set-av-mute'"
  '(member :vm-on :am-on :avm-on :avm-off))

(deftype error-component ()
  "A component of a projector that can be reported

see `get-error-status'
see `projector-status'
see `error-status'"
  '(member :fan :lamp :temperature :cover-open :filter :other))

(deftype error-status ()
  "Status of a component of a projector.

see `get-error-status'
see `projector-status'"
  '(member nil :warning :error))

(deftype projector-status ()
  "An alist representing the projector error state.
the keys are `error-component' and values `error-status'
eg.
'((:fan . nil)
  (:lamp . nil)
  (:temperature . :warning)
  (:cover-open . nil)
  (:filter . :error)
  (:other . nil))

see `get-error-status'"
  'list)

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
  (check-type input-type input-type)
  (check-type input-number input-number)
  (%chars->string
   (ecase input-type
     (:rgb #\1)
     (:video #\2)
     (:digital #\3)
     (:storage #\4)
     (:network #\5))
   (char "0123456789" input-number)))

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

(defun %lamp-str->lamp-status (lamps-str)
  "Parses a lamp string into a list of `lamp-status's
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
    :collecting (lamp-status hours is-on)))

(defun %inst-str->projector-inputs (inst-str)
  "Parses a inst string into a list of `projector-input's
`inst-str' should be a string where each input is represented by

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
    :collecting (projector-input type number)))

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

(%defpjlink-set set-input* (1 "INPT") (projector-input)
  "As `set-input' but using a `projector-input' object instead."
  (%input->string (input-type projector-input) (input-number projector-input)))

(%defpjlink-get get-input (1 "INPT") nil (result)
  "Query the current `projector-input' on the projector."
  (projector-input
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
  (%lamp-str->lamp-status result))

(%defpjlink-get get-inputs (1 "INST") nil (result)
  "Query the available `projector-input's on the projector as a list."
  (%inst-str->projector-inputs result))

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
