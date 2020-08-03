(in-package #:cl-user)

(defpackage #:pjlink
  (:use #:cl)
  (:export
   #:+default-port+
   #:+max-password-length+

   #:hostname

   ;; host-info
   #:host
   #:port
   #:password
   #:local-host
   #:local-port

   #:pjlink-config

   #:projector-command-error
   #:projector-command-error-host
   #:projector-command-error-class
   #:projector-command-error-command

   #:authorization-error

   #:undefined-command-error

   #:out-of-parameter-error
   #:out-of-parameter-error-parameter

   #:unavailable-time-error

   #:projector-display-error

   ;;; Class 1
   #:power-status
   #:input-type
   #:input-number
   #:projector-input
   #:lamp-status
   #:lamp-hours
   #:lamp-on-p
   #:av-mute-status
   #:error-component
   #:error-status
   #:projector-status

   #:power-on
   #:power-off
   #:get-power-status
   #:set-input
   #:set-input*
   #:get-input
   #:set-av-mute
   #:get-av-mute
   #:get-error-status
   #:get-lamps
   #:get-inputs
   #:get-projector-name
   #:get-manufacturer-name
   #:get-product-name
   #:get-pjlink-class

   ;;; Class 2
   #:input-type2
   #:input-number2
   #:projector-input2
   #:projector-resolution
   #:mac-address
   #:status-event

   #:set-input2
   #:set-input2*
   #:get-input2
   #:get-inputs2
   #:get-serial-number
   #:get-software-version
   #:get-input-name
   #:get-input-name*
   #:get-resolution
   #:get-recommended-resolution
   #:get-filter-usage-time
   #:get-lamp-model
   #:get-filter-model
   #:increase-speaker
   #:decrease-speaker
   #:increase-microphone
   #:decrease-microphone
   #:freeze-screen
   #:unfreeze-screen
   #:get-freeze-status

   #:search-projectors

   ;; Status notification protocol
   #:status-listener
   #:make-status-listener
   #:status-handler
   #:add-handler
   #:remove-handler
   #:start-listener
   #:stop-listener)
  (:import-from
   #:alexandria
   #:ensure-list
   #:parse-body
   #:removef
   #:switch
   #:eswitch
   #:required-argument
   #:when-let
   #:with-gensyms))
