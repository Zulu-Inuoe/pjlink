(in-package #:cl-user)

(defpackage #:pjlink
  (:use #:alexandria #:cl)
  (:export
   #:hostname

   #:pjlink-config
   #:host
   #:port
   #:password
   #:local-host
   #:local-port

   ;;; Class 1
   #:power-status
   #:input-type
   #:input-number
   #:projector-input
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
   #:stop-listener))
