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

   ;; Class 1
   #:power-status
   #:input-type
   #:av-mute-status
   #:error-status

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

   ;; Class 2
   #:input-type2

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
   #:get-freeze-status))
