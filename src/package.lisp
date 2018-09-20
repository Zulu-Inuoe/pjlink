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

   #:power-status

   #:input-type

   #:projector-input
   #:input-type
   #:input-number

   #:av-mute-status

   #:error-status

   #:projector-status
   #:fan-status
   #:lamp-status
   #:temperature-status
   #:cover-open-status
   #:filter-status
   #:other-status

   #:projector-lamp
   #:lamp-number
   #:lamp-hours
   #:lamp-is-on

   ;;; Operations on the projector
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
   #:get-pjlink-class))
