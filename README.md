# PJLink
A Common Lisp library for interfacing with [PJLink](https://pjlink.jbmia.or.jp/english/) standard compliant devices.

PJLink projectors operate over TCP/IP.

#### Table of Contents
* [Overview](#overview)
* [Usage](#usage)
  * [Connection Parameters](#connection-parameters)
  * [Class 2](#class-2)
    * [Search Procedure](#search-procedure)
    * [Status Notification](#status-notification)
* [Dependencies](#dependencies)
* [License](#license)

# Overview
This library implements both [class 1](https://pjlink.jbmia.or.jp/english/data/5-1_PJLink_eng_20131210.pdf) and [class 2](https://pjlink.jbmia.or.jp/english/data_cl2/PJLink_5-1.pdf) support, in which you can:

Class 1:

* Query power status and power projector on/off
* Query current and available inputs, and set active input
* Query and set audio-video mute status
* Query error status, for projector components (fan, lamp, etc)
* Get lamp information (usage time, on/off)
* Query projector, manufacturer, and product names.

Class 2:

* LAN Projector discovery.
* Projector status notifications.
* Serial version query.
* Resolution query.
* Lamp and filter model numbers.
* Microphone and speaker control.
* Projector "freeze" control
* Software version information.


# Usage
Code examples have `*ip*` bound to the IP of the projector, eg `"192.168.1.2"`

``` common-lisp
;; Get power status
(pjlink:get-power-status *ip*)
;; =>
:STANDBY

;; Turn projector on
(pjlink:turn-on *ip*)

(pjlink:get-power-status *ip*)
;; =>
:WARM-UP

;; After some time
(pjlink:get-power-status *ip*)
;; =>
:LAMP-ON


;; Query available inputs
(pjlink:get-inputs *ip*)
;; =>
((:RGB . 1) (:VIDEO . 2) (:DIGITAL . 3) (:STORAGE . 4) (:RGB . 2) (:VIDEO . 6) (:DIGITAL . 7))

;; Set the input to one of them
(pjlink:set-input '(:video . 6) *ip*)

;; Get the currently active input
(pjlink:get-input *ip*)
;; =>
(:VIDEO . 6)
```

## Connection Parameters
In addition to parameters for the commands themselves and the host designator,
all of the PJlink commands accept a 'host designator' and the following keyword parameters:

* **port** - Port to connect to. Defaults to PJLink port (4352)
* **password** - Password to use if authentication is required
* **local-hort** - Local interface to use for the connection
* **local-port** - Local port to use for the connection

Each of these parameters has a corresponding generic function you may specialize
in order to make it easier to pass along similar parameters, or to customize eg
password retrieval:

``` common-lisp
(defclass my-pjlink-host ()
  ())

(defmethod pjlink:host ((obj my-pjlink-host))
  *ip*)

(defmethod pjlink:password ((obj my-pjlink-host))
  "Get the password by reading it from the user"
  (read-password-from-user))

;; ...

(pjlink:power-on (make-instance 'my-pjlink-host))
```

The class `pjlink-config` is provided as a simple container:

``` common-lisp
(let ((config (make-instance 'pjlink:pjlink-config
                             :host *ip*
                             :password "JBMIAProjectorLink)))
  ;;Set the input to one of the available ones
  (let ((inputs (pjlink:get-inputs config)))
    (pjlink:set-input* (first inputs) config))
  ;;Power off the projector
  (pjlink:power-off config))
```

## Class 2
Class 2 the ability to query the LAN for projectors and receive notifications.

### Search Procedure
Projectors on the LAN can be queried by using the class 2 search procedure.

**Note:** It is highly recommend to specify an interface (network card) to conduct the search, as it is based on UDP broadcast and will not route properly otherwise.

**Note:** This is a synchronous, 30 second call as per the spec

**Note:** Because the search procedure uses the same port as status notification, currently there's no way to conduct a search while a status listener is active.

``` common-lisp
(pjlink:search-projectors :local-host *interface-address*)
;; 30 seconds later
;; =>
((#(192 168 2 5) . #(188 95 244 185 102 171))
 (#(192 168 2 5) . #(10 0 39 0 0 14)))
```

The return value here is a list of projector IP's and mac-addresses

### Status Notification
A `status-listener` can be created to asynchronously monitor projector status. These updates have to be configured on the projector in a model-specific fashion so that they notify a host of status updates via UDP.
Creating and starting a listener will create a background worker thread to monitor this socket and signal an event when a status update occurs:

**Note:** It is highly recommended to specify an interface (network card) to listen on, rather than using the 'default' network interface.

``` common-lisp
(defun print-pjlink-event (remote-host event-type args)
  (format t "Received notification from ~A: ~A = ~A" remote-host event-type args))

;; When 'handlers' is provided, listener is started automatically
(defparameter *listener* (pjlink:make-status-listener :local-host *interface-address* :handlers #'print-pjlink-event))

;; Otherwise call (pjlink:start-listener *listener*)

;; When finished listening for events:
(pjlink:stop-listener *listener*)

;; Can always resume listening later:
(pjlink:start-listening *listener*)
```

# Dependencies
* [alexandria](http://quickdocs.org/alexandria/)
* [bordeaux-threads](https://github.com/sionescu/bordeaux-threads)
* [ip-interfaces](http://quickdocs.org/ip-interfaces/)
* [md5](http://quickdocs.org/md5/)
* [split-sequence](http://quickdocs.org/split-sequence/)
* [trivial-garbage](https://github.com/trivial-garbage/trivial-garbage)
* [usocket](http://quickdocs.org/usocket/)

# License
See [LICENSE](LICENSE.txt)
