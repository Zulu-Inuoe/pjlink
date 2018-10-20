# PJLink
A Common Lisp library for interfacing with [PJLink](https://pjlink.jbmia.or.jp/english/) standard compliant devices.

PJLink projectors operate over TCP/IP.

This library implements [class 1](https://pjlink.jbmia.or.jp/english/data/5-1_PJLink_eng_20131210.pdf), in which you can:

* Query power status and power projector on/off
* Query current and available inputs, and set active input
* Query and set audio-video mute status
* Query error status, for projector components (fan, lamp, etc)
* Get lamp information (usage time, on/off)
* Query projector, manufacturer, and product names.

Support for [class 2](https://pjlink.jbmia.or.jp/english/data_cl2/PJLink_5-1.pdf) is planned, which allows for:
* LAN Projector discovery.
* Serial version query.
* Detailed lamp information (replacement number)
* Microphone and speaker control.
* Software version information.

# Usage

Some code examples assuming projector at IP address "192.168.1.5"

## Querying Power Status
``` common-lisp
(pjlink:get-power-status "192.168.1.5")
;; =>
:STANDBY
```

## Getting projector name

``` common-lisp
(pjlink:get-projector-name "192.168.1.5")
;; =>
"Projector Name"
```

## Set audio-video mute:
``` common-lisp
(pjlink:set-av-mute :avmt-on "192.168.1.5"" :password "JBMIAProjectorLink")
```
This example shows using authentication

Alternatively, all operations support taking in an object of class `pjlink:pjlink-config`:

``` common-lisp
(let ((config (make-instance 'pjlink:pjlink-config
                             :host "192.168.1.1"
                             :password "JBMIAProjectorLink)))
  ;;Set the input to one of the available ones
  (let ((inputs (pjlink:get-inputs config)))
    (pjlink:set-input* (first inputs) config))
  ;;Power off the projector
  (pjlink:power-off config))
```

This prevents having to pass around connection information such as host and password information.

# Dependencies
* [alexandria](http://quickdocs.org/alexandria/)
* [flexi-streams](http://quickdocs.org/flexi-streams/)
* [md5](http://quickdocs.org/md5/)
* [split-sequence](http://quickdocs.org/split-sequence/)
* [usocket](http://quickdocs.org/usocket/)

# License
See [LICENSE](LICENSE.txt)
