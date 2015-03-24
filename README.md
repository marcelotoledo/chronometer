chronometer
==========

**A Chronometer for GNU Emacs**

This mode is for counting times. I basically created it because I
used to cook pizza while coding and I used to forget the time and a
few minutes later I could smell a burned pizza.

You can use it for whatever purpose you like, but the typical
scenario is to keep alerted of how much time has past.

To make it available for Emacs, put it in your `load-path' and
insert (require 'chronometer) in your .emacs.

Use `chronometer' to start the chronometer, it will automaticaly
start from zero and will keep incrementing every one second.

From now on you may want to play with the following keybindings:

* a - set alarm
* u - unset alarm
* p - toggle pause
* r - restart
* h - hide
* q - exit
* ? - help

TODO
==========

* stop/start
* count down (maybe)
* allow more input formats for the alarm
* make it unable to set alarm to less then the elapsed time is
* execute a command when reach alarm
