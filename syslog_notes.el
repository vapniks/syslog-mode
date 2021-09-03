;; This file contains a definition of `syslog-notes' used by the `syslog-show-notes' function.
;; See the `syslog-notes' documentation for info about the correct format
;; After editing save & kill this buffer, and then in the syslog-mode buffer do: M-x syslog-load-notes
;; To always use this file add an entry to the `syslog-notes-files' user option.
;; See also `syslog-show-note-from-manpages' `syslog-show-note-from-file-or-buffer',
;; `syslog-show-note-from-info-node' and `syslog-show-note-from-apropos'
(setq-local syslog-notes
	    (list
	     '(nil nil syslog-show-note-from-apropos word t)
	     ))
