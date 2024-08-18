;; This file contains a definition of `syslog-notes' used by the `syslog-show-notes' function
;; for buffers containing output from running make.
;; See the `syslog-notes' documentation for info about the correct format.
;; After editing save & kill this buffer, and then in the syslog-mode buffer do: M-x syslog-load-notes
;; To always use this file add an entry to the `syslog-notes-files' user option.
;; See also `syslog-show-note-from-manpages' `syslog-show-note-from-file-or-buffer',
;; and `syslog-show-info-node-note'.
;; It is recommended to compile this file for faster loading.

(setq-local syslog-notes (list
			  ;; gcc options
			  '("-[a-zA-Z-]+" "^\\(?:\\S-+/\\)?\\(?:[gc]\\+\\+\\|gcc\\)\\s-.*" syslog-show-note-from-manpages word "gcc" t 7 Man-overstrike)
			  ;; make options
			  '("-[a-zA-Z-]+" "^\\(?:\\S-+/\\)?make\\s-.*" syslog-show-note-from-manpages word "make" t 7 Man-overstrike)
			  ;; cmake options
			  '("-[a-zA-Z-]+" "^\\(?:\\S-+/\\)?cmake\\s-.*" syslog-show-note-from-manpages word "cmake" t 7 Man-overstrike)
			  ))
