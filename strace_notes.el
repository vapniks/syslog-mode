;; This file contains a definition of `syslog-notes' used by the `syslog-show-notes' function.
;; See the `syslog-notes' documentation for info about the correct format
;; After editing save & kill this buffer, and then in the syslog-mode buffer do: M-x syslog-load-notes
;; To always use this file add an entry to the `syslog-notes-files' user option.
;; See also `syslog-show-note-from-manpages' `syslog-show-note-from-file-or-buffer',
;; and `syslog-show-info-node-note'.

(setq syslog-notes (list
		    ;; signal descriptions are found in the signal(7) manpage
		    '("ABRT\\|ALRM\\|BUS\\|CHLD\\|CLD\\|CONT\\|EMT\\|FPE\\|HUP\\|ILL\\|INFO\\|INT\\|IO\\|IOT\\|KILL\\|LOST\\|PIPE\\|POLL\\|PROF\\|PWR\\|QUIT\\|SEGV\\|STKFLT\\|STOP\\|SYS\\|TERM\\|TRAP\\|TSTP\\|TTIN\\|TTOU\\|UNUSED\\|URG\\|USR1\\|USR2\\|VTALRM\\|WINCH\\|XCPU\\|XFSZ"
		      "rt_sigprocmask" syslog-show-note-from-manpages (lambda (word) (concat "SIG" word)) "signal(7)")
		    ;; ioctl has several different manpages
		    '(nil "^\\S-+ \\(ioctl\\)(" syslog-show-note-from-manpages
			  word ("ioctl_console" "ioctl_fat" "ioctl_ficlonerange" "ioctl_fideduperange"
				"ioctl_getfsmap" "ioctl_iflags" "ioctl_list" "ioctl_ns" "ioctl_tty"
				"ioctl_userfaultfd"))
		    ;; rt_sigaction uses different indentations for different types of words 
		    '(nil "^\\S-+ \\(rt_sigaction\\)(" syslog-show-note-from-manpages word line t 11)
		    '("sa_[a-z]+" "^\\S-+ \\(rt_sigaction\\)("
		      syslog-show-note-from-manpages word line t 7 Man-underline)
		    ;; by default search manpage of function at start of line
		    '(nil "^\\S-+ \\([^(]+\\)(" syslog-show-note-from-manpages word line)
		    ))

