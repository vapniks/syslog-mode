;; This file contains a definition of `syslog-notes' used by the `syslog-show-notes' function.
;; See the `syslog-notes' documentation for info about the correct format
;; After editing save & kill this buffer, and then in the syslog-mode buffer do: M-x syslog-load-notes
;; To always use this file add an entry to the `syslog-notes-files' user option.
;; See also `syslog-show-note-from-manpages' `syslog-show-note-from-file-or-buffer',
;; and `syslog-show-info-node-note'.

(setq syslog-notes (list
		    ;; signal descriptions are found in the signal(7) manpage
		    '("ABRT\\|ALRM\\|BUS\\|CHLD\\|CLD\\|CONT\\|EMT\\|FPE\\|HUP\\|ILL\\|INFO\\|INT\\|IO\\|IOT\\|KILL\\|LOST\\|PIPE\\|POLL\\|PROF\\|PWR\\|QUIT\\|SEGV\\|STKFLT\\|STOP\\|SYS\\|TERM\\|TRAP\\|TSTP\\|TTIN\\|TTOU\\|UNUSED\\|URG\\|USR1\\|USR2\\|VTALRM\\|WINCH\\|XCPU\\|XFSZ"
		      "\\<rt_sigprocmask\\((\\| resumed\\)" syslog-show-note-from-manpages
		      (lambda (word) (concat "SIG" word)) "signal(7)")
		    ;; ioctl has several different manpages
		    '("ioctl\\|\\(.*\\)" "\\<ioctl\\((\\| resumed\\)" syslog-show-note-from-manpages
		      word ("ioctl_console" "ioctl_fat" "ioctl_ficlonerange" "ioctl_fideduperange"
			    "ioctl_getfsmap" "ioctl_iflags" "ioctl_list" "ioctl_ns" "ioctl_tty"
			    "ioctl_userfaultfd"))
		    ;; termios struct flags are found in the termios(3) manpage
		    '("-?\\(ignbrk\\|brkint\\|ignpar\\|parmrk\\|inpck\\|istrip\\|inlcr\\|igncr\\|icrnl\\|iuclc\\|ixon\\|ixany\\|ixoff\\|imaxbel\\|iutf8\\|opost\\|olcuc\\|onlcr\\|ocrnl\\|onocr\\|onlret\\|ofill\\|ofdel\\|nldly\\|crdly\\|tabdly\\|bsdly\\|vtdly\\|ffdly\\|cbaud\\|cbaudex\\|csize\\|cstopb\\|cread\\|parenb\\|parodd\\|hupcl\\|clocal\\|loblk\\|cibaud\\|cmspar\\|crtscts\\|isig\\|icanon\\|xcase\\|echo\\|echoe\\|echok\\|echonl\\|echoctl\\|echoprt\\|echoke\\|defecho\\|flusho\\|noflsh\\|tostop\\|pendin\\|iexten\\|vdiscard\\|vdsusp\\|veof\\|veol\\|veol2\\|verase\\|vintr\\|vkill\\|vlnext\\|vmin\\|vquit\\|vreprint\\|vstart\\|vstatus\\|vstop\\|vsusp\\|vswtch\\|vtime\\|vwerase\\)"
		      "\\<ioctl\\((\\| resumed\\)" syslog-show-note-from-manpages
		      (lambda (word) (upcase (replace-regexp-in-string "^-" "" word)))
		      "termios(3)")
		    ;; rt_sigaction uses different indentations for different types of words 
		    '(".*" "\\<rt_sigaction\\((\\| resumed\\)"
		      syslog-show-note-from-manpages word "rt_sigaction" t 11)
		    '("sa_[a-z]+" "\\<rt_sigaction\\((\\| resumed\\)"
		      syslog-show-note-from-manpages word "rt_sigaction" t 7 Man-underline)
		    ;; by default search manpage of function at start of line
		    '(".*" "^\\S-+ \\([^(]+\\)(" syslog-show-note-from-manpages word line)
		    ;; it could also be resumed from a previous line
		    '(".*" "^\\S-+ <... \\(\\S-+\\) resumed" syslog-show-note-from-manpages word line)
		    ;; if point is on the function itself, show the apropos description
		    '(".*" "^\\S-+ \\([^(]+\\)("
		      (lambda (word line) (if (string= word line)
					      (syslog-show-note-from-apropos word nil 2)))
		      word line)))
