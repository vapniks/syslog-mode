;; This file contains a definition of `syslog-notes' used by the `syslog-show-notes' function
;; for strace buffers.
;; See the `syslog-notes' documentation for info about the correct format
;; After editing save & kill this buffer, and then in the syslog-mode buffer do: M-x syslog-load-notes
;; To always use this file add an entry to the `syslog-notes-files' user option.
;; See also `syslog-show-note-from-manpages' `syslog-show-note-from-file-or-buffer',
;; and `syslog-show-info-node-note'.
;; It is recommended to compile this file for faster loading.

(setq-local syslog-notes (list
			  ;; signal descriptions are found in the signal(7) manpage
			  '("ABRT\\|ALRM\\|BUS\\|CHLD\\|CLD\\|CONT\\|EMT\\|FPE\\|HUP\\|ILL\\|INFO\\|INT\\|IO\\|IOT\\|KILL\\|LOST\\|PIPE\\|POLL\\|PROF\\|PWR\\|QUIT\\|SEGV\\|STKFLT\\|STOP\\|SYS\\|TERM\\|TRAP\\|TSTP\\|TTIN\\|TTOU\\|UNUSED\\|URG\\|USR1\\|USR2\\|VTALRM\\|WINCH\\|XCPU\\|XFSZ"
			    "\\<rt_sigprocmask" syslog-show-note-from-manpages
			    (lambda (word) (concat "SIG" word)) "signal(7)")
			  '("ABRT\\|ALRM\\|BUS\\|CHLD\\|CLD\\|CONT\\|EMT\\|FPE\\|HUP\\|ILL\\|INFO\\|INT\\|IO\\|IOT\\|KILL\\|LOST\\|PIPE\\|POLL\\|PROF\\|PWR\\|QUIT\\|SEGV\\|STKFLT\\|STOP\\|SYS\\|TERM\\|TRAP\\|TSTP\\|TTIN\\|TTOU\\|UNUSED\\|URG\\|USR1\\|USR2\\|VTALRM\\|WINCH\\|XCPU\\|XFSZ"
			    "\\<rt_sigprocmask\\((\\| resumed\\).*~\\["
			    " ~[...] represents the complement of the signals used in the mask, i.e. use all signals apart from those listed")
			  ;; other signals
			  '("SIG_0" "kill(.*SIG_0)"
			    "SIG_0 does nothing, but can be used in kill calls to test if a process ID is valid")
			  '("RTMIN" "\\<rt_sigprocmask" "RTMIN is the first real-time signal for user-defined purposes")
			  '("RTMAX" "\\<rt_sigprocmask" "RTMAX is the last real-time signal for user-defined purposes")
			  '("RT_1" "\\<rt_sigprocmask" "RT_1 is a real-time signal used internally by pthread library")
			  '("-1" "^\\S-+ \\(wait4\\|waitpid\\)(" "-1 arg means wait for any child process")
			  ;; ioctl has several different manpages (don't match "ioctl" itself, that's matched by another entry)
			  '("ioctl\\|\\(.*\\)" "\\<ioctl" syslog-show-note-from-manpages
			    word ("ioctl_console" "ioctl_fat" "ioctl_ficlonerange" "ioctl_fideduperange"
				  "ioctl_getfsmap" "ioctl_iflags" "ioctl_list" "ioctl_ns" "ioctl_tty"
				  "ioctl_userfaultfd")
			    t)
			  ;; termios struct flags are found in the termios(3) manpage
			  '("-?\\(ignbrk\\|brkint\\|ignpar\\|parmrk\\|inpck\\|istrip\\|inlcr\\|igncr\\|icrnl\\|iuclc\\|ixon\\|ixany\\|ixoff\\|imaxbel\\|iutf8\\|opost\\|olcuc\\|onlcr\\|ocrnl\\|onocr\\|onlret\\|ofill\\|ofdel\\|nldly\\|crdly\\|tabdly\\|bsdly\\|vtdly\\|ffdly\\|cbaud\\|cbaudex\\|csize\\|cstopb\\|cread\\|parenb\\|parodd\\|hupcl\\|clocal\\|loblk\\|cibaud\\|cmspar\\|crtscts\\|isig\\|icanon\\|xcase\\|echo\\|echoe\\|echok\\|echonl\\|echoctl\\|echoprt\\|echoke\\|defecho\\|flusho\\|noflsh\\|tostop\\|pendin\\|iexten\\|vdiscard\\|vdsusp\\|veof\\|veol\\|veol2\\|verase\\|vintr\\|vkill\\|vlnext\\|vmin\\|vquit\\|vreprint\\|vstart\\|vstatus\\|vstop\\|vsusp\\|vswtch\\|vtime\\|vwerase\\)"
			    "\\<ioctl" syslog-show-note-from-manpages
			    (lambda (word) (upcase (replace-regexp-in-string "^-" "" word)))
			    "termios(3)")
			  '("-\\(ignbrk\\|brkint\\|ignpar\\|parmrk\\|inpck\\|istrip\\|inlcr\\|igncr\\|icrnl\\|iuclc\\|ixon\\|ixany\\|ixoff\\|imaxbel\\|iutf8\\|opost\\|olcuc\\|onlcr\\|ocrnl\\|onocr\\|onlret\\|ofill\\|ofdel\\|nldly\\|crdly\\|tabdly\\|bsdly\\|vtdly\\|ffdly\\|cbaud\\|cbaudex\\|csize\\|cstopb\\|cread\\|parenb\\|parodd\\|hupcl\\|clocal\\|loblk\\|cibaud\\|cmspar\\|crtscts\\|isig\\|icanon\\|xcase\\|echo\\|echoe\\|echok\\|echonl\\|echoctl\\|echoprt\\|echoke\\|defecho\\|flusho\\|noflsh\\|tostop\\|pendin\\|iexten\\|vdiscard\\|vdsusp\\|veof\\|veol\\|veol2\\|verase\\|vintr\\|vkill\\|vlnext\\|vmin\\|vquit\\|vreprint\\|vstart\\|vstatus\\|vstop\\|vsusp\\|vswtch\\|vtime\\|vwerase\\)"
			    "\\<ioctl" "This setting is negated")
			  ;; rt_sigaction uses different indentations for different types of words 
			  '("sa_[a-z]+" "\\<rt_sigaction"
			    syslog-show-note-from-manpages word "rt_sigaction" t 7 Man-underline)
			  '(".*" "\\<rt_sigaction"
			    syslog-show-note-from-manpages word "rt_sigaction" t 11)
			  ;; CSI sequences can be found in the console_codes manpage
			  ;; (you may need to adjust the start & end positions)
			  '("\\\\33\\[[0-9?]*\\([A-Za-z@`]\\)" "\\(read\\|write\\).*\".*\""
			    syslog-show-note-from-manpages word "console_codes" nil 7 nil 6854 9360)
			  ;; if point is on the function itself, show the apropos description
			  '(".*" "^\\S-+\\s-+\\([^(]+\\)("
			    (lambda (word line) (if (string= word line)
						    (syslog-show-note-from-apropos word nil 2)))
			    word line)
			  ;; search manpage of function at start of line if nothing else was found
			  '(".*" "^\\S-+\\s-+\\([^(]+\\)(" syslog-show-note-from-manpages
			    word (lambda (line) (concat line "(2)")) accum)
			  ;; it could also be resumed from a previous line
			  '(".*" "^\\S-+\\s-+<... \\(\\S-+\\) resumed" syslog-show-note-from-manpages
			    word (lambda (line) (concat line "(2)")) accum)
			  ;; search console_codes manpage (available when prefix arg is used)
			  '(nil consolecodes syslog-show-note-from-manpages
				word "console_codes" nil nil)
			  ))
