;;; syslog-mode.el --- Major-mode for viewing log files & strace output

;; Filename: syslog-mode.el
;; Description: Major-mode for viewing log files & strace output
;; Author: Harley Gorrell <harley@panix.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Created: 2003-03-17 18:50:12 Harley Gorrell
;; URL: https://github.com/vapniks/syslog-mode
;; Keywords: unix
;; Compatibility: GNU Emacs 24.3.1
;; Package-Requires:  ((hide-lines "20130623") (ov "20150311") (hsluv "20181127"))
;;
;; Features that might be required by this library:
;;
;; hide-lines cl ido dired+ ov thingatpt hi-lock net-utils hsluv
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;; Commentary:
;; * Handy functions for looking at system logs, and strace output.
;; * Fontifys the date and su messages.

;;; Keybindings
;; "C-down" : syslog-boot-start
;; "R"      : revert-buffer
;; "/"      : syslog-filter-lines
;; "C-/"    : syslog-filter-dates
;; "g"      : hide-lines-show-all
;; "k"      : hide-lines-kill-hidden
;; "h r"    : highlight-regexp
;; "h p"    : highlight-phrase
;; "h l"    : highlight-lines-matching-regexp
;; "h u"    : unhighlight-regexp
;; "h U"    : unhiglight all regexps
;; "h q"    : highlight-regexp-unique
;; "C-/"    : syslog-filter-dates
;; "D"      : open dired buffer in log directory (`syslog-log-file-directory')
;; "c"      : syslog-count-matches
;; "x"      : syslog-extract-matches
;; "j"/"f"  : ffap
;; "!"      : syslog-shell-command
;; "?"      : syslog-show-notes
;; "v"      : syslog-view
;; "W"      : syslog-whois-reverse-lookup
;; "m"      : manual-entry
;; "o"      : syslog-open-files
;; "a"      : syslog-append-files
;; "p"      : syslog-prepend-files
;; "t"      : syslog-toggle-filenames
;; "<"      : syslog-previous-file
;; ">"      : syslog-next-file
;; "<M-up>" : syslog-move-previous-file
;; "<M-down>": syslog-move-next-file
;; "q"      : quit-window

;; To update lists of commands & options show below: M-x auto-document

;;; Commands:
;;
;; Below is a complete list of commands:
;;
;;  `syslog-shell-command'
;;    Execute a shell COMMAND synchronously, with prefix arg (SUDOP) run under sudo.
;;    Keybinding: !
;;  `syslog-append-files'
;;    Append FILES into buffer BUF.
;;    Keybinding: a
;;  `syslog-prepend-files'
;;    Prepend FILES into buffer BUF.
;;    Keybinding: M-x syslog-prepend-files
;;  `syslog-open-files'
;;    Insert log FILES into new buffer, and switch to that buffer.
;;    Keybinding: o
;;  `syslog-view'
;;    Open a view of syslog files with optional filters and highlights applied.
;;    Keybinding: v
;;  `syslog-previous-file'
;;    Open the previous logfile backup, or the next one if a prefix arg is used.
;;    Keybinding: <
;;  `syslog-next-file'
;;    Open the next logfile.
;;    Keybinding: >
;;  `syslog-move-next-file'
;;    Move to the next file in the current `syslog-mode' buffer.
;;    Keybinding: <M-down>
;;  `syslog-move-previous-file'
;;    Move to the next file in the current `syslog-mode' buffer.
;;    Keybinding: <M-up>
;;  `syslog-toggle-filenames'
;;    Toggle the display of filenames before each line.
;;    Keybinding: t
;;  `syslog-filter-lines'
;;    Restrict buffer to blocks of text between matching regexps.
;;    Keybinding: /
;;  `highlight-regexp-unique'
;;    Highlight each unique string matched by REGEXP with a different face.
;;    Keybinding: q
;;  `syslog-filter-dates'
;;    Restrict buffer to lines between times START and END (Emacs time lists).
;;    Keybinding: C-/
;;  `syslog-mode'
;;    Major mode for working with system logs, and strace output.
;;    Keybinding: M-x syslog-mode
;;  `syslog-count-matches'
;;    Count all matches to regexp RX in current buffer.
;;    Keybinding: c
;;  `syslog-extract-matches'
;;    Extract & concatenate strings matching regexp RX (or its match groups).
;;    Keybinding: x
;;  `syslog-boot-start'
;;    Jump forward in the log to when the system booted.
;;    Keybinding: <C-down>
;;  `syslog-whois-reverse-lookup'
;;    This is a wrapper around the `whois' command using symbol at point as default search string.
;;    Keybinding: W
;;  `syslog-transform-strace'
;;    Transform strace output in the current buffer.
;;    Keybinding: M-x syslog-transform-strace
;;  `syslog-extract-fds-from-strace'
;;    Extract strace output lines involving a particular file descriptor(s).
;;    Keybinding: X
;;  `forward-syslog-token'
;;    Move point forward over ARG tokens (backwards if ARG is negative).
;;    Keybinding: M-x forward-syslog-token
;;  `syslog-show-notes'
;;    In the minibuffer display notes associated with the region or WORD at point.
;;    Keybinding: ?
;;  `syslog-notes-next-match'
;;    Search other window for the next match to the word/regexp used by the last call to `syslog-show-notes'.
;;    Keybinding: M-n
;;  `syslog-notes-prev-match'
;;    Search other window for the previous match to the word/regexp used by the last call to `syslog-show-notes'.
;;    Keybinding: M-p
;;  `syslog-load-notes'
;;    Load appropriate notes file for the current buffer.
;;    Keybinding: M-x syslog-load-notes
;;  `syslog-edit-notes'
;;    Edit syslog notes file associated with current buffer.
;;    Keybinding: M-x syslog-edit-notes
;;
;;; Customizable Options:
;;
;; Below is a list of customizable options:
;;
;;  `syslog-mode-hook'
;;    *Hook to setup `syslog-mode'.
;;    default = nil
;;  `syslog-views'
;;    A list of views.
;;    default = nil
;;  `syslog-datetime-regexp'
;;    A regular expression matching the date-time at the beginning of each line in the log file.
;;    default = "^\\(?:[^ :]+: \\)?\\(\\(?:\\(?:[[:alpha:]]\\{3\\}\\)?[[:space:]]*[[:alpha:]]\\{3\\}\\s-+[0-9]+\\s-+[0-9:]+\\)\\|\\(?:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\s-+[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)\\)"
;;  `syslog-log-file-directory'
;;    The directory in which log files are stored.
;;    default = "/var/log/"
;;  `syslog-large-file-size'
;;    When `syslog-show-note-from-file-or-buffer' tries to load a file larger than this it prompts the user.
;;    default = 500000
;;  `syslog-hi-face-defaults'
;;    Alist of face sets to use for automatic highlighting.
;;    default = (list (quote (bg lambda nil ...)) (quote (fg lambda nil ...)) (quote (light_bg lambda nil ...)) (quote (dark_bg lambda nil ...)) ...)
;;  `syslog-notes-files'
;;    An alist used by `syslog-load-notes' for choosing a notes file to load.
;;    default = (let ((dir ...)) (list (cons ".*\\.strace" ...) (cons "^strace:" ...) (cons "syslog.*" ...)))
;;  `syslog-notes-default'
;;    List of `syslog-notes' items that are always available.
;;    default = nil
;;  `syslog-manpage-wait'
;;    Amount of time to wait for manpage to finish rendering, when processing manpages.
;;    default = 0.2
;;  `syslog-note-things'
;;    An alist of (REGEX . SYMB) pairs for choosing `syslog-note-thing'.
;;    default = nil


;; All of the above can customized by:
;;      M-x customize-group RET syslog-mode RET
;;

;;; Installation:
;;
;; Put syslog-mode.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'syslog-mode)



;;; Change log:
;;
;; This is out of date, please refer to the git commits on github:
;; https://github.com/vapniks/syslog-mode
;; 12-07-2021    Joe Bloggs
;;    Many changes: added transform functions to syslog-views option,
;;    highlight-regexp-unique, syslog-extract-matches
;;    and extra functions and font-locking for strace output files.
;; 
;; 21-03-2013    Joe Bloggs
;;    Added functions and keybindings for filtering
;;    lines by regexps or dates, and for highlighting,
;;    and quick key for find-file-at-point
;;
;; 20-03-2013    Christian Giménez
;;    Added more keywords for font-lock.
;;
;; 16-03-2003 : Updated URL and contact info.

;;; Acknowledgements:
;;
;;  Harley Gorrell  (Author)
;;  Christian Giménez
;;

;; If anyone wants to make changes please fork the following github repo: https://github.com/vapniks/syslog-mode

;;; TODO: statistical reporting - have a regular expression to match item type, then report counts of each item type.
;;        also statistics on number of items per hour/day/week/etc.


;;; Require
(require 'hide-lines)
(eval-when-compile (require 'cl))
(require 'ido)
(require 'hi-lock)
(require 'net-utils)
(require 'ov)
(require 'thingatpt)
(require 'man nil t)
(require 'hsluv)

;;; Code:

;; Setup
;; simple-call-tree-info: DONE  
(defgroup syslog nil
  "syslog-mode - a major mode for viewing log files"
  :link '(url-link "https://github.com/vapniks/syslog-mode"))

;; simple-call-tree-info: DONE  
(defcustom syslog-mode-hook nil
  "*Hook to setup `syslog-mode'."
  :group 'syslog
  :type 'hook)

;; simple-call-tree-info: DONE  
(defvar syslog-mode-load-hook nil
  "*Hook to run when `syslog-mode' is loaded.")

;;;###autoload
;; simple-call-tree-info: DONE
(defvar syslog-setup-on-load nil
  "*If not nil setup syslog mode on load by running syslog-add-hooks.")

;; I also use "Alt" as C-c is too much to type for cursor motions.
;; simple-call-tree-info: DONE
(defvar syslog-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Ctrl bindings
    (define-key map [C-down] 'syslog-boot-start)
    (define-key map "R" 'revert-buffer)
    (define-key map "/" 'syslog-filter-lines)
    (define-key map "g" 'hide-lines-show-all)
    (define-prefix-command 'syslog-highlight-map)
    (define-key map "h" 'syslog-highlight-map)
    (define-key map (kbd "h r") 'highlight-regexp)
    (define-key map (kbd "h p") 'highlight-phrase)
    (define-key map (kbd "h l") 'highlight-lines-matching-regexp)
    (define-key map (kbd "h u") 'unhighlight-regexp)
    (define-key map (kbd "h U") (lambda nil (interactive) (unhighlight-regexp t)))
    (define-key map (kbd "h q") 'highlight-regexp-unique)
    (define-key map (kbd "C-/") 'syslog-filter-dates)

    (define-key map "D" (lambda nil (interactive) (dired syslog-log-file-directory)))
    (define-key map "j" 'ffap)
    (define-key map "f" 'ffap)
    (define-key map "<" 'syslog-previous-file)
    (define-key map ">" 'syslog-next-file)
    (define-key map "o" 'syslog-open-files)
    (define-key map "a" 'syslog-append-files)
    (define-key map "p" 'syslog-prepend-files)
    (define-key map "v" 'syslog-view)
    (define-key map "c" 'syslog-count-matches)
    (define-key map "x" 'syslog-extract-matches)
    (define-key map "X" 'syslog-extract-fds-from-strace)
    (define-key map "k" 'hide-lines-kill-hidden)
    (define-key map "W" 'syslog-whois-reverse-lookup)
    (define-key map "m" 'manual-entry)
    (define-key map "q" 'quit-window)
    (define-key map "!" 'syslog-shell-command)
    (define-key map "?" 'syslog-show-notes)
    (define-key map (kbd "M-n") 'syslog-notes-next-match)
    (define-key map (kbd "M-p") 'syslog-notes-prev-match)
    (define-key map (kbd "<M-down>") 'syslog-move-next-file)
    (define-key map (kbd "<M-up>") 'syslog-move-previous-file)
    (define-key map "t" 'syslog-toggle-filenames)
    ;; XEmacs does not like the Alt bindings
    (if (string-match "XEmacs" (emacs-version)) t)
    map)
  "The local keymap for `syslog-mode'.")

;; simple-call-tree-info: DONE
(defvar syslog-number-suffix-start 1
  "The first number used as rotation suffix.")

;; simple-call-tree-info: DONE
(defun syslog-shell-command (command &optional sudop tostrings)
  "Execute a shell COMMAND synchronously, with prefix arg (SUDOP) run under sudo.
If TOSTRINGS is non-nil then output will be returned as a list of strings (one per line),
otherwise it will be place in the *Shell Command Output* buffer."
  (interactive (list (read-shell-command (if current-prefix-arg
					     "Shell command (root): "
					   "Shell command: "))
		     current-prefix-arg))
  (with-temp-buffer
    (when sudop
      (cd (concat "/sudo::"
		  (replace-regexp-in-string
		   "^/sudo[^/]+" "" default-directory))))
    (if tostrings
	(split-string (shell-command-to-string command) "\n" t)
      (shell-command command))))



;; simple-call-tree-info: DONE
(defun syslog-get-basename-and-number (filename)
  "Return the basename and number suffix of a log file in FILEPATH.
Return results in a cons cell '(basename . number) where basename is a string,
and number is a number."
  (let* ((res (string-match "\\(.*?\\)\\.\\([0-9]+\\)\\(\\.t?gz\\)?" filename))
         (basename (if res (match-string 1 filename) filename))
         (str (and res (match-string 2 filename)))
         (num (or (and str (string-to-number str)) (1- syslog-number-suffix-start))))
    (cons basename num)))

;; simple-call-tree-info: DONE
(defun syslog-get-filenames (&optional pairs prompt onlyone)
  "Get log files associated with PAIRS argument, or prompt user for files.
The PAIRS argument should be a list of cons cells whose cars are paths to log files,
and whose cdrs are numbers indicating how many previous log files (if positive) or days 
 (if negative) to include. If PAIRS is missing then the user is prompted for those values.
If ONLYONE is non-nil then the user is only prompted for a single file.
The PROMPT argument is an optional prompt to use for prompting the user for files."
  (let* ((continue t)
	 (num 0)
	 (pairs
	  (or pairs
	      (cl-loop
	       while continue
	       do (setq
		   filename
		   (ido-read-file-name
		    (or prompt "Log file: ")
		    syslog-log-file-directory "syslog" nil)
		   num (if onlyone 0
			 (read-number
			  "Number of previous files (if positive) or days (if negative) to include"
			  num)))
	       collect (cons filename num)
	       if onlyone do (setq continue nil)
	       else do (setq continue (y-or-n-p "Add more files? "))))))
    (cl-remove-duplicates
     (cl-loop for pair1 in pairs
	      for filename = (car pair1)
	      for num = (cdr pair1)
	      for pair = (syslog-get-basename-and-number filename)
	      for basename = (car pair)
	      for basename2 = (file-name-nondirectory basename)
	      for curver = (cdr pair)
	      for num2 = (if (>= num 0) num
			   (- (let* ((startdate (+ (float-time (nth 5 (file-attributes filename)))
						   (* num 86400))))
				(cl-loop for file2 in (directory-files (file-name-directory filename)
								       t basename2)
					 for filedate2 = (float-time (nth 5 (file-attributes file2)))
					 if (>= filedate2 startdate)
					 maximize (cdr (syslog-get-basename-and-number file2))))
			      curver))
	      for files = (cl-loop for n from (1+ curver) to (+ curver num2)
				   for numstr = (number-to-string n)
				   for nextfile = (cl-loop for suffix in '(nil ".gz" ".tgz")
							   for filename3 = (concat basename "." numstr suffix)
							   if (file-readable-p filename3)
							   return filename3)
				   collect nextfile)
	      nconc (nconc (list filename) (cl-remove-if 'null files))) :test 'equal)))

;; simple-call-tree-info: DONE
(defun syslog-append-files (files buf &optional replace)
  "Append FILES into buffer BUF.
If REPLACE is non-nil then the contents of BUF will be overwritten.
When called interactively the current buffer is used, FILES are prompted for
using `syslog-get-filenames', and REPLACE is set to nil, unless
a prefix argument is used in which case they are prompted for."
  (interactive (list (syslog-get-filenames nil "Append log file: ")
		     (current-buffer)
		     (if current-prefix-arg
			 (y-or-n-p "Replace current buffer contents? "))))
  (with-current-buffer buf
    (let ((inhibit-read-only t))
      (set-visited-file-name nil)
      (save-excursion
	(cl-loop for file in (cl-remove-duplicates files :test 'equal)
		 do (goto-char (point-max))
		 (let ((start (point)))
		   (insert-file-contents file)
		   (goto-char (point-max))
		   (put-text-property start (point) 'syslog-filename file)))))))

;; simple-call-tree-info: DONE
(defun syslog-prepend-files (files buf &optional replace)
  "Prepend FILES into buffer BUF.
If REPLACE is non-nil then the contents of BUF will be overwritten.
When called interactively the current buffer is used, FILES are prompted for
using `syslog-get-filenames', and REPLACE is set to nil, unless
a prefix argument is used in which case they are prompted for."
  (interactive (list (syslog-get-filenames nil "Prepend log file: ")
		     (current-buffer)
		     (if current-prefix-arg
			 (y-or-n-p "Replace current buffer contents? "))))
  (with-current-buffer buf
    (let ((inhibit-read-only t))
      (set-visited-file-name nil)
      (cl-loop for file in (cl-remove-duplicates files :test 'equal)
	       do (let ((start (goto-char (point-min))))
		    (forward-char (cl-second (insert-file-contents file)))
		    (put-text-property start (point) 'syslog-filename file))))))

;; simple-call-tree-info: DONE
(defun syslog-create-buffer (filenames)
  "Create a new buffer named after the files in FILENAMES."
  (let* ((uniquefiles (mapcar 'file-name-nondirectory
			      (cl-remove-duplicates filenames :test 'equal)))
	 (basenames (mapcar (lambda (x)
			      (replace-regexp-in-string
			       "\\(\\.gz\\|\\.tgz\\)$" ""
			       (file-name-nondirectory x)))
			    uniquefiles))
	 (basenames2 (cl-remove-duplicates
		      (mapcar (lambda (x) (replace-regexp-in-string "\\.[0-9]+$" "" x)) basenames)
		      :test 'equal)))
    (get-buffer-create
     (if (> (length filenames) 1)
	 (substring (cl-loop for file in basenames2
			     for files = (cl-remove-if-not
					  (lambda (x) (string-match-p (regexp-quote file) x))
					  basenames)
			     for nums = (mapcar (lambda (x)
						  (let* ((match (string-match "\\.\\([0-9]+\\)" x))
							 (n (if match (match-string 1 x) "0")))
						    (string-to-number n)))
						files)
			     for min = (if nums (apply 'min nums) 0)
			     for max = (if nums (apply 'max nums) 0)
			     concat (concat file "." (if (= min max) (number-to-string min)
						       (concat "{" (number-to-string min)
							       "-" (number-to-string max) "}"))
					    ","))
		    0 -1)
       (car basenames)))))

;; simple-call-tree-info: DONE
(defun syslog-open-files (files &optional label)
  "Insert log FILES into new buffer, and switch to that buffer.
If the optional argument LABEL is non-nil then each new line will be labelled
with the corresponding filename.
When called interactively the FILES are prompted for using `syslog-get-filenames'."
  (interactive (list (syslog-get-filenames nil "View log file: ")
		     (y-or-n-p "Label lines with filenames? ")))
  (let ((buf (syslog-create-buffer files)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
	(set-visited-file-name nil)
	(cl-loop for file in (cl-remove-duplicates files :test 'equal)
		 do (let ((start (goto-char (point-max))))
		      (insert-file-contents file)
		      (goto-char (point-max))
		      (unless (not label)
			(forward-line 0)
			(goto-char
			 (apply-on-rectangle
			  'string-rectangle-line start (point)
			  (concat (file-name-nondirectory file) ": ") nil)))
		      (put-text-property
		       start (point) 'syslog-filename file))))
      (syslog-mode)
      (setq default-directory (file-name-directory (car files))))
    (switch-to-buffer buf)))

;;;###autoload
;; simple-call-tree-info: CHECK
(defun syslog-view (files &optional label treatments rxshowstart rxshowend
			  rxhidestart rxhideend startdate enddate removedates
			  highlights bufname)
  "Open a view of syslog files with optional filters and highlights applied.
When called interactively the user is prompted for a member of `syslog-views' and the
arguments are determined from the chosen member.
FILES can be either nil in which case the view is applied to the current log file, or
it can be the same as the first argument to `syslog-get-filenames' - a list of cons
cells whose cars are filenames and whose cdrs indicate how many logfiles to include.
LABEL indicates whether or not to label each line with the filename it came from.
RXSHOWSTART, RXSHOWEND and RXHIDESTART, RXHIDEEND are optional regexps which will be 
used to filter in/out blocks of buffer lines with `syslog-filter-lines'. 
STARTDATE and ENDDATE are optional dates used to filter the lines with `syslog-filter-dates'; 
they can be either date strings or time lists as returned by `syslog-date-to-time'.
HIGHLIGHTS is a list of cons cells whose cars are regexps and whose cdrs are faces to 
highlight those regexps with."
  (interactive (cdr (cl-assoc (ido-completing-read "View: " (mapcar 'car syslog-views))
			      syslog-views :test 'string=)))
  (cl-flet ((getstr (str) (and (not (string= str "")) str)))
    (let ((rxshowstart (getstr rxshowstart))
	  (rxshowend (getstr rxshowend))
	  (rxhidestart (getstr rxhidestart))
	  (rxhideend (getstr rxhideend))
	  (startdate (getstr startdate))
	  (enddate (getstr enddate))
	  (bufname (getstr bufname))) 
      (when files (syslog-open-files (syslog-get-filenames files) label))
      (if (not (eq major-mode 'syslog-mode))
	  (error "Not in syslog-mode")
	(dolist (trt treatments)
	  (cond ((and (consp trt)
		      (functionp (car trt)))
		 (let* ((args1 (cdadr (interactive-form (car trt))))
			(args2 (cdr trt))
			(args3 (cl-loop
				for i from 0 to (1- (max (length args1) (length args2)))
				for arg1 = (nth i args1)
				for arg2 = (eval (nth i args2))
				collect (if (eq arg2 'interactive)
					    (eval arg1)
					  arg2))))
		   (apply (car trt) args3)))
		((and (consp trt)
		      (stringp (car trt)))
		 (save-excursion
		   (goto-char (point-min))
		   (while (re-search-forward (car trt) nil t)
		     (replace-match (if (functionp (cdr trt))
					(funcall (cdr trt) (match-string 1))
				      (cdr trt))
				    t nil nil 1))))
		(t (error "Invalid treatments arg"))))
	(if rxshowstart
	    (if rxshowend
		(hide-blocks-not-matching rxshowstart rxshowend)
	      (hide-lines-not-matching rxshowstart)))
	(if rxhidestart
	    (if rxhideend
		(hide-blocks-not-matching rxhidestart rxhideend)
	      (hide-lines-matching rxhidestart)))
	(if (or startdate enddate)
	    (syslog-filter-dates startdate enddate removedates))
	(if highlights
	    (cl-loop for hl in highlights
		     for (regex . face) = hl
		     do (highlight-regexp regex face)))
	(if bufname (rename-buffer bufname t))))))

;; simple-call-tree-info: DONE
(defun syslog-previous-file (&optional arg)
  "Open the previous logfile backup, or the next one if a prefix arg is used.
Unix systems keep backups of log files with numbered suffixes, e.g. syslog.1 syslog.2.gz, etc.
where higher numbers indicate older log files.
This function will load the previous log file to the current one (if it exists), or the next
one if ARG is non-nil."
  (interactive "P")
  (let* ((pair (syslog-get-basename-and-number
		(syslog-get-filename-at-point)))
         (basename (car pair))
         (curver (cdr pair))
         (nextver (if arg (1- curver) (1+ curver)))
         (nextfile (if (> nextver (1- syslog-number-suffix-start))
                       (concat basename "." (number-to-string nextver))
                     basename)))
    (let ((inhibit-read-only t))
      (cond ((file-readable-p nextfile)
	     (find-file nextfile))
	    ((file-readable-p (concat nextfile ".bz2"))
	     (find-file (concat nextfile ".bz2")))
	    ((file-readable-p (concat nextfile ".gz"))
	     (find-file (concat nextfile ".gz")))
	    ((file-readable-p (concat nextfile ".tgz"))
	     (find-file (concat nextfile ".tgz"))))
      (put-text-property (point-min) (point-max) 'syslog-filename nextfile))))

;; simple-call-tree-info: DONE
(defun syslog-next-file nil
  "Open the next logfile.
This just calls `syslog-previous-file' with non-nil argument, so we can bind it to a key."
  (interactive)
  (syslog-previous-file t))

;; simple-call-tree-info: DONE
(defun syslog-move-next-file (&optional arg)
  "Move to the next file in the current `syslog-mode' buffer.
If ARG is non-nil (or called with numeric prefix arg), move that many
files forward."
  (interactive "p")
  (cl-loop for i from 1 to arg
	   do (goto-char (next-single-property-change
			  (point) 'syslog-filename nil (point-max)))))

;; simple-call-tree-info: DONE
(defun syslog-move-previous-file (&optional arg)
  "Move to the next file in the current `syslog-mode' buffer.
If ARG is non-nil (or called with numeric prefix arg), move that many
files forward."
  (interactive "p")
  (cl-loop for i from 1 to arg
	   do (goto-char (previous-single-property-change
			  (point) 'syslog-filename nil (point-min)))))

;; simple-call-tree-info: DONE
(defun syslog-get-filename-at-point nil
  "Get the filename associated with the line at point."
  (or (get-text-property (point) 'syslog-filename)
      buffer-file-name))

;; simple-call-tree-info: DONE
(defun syslog-toggle-filenames (&optional arg)
  "Toggle the display of filenames before each line.
If prefix ARG is positive display filenames, and if its negative hide them,
otherwise toggle them."
  (interactive "P")
  (save-excursion
    (ov-set (ov-in) 'invisible nil)
    (let* ((start (goto-char (point-min)))
	   (filename (syslog-get-filename-at-point))
	   (fileshownp (and filename
			    (looking-at
			     (concat "^" (regexp-quote (file-name-nondirectory filename))
				     ": "))))
	   (hidep (if arg (prefix-numeric-value arg) 0)))
      (let ((inhibit-read-only t))
	(while (and (goto-char
		     (next-single-property-change
		      (point) 'syslog-filename nil (point-max)))
		    (/= start (point)))
	  (if fileshownp
	      (if (<= hidep 0)
		  (apply-on-rectangle
		   'delete-rectangle-line
		   start (+ (line-beginning-position 0)
			    (length (match-string 0)))
		   nil))
	    (unless (< hidep 0)
	      (apply-on-rectangle
	       'string-rectangle-line start
	       (line-beginning-position 0)
	       (concat (file-name-nondirectory filename) ": ")
	       nil)
	      (put-text-property start (point) 'syslog-filename filename)))
	  (setq start (point)
		filename (syslog-get-filename-at-point)
		fileshownp (and filename
				(looking-at
				 (concat "^" (regexp-quote (file-name-nondirectory filename))
					 ": ")))))))
    (ov-set (ov-in) 'invisible 'hl)))

;;;###autoload
;; simple-call-tree-info: DONE
(defun syslog-filter-lines (&optional arg)
  "Restrict buffer to blocks of text between matching regexps.
If the user only enters one regexp then just filter matching lines instead of blocks.
With prefix ARG: remove matching blocks."
  (interactive "p")
  (let* ((str (if (> arg 1) "to remove" "to keep"))
	 (startregex (read-regexp
		      (format "Regexp matching start lines of blocks %s" str)
		      (symbol-name (symbol-at-point))))
	 (endregex (read-regexp
		    (format "Regexp matching end lines of blocks %s (default=filter start lines only)" str)))
	 (n (length (overlays-in (point-min) (point-max)))))
    (unless (string= startregex "")
      (if (> arg 1)
	  (if (string= endregex "")
	      (hide-lines-matching startregex)
	    (hide-blocks-matching startregex endregex))
	(if (string= endregex "")
	    (hide-lines-not-matching startregex)
	  (hide-blocks-not-matching startregex endregex)))
      (if (= n (length (overlays-in (point-min) (point-max))))
	  (message "No matches found")))))

;; simple-call-tree-info: DONE
(defun highlight-regexp-unique (regexp &optional faces)
  "Highlight each unique string matched by REGEXP with a different face.
Interactively, prompt for REGEXP using `read-regexp', and prompt for a
set of FACES to use for highlighting (see `syslog-hi-face-defaults').
When called non-interactively, FACES can be either a symbol (the car of one of the 
face sets defined in `syslog-hi-face-defaults'), or a list of faces.

If REGEXP contains non-shy match groups, then only those parts of the match will 
be treated as unique strings & highlighted (rather than the whole regexp).
In this case overlays will always be used (which can be slow if there are many matches).
If there are no non-shy match groups, and variable `font-lock-mode' is enabled then 
that will be used for doing the highlighting."
  (interactive
   (list (read-regexp "Regexp to highlight" 'regexp-history-last)
	 (intern-soft (completing-read "Highlight type: "
				       (mapcar 'car syslog-hi-face-defaults)))))
  (hi-lock-regexp-okay regexp)
  (when (stringp faces) (hi-lock-regexp-okay faces))
  (unless hi-lock-mode (hi-lock-mode 1))
  (let* ((faces (cond ((functionp faces) (funcall faces))
		      ((symbolp faces)
		       (let ((f (cdr (assoc faces syslog-hi-face-defaults))))
			 (if (functionp f) (funcall f) f)))
		      ((listp faces) faces)
		      (t (error "Invalid FACES arg: %S" faces))))
	 (unused-faces (set-difference faces 
				       (mapcar (lambda (p) (eval (cadadr p)))
					       hi-lock-interactive-patterns)))
	 (matchrx "[^(]*\\\\(\\(.*?\\)\\\\)")
	 (matches (syslog-unique-matches regexp))
	 (nfaces (length unused-faces))
	 (nmatches (length matches)))
    (when (< nfaces nmatches)
      (warn "Not enough unused faces (%s) to cover all matches (%s). 
 %s matches will not be coloured."
	    nfaces nmatches (- nmatches nfaces)))
    (cl-flet ((repmatch (i) (let (ss)
			      (while (> i 0)
				(setq ss (cons matchrx ss))
				(setq i (1- i)))
			      (apply 'concat ss))))
      (cl-loop for pair in matches
	       for face in unused-faces
	       do (let* ((match (car pair))
			 (i (cadr pair)))
		    (hi-lock-set-subpattern
		     (if (> i 0)
			 (replace-regexp-in-string
			  (if (> i 1) (repmatch i) matchrx)
			  (regexp-quote match) regexp t t i)
		       (regexp-quote match))
		     face i))))))

;; simple-call-tree-info: DONE (tweaked version of `hi-lock-set-pattern')
(defun hi-lock-set-subpattern (regexp face subx)
  "Highlight the SUBX match group of REGEXP with face FACE."
  ;; Hashcons the regexp, so it can be passed to remove-overlays later.
  (setq regexp (hi-lock--hashcons regexp))
  (let ((pattern (list regexp (list 0 (list 'quote face) 'prepend))))
    ;; Refuse to highlight a text that is already highlighted.
    (unless (assoc regexp hi-lock-interactive-patterns)
      (push pattern hi-lock-interactive-patterns)
      (if (and font-lock-mode (font-lock-specified-p major-mode)
	       (= subx 0))
	  (progn
	    (font-lock-add-keywords nil (list pattern) t)
	    (font-lock-flush))
        (let* ((range-min (- (point) (/ hi-lock-highlight-range 2)))
               (range-max (+ (point) (/ hi-lock-highlight-range 2)))
               (search-start
                (max (point-min)
                     (- range-min (max 0 (- range-max (point-max))))))
               (search-end
                (min (point-max)
                     (+ range-max (max 0 (- (point-min) range-min))))))
          (save-excursion
            (goto-char search-start)
            (while (re-search-forward regexp search-end t)
              (let ((overlay (make-overlay (match-beginning subx) (match-end subx))))
                (overlay-put overlay 'hi-lock-overlay t)
                (overlay-put overlay 'hi-lock-overlay-regexp regexp)
                (overlay-put overlay 'face face))
              (goto-char (match-end 0)))))))))

;;;###autoload
;; simple-call-tree-info: DONE
(defcustom syslog-views nil
  "A list of views.
Each view is a list of:
 - a name for the view
 - a list of files to display; each item in the list is a cons cell whose car is the base log file, 
     and whose cdr is a number indicating how many previous log files of the same type to include.
     If nil then the view will be applied to the currently displayed file.
 - a boolean indicating whether or not to label each line with the filename
 - an optional list of functions to apply to transform the buffer before filtering & highlighting. 
     Each element is either:
     - a list whose car is a function and whose cdr is a list of arguments for the function. The arglist
       may contain the symbol 'interactive which means the value will be prompted for when the view is invoked.
     - a cons cell whose car is a regexp containing a match group, and whose cdr is either a replacement
       string, or a function that takes the text captured by that match group as its only arg, and returns 
       some text to replace it. This function/string will be used for replacing all matches in the buffer.
 - a regexp matching start lines of blocks to show
 - a regexp matching end lines of blocks to show (if blank then lines will be filtered instead of blocks)
 - a regexp matching start lines of blocks to hide
 - a regexp matching end lines of blocks to hide (if blank then lines will be hidden instead of blocks)
 - an optional start date for filtering lines with `syslog-filter-dates'
 - an optional end date for filtering lines with `syslog-filter-dates'
 - a boolean; if non-nil hide lines matching above dates, otherwise display only those lines
 - a list of highlighting info; each element is a cons cell whose car is a regexp to highlight and 
   whose cdr is a face to use for highlighting
 - an optional name to rename the buffer"
  :group 'syslog
  :type '(repeat (list (string :tag "Name")
		       (repeat :tag "File(s)"
			       (cons (string :tag "Base file")
				     (number :tag "Number of previous files/days")))
		       (choice (const :tag "No file labels" nil)
			       (const :tag "Add file labels" t))
		       (repeat :tag "Treatment(s)"
			       (choice (cons :tag "Apply function"
					     (function :tag "Function")
					     (repeat
					      :tag "Args"
					      (choice :tag "Arg"
						      (sexp)
						      (const :tag "Prompt user when view is invoked"
							     'interactive))))
				       (cons :tag "Replace string"
					     (regexp
					      :help-echo "Regexp containing a match group"
					      :validate
					      (lambda (w)
						(let ((v (widget-value w)))
						  (when (< (regexp-opt-depth v) 1)
						    (widget-put
						     w :error
						     "Regexp must have at least one match group")
						    w))))
					     (choice (function
						      :help-echo
						      "Function of one argument (a string captured by regexp match group)")
						     (string :help-echo "Replacement string")))))
		       (regexp :tag "Regexp matching start lines of blocks to show")
		       (regexp :tag "Regexp matching end lines of blocks to show")
		       (regexp :tag "Regexp matching start lines of blocks to hide")
		       (regexp :tag "Regexp matching end lines of blocks to hide")
		       (string :tag "Start date")
		       (string :tag "End date")
		       (choice (const :tag "Keep matching dates" nil)
			       (const :tag "Remove matching dates" t))
		       (repeat :tag "Highlights"
			       (cons (regexp :tag "Regexp to highlight")
				     (face :tag "Face")))
		       (string :tag "Buffer name"))))

;; simple-call-tree-info: DONE
(defcustom syslog-datetime-regexp
  "^\\(?:[^ :]+: \\)?\\(\\(?:\\(?:[[:alpha:]]\\{3\\}\\)?[[:space:]]*[[:alpha:]]\\{3\\}\\s-+[0-9]+\\s-+[0-9:]+\\)\\|\\(?:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\s-+[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)\\)"
  "A regular expression matching the date-time at the beginning of each line in the log file.
It should contain one non-shy subexpression matching the datetime string."
  :group 'syslog
  :type 'regexp)

;; simple-call-tree-info: DONE
(defcustom syslog-log-file-directory "/var/log/"
  "The directory in which log files are stored."
  :group 'syslog
  :type 'directory)

(defcustom syslog-large-file-size 500000
  "When `syslog-show-note-from-file-or-buffer' tries to load a file larger than this it prompts the user."
  :group 'syslog
  :type 'integer)

;; simple-call-tree-info: CHECK
(cl-defun syslog-spaced-floats (n &optional (min 0.0) (max 1.0))
  "Return list of floats optimally circularly spaced between MIN and MAX.
The results are equivalent to sequentially placing points on a circle using
the golden section (which gives an optimal spacing according Tony van Ravenstein), 
and then projecting them onto the range [MIN MAX)."
  (cl-loop with range = (- max min)
	   for i from 0 to (1- n)
	   collect (+ min (mod (* i 0.61803398875 range)
			       range))))

;; simple-call-tree-info: CHECK
(cl-defmacro syslog-spaced-colours (n &key (h 180) (s 100) (l 50))
  "Return list of N optimally spaced hex colour strings.
Keyword args :H, :S & :L indicate the required hue, saturation & lightness 
values respectively (see https://www.hsluv.org). Each can be either a single 
positive number, or an unquoted list of two numbers indicating a range from 
which to select the N values (using `syslog-spaced-floats'). If more than one 
of them is a range, then the values will be selected in parallel."
  (cl-flet ((getlst (v) (cond ((integerp v) `(make-list ,n ,v))
			      ((and (consp v) (integerp (car v)) (integerp (cadr v)))
			       `(syslog-spaced-floats ,n ,(car v) ,(cadr v)))
			      (t (error "Invalid arg: %S" v)))))
    `(cl-mapcar (lambda (h s l) (hsluv-hsluv-to-hex (list h s l)))
		,(getlst h) ,(getlst s) ,(getlst l))))

;; simple-call-tree-info: CHECK
(defcustom syslog-hi-face-defaults
  (list
   '(bg . (lambda nil (mapcar (lambda (h) (list :background h))
			      (syslog-spaced-colours 64 :h (0 360)))))
   '(fg . (lambda nil (mapcar (lambda (h) (list :foreground h))
			      (syslog-spaced-colours 64 :h (0 360)))))
   '(light_bg . (lambda nil (mapcar (lambda (h) (list :background h :foreground "black"))
				    (syslog-spaced-colours 64 :h (0 360) :l 85))))
   '(dark_bg . (lambda nil (mapcar (lambda (h) (list :background h :foreground "white"))
				   (syslog-spaced-colours 64 :h (0 360) :l 15))))
   '(light_fg . (lambda nil (mapcar (lambda (h) (list :foreground h))
				    (syslog-spaced-colours 64 :h (0 360) :l 85))))
   '(dark_fg . (lambda nil (mapcar (lambda (h) (list :foreground h))
				   (syslog-spaced-colours 64 :h (0 360) :l 15))))
   '(blue_bg . (lambda nil (mapcar (lambda (h) (list :background h))
				   (syslog-spaced-colours 64 :h (200 275) :l (70 5)))))
   '(blue_fg . (lambda nil (mapcar (lambda (h) (list :foreground h))
				   (syslog-spaced-colours 64 :h (200 275) :l (70 5)))))
   '(red_bg . (lambda nil (mapcar (lambda (h) (list :background h))
				  (syslog-spaced-colours 64 :h (-15 15) :l (10 60)))))
   '(red_fg . (lambda nil (mapcar (lambda (h) (list :foreground h))
				  (syslog-spaced-colours 64 :h (-15 15) :l (10 60)))))
   '(green_bg . (lambda nil (mapcar (lambda (h) (list :background h))
				    (syslog-spaced-colours 64 :h (90 165) :l (70 10)))))
   '(green_fg . (lambda nil (mapcar (lambda (h) (list :foreground h))
				    (syslog-spaced-colours 64 :h (90 165) :l (70 10))))))
  
  "Alist of face sets to use for automatic highlighting.
The car of each set is a symbol naming the set, and the cdr is either a list of faces,
or a function which returns such a list."
  :group 'syslog
  :type '(repeat (string :tag "Face")))

;;;###autoload
;; simple-call-tree-info: DONE
(cl-defun syslog-date-to-time (date &optional safe)
  "Convert DATE string to time.
If no year is present in the date then the current year is used.
If DATE can't be parsed then if SAFE is non-nil return nil otherwise throw an error."
  (if safe
      (let ((time (safe-date-to-time (concat date " " (substring (current-time-string) -4)))))
	(if (and (= (car time) 0) (= (cdr time) 0))
	    nil
	  time))
    (date-to-time (concat date " " (substring (current-time-string) -4)))))

;;;###autoload
;; simple-call-tree-info: DONE
(defun syslog-filter-dates (start end &optional arg)
  "Restrict buffer to lines between times START and END (Emacs time lists).
With prefix ARG: remove lines between dates.
If either START or END are nil then treat them as the first/last time in the
buffer respectively."
  (interactive (let (firstdate lastdate)
                 (save-excursion
                   (goto-char (point-min))
                   (beginning-of-line)
                   (re-search-forward syslog-datetime-regexp nil t)
                   (setq firstdate (match-string 1))
                   (goto-char (point-max))
                   (beginning-of-line)
                   (re-search-backward syslog-datetime-regexp nil t)
                   (setq lastdate (match-string 1)))
                 (list (syslog-date-to-time (read-string "Start date and time: "
                                                         firstdate nil firstdate))
                       (syslog-date-to-time (read-string "End date and time: "
                                                         lastdate nil lastdate))
		       current-prefix-arg)))
  (let ((start (if (stringp start)
		   (syslog-date-to-time start)
		 start))
	(end (if (stringp end)
		 (syslog-date-to-time end)
	       end)))
    (set (make-local-variable 'line-move-ignore-invisible) t)
    (goto-char (point-min))
    (let* ((start-position (point-min))
	   (pos (re-search-forward syslog-datetime-regexp nil t))
	   (intime-p (lambda (time)
		       (let ((isin (and (or (not end) (time-less-p time end))
					(or (not start) (not (time-less-p time start))))))
			 (and time (if arg (not isin) isin)))))
	   (keeptime (funcall intime-p (syslog-date-to-time (match-string 1) t)))
	   (dodelete t))
      (while pos
	(cond ((and keeptime dodelete)
	       (hide-lines-add-overlay start-position (point-at-bol))
	       (setq dodelete nil))
	      ((not (or keeptime dodelete))
	       (setq dodelete t start-position (point-at-bol))))
	(setq pos (re-search-forward syslog-datetime-regexp nil t)
	      keeptime (funcall intime-p (syslog-date-to-time (match-string 1) t))))
      (if dodelete (hide-lines-add-overlay start-position (point-max))))))

;;;###autoload
;; simple-call-tree-info: DONE
(defun syslog-mode ()
  "Major mode for working with system logs, and strace output.

\\{syslog-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "syslog")
  (setq major-mode 'syslog-mode)
  (use-local-map syslog-mode-map)
  ;; Menu definition
  (easy-menu-define nil syslog-mode-map "test"
    `("Syslog"
      ["Quit" quit-window :help "Quit and bury this buffer" :key "q"]
      ["Revert buffer" revert-buffer :help "View the function at point" :key "R"]
      ["Show all"  hide-lines-show-all :help "Show all hidden lines/blocks" :key "g"]
      ["Filter lines..." syslog-filter-lines :help "Show/hide blocks of text between matching regexps" :key "/"]
      ["Filter dates..." syslog-filter-dates :help "Show/hide lines between start and end dates" :key "C-/"]
      ["Kill hidden" hide-lines-kill-hidden :help "Kill (with prefix delete) hidden lines" :key "k"]
      ["Jump to boot start" syslog-boot-start :help "Jump forward in the log to when the system booted" :key "<C-down>"]
      ["Open previous log file" syslog-previous-file :help "Open previous logfile backup" :key "<"]
      ["Open next log file" syslog-next-file :help "Open next logfile backup" :key ">"]
      ["Move to previous log file" syslog-move-previous-file :help "Move to previous logfile in buffer" :key "<M-up>"]
      ["Move to next log file" syslog-move-next-file :help "Move to next logfile in buffer" :key "<M-down>"]
      ["Open log files..." syslog-open-files :help "Insert log files into new buffer" :key "o"]
      ["Append files..." syslog-append-files :help "Append files into current buffer" :key "a"]
      ["Prepend files..." syslog-prepend-files :help "Prepend files into current buffer" :key "p"]
      ["Toggle filenames" syslog-toggle-filenames :help "Toggle display of filenames" :key "t"]
      ["Find file at point" ffap :help "Find file at point" :key "f"]
      ["Whois" syslog-whois-reverse-lookup :help "Perform whois lookup on hostname at point" :key "W"]
      ["Dired" (lambda nil (interactive) (dired syslog-log-file-directory)) :help "Enter logfiles directory" :keys "D"]
      ["Shell command" syslog-shell-command :help "Execute shell command (as root if prefix arg used)" :key "!"]
      ["Extract/count strings..."
       (keymap "Matching"
	       (count-matches menu-item "Count matches" syslog-count-matches
			      :help "Count strings which match the given pattern"
			      :key "c")
	       (extract-matches menu-item "Extract matches" syslog-extract-matches
				:help "Extract & concatenate strings which match the given pattern"
				:key "x")
	       (extract-pipe menu-item "Extract file descriptor" syslog-extract-fds-from-strace
			     :help "Extract all lines related to a given file descriptor(s) in an strace buffer"
			     :key "X"))]
      ["Notes..." (keymap "Notes"
			  (show-note menu-item "Show note" syslog-show-notes
				     :help "Show note for word at point"
				     :keys "?")
			  (load-notes menu-item "Load notes" syslog-load-notes
				      :help "Load notes associated with current buffer")
			  (edit-notes menu-item "Edit notes" syslog-edit-notes
				      :help "Edit notes associated with current buffer"))]
      ["Highlighting..." (keymap "Highlight"
				 (regexp menu-item "Regexp" highlight-regexp
					 :help "Highlight each match of regexp"
					 :keys "h r")
				 (regexp-unique menu-item "Regexp unique" highlight-regexp-unique
						:help "Highlight each unique match of regexp differently"
						:keys "h q")
				 (phrase menu-item "Phrase" highlight-phrase
					 :help "Highlight each match of phrase"
					 :keys "h p")
				 (lines menu-item "Lines matching regexp" highlight-lines-matching-regexp
					:help "Highlight lines containing match of regexp"
					:keys "h l")
				 (unhighlight menu-item "Unhighlight regexp" unhighlight-regexp
					      :help "Remove highlighting"
					      :keys "h u")
				 (unhighlight-all menu-item "Unhighlight all"
						  (lambda nil (interactive) (unhighlight-regexp t))
						  :help "Remove all highlighting"
						  :keys "h U"
						  ))]
      ["Open stored view..." syslog-view :help "Open a stored view of syslog files" :key "v"]
      ["Edit stored views..." (lambda nil (interactive) (customize-variable 'syslog-views)) :help "Customize `syslog-views'"]
      ["---" "---"]))
  ;; font locking
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	(if (string-match "\\..?trace$" (or buffer-file-name (buffer-name)))
	    '((syslog-strace-font-lock-keywords))
	  '(syslog-font-lock-keywords t t nil )))
  (buffer-disable-undo)
  (toggle-read-only 1)
  (syslog-load-notes)
  (setq-local syslog-note-thing
	      (or (let ((bfn (or (and buffer-file-name
				      (expand-file-name buffer-file-name))
				 (buffer-name))))
		    (cdr (cl-assoc-if (lambda (r) (string-match r bfn))
				      syslog-note-things)))
		  syslog-note-thing))
  (run-hooks 'syslog-mode-hook))

;; simple-call-tree-info: DONE
(defvar syslog-boot-start-regexp "unix: SunOS"
  "Regexp to match the first line of boot sequence.")

;; simple-call-tree-info: DONE  
(defun syslog-unique-matches (rx &optional ignorecase)
  "Find unique strings matching RX or it's non-shy match groups if it has any.
Return value is an alist whose car values are the unique matched strings,
and the cdr values are integers indicating the match group that the string
was matched with, or 0 if RX contains no non-shy match groups.
If optional argument IGNORECASE is non-nil then case will be ignored during
matching."
  (let ((ngrps (regexp-opt-depth rx))
	(case-fold-search ignorecase)
	matches)
    (cl-flet ((addmatch (m i)
			(let ((pair (assoc m matches)))
			  (if pair
			      (when (not (memq i (cdr pair)))
				(setf (cdr pair) (cons i (cdr pair))))
			    (add-to-list
			     'matches
			     (cons m (list i)))))))
      (save-excursion
	(goto-char (point-min))
	(if (> ngrps 0)
	    (while (re-search-forward rx nil t)
	      (cl-loop for i from 1
		       for match = (match-string-no-properties i)
		       if match do (addmatch match i)
		       else return nil))
	  (while (re-search-forward rx nil t)
	    (addmatch (match-string-no-properties 0) 0)))))
    matches))

;; simple-call-tree-info: DONE
(defun syslog-count-matches (rx &optional display ignorecase)
  "Count all matches to regexp RX in current buffer.
If RX contains non-shy match groups then matches to them will
be counted, otherwise matches to the whole regexp will be counted.
If DISPLAY is non-nil or if called interactively then the counts
will be displayed in the minibuffer, otherwise an alist of (string . count)
pairs will be returned.
If IGNORECASE is non-nil or when called interactively with a prefix arg
case will be ignored when searching for matches."
  (interactive (list (read-regexp
		      "Regexp: "
		      '("^[0-9]+" "pid=\"\\([^ ]+\\)\""))
		     t current-prefix-arg))
  (let ((ngrps (regexp-opt-depth rx))
	(case-fold-search ignorecase)
	matches)
    (cl-flet ((addmatch (m)
			(let ((pair (assoc m matches)))
			  (if pair
			      (setf (cdr pair) (1+ (cdr pair)))
			    (add-to-list
			     'matches
			     (cons m 1))))))
      (save-excursion
	(goto-char (point-min))
	(if (> ngrps 0)
	    (while (re-search-forward rx nil t)
	      (cl-loop for i from 1
		       for match = (match-string-no-properties i)
		       if match do (addmatch match)
		       else return nil))
	  (while (re-search-forward rx nil t)
	    (addmatch (match-string-no-properties 0))))))
    (setq matches (sort matches (lambda (a b) (> (cdr a) (cdr b)))))
    (if display
	(cl-format t "~s unique values\n~:{~a:~s ~}" (length matches) matches)
      matches)))

;; simple-call-tree-info: CHECK  
(defun syslog-extract-matches (rx &optional sep count outbuf overwrite colourise display)
  "Extract & concatenate strings matching regexp RX (or its match groups).
Separate the matches with SEP if non-nil. If COUNT is non-nil then only collect
the first COUNT matches. When called interactively the extracted strings will be printed
to the *Syslog extract* buffer, otherwise a buffer or buffer name can be supplied in OUTBUF.
If OUTBUF is nil then the extract will be returned as a string.
If OVERWRITE is non-nil then the buffer will be overwritten otherwise it will be appended to.
If COLOURISE is non-nil then the same fontification and highlighting as the original buffer
will be applied."
  (interactive (list (read-regexp "Regexp matching strings to collect: "
				  '("\"\\([^\"]*\\)\""))
 		     (read-string "Separator (for newline enter C-q C-j): ")
		     (when current-prefix-arg
		       (prefix-numeric-value current-prefix-arg))
		     (get-buffer-create "*Syslog extract*")
		     (when (with-current-buffer (get-buffer "*Syslog extract*")
			     (goto-char (point-min))
			     (re-search-forward "\\S-" nil t))
		       (y-or-n-p "Overwrite existing text in *Syslog extract* buffer?"))
		     (y-or-n-p "Copy fontification and highlighting?")
		     t))
  (let ((ngrps (regexp-opt-depth rx))
	(fld font-lock-defaults)
	(hlip hi-lock-interactive-patterns)
	str)
    (cl-flet ((addmatch (m) (setq str (concat str sep m))
			(when count (setq count (1- count)))))
      (save-excursion
	(if (> ngrps 0)
	    (while (and (re-search-forward rx nil t)
			(or (not count) (> count 0)))
	      (cl-loop for i from 1
		       for match = (match-string-no-properties i)
		       if match do (addmatch match)
		       else return nil))
	  (while (and (re-search-forward rx nil t)
		      (or (not count) (> count 0)))
	    (addmatch (match-string-no-properties 0))))))
    (if (not outbuf)
	str
      (with-current-buffer outbuf
	(when overwrite
	  (delete-region (point-min) (point-max)))
	(goto-char (point-max))
	(insert "\n" str)
	(when colourise
	  (setq font-lock-defaults fld)
	  (font-lock-ensure)
	  (hi-lock-mode 1)
	  (dolist (pat hlip)
	    (hi-lock-set-pattern (car pat)
				 (eval (second (cadr pat)))))))
      (when display (display-buffer outbuf)))))

;; simple-call-tree-info: DONE
(defun syslog-boot-start ()
  "Jump forward in the log to when the system booted."
  (interactive)
  (search-forward-regexp syslog-boot-start-regexp (point-max) t)
  (beginning-of-line))

;; simple-call-tree-info: DONE
(defun syslog-whois-reverse-lookup (arg search-string)
  "This is a wrapper around the `whois' command using symbol at point as default search string.
Also `whois-server-name' is set to `whois-reverse-lookup-server'.
The ARG and SEARCH-STRING arguments are the same as for `whois'."
  (interactive (list current-prefix-arg
		     (let* ((symb (symbol-at-point))
			    (default (replace-regexp-in-string ":[0-9]+$" "" (symbol-name symb))))
		       (read-string (if symb (concat "Whois (default " default "): ")
				      "Whois: ") nil nil default))))
  (let ((whois-server-name whois-reverse-lookup-server))
    (whois arg search-string)))

;; simple-call-tree-info: DONE
(cl-defun syslog-lsof (&optional (pids nil) (filtercmd nil)
				 (sudop nil) (flags "-n -P -L -E"))
  "Return list of strings containing lines of output from lsof command.
PIDS is either a list of integers, or a single string which will be passed to
the -p option to lsof to limit the output to certain process IDs (if nil then no such 
limiting takes place).
FILTERCMD is an optional shell command to process the output of lsof (e.g. \"grep ' FIFO '\").
SUDOP indicates whether to run lsof under sudo or not.
FLAGS is a list of flags for lsof, and is set to \"-n -P -L -E\" by default."
  (interactive (list (read-string "Comma separated list of PIDs &/or their negations (e.g. \"123,^456\"): ")
		     (read-string "Optional filter command (e.g. \"grep ' FIFO '\"): "
				  nil nil '("grep ' FIFO '"))
		     current-prefix-arg
		     (read-string "Flags for lsof: " "-n -P -L -E")))
  (let ((output (syslog-shell-command (concat "lsof " flags 
					      (when pids
						(concat " -p"
							(cond
							 ((stringp pids) pids)
							 ((listp pids)
							  (mapconcat 'number-to-string pids ","))
							 (t (error "Invalid PIDS arg")))))
					      (when (and filtercmd (> (length filtercmd) 0))
						(concat " | " filtercmd)))
				      sudop (not (called-interactively-p 'any)))))
    (when (called-interactively-p 'any)
      (with-current-buffer "*Shell Command Output*"
	(rename-buffer "*lsof output*" t)))
    output))

;; simple-call-tree-info: CHECK
(defun syslog-lsof-get-pipes (lsof &optional checkhdr)
  "Return info about unix pipes from LSOF output.
Pipe info will be extracted from the buffer, file or lines supplied the LSOF arg.
The return value is a list of lists, each containing info about a particular pipe.
The first element of each list is a string containing the inode for the pipe, 
and subsequent elements contain pipe endpoint info as returned by the lsof command.
See the lsof manpage for more info."
  (let* ((lines (cl-remove-if-not
		 (lambda (l) (string-match " FIFO " l))
		 (let* ((lns (if (listp lsof)
				 lsof
			       (split-string
				(with-current-buffer
				    (if (stringp lsof)
					(find-file-noselect lsof)
				      lsof)
				  (buffer-substring-no-properties
				   (point-min) (point-max)))
				"\n" t)))
			(ln1 (split-string (car lns) " " t)))
		   ;; check that lsof output has required columns
		   (when checkhdr
		     (unless (and (string= (nth 7 ln1) "NODE")
				  (string= (nth 8 ln1) "NAME"))
		       (error "Invalid format for lsof output")))
		   lns)))
	 (pipes (mapcar (lambda (l)
			  (let ((parts (split-string l "\s+")))
			    (cons (nth 7 parts) (nthcdr 9 parts)))) 
			lines))
	 pipes2)
    (cl-loop for p in pipes
	     for elem = (assoc (car p) pipes2)
	     for lst = (cdr elem)
	     if elem do (progn (dolist (p2 (cdr p))
				 (add-to-list 'lst p2))
			       (setcdr elem lst))
	     else do (add-to-list 'pipes2 p))
    pipes2))

;; simple-call-tree-info: DONE
(defun syslog-pid-to-comm (pid &optional lsof)
  "Return name of process associated with PID.
If LSOF is nil (default) the ps shell command will be used to find the process name,
which means it should be currently running. Otherwise LSOF should be either the path to 
a file or a buffer containing lsof output, or a list of lines of lsof output as returned 
by `syslog-lsof'. This lsof output will be used to find the process name.
If the process name cannot be determined then the pid will be returned as a string."
  (let* ((pid (cond ((integerp pid) (number-to-string pid))
		    ((stringp pid) pid)
		    (t (error "Invalid pid arg"))))
	 (pidrx (concat "^\\(\\S-+\\)\\s-+\\(" pid "\\)\\s-")))
    (cond ((or (bufferp lsof) (stringp lsof))
	   (with-current-buffer
	       (if (stringp lsof)
		   (find-file-noselect lsof)
		 lsof)
	     (save-excursion
	       (goto-char (point-min))
	       (if (re-search-forward pidrx nil t)
		   (match-string 1)
		 pid))))
	  ((null lsof)
	   (let ((str (shell-command-to-string
		       (concat "ps -p " pid " -o comm --no-headers"))))
	     (if (> (length str) 0)
		 (substring-no-properties str 0 -1)
	       pid)))
	  ((listp lsof)
	   (or (cl-loop for str in lsof
			if (string-match pidrx str)
			return (match-string 1 str))
	       pid)))))

;; simple-call-tree-info: CHECK
(defmacro syslog-alter-buffer (&rest body)
  "Execute BODY with `buffer-read-only' disabled then restore to its previous value.
If an error occurs while executing BODY then `buffer-read-only' will be restored to 
it's previous value. Position of point will also be restored."
  (declare (debug (body)) (indent 1))
  (let ((ro (gensym)))
    `(let ((,ro buffer-read-only))
       (save-excursion
	 (condition-case err
	     (progn (setq buffer-read-only nil)
		    ,@body
		    (setq buffer-read-only ,ro))
	   (error (setq buffer-read-only ,ro)
		  (error "%s: %s" (car err) (cdr err))))))))

;; simple-call-tree-info: CHECK
(cl-defun syslog-replace-pids (pidrx &optional lsof)
  "Replace PIDs with process names in buffer, and return the list of process names.
PIDRX should be a regexp matching the pids or containing a non-shy match group for 
matching pids. The LSOF arg is interpreted in the same way as `syslog-pid-to-comm'.

When called interactively with no prefix arg, a file containing lsof output will be
prompted for, with a single prefix arg a buffer will be prompted for, otherwise the
ps shell command will be used to find process names.

Note: if PIDRX doesn't contain any non-shy group then it will be used for finding pids,
but any string matching the discovered pids (even in different locations to the match)
will be replaced. If you only want to do the replacements in the positions matched by 
PIDRX use a non-shy group to surround the pid."
  (interactive (list (read-regexp
		      "Regexp with match group for PID"
		      '("^[0-9]+" "pid=\\([0-9]+\\)"))
		     (cond ((equal current-prefix-arg '(4))
			    (get-buffer
			     (read-buffer "Buffer containing lsof output: " nil t)))
			   (current-prefix-arg nil)
			   (t (read-file-name "File containing lsof output: ")))))
  (let ((pids (mapcar 'car (syslog-unique-matches pidrx)))
	names)
    (syslog-alter-buffer
     (setq names
	   (mapcar (lambda (p)
		     (goto-char (point-min))
		     (let ((rx (if (< (regexp-opt-depth pidrx) 1)
				   p
				 (replace-regexp-in-string "\\\\(.*?\\\\)" p pidrx)))
			   (name (syslog-pid-to-comm p lsof)))
		       (replace-regexp rx name)
		       name))
		   pids)))
    names))

;; simple-call-tree-info: CHECK
(cl-defun syslog-replace-pipes (piperx lsof)
  "Replace pipe inode numbers with info about processes using the info obtained from LSOF.
PIPERX should be a regular expression containing a single non-shy match group for matching
the inode numbers of the pipes (the match group should ONLY match the inode number). 
The LSOF arg can be a file, buffer or list of strings containing output from lsof.

When called interactively with no prefix arg a file containing lsof output will be prompted for, 
with a prefix arg a buffer containing lsof output will be prompted for."
  (interactive (list (read-regexp "Regexp containing a non-shy match group"
				  '("pipe:\\[\\([0-9]+\\)\\]"))
		     (if current-prefix-arg
			 (read-buffer "Buffer containing lsof output: " nil t)
		       (read-file-name "File containing lsof output: "))))
  (when (< (regexp-opt-depth piperx) 1)
    (error "PIPERX arg must contain a non-shy match group"))
  (syslog-alter-buffer
   (dolist (pipe (syslog-lsof-get-pipes lsof))
     (let ((rx (replace-regexp-in-string
		"\\\\([^()]+\\\\)"
		(concat "\\(" (car pipe) "\\)") piperx t t))
	   (str (mapconcat (lambda (x)
			     (replace-regexp-in-string "[0-9]+," "" x))
			   (cdr pipe) ":")))
       (goto-char (point-min))
       (while (re-search-forward rx nil t)
	 (replace-match str t t nil 1))))))

;; simple-call-tree-info: CHECK
(defun syslog-transform-strace (lsof &optional faces)
  "Transform strace output in the current buffer.
This is a wrapper for `syslog-replace-pids' & `syslog-replace-pipes':
pids will be replaced with process names, and pipe inode numbers
will be replaced with comma separated lists of file descriptors
and associated processes connected to the pipe.

LSOF can be a file, buffer or list of strings containing lsof output.
If nil then lsof will be invoked with a list of pids extracted from the
beginning of lines in current buffer.
When called interactively with no prefix arg a file will be prompted for,
with a single prefix arg a buffer will be prompted for, and any other prefix
arg will invoke a call to the lsof shell command.

The FACES arg is the same as for `highlight-regexp-unique' (which see)."
  (interactive (list (cond ((equal current-prefix-arg '(4))
			    (get-buffer
			     (read-buffer "Buffer containing lsof output: "
					  nil t)))
			   (current-prefix-arg nil)
			   (t (read-file-name "File containing lsof output: ")))
		     (intern-soft
		      (completing-read "Highlight type: "
				       (mapcar 'car syslog-hi-face-defaults)))))
  (let ((lsof (or lsof
		  (syslog-lsof (mapconcat 'car
					  (syslog-unique-matches "^[0-9]+")
					  ","))
		  (error "No output from lsof command"))))
    (syslog-replace-pipes "pipe:\\[\\([0-9]+\\)\\]" lsof)
    (highlight-regexp-unique
     (let ((arg (mapconcat 'identity (syslog-replace-pids "^[0-9]+" lsof) "\\|")))
       (if (> (length arg) 0)
	   arg
	 (error "No pids found in buffer")))
     faces)))

;; simple-call-tree-info: CHECK
(defun syslog-extract-fds-from-strace (fds &optional copyhl display bufname)
  "Extract strace output lines involving a particular file descriptor(s).
FDS can be a string containing the name of the file descriptor enclosed in
angle brackets as it appears in strace output, e.g \"<pipe:[123456]>\",
\"</dev/pts/5>\", or a list of such strings. When called interactively
a single file descriptor will be prompted for using `ido-completing-read'.
If a prefix arg is used then `ido-completing-read-multiple' will be used
to select multiple file descriptors.
Note: if the strace buffer had been processed by `syslog-replace-pipes'
a file descriptor string could be like \"<pipe:[proc1,3r:proc3,4w]>\").

By default the lines will be copied to a new buffer named like \"strace:NAME\",
where NAME is the name of the last element of FDS when it is a list.
When called from code (non-interactively) the BUFNAME argument can be 
supplied to name the buffer.
If COPYHL is non-nil then any highlighting added by the user in the
current buffer will be copied over (font-locking is always applied).

When called interactively, or if DISPLAY is non-nil the resulting buffer
will be displayed."
  (interactive (list
		(funcall
		 (if current-prefix-arg
		     'ido-completing-read-multiple
		   'ido-completing-read)
		 "Select pipe: "
		 (or (syslog-unique-matches
		      "<\\(?:pipe:\\[\\(?:[0-9]\\{3,\\}\\|\\(?::?[^][,]+,[0-9]+[rw]\\)+\\)\\]\\|[a-z0-9/]+\\)>")
		     (error "No pipe references found in current buffer"))
		 nil t)
		(y-or-n-p "Copy highlighting? ")
		t))
  (when (or (string-match "\\.s?trace$" (or buffer-file-name (buffer-name)))
	    (y-or-n-p "This does not appear to be an strace buffer. Continue? "))
    (let* ((fld font-lock-defaults)
	   (hlip hi-lock-interactive-patterns)
	   (fdsrx (funcall (if (listp fds) 'regexp-opt 'regexp-quote) fds))
	   (fullrx (concat "^\\(?1:\\S-+\\) +\\(?:\\(?2:[^([:space:]\n]+\\)(.*\\|<\\.\\.\\. pipe resumed>.*\\)"
			   fdsrx ".*?\\(?:<\\(?3:unfinished\\) \\.\\.\\.>$\\)?$"))
	   (bufname (or bufname
			(concat "strace:" (if (listp fds)
					      (concat (car fds) "(+"
						      (number-to-string (1- (length fds)))
						      ")")
					    fds))))
	   output unfinished)
      (unless (and (get-buffer bufname)
		   (not (y-or-n-p
			 (format "Buffer %S already exists!\nOverwrite? " bufname))))
	(save-excursion
	  (goto-char (point-min))
	  (while (re-search-forward fullrx nil t)
	    (let ((cont (match-string 3)))
	      (when cont
		(let ((resume (concat "\\(?1:" (regexp-quote (match-string 1))
				      "\\) +<\\.\\.\\. \\(?2:" (regexp-quote (match-string 2))
				      "\\) \\(?3:resumed\\)>.*")))
		  (if (string= cont "unfinished")
		      (add-to-list 'unfinished resume)
		    (cl-remove-if (lambda (str) (string= str resume)) unfinished)))))
	    (setq fullrx (concat "^\\(?:"
				 "\\(?1:\\S-+\\) +\\(?2:[^([:space:]\n]+\\)(.*"
				 fdsrx ".*?\\(?:<\\(?3:unfinished\\) \\.\\.\\.>\\)?"
				 (when (> (length unfinished) 0)
				   (concat "\\|" (mapconcat 'identity unfinished "\\|")))
				 "\\)$")
		  output (concat output (substring-no-properties (match-string 0)) "\n"))))
	(with-current-buffer (get-buffer-create bufname)
	  (read-only-mode -1)
	  (delete-region (point-min) (point-max))
	  (goto-char (point-min))
	  (insert output)
	  (syslog-mode)
	  (when copyhl
	    (hi-lock-mode 1)
	    (dolist (pat hlip)
	      (hi-lock-set-pattern (car pat)
				   (eval (second (cadr pat))))))
	  (goto-char (point-min))))
      (when display (display-buffer (get-buffer bufname))))))

;; simple-call-tree-info: CHECK
(cl-defun syslog-strace-fds-treatment (&optional (shortenpipes t)
						 (shortenptys t)
						 (shortenwspace t)
						 (alignstrings t))
  "Apply some replacements & alignment to improve readability of `syslog-extract-fds-from-strace' output.
This can be used as a treatment function in `syslog-views'.
To turn off a particular treatment set the corresponding arg to nil:

SHORTENPIPES      - replace \"<pipe:...>\" with \"<pipe>\"
SHORTENPTYS       - replace \"</dev/pts/N>\" with \"<ptyN>\", and \"</dev/ptmx>\" with \"<ptmx>\"
SHORTENWSPACE     - truncate whitespace that appears before the return values at the end of each line
ALIGNSTRINGS      - align the first strings of each line 
"
  (interactive (list (y-or-n-p "Shorten pipe descriptions? ")
		     (y-or-n-p "Shorten pty descriptions? ")
		     (y-or-n-p "Truncate whitespace at the end of lines? ")
		     (y-or-n-p "Align strings? ")))
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (when shortenpipes
      (let ((pipes (mapcar 'car (syslog-unique-matches "<pipe:\\[[^][]+\\]>"))))
	(cl-loop for i from 0 to (1- (length pipes))
		 for str = (nth i pipes)
		 do (goto-char (point-min))
		 (while (search-forward str nil t)
		   (replace-match
		    (concat "<pipe" (number-to-string i) ">"))))))
    (when shortenptys
      (goto-char (point-min))
      (while (search-forward-regexp "</dev/pts/\\([^>]+\\)>" nil t)
	(replace-match (concat "<pty\\1>")))
      (goto-char (point-min))
      (while (search-forward "</dev/ptmx>" nil t)
	(replace-match "<ptmx>")))
    (goto-char (point-min))
    (when shortenwspace
      (while (re-search-forward "\\s-+\\(= [0-9]+\\)" nil t)
	(replace-match " \\1")))
    (when alignstrings
      (align-regexp (point-min) (point-max) "\\(\\s-*\\)\"")))
  (goto-char (point-min)))

;; simple-call-tree-info: DONE  
(defcustom syslog-notes-files (let ((dir (file-name-directory
					  (or load-file-name
					      (symbol-file 'syslog-mode)))))
				(list (cons ".*\\.strace" (concat dir "strace_notes.el"))
				      (cons "^strace:" (concat dir "strace_notes.el"))
				      (cons "syslog.*" (concat dir "syslog_notes.el"))))
  "An alist used by `syslog-load-notes' for choosing a notes file to load.
The car of each element in the list is a regexp used for matching against
the file visited in the current buffer or just the buffer name if there is 
no associated file, and the cdr is a notes file to load in case of a match. 
The first matching element will be used.

The notes file should contain an s-expression setting the local value of `syslog-notes':
 (setq-local syslog-notes ...)"
  :group 'syslog
  :type '(alist :key-type (regexp :help-echo "Regexp for matching file visited by buffer")
		:value-type (file :help-echo "Syslog notes file")))

;; simple-call-tree-info: TODO
(defcustom syslog-notes-default nil
  "List of `syslog-notes' items that are always available.
These items will always be available when `syslog-show-notes' is called,
no matter what buffer is current. Generally these should be items with
symbols for their LINERX entry so that they are only offered when
 `syslog-show-notes' is called with a prefix arg."
  :group 'syslog
  :type '(repeat (list (choice :tag "Word"
			       (const :tag "Ignore word at point" nil)
			       (regexp :tag "Word regexp"
				       :help-echo "Regexp to match word at point"))
		       (choice :tag "Line"
			       (symbol :tag "Identifier symbol"
				       :help-echo "Symbol identifying the note")
			       (regexp :tag "Line regexp"
				       :help-echo "Regexp to match line at point"))
		       (choice :tag "Note"
			       (function-item syslog-show-note-from-file-or-buffer)
			       (function-item syslog-show-note-from-manpages)
			       (function-item syslog-show-note-from-info-node)
			       (function-item syslog-show-note-from-apropos)
			       (function :tag "Other function"
					 :help-echo "Function which returns or displays a note")
			       (string :tag "String" :help-echo "Contents of note"))
		       (repeat :inline t :tag "Args for note function"
			       (choice (const :tag "word" word)
				       (const :tag "line" line)
				       (const :tag "accum" accum)
				       (function :tag "word transformer"
						 :value (lambda (word)))
				       (function :tag "line transformer"
						 :value (lambda (line)))
				       (function :tag "accum transformer"
						 :value (lambda (accum) (not accum)))
				       (sexp :tag "General argument"))))))

;; simple-call-tree-info: DONE
(defvar-local syslog-notes nil
  "List of syslog notes for current buffer.
Each item is a list of the form (WORDRX LINERX NOTE . ARGS)
where:
 WORDRX is a regexp to match the active region or word at point. If WORDRX 
        contains a non-shy match group the match to the first such group will 
        be used instead of the entire match.
 LINERX is a regexp to match the current line. Non-shy match groups are treated 
        the same way as for WORDRX. It can also be a symbol, in which case it
        will be offered as a candidate when `syslog-show-notes' is called with
        a prefix arg. This allows you to create special notes items that are
        only used when a prefix arg is used. You should make sure the symbol 
        is a word which is unlikely to match any text in the syslog buffer.
 NOTE   is the note to be displayed: either a string, or a function which will
        be called with the remaining ARGS. If it's a function it should either 
        return a string to be displayed, display the note itself and return the
        window used to display it, or return nil to indicate that it is unable 
        to find an appropriate note (in which case `syslog-show-notes' may display 
        notes from other matching items).
        `syslog-show-note-from-manpages', `syslog-show-note-from-file-or-buffer', 
        `syslog-show-note-from-info-node' and `syslog-show-note-from-apropos' can 
        be used here.
 ARGS   are arguments for the NOTES function. Any occurrence of the symbols
        `word', `line' or `accum' among ARGS will be replaced by the matches to 
        WORDRX & LINERX, or the accumulated note string collected so far from 
        previous items (so the function can decide whether it needs to display
        a note or not).
        Any function among ARGS whose arglist consists of a single symbol 'word,
        'line or 'accum will be replaced by the results of calling that function
        with the match to WORDRX or LINERX, or the accumulated note string.

WORDRX or LINERX may be nil, in which case only the non-nil regexp will be used
for matching. WORDRX only matches have higher precedence than LINERX only matches, 
but lower precedence than combined WORDRX & LINERX matches. To increase the
priority of an item change WORDRX/LINERX from nil to \".*\". To do a negative match
on WORD use the following trick \"WORD\\\\|\\\\(.*\\\\)\". This will return a
match if the parenthesised match group matches, but not if WORD matches.

An entry with nil values for both WORDRX & LINERX may be used as the default.
All matches of the highest precedence will be displayed.

See also `syslog-notes-default'.")

;; simple-call-tree-info: DONE
(defcustom syslog-manpage-wait 0.2
  "Amount of time to wait for manpage to finish rendering, when processing manpages."
  :group 'syslog
  :type 'float)

;; simple-call-tree-info: CHECK
(defvar-local syslog-note-thing 'syslog-token
  "Symbol indicating the thing used by `syslog-show-notes' for looking up notes.
This is used as the first arg to `thing-at-point', and must have an associated
 `forward-OP' function.")

;; simple-call-tree-info: CHECK
(defcustom syslog-note-things nil
  "An alist of (REGEX . SYMB) pairs for choosing `syslog-note-thing'.
When `syslog-mode' starts up it will check the REGEX's for matches against the
buffer file name. If a match is found then `syslog-note-thing' will be set to
the associated SYMB value. Otherwise the default value of `syslog-token' will be used."
  :group 'syslog
  :type '(alist :key-type (regexp :help-echo "Regexp for matching file visited by buffer")
		:value-type (symbol :help-echo "Function which returns token at point")))

;; simple-call-tree-info: CHECK
(defun forward-syslog-token (&optional arg)
  "Move point forward over ARG tokens (backwards if ARG is negative).
If ARG is omitted or nil, move point forward one token."
  (interactive "p")
  (re-search-forward "[][[:space:]:;,\n=|(){}<>'\"]" nil nil arg)
  (backward-char (if (and arg (> arg 0)) 1 -1)))

;; simple-call-tree-info: CHECK
(defun syslog-show-notes (&optional word line)
  "In the minibuffer display notes associated with the region or WORD at point.
The notes are chosen from the current value of `syslog-notes'.
If there are notes which match the current region/word & LINE, then all those
notes will be displayed, otherwise all notes matching the current region/word
 (but with no line regexp) will be displayed, or if there are none of those
then all notes matching the current line (but with no region/word regexp)
will be displayed.
If there are no `syslog-notes' entries matching the region/word or line at point,
and `syslog-notes' contains a default item(s) with no region/word or line entries
then that will be used.
When called interactively with a prefix arg a WORD will be prompted for, and
items in `syslog-notes' which have a symbol for the LINERX entry will be offered
as selection candidates for LINE. You may also choose \"current line\" or
\"enter line\" at this point. Note that in this case WORD is treated as a regexp."
  (interactive (list (when current-prefix-arg
		       (read-string "Find notes for word: "))
		     (when current-prefix-arg
		       (let* ((allnotes (append syslog-notes syslog-notes-default))
			      (selection
			       (ido-completing-read
				"Note type: "
				(append	;don't use nconc here
				 (mapcar (lambda (y) (symbol-name (cadr y)))
					 (cl-remove-if-not
					  (lambda (x) (and (cadr x) (symbolp (cadr x))))
					  allnotes))
				 '("current line" "enter line")))))
			 (cond ((equal selection "current line") nil)
			       ((equal selection "enter line") (read-string "Enter line: "))
			       (t selection))))))
  (cl-labels ((getstr (s) (if (symbolp s) (symbol-name s) s))
	      (strmatch (rx1 str) (and rx1 str (string-match (getstr rx1) str)))
	      (wdmatch (elem) (strmatch (car elem) word))
	      (lnmatch (elem) (strmatch (cadr elem) line))
	      (getmatch (regex str)
			(if regex
			    (and (string-match regex str)
				 (match-string (if (> (regexp-opt-depth regex) 0)
						   1
						 0)
					       str))
			  str)))
    (if (or syslog-notes syslog-notes-default)
	(let* ((allnotes (append syslog-notes syslog-notes-default))
	       (case-fold-search nil)
	       (line (or line
			 (buffer-substring-no-properties (line-beginning-position)
							 (line-end-position))))
	       (word (or word
			 (if mark-active
			     (buffer-substring-no-properties
			      (region-beginning) (region-end))
			   (substring-no-properties
			    (or (thing-at-point syslog-note-thing)
				(error "Thing at point is not a valid item for notes"))))))
	       (haswd (cl-remove-if-not (function wdmatch) allnotes))
	       (nowd (cl-remove-if 'car allnotes))
	       (items (or (cl-remove-if-not (function lnmatch) haswd)
			  (cl-remove-if 'cadr haswd)
			  (cl-remove-if-not (function lnmatch) nowd)
			  (cl-remove-if 'cadr nowd)))
	       (msgstr nil)
	       (note (cl-loop for item in items
			      for value = (let* ((wd (getmatch (car item) word))
						 (ln (getmatch (getstr (cadr item)) line))
						 (nt (third item))
						 (args (mapcar
							(lambda (arg)
							  (cond
							   ((eq arg 'word) wd)
							   ((eq arg 'line) ln)
							   ((eq arg 'accum) msgstr)
							   ((and (functionp arg)
								 (equal
								  (help-function-arglist arg)
								  '(word)))
							    (funcall arg wd))
							   ((and (functionp arg)
								 (equal
								  (help-function-arglist arg)
								  '(line)))
							    (funcall arg ln))
							   ((and (functionp arg)
								 (equal
								  (help-function-arglist arg)
								  '(accum)))
							    (funcall arg ln))
							   (t arg)))
							(cdddr item))))
					    (cond ((null nt) "")
						  ((functionp nt)
						   (let ((rval (apply nt args)))
						     (when (windowp rval)
						       (setq-local syslog-notes-last-buffer
								   (window-buffer rval)))
						     rval))
						  ((stringp nt) nt)
						  (t (error "Invalid note entry in %s"
							    (syslog-notes-file)))))
			      if (stringp value) do (setq msgstr (concat msgstr value "\n"))
			      else if value return nil
			      finally return msgstr)))
	  (setq-local syslog-notes-last-word (if current-prefix-arg
						 word
					       (regexp-quote word)))
	  (when note
	    (message (if (> (length note) 0)
			 (replace-regexp-in-string "\n$" "" note)
		       (concat "No notes found for " word
			       " (to create one: M-x syslog-edit-notes)")))
	    (setq-local syslog-notes-last-buffer nil)))
      (when (and (y-or-n-p "No notes loaded, load now? ")
		 (syslog-load-notes))
	(syslog-show-notes)))))

;; simple-call-tree-info: TODO figure out way to get `Info-search' working properly with backward searches
(defun syslog-notes-next-match (arg)
  "Search other window for the next match to the word/regexp used by the last call to `syslog-show-notes'.
If a prefix ARG is used, prompt for the ARGth next match.
When the search window is in `Info-mode' then `Info-search' will be used to search across all related nodes."
  (interactive "P")
  (let ((word syslog-notes-last-word)
	(Info-isearch-search t))
    (if (buffer-live-p syslog-notes-last-buffer)
	(with-selected-window (display-buffer syslog-notes-last-buffer)
	  (funcall (if (and (eq major-mode 'Info-mode)
			    (> (prefix-numeric-value arg) 0))
		       'Info-search 're-search-forward)
		   word nil t (prefix-numeric-value arg))
	  (recenter 0))
      (message "No notes buffer visible"))))

;; simple-call-tree-info: CHECK
(defun syslog-notes-prev-match (arg)
  "Search other window for the previous match to the word/regexp used by the last call to `syslog-show-notes'.
If a prefix ARG is used, prompt for the ARGth previous match."
  (interactive "P")
  (syslog-notes-next-match (* -1 (prefix-numeric-value arg))))

;; simple-call-tree-info: DONE
(defun syslog-notes-file nil
  "Return the syslog notes file associated with the current buffer, or nil if none exists.
The file is chosen using `syslog-notes-files'."
  (let* ((bfn (or (and buffer-file-name
		       (expand-file-name buffer-file-name))
		  (buffer-name)))
	 (file (cdr (cl-assoc-if (lambda (f) (string-match f bfn))
				 syslog-notes-files))))
    (when file
      (if (file-name-directory file)
	  file
	(concat (file-name-directory (symbol-file 'syslog-mode))
		file)))))

;; simple-call-tree-info: DONE  
(defun syslog-load-notes nil
  "Load appropriate notes file for the current buffer.
The file is chosen using `syslog-notes-files'"
  (interactive)
  (let ((file (syslog-notes-file))
	(load-prefer-newer t))
    (if file
	(load (replace-regexp-in-string "\\.elc?$" "" file))
      (message "No notes file associated with this buffer (create one with: M-x syslog-edit-notes)")
      nil)))

;; simple-call-tree-info: CHECK  
(defun syslog-edit-notes nil
  "Edit syslog notes file associated with current buffer.
If this is none, then create new notes file, and add it to `syslog-notes-files'."
  (interactive)
  (let* ((file (syslog-notes-file))
	 (notesdir (file-name-directory (symbol-file 'syslog-mode)))
	 (bfn (or (and buffer-file-name
		       (file-name-nondirectory buffer-file-name))
		  (buffer-name)))
	 (newfile (concat bfn "_notes.el")))
    (if (and file (file-exists-p file))
	(find-file file)
      (setq file (read-file-name "Select/create a notes file to associate with this buffer: "
				 notesdir newfile nil newfile
				 (lambda (f) (string-match "\\.el" (file-name-nondirectory f)))))
      (find-file file)
      (if (file-exists-p file)
	  (when (save-excursion
		  (not (and (eq major-mode 'emacs-lisp-mode)
			    (goto-char (point-min))
			    (search-forward "syslog-notes" nil t))))
	    (error "This is not a syslog notes file"))
	(insert ";; This file contains a definition of `syslog-notes' used by the `syslog-show-notes' function.\n")
	(insert ";; See the `syslog-notes' documentation for info about the correct format\n")
	(insert ";; After editing save & kill this buffer, and then in the syslog-mode buffer do: M-x syslog-load-notes\n")
	(insert ";; To always use this file add an entry to the `syslog-notes-files' user option.\n")
	(insert ";; See also `syslog-show-note-from-manpages' `syslog-show-note-from-file-or-buffer',\n")
	(insert ";; `syslog-show-note-from-info-node' and `syslog-show-note-from-apropos'\n")
	(insert ";; It is recommended to compile this file for faster loading.\n")
	(insert "(setq-local\n syslog-notes\n '((\"EXAMPLE\" \"^.*stuff.*\" \"An example note. Delete this entry\")))"))
      (add-to-list 'syslog-notes-files (cons (regexp-quote bfn) file)))))

;; simple-call-tree-info: CHECK
(defmacro syslog-process-manpage (page &rest body)
  "Execute BODY in buffer containing contents of manpage PAGE.
Do not display the manpage."
  (declare (debug (sexp body)) (indent 1))
  (let ((manbuf (gensym)))
    `(let (case-fold-search
	   (Man-notify-method 'meek)
	   (,manbuf (Man-getpage-in-background
		     (Man-translate-references ,page))))
       (while (and (get-buffer-process ,manbuf)
		   (eq (process-status ,manbuf) 'run))
	 (sleep-for syslog-manpage-wait))
       (with-current-buffer ,manbuf
	 (delete-window (get-buffer-window ,manbuf))
	 (save-excursion
	   (goto-char (point-min))
	   ,@body)))))

;; simple-call-tree-info: CHECK
(defun syslog-search-regexp-and-face (regex &optional matchface noerr bound)
  "Search forward for REGEX, and optionally check that it has face MATCHFACE.
If no match is found an error will be thrown unless NOERR is non-nil in which
case nil will be returned (t is returned if a match is found).
The optional BOUND arg can be used to bound the search."
  (let ((found t))
    (if (re-search-forward regex bound t)
	(while (and matchface
		    (not (eq (get-text-property (1- (point)) 'face)
			     matchface)))
	  (unless (re-search-forward regex bound t)
	    (if noerr ;;set mathface to nil to break out of loop
		(setq found nil matchface nil)
	      (error "Cannot find match for %s with face %s" regex matchface))))
      (if noerr
	  (setq found nil)
	(error "Cannot find match for %s" regex)))
    found))

;; simple-call-tree-info: CHECK
(defun syslog-extract-manpage-regions (page regex1 regex2
					    &optional face1 face2 start end)
  "Return list of regions of manpage PAGE delimited by regexps REGEX1 & REGEX2.
Optional args FACE1 & FACE2 specify faces for the last char of the matches
to REGEX1 & REGEX2. REGEX1 may contain a single non-shy match group whose
matching content will be returned in a cons cell (the car) with the matched
region (the cdr), otherwise if there is no non-shy match group, the whole
match will be returned in the car.
You can restrict the search region by supplying buffer positions in the
optional START and END args."
  (syslog-process-manpage
      page
    (let ((n (regexp-opt-depth regex1))
	  startpos endpos regions word)
      (when start (goto-char start))
      (while (syslog-search-regexp-and-face regex1 face1 t end)
	(setq startpos (match-end 0)
	      word (match-string-no-properties (if (> n 0) 1 0)))
	(when (syslog-search-regexp-and-face regex2 face2 t end)
	  (setq endpos (match-beginning 0))
	  (add-to-list 'regions
		       (cons word
			     (replace-regexp-in-string
			      "\\`\\s-+\\|\\s-+\\'" ""
			      (buffer-substring-no-properties startpos endpos)))
		       t)
	  (forward-line 0)))
      regions)))

;; simple-call-tree-info: CHECK  
(cl-defun syslog-show-note-from-manpages (regex pages &optional nodefault
						(indent 7)
						(face 'Man-overstrike)
						start end)
  "Show a description extracted from manpage(s) PAGES, or display part of a manpage.
REGEX should be a regular expression matching a word whose description is to be extracted,
or if INDENT is nil, the top of the part of the manpage to be displayed.
PAGES can be either the name of a single manpage, or a list of manpage names.
The manpage names may include section numbers, e.g. \"signal(7)\" or \"7 signal\".

The description is found by searching for indented text following the first appearance
of REGEX at indentation level INDENT and face FACE in the manpage.
The description is assumed to end when the indentation level of the text
returns to INDENT. To bound the regions searched within each manpage you can supply
START & END args which should be either single buffer positions if PAGES is the
name of a single manpage, or lists of buffer positions corresponding to each manpage
otherwise.

If no match can be found at INDENT level or INDENT is nil, and if NODEFAULT is also nil 
then search for the first match to REGEX in the manpages regardless of indentation level 
or FACE, and display it in another window which is returned by the function."
  (when regex
    (let* ((indstr (and indent (number-to-string indent)))
	   (pages (if (listp pages) pages (list pages)))
	   (starts (if (and start (listp start))
		       start
		     (make-list (length pages) start)))
	   (ends (if (and end (listp end))
		     end
		   (make-list (length pages) end)))
	   (notes (when indstr
		    (cl-loop for page in pages
			     for s in starts
			     for e in ends
			     for notes = (mapcar 'cdr
						 (syslog-extract-manpage-regions
						  page
						  (concat (concat "^\\s-\\{" indstr "\\}")
							  regex)
						  (concat "^\\s-\\{," indstr "\\}\\S-")
						  face nil s e))
			     if notes concat (concat regex " in " page " manpage: "
						     (mapconcat 'identity notes "\n")
						     "\n")))))
      (if (> (length notes) 0)
	  (substring notes nil -1)
	(when (not (and nodefault indent))
	  (let (win)
	    (cl-loop for page in pages
		     if (setq win
			      (syslog-show-note-from-file-or-buffer
			       (get-buffer (concat "*Man " (Man-translate-references page) "*"))
			       regex))
		     return win)))))))

;; simple-call-tree-info: CHECK
(defun syslog-extract-matches-from-manpage (page regex &optional face)
  "Return list of matches to REGEX in manpage PAGE.
If REGEX has a non-shy match group then matches to the first such group will be returned.
If optional symbol FACE is supplied then only matches with that face property will be returned. 
Searching is done case sensitively."
  (syslog-process-manpage page
    (let ((n (regexp-opt-depth regex))
	  matches)
      (save-excursion
	(goto-char (point-min))
	(while (syslog-search-regexp-and-face regex face t)
	  (add-to-list 'matches
		       (match-string-no-properties (if (> n 0) 1 0))
		       t)))
      matches)))

;; simple-call-tree-info: CHECK
(defvar-local syslog-notes-last-buffer nil
  "Buffer last used for displaying a note with `syslog-show-notes', if any.")

;; simple-call-tree-info: CHECK
(defvar-local syslog-notes-last-word nil
  "Regexp last used for finding a note with `syslog-show-notes'.")

;; simple-call-tree-info: CHECK
(defun syslog-show-note-from-file-or-buffer (fileorbuf line &optional count start end)
  "Display a section of FILEORBUF in another window, and return that window.
If LINE is a positive number display that line at the top of the window.
If LINE is a regexp display the first match at the top of the window.
If COUNT is an integer display instead the COUNT'th match at the top,
or the COUNT'th last match if COUNT is negative.
Optional args START & END can be used to delimit the search area when LINE is
a regexp. They can be either integers indicating buffer positions, or regexps
indicating positions to search for.

If FILEORBUF is an org file or buffer, the buffer will be place in `org-mode',
and widened around the displayed section.
If no match for LINE can be found, return nil.

Note: if FILEORBUF is a file larger than `syslog-large-file-size' bytes the
user will be prompted before loading the file (unless it's already loaded)."
  (when (or (bufferp fileorbuf)
	    (let ((fsize (file-attribute-size (or (file-attributes fileorbuf)
						  (error "Cannot find file: %S" fileorbuf)))))
	      (or (member (expand-file-name fileorbuf)
			  (mapcar 'buffer-file-name (buffer-list)))
		  (< fsize syslog-large-file-size)
		  (y-or-n-p (format "%s is a large file (%S bytes), continue? "
				    (file-name-nondirectory fileorbuf) fsize)))))
    (let ((wconfig (current-window-configuration))
	  (win (display-buffer
		(if (bufferp fileorbuf)
		    fileorbuf
		  (message "Loading %s..." fileorbuf)
		  (find-file-noselect fileorbuf)))))
      (with-selected-window win
	(widen)
	(push-mark)
	(if (not (cond ((integerp line)
			(goto-char (point-min))
			(= (forward-line (1- line)) 0))
		       ((stringp line)
			(if (and count (< count 0))
			    (cond ((numberp end) (goto-char end))
				  ((stringp end)
				   (goto-char (point-max))
				   (unless (re-search-backward end nil t)
				     (goto-char (point-max))))
				  (t (goto-char (point-max))))
			  (cond ((numberp start) (goto-char start))
				((stringp start)
				 (goto-char (point-min))
				 (unless (re-search-forward start nil t)
				   (goto-char (point-max))))
				(t (goto-char (point-min)))))
			(re-search-forward line
					   (if (and count (< count 0))
					       (cond ((numberp start) start)
						     ((stringp start)
						      (save-excursion
							(re-search-backward start nil t)
							(point))))
					     (cond ((numberp end) end)
						   ((stringp end)
						    (save-excursion
						      (re-search-forward end nil t)
						      (point)))))
					   t count))
		       (t (error "Invalid value for line arg: %S" line))))
	    (progn (delete-window win)
		   (set-window-configuration wconfig))
	  (when (derived-mode-p 'org-mode)
	    (org-show-context 'agenda))
	  (recenter 0)))
      (and (window-valid-p win) win))))

;; simple-call-tree-info: TODO figure out way to get `Info-search' working with backward searches
(defun syslog-show-note-from-info-node (node &optional regex count all)
  "Display info NODE in another window, and return that window.
If REGEX is non-nil recenter the buffer so that the first match is displayed
at the top. If COUNT is an integer display instead the COUNT'th match at the top, 
or the COUNT'th last match if COUNT is negative.
If no match for REGEX can be found, return nil.
If ALL is non-nil, the search may go beyond the end of the current node & file 
if necessary until a match is found. In that case it currently only works properly
if COUNT >= 0."
  (let ((wconfig (current-window-configuration))
	(win (display-buffer
	      (get-buffer-create
	       (concat "*info-" (and (string-match "^(\\([^()]+\\))" node)
				     (match-string 1 node))
		       "*"))))
	(Info-isearch-search t))
    (with-selected-window win
      (unless (eq major-mode 'Info-mode) (Info-mode))
      (condition-case nil
	  (progn (Info-goto-node node)
		 (when regex
		   (goto-char (if (and count (< count 0))
				  (point-max)
				(point-min)))
		   (funcall (if all 'Info-search 're-search-forward)
			    regex nil nil count)
		   (recenter 0)))
	(error (delete-window win)
	       (set-window-configuration wconfig))))
    (and (window-valid-p win) win)))

;; simple-call-tree-info: CHECK
(defun syslog-show-note-from-apropos (regex &optional retry &rest sections)
  "Use apropos to find descriptions of REGEX, and display the results.
REGEX will be wrapped in \"^\" & \"$\" anchors to match manpage names.
If no such match can be found, and if RETRY is non-nil then it will be
tried again but prepended & appended with \".*\" to match anywhere.
By default all matches in all manual sections will be displayed.
Further arguments may be supplied to indicate which SECTIONS to search.
Each such argument may be either a single section number, or a list of
section numbers, and all results from the first one/list which returns
a match will be displayed."
  (cl-flet ((getoutput (page &optional opt)
		       (let ((output (shell-command-to-string
				      (concat "apropos " opt " \"^" page "$\""))))
			 (unless (string-match ": nothing appropriate." output)
			   (substring output 0 -1)))))
    (or (if sections
	    (cl-loop for secs in sections
		     for opt = (concat
				"-s "
				(cond ((numberp secs) (number-to-string secs))
				      ((listp secs) (mapconcat 'number-to-string secs ","))
				      (t (error "Invalid section arg"))))
		     for str = (getoutput regex opt)
		     if (> (length str) 0) return str)
	  (getoutput regex))
	(and retry
	     (apply 'syslog-show-note-from-apropos (concat ".*" regex ".*") nil sections)))))

;; simple-call-tree-info: CHECK
(cl-defun syslog-text-notes-from-manpages (manpages &key
						    (wordrx "\\(\\<[A-Z_]+\\>\\)")
						    (linerx nil)
						    (face 'Man-overstrike)						    
						    (indent 7)
						    (transformer nil)
						    (exceptions nil))
  "Extract notes from manpages, and inserts elisp code to update `syslog-notes'.
MANPAGES should be a list whose car is the name of a manpage, and whose cdr is
a property list of the following optional keywords & arguments:

 :WORDRX - a regexp for matching text preceeding the start of each region of text 
        to be extracted. If the value of :WORDRX contains a non-shy match group it 
        will be used for the returned WORD (to be matched in the syslog buffer), 
        otherwise the entire match with leading and trailing whitespace will be used
        (see also :TRANSFORMER below).
 :LINERX - a regexp that (if non-nil) the current line must match in order to view 
        notes extracted from this manpage.
 :FACE - a face symbol which (if non-nil) the text preceeding the region must match.
 :INDENT - a number indicating the level of indentation of the text preceeding the
        region, and also the maximum level of indentation of text immediately following 
        the end of the region.
 :TRANSFORMER - a function for transforming the returned WORD before inserting it
        into `syslog-notes'. Use this if the words matched in the manpage differ
        from the corresponding words in the syslog buffer. 
        Note: leading & trailing whitespace is removed before applying the transformer.
 :EXCEPTIONS - a list of words (after transformation) whose descriptions will be 
        omitted from the results. 

Default values for any of the above (manpage specific) keywords are obtained from
the main keyword args of the function.

The inserted code, when evaluated, will nconc a list of (WORD nil NOTE) triples to 
the current value of `syslog-notes'. You may need to make some alterations before
evaluating it."
  (insert "\n(setq-local\n syslog-notes\n (nconc\n syslog-notes\n '(")
  (cl-loop for (page . rest) in manpages
	   for trans = (or (plist-get rest :transformer)
			   (plist-get rest :transformers))
	   for indstr = (number-to-string (or (plist-get rest :indent) indent))
	   for rxA = (concat "^\\s-\\{" indstr "\\}" (or (plist-get rest :wordrx) wordrx))
	   for rxB = (concat "^\\s-\\{," indstr "\\}\\S-")
	   for regions = (syslog-extract-manpage-regions
			  page rxA rxB (or (plist-get rest :face) face))
	   do (dolist (region regions)
		(let ((word (funcall (cond
				      ((null trans) (or transformer 'identity))
				      ((functionp trans) trans)
				      ((consp trans) (or (car trans) 'identity))
				      (t (error "Invalid transformer arg")))
				     (replace-regexp-in-string "^\\s-*\\|\\s-*$" ""
							       (car region)))))
		  (unless (member word (or (plist-get rest :exceptions) exceptions))
		    (insert (format "(%S %S %S)\n"
				    word
				    (or (plist-get rest :linerx) linerx)
				    (cdr region))))))
	   finally (insert ")))")))

;; simple-call-tree-info: DONE
(defface syslog-ip
  '((t :underline t :slant italic :weight bold))
  "Face for IPs"
  :group 'syslog)

;; simple-call-tree-info: DONE
(defface syslog-file
  (list (list t :weight 'bold
	      :inherit (if (facep 'diredp-file-name)
			   'diredp-file-name
			 'dired-ignored)))
  "Face for filenames"
  :group 'syslog)

;; simple-call-tree-info: DONE
(defface syslog-hour
  '((t :weight bold  :inherit font-lock-type-face))
  "Face for hours"
  :group 'syslog)

;; simple-call-tree-info: DONE
(defface syslog-error
  '((t  :weight bold :foreground "red"))
  "Face for errors"
  :group 'syslog)

;; simple-call-tree-info: DONE
(defface syslog-warn
  '((t  :weight bold :foreground "goldenrod"))
  "Face for warnings"
  :group 'syslog)

;; simple-call-tree-info: DONE
(defface syslog-info
  '((t  :weight bold :foreground "deep sky blue"))
  "Face for info lines"
  :group 'syslog)

;; simple-call-tree-info: DONE
(defface syslog-debug
  '((t  :weight bold :foreground "medium spring green"))
  "Face for debug lines"
  :group 'syslog)

;; simple-call-tree-info: DONE
(defface syslog-su
  '((t  :weight bold :foreground "firebrick"))
  "Face for su and sudo"
  :group 'syslog)

;; simple-call-tree-info: DONE
(defface syslog-hide
  '((t :foreground "black" :background "black"))
  "Face for hiding text"
  :group 'syslog)

;; Keywords
;; TODO: Seperate the keywords into a list for each format, rather than one for all.
;;       Better matching of dates (even when not at beginning of line).
;; simple-call-tree-info: CHECK  
(defvar syslog-font-lock-keywords
  '(("\"[^\"]*\"" . 'font-lock-string-face)
    ("'[^']*'" . 'font-lock-string-face)
    ;; Filename at beginning of line
    ("^\\([^ :]+\\): " 1 'syslog-file append)
    ;; Hours: 17:36:00
    ("\\(?:^\\|[[:space:]]\\)\\([[:digit:]]\\{1,2\\}:[[:digit:]]\\{1,2\\}\\(:[[:digit:]]\\{1,2\\}\\)?\\)\\(?:$\\|[[:space:]]\\)" 1 'syslog-hour append)
    ;; Date
    ("\\(?:^\\|[[:space:]]\\)\\([[:digit:]]\\{1,2\\}/[[:digit:]]\\{1,2\\}/[[:digit:]]\\{2,4\\}\\)\\(?:$\\|[[:space:]]\\)" 1 'syslog-hour append)
    ;; Dates: May  9 15:52:34
    ("^\\(?:[^ :]+: \\)?\\(\\(?:[[:alpha:]]\\{3\\}\\)?[[:space:]]*[[:alpha:]]\\{3\\}\\s-+[0-9]+\\s-+[0-9:]+\\)" 1 'font-lock-type-face t)
    ;; Su events
    ("\\(su:.*$\\)" 1 'syslog-su t)
    ("\\(sudo:.*$\\)" 1 'syslog-su t)
    ("\\[[^]]*\\]" . 'font-lock-comment-face)
    ;; IPs
    ("[[:digit:]]\\{1,3\\}\\.[[:digit:]]\\{1,3\\}\\.[[:digit:]]\\{1,3\\}\\.[[:digit:]]\\{1,3\\}" 0 'syslog-ip append)
    ("\\<[Ee][Rr][Rr]\\(?:[Oo][Rr][Ss]?\\)?\\>" 0 'syslog-error append)
    ("\\<[Ii][Nn][Ff][Oo]\\>" 0 'syslog-info append)
    ("\\<[Cc][Rr][Ii][Tt][Ii][Cc][Aa][Ll]\\>" 0 'syslog-error append)
    ("STARTUP" 0 'syslog-info append)
    ("CMD" 0 'syslog-info append)
    ("\\<[Ww][Aa][Rr][Nn]\\(?:[Ii][Nn][Gg]\\)?\\>" 0 'syslog-warn append)
    ("\\<[Dd][Ee][Bb][Uu][Gg]\\>" 0 'syslog-debug append)
    ("(EE)" 0 'syslog-error append)
    ("(WW)" 0 'syslog-warn append)
    ("(II)" 0 'syslog-info append)
    ("(NI)" 0 'syslog-warn append)
    ("(!!)" 0 'syslog-debug append)
    ("(--)" 0 'syslog-debug append)
    ("(\\*\\*)" 0 'syslog-debug append)
    ("(==)" 0 'syslog-debug append)
    ("(\\+\\+)" 0 'syslog-debug append))
  "Expressions to hilight in `syslog-mode'.")

;; simple-call-tree-info: CHECK  
(defvar syslog-strace-font-lock-keywords
  '(("^\\([0-9]+\\) " . (1 font-lock-warning-face))
    ("^[0-9]+ \\([a-zA-Z0-9_]*\\)(" . (1 font-lock-constant-face))
    (" = 0x[[:xdigit:]]+ \\([[:upper:]]+\\).*$" . (1 font-lock-warning-face))
    (" = -?[[:digit:]?]+ \\([[:upper:]]+\\).*$" . (1 font-lock-warning-face))
    (" = \\(0x[[:xdigit:]]+\\).*$" . (1 font-lock-keyword-face))
    (" = \\(-?[[:digit:]?]+\\).*$" . (1 font-lock-keyword-face))
    ("[ =\(\[\{]\\([[:upper:]_|]+\\)[] |\,\(\)\}]" . (1 font-lock-constant-face))
    (" \\((.*)\\)$" . (1 font-lock-comment-face))
    ("\\(/\\*.*\\*/\\)" . (1 font-lock-comment-face))
    ("0x[[:xdigit:]]+" . font-lock-type-face)
    ("-?[[:digit:]]+" . font-lock-type-face))
  "Font locking definitions for trace output in syslog mode.")

;;; Setup functions
;; simple-call-tree-info: DONE
(defun syslog-find-file-func ()
  "Invoke `syslog-mode' if the buffer appears to be a system logfile.
and another mode is not active.
This function is added to `find-file-hooks'."
  (if (and (eq major-mode 'fundamental-mode)
	   (looking-at syslog-sequence-start-regexp))
      (syslog-mode)))

;; simple-call-tree-info: DONE
(defun syslog-add-hooks ()
  "Add a default set of syslog-hooks.
These hooks will activate `syslog-mode' when visiting a file
which has a syslog-like name (.fasta or .gb) or whose contents
looks like syslog.  It will also turn enable fontification for `syslog-mode'."
  ;; (add-hook 'find-file-hooks 'syslog-find-file-func)
  (add-to-list 'auto-mode-alist
	       '("\\(messages\\(\\.[0-9]\\)?\\|SYSLOG\\|\\.s?trace\\)\\'" . syslog-mode)))

;; Setup hooks on request when this mode is loaded.
(if syslog-setup-on-load (syslog-add-hooks))

;; done loading
(run-hooks 'syslog-mode-load-hook)

(provide 'syslog-mode)

;;; syslog-mode.el ends here

;;; (magit-push)
;;; (yaoddmuse-post "EmacsWiki" "syslog-mode.el" (buffer-name) (buffer-string) "update")
