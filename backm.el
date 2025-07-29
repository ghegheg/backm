;; backm.el --- create more backups -*- lexical-binding: t -*-
;; It creates `backm-backup-number' backups. Later, move to trash the
;; oldest, increment the others, and save current buffer as 0 number.   
;; Not recommended as `backm-directory' to contain old backups having
;; the same pattern, they will be moved to trash. Not put `.'
;; character in buffer name, except its extension that is correct.
;; Not put `/' or `\' characters in buffer-name, pattern or time-format.

(defgroup backm nil
	"Group backm package."
	:group 'convenience)

(defcustom backm-directory nil 
	"Backup directory, implicit is buffer's `default-directory' variable content."
	:type '(choice (directory :tag "existent directory") 
								 (const :tag "default-directory" nil))
	:group 'backm)

(defcustom backm-pattern "backup.$$$f.$$$n0-time:$$$d" 
	"Pattern for name for backup files.
Next groups of characters have special meaning:
`$$$f' is current buffer name without extension if any, use it once. (mandatory) 
`$$$n' is positive integer number what will be incremented from one backup to
other, use it once. (mandatory)   
`$$$d' time when backup file was created, see `backm-time-format', it is
optional, but if you use it, put it at the end of the pattern. 
Not use these groups of characters in buffer name that use this app!
To avoid confusion, errors an bugs put distinctive words in patterns
as `backup', `time'.
Always, if any, buffer extension goes at the end of backup name."
	:type 'string
	:group 'backm)

(defcustom backm-time-format "%d.%m.%y-%k:%M" 
	"Time format that appears in backup name. It replaces `$$$d' in pattern.
For more information about time format visit: 
(URL `https://www.gnu.org/software/emacs/manual/html_node/elisp/Time-Parsing.html')."
	:type 'string
	:group 'backm)

(defcustom backm-backup-number 5
	"Maximum number of backups to save. The newest backup starts from 0."
	:type 'natnum 
	:group 'backm)

(defvar backm-name-before-number nil
	"The backup name before `$$$n'. Not set it..")

(defvar backm-name-after-number nil
	"The backup name between `$$$n' and `$$$d' (or pattern end) Not set.")

(defvar backm-ext nil
	"Buffer's extension. Not set it.")

(defvar backm-dir nil
	"Backup directory for code. Not set it.")

(defvar backm-has-time nil
	"Is time contain in backup? Not set it.")

(defun backm-set ()
	"Set variables declared with defvar from this code."
	(let* ((pattern-sans-date (string-replace "$$$d" "" backm-pattern))
				 (number-index (string-match (regexp-quote "$$$n") pattern-sans-date)) 
				 (pattern-before-number (substring pattern-sans-date 0 number-index))
				 (pattern-after-number (substring pattern-sans-date (+ 4 number-index))))
		(setq backm-name-before-number
					(string-replace "$$$f"
													(file-name-sans-extension (buffer-name))
													pattern-before-number))
		(setq backm-name-after-number
					(string-replace "$$$f"
													(file-name-sans-extension (buffer-name))
													pattern-after-number))
		(setq backm-ext (file-name-extension (buffer-name) 'period))
		(unless backm-ext
			(setq backm-ext ""))
		(setq backm-dir (let ((directory (if backm-directory
																				 (file-name-as-directory backm-directory)
																			 default-directory)))
											(unless (file-directory-p directory)
												(error (format "The %S directory does not exists!" directory)))
											directory))
		(when (string-match (regexp-quote "$$$d") backm-pattern)
			(setq backm-has-time t))))

(defun backm-get-current-time () 
	"Get current time as a string depening on `backm-time-format'."
	(format-time-string backm-time-format (current-time)))

(defun backm-save-buf-as-backup ()
	"Save current buffer as nr. 0 backup on disk."
	(let ((file (concat backm-dir  
											backm-name-before-number
											"0"
											backm-name-after-number
											(if backm-has-time 
													(backm-get-current-time)
												"")
											backm-ext)))	
		(unless (file-exists-p file)
			(make-empty-file file))
		(write-region nil
									nil
									file)))

(defun backm-get-backup-sans-time-ext (backup)
	"Get backup name without time or/and extension."
	(let* ((time-length (if backm-has-time 
													(length (backm-get-current-time))
												0))
				 (time+ext-length (+ time-length (length backm-ext)))) 
		(if (< 0 time+ext-length) 
				(substring backup 0 (* -1 time+ext-length)) 
			backup)))

(defun backm-increment-backup (backup)
	"Increment backup name."
	(let* ((backup-sans-time-ext (backm-get-backup-sans-time-ext backup))
				 (time (substring backup
													(length backup-sans-time-ext)
													(if (eql 0 (length backm-ext))
															nil
														(* -1 (length backm-ext)))))
				 (beg-number (if (string-match (regexp-quote backm-name-before-number)
																			 backup-sans-time-ext)
												 (length backm-name-before-number)
											 nil))
				 (end-number (string-match (regexp-quote backm-name-after-number)
																	 backup-sans-time-ext))
				 (next-end nil))
		(while (setq next-end (string-match (regexp-quote backm-name-after-number)
																				backup-sans-time-ext
																				(1+ end-number)))
			(setq end-number next-end))
		(let ((number (if (and beg-number end-number)
											(string-to-number (substring backup-sans-time-ext
																									 beg-number
																									 end-number))
										nil)))
			(if number
					(progn (setq number (1+ number))
								 (concat backm-name-before-number
												 (number-to-string number)
												 backm-name-after-number
												 time
												 backm-ext))
				nil))))

(defun backm-delete-last-increment-other-backups ()
	"Delete the older and increment the number from all backups." 
	(let* ((files (reverse (directory-files
													backm-dir
													'nil
													(concat "\\`"
																	(regexp-quote backm-name-before-number)
																	"[0-9]+"
																	(regexp-quote backm-name-after-number)
																	(if backm-has-time
																			(let ((value ""))
																				(dotimes (i (length (backm-get-current-time)))
																					(setq value (concat "." value))
																					(+ 1 i))
																				value)
																		"")
																	(if (< 0 (length backm-ext))
																			backm-ext
																		"")
																	"\\'"))))
				 (choice (if files
										 (y-or-n-p
											(if (>= (length files) backm-backup-number)
													(format
													 "Move %S backup to trash and increment from: %s?"
													 (car files)
													 (reverse (cdr files))) 
												(format
												 "Increment backups from %s" (reverse files))))
									 'do-nothing)))
		(when (and files choice (>= (length files) backm-backup-number)) 
			(let ((trash-directory nil))
				(move-file-to-trash (concat backm-dir (car files)))
				(setq files (cdr files))))
		(when (and files choice (< (length files) backm-backup-number))
			(while files
				(let ((last-file (car files)))
					(rename-file last-file (backm-increment-backup last-file)))
				(setq files (cdr files))))
		choice))

(defun backm-make-backup ()
	"Delete the oldest backup, rename the others and save buf as the newest."
	(interactive)
	(backm-set)
	(when (backm-delete-last-increment-other-backups)
		(backm-save-buf-as-backup)))

(provide 'backm)

