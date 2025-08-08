;;; buffer-terminator.el --- Safely Terminate/Kill Buffers Automatically  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1.1.1
;; URL: https://github.com/jamescherti/buffer-terminator.el
;; Keywords: convenience
;; Package-Requires: ((emacs "25.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; The buffer-terminator package automatically kills buffers to help maintain a
;; clean and efficient workspace, while also improving Emacs' performance by
;; reducing the number of open buffers, thereby decreasing the number of active
;; modes, timers, and other processes associated with those buffers.
;;
;; Activating `(buffer-terminator-mode)` terminates all buffers that have been
;; inactive for longer than the duration specified by
;; `buffer-terminator-inactivity-timeout` (default: 30 minutes). It checks every
;; `buffer-terminator-interval` (default: 10 minutes) to determine if a buffer
;; should be terminated.
;;
;; The following buffers are not terminated by default:
;; - Special buffers (buffers whose names start with a space, start and end with
;;   `*`, or whose major mode is derived from `special-mode`).
;; - Modified file-visiting buffers that have not been saved; the user must save
;; - them first.
;; - Buffers currently displayed in any visible window.
;; - Buffers associated with running processes.
;;
;; (The default rules above are fully customizable. Users can define specific
;; rules for keeping or terminating certain buffers by specifying a set of rules
;; using `buffer-terminator-rules-alist`. These rules can include buffer name
;; patterns or regular expressions, major-modes, buffer properties, etc.)
;;
;; Installation from MELPA:
;; ------------------------
;; (use-package buffer-terminator
;;   :ensure t
;;   :custom
;;   (buffer-terminator-verbose nil)
;;   :config
;;   (buffer-terminator-mode 1))
;;
;; Links:
;; ------
;; - buffer-terminator.el @GitHub (Frequently Asked Questions, usage...):
;;   https://github.com/jamescherti/buffer-terminator.el

;;; Code:

(require 'cl-lib)

;;; Customizations

(defgroup buffer-terminator nil
  "Safely terminate buffers automatically."
  :group 'buffer-terminator
  :prefix "buffer-terminator-"
  :link '(url-link
          :tag "Github"
          "https://github.com/jamescherti/buffer-terminator.el"))

(defcustom buffer-terminator-inactivity-timeout (* 30 60)
  "Time in seconds before a buffer is considered inactive.
See also `buffer-terminator-interval'.
Default: 30 minutes."
  :type 'integer
  :group 'buffer-terminator)

(defcustom buffer-terminator-verbose nil
  "Enable verbose mode to log when a buffer is automatically killed."
  :type 'boolean
  :group 'buffer-terminator)

(defvar buffer-terminator--kill-inactive-buffers-timer nil
  "Timer object for killing inactive buffers.")

(defun buffer-terminator--cancel-timer ()
  "Cancel the `buffer-terminator' timer."
  (when buffer-terminator--kill-inactive-buffers-timer
    (cancel-timer buffer-terminator--kill-inactive-buffers-timer)
    (setq buffer-terminator--kill-inactive-buffers-timer nil)))

(defun buffer-terminator--start-timer (seconds)
  "Start the `buffer-terminator' timer every SECONDS."
  (setq buffer-terminator--kill-inactive-buffers-timer
        (run-with-timer seconds
                        seconds
                        'buffer-terminator--timer-apply-rules)))

(defcustom buffer-terminator-interval (* 10 60)
  "Frequency in seconds to repeat the buffer cleanup process.
See also `buffer-terminator-inactivity-timeout'.
Default: 10 minutes."
  :type 'integer
  :group 'buffer-terminator
  :set (lambda (symbol value)
         (buffer-terminator--cancel-timer)
         (set-default symbol value)
         (buffer-terminator--start-timer value)))

(defvar buffer-terminator-rules-alist
  '(;; Retain special buffers (DO NOT REMOVE).
    ;; DO NOT REMOVE (keep-buffer-property . special) unless you know of what
    ;; you are doing.
    (keep-buffer-property . special)

    ;; Keep process buffers.
    ;; (Process buffers are buffers where an active process is running.)
    (keep-buffer-property . process)

    ;; Keep visible buffers (DO NOT REMOVE)
    ;; (Buffers currently displayed in a window.)
    (keep-buffer-property . visible)

    ;; Kill inactive buffers.
    ;; (This can be customized with `buffer-terminator-inactivity-timeout'
    ;; and `buffer-terminator-interval'.)
    (kill-buffer-property . inactive))
  "Rules for processing buffers.
Each rule is a cons cell where the key is a symbol indicating the rule type, and
the value is either a string or a list of strings.

It is generally recommended to keep at least:
    (keep-buffer-property . special)
    (keep-buffer-property . visible)
    (kill-buffer-property . inactive)

If you choose to remove the above, ensure that the special buffers you want to
keep are added to `buffer-terminator-rules-alist'.")

(defcustom buffer-terminator-debug nil
  "Non-nil to display debug messages in the *buffer-terminator:debug* buffer.
This displays a lot of messages."
  :type 'boolean
  :group 'buffer-terminator)

(defvar buffer-terminator-before-hook nil
  "Hook run before applying `buffer-terminator' rules.
This hook is executed before evaluating the rules that determine which buffers
should be killed or retained.")

(defvar buffer-terminator-after-hook nil
  "Hook run after applying `buffer-terminator' rules.
This hook is executed after evaluating the rules that determine which buffers
should be killed or retained.")

;;; Internal variables

(defvar-local buffer-terminator--buffer-activity-time nil)
(defvar-local buffer-terminator--associated-buffers nil)

;;; Obsolete variables

(defcustom buffer-terminator-predicate nil
  "This variable is obsolete.
Use `buffer-terminator-rules-alist' instead."
  :group 'buffer-terminator
  :type '(choice (const nil) (function)))

(make-obsolete-variable 'buffer-terminator-predicate
                        'buffer-terminator-rules-alist
                        "1.1.0")

(defvar buffer-terminator-keep-buffers-with-process t
  "When non-nil, do not kill buffers associated with running processes.
This variable is obsolete.")

(make-obsolete-variable 'buffer-terminator-keep-buffers-with-process
                        'buffer-terminator-rules-alist
                        "1.1.0")

(defvar buffer-terminator-keep-major-modes nil
  "List of major-modes. Buffers with these major mode are never killed.
This variable is obsolete.")

(make-obsolete-variable 'buffer-terminator-keep-major-modes
                        'buffer-terminator-rules-alist
                        "1.1.0")

(defvar buffer-terminator-keep-visible-buffers t
  "When non-nil, `buffer-terminator' will not kill visible buffers.
This variable is obsolete.")

(make-obsolete-variable 'buffer-terminator-keep-visible-buffers
                        'buffer-terminator-rules-alist
                        "1.1.0")

(defvar buffer-terminator-keep-file-visiting-buffers nil
  "When non-nil, `buffer-terminator' will not kill buffers visiting files.
This variable is obsolete.")

(make-obsolete-variable 'buffer-terminator-keep-file-visiting-buffers
                        'buffer-terminator-rules-alist
                        "1.1.0")

(defvar buffer-terminator-keep-special-buffers t
  "If non-nil, `buffer-terminator' will never kill special buffers.
This variable is obsolete.")

(make-obsolete-variable 'buffer-terminator-keep-special-buffers
                        'buffer-terminator-rules-alist
                        "1.1.0")

(defvar buffer-terminator-keep-buffer-names nil
  "List of buffer names that will never be killed.
This variable is obsolete.")

(make-obsolete-variable 'buffer-terminator-keep-buffer-names
                        'buffer-terminator-rules-alist
                        "1.1.0")

(defvar buffer-terminator-keep-buffer-names-regexps nil
  "List of regexps that match buffer names that will never be killed.
This variable is obsolete.")

(make-obsolete-variable 'buffer-terminator-keep-buffer-names-regexps
                        'buffer-terminator-rules-alist
                        "1.1.0")

(defvar buffer-terminator-kill-buffer-names nil
  "List of buffer names that can be killed.
This variable is obsolete.")

(make-obsolete-variable 'buffer-terminator-kill-buffer-names
                        'buffer-terminator-rules-alist
                        "1.1.0")

(defvar buffer-terminator-kill-buffer-names-regexps nil
  "List of regex patterns matching buffer names that can be killed.
This variable is obsolete.")

(make-obsolete-variable 'buffer-terminator-kill-buffer-names-regexps
                        'buffer-terminator-rules-alist
                        "1.1.0")

(defvar buffer-terminator-kill-special-buffer-names nil
  "List of special buffer names that can be killed.
This variable is obsolete.")

(make-obsolete-variable 'buffer-terminator-kill-special-buffer-names
                        'buffer-terminator-rules-alist
                        "1.1.0")

(defvar buffer-terminator-kill-special-buffer-names-regexps nil
  "List of regex patterns matching special buffer names that can be killed.
This variable is obsolete.")

(make-obsolete-variable 'buffer-terminator-kill-special-buffer-names-regexps
                        'buffer-terminator-rules-alist
                        "1.1.0")

;;; Functions


(defun buffer-terminator--message (&rest args)
  "Display a message with '[buffer-terminator]' prepended.
The message is formatted with the provided arguments ARGS."
  (apply #'message (concat "[buffer-terminator] " (car args)) (cdr args)))

(defun buffer-terminator--insert-message (buffer-name msg &rest args)
  "Insert formatted MSG with ARGS into BUFFER-NAME buffer."
  (let ((buffer (get-buffer-create buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq-local buffer-read-only t)
        (save-excursion
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert (apply 'format msg args) "\n")))))))

(defmacro buffer-terminator--debug-message (&rest args)
  "Display a debug message with the same ARGS arguments as `message'.
The messages are displayed in the *buffer-terminator* buffer."
  `(when buffer-terminator-debug
     (buffer-terminator--insert-message "*buffer-terminator:debug*"
                                        ,(car args) ,@(cdr args))))

(defun buffer-terminator--buffer-visible-p ()
  "Return non-nil if the current buffer or any associated buffer is visible.

This includes visibility in any window on any frame or presence in a tab-bar
tab, so that indirect buffers and associated buffers count as visible if their
base or related buffer is visible."
  (let (result)
    (catch 'visible
      (dolist (buffer (delete-dups
                       (append (list (current-buffer))
                               ;; When the current buffer is a base buffer:
                               ;; include indirect buffers, ensuring visibility
                               ;; checks also consider the indirect buffers'
                               ;; visible states.
                               (unless (buffer-base-buffer)
                                 (seq-filter
                                  (lambda(buf)
                                    (buffer-live-p buf))
                                  buffer-terminator--associated-buffers)))))
        (when (or (get-buffer-window buffer t)
                  ;; Tab-bar
                  (and (bound-and-true-p tab-bar-mode)
                       (fboundp 'tab-bar-get-buffer-tab)
                       (funcall 'tab-bar-get-buffer-tab buffer t nil)))
          (setq result t)
          (throw 'visible t))))
    result))

(defun buffer-terminator--special-buffer-p ()
  "Return non-nil if the current buffer is a special buffer."
  (let ((buffer-name (buffer-name)))
    (when buffer-name
      (and (or (string-prefix-p " " buffer-name)
               (and (string-prefix-p "*" buffer-name)
                    (string-suffix-p "*" buffer-name))
               (derived-mode-p 'special-mode)
               (minibufferp (current-buffer)))
           (not (buffer-file-name (buffer-base-buffer)))))))

(defun buffer-terminator--match-buffer-inactive-p ()
  "Return non-nil when BUFFER is inactive."
  (let ((last-display-time (buffer-terminator--last-display-time)))
    (cond
     ((not last-display-time)
      t)

     ((>= last-display-time buffer-terminator-inactivity-timeout)
      t))))

(defun buffer-terminator--match-buffer-property-p (property)
  "Return non-nil when the buffer property of the current buffer is PROPERTY."
  (cond
   ((eq property 'special)
    (when (buffer-terminator--special-buffer-p)
      t))

   ((eq property 'process)
    (when (get-buffer-process (current-buffer))
      t))

   ((eq property 'visible)
    (when (buffer-terminator--buffer-visible-p)
      t))

   ((eq property 'active)
    (unless (buffer-terminator--match-buffer-inactive-p)
      t))

   ((eq property 'inactive)
    (when (buffer-terminator--match-buffer-inactive-p)
      t))

   ((eq property 'file)
    (when (buffer-file-name (buffer-base-buffer))
      t))

   (t
    (error "Invalid buffer-terminator-rules-alist value: '%s' (%s)"
           property (type-of property))
    nil)))

(defun buffer-terminator--match-buffer-p (match-names)
  "Check if the name if the current buffer matches MATCH-NAMES.
MATCH-NAMES can be a string for a single exact match or a list of strings.
Returns non-nil if the buffer name matches any of the names."
  (let ((buffer-name (buffer-name)))
    (when buffer-name
      (cond
       ((stringp match-names)
        (string-equal buffer-name match-names))

       ((listp match-names)
        (cl-find buffer-name match-names :test #'string-equal))

       (t
        (error "Invalid buffer-terminator-rules-alist value: '%s' -> '%s'"
               buffer-name match-names))))))

(defun buffer-terminator--match-buffer-regexp-p (match-names-regexp)
  "Check if the name of the current buffer matches one of MATCH-NAMES-REGEXP.
MATCH-NAMES-REGEXP can be a string for a single regexp or a list of regexps.
Returns non-nil if BUFFER-NAME matches any of the regexps."
  (let ((buffer-name (buffer-name)))
    (when buffer-name
      (cond
       ((stringp match-names-regexp)
        (string-match match-names-regexp buffer-name))

       ((listp match-names-regexp)
        (cl-find buffer-name
                 match-names-regexp
                 :test (lambda (buffer-name regex)
                         (string-match regex buffer-name))))

       (t
        (error "Invalid buffer-terminator-rules-alist value: '%s'"
               match-names-regexp))))))

(defun buffer-terminator--match-buffer-major-mode-p (major-modes)
  "Return non-nil if the major mode of the current buffer is MAJOR-MODES.
MAJOR-MODES is a list of major mode symbols."
  (cond
   ((symbolp major-modes)
    (eq major-mode major-modes))

   ((listp major-modes)
    (when (cl-find major-mode major-modes :test #'eq)
      t))

   (t
    (error "Invalid buffer-terminator-rules-alist value: '%s'"
           major-modes))))

(defun buffer-terminator--process-rule (rule value)
  "Run the rule RULE with the value VALUE."

  (cond
   ((not (symbolp rule))
    (error "Invalid buffer-terminator-rules-alist key: '%s' -> '%s'"
           rule value)
    nil)

   ((and (eq rule 'call-function) (functionp value))
    (funcall value))

   ((eq rule 'return)
    value)

   ((eq rule 'keep-buffer-property)
    (if (buffer-terminator--match-buffer-property-p value) :keep nil))

   ((eq rule 'kill-buffer-property)
    (if (buffer-terminator--match-buffer-property-p value) :kill nil))

   ((eq rule 'keep-buffer-name)
    (if (buffer-terminator--match-buffer-p value) :keep nil))

   ((eq rule 'kill-buffer-name)
    (if (buffer-terminator--match-buffer-p value) :kill nil))

   ((eq rule 'keep-buffer-name-regexp)
    (if (buffer-terminator--match-buffer-regexp-p value) :keep nil))

   ((eq rule 'kill-buffer-name-regexp)
    (if (buffer-terminator--match-buffer-regexp-p value) :kill nil))

   ((eq rule 'keep-buffer-major-modes)
    (if (buffer-terminator--match-buffer-major-mode-p value) :keep nil))

   ((eq rule 'kill-buffer-major-modes)
    (if (buffer-terminator--match-buffer-major-mode-p value) :kill nil))

   (t
    (error
     "Invalid buffer-terminator-rules-alist entry: '%s' -> '%s'" rule value)
    nil)))

(defun buffer-terminator--process-buffer-rules (rules)
  "Process RULES.
RULES should be a list of conditions formatted similarly to
`buffer-terminator-rules-alist'. If RULES is nil, the default rules defined in
`buffer-terminator-rules-alist' are used.
Return :kill or :keep or nil."
  (catch 'result
    (dolist (rule rules)
      (let ((key (car rule))
            (value (cdr rule)))
        (let ((result (buffer-terminator--process-rule key value)))
          (buffer-terminator--debug-message "RULE RESULT: [%s] %s(%s) = %s"
                                            (buffer-name)
                                            key value result)
          (when result
            (throw 'result result)))))
    ;; Return nil if no rule produces a result
    nil))

(defun buffer-terminator--update-buffer-last-view-time ()
  "Update the last view time for the current buffer."
  (setq-local buffer-terminator--buffer-activity-time (current-time)))

(defun buffer-terminator--last-display-time ()
  "Return the time in seconds since current buffer was last displayed.
Return nil when if buffer has never been displayed."
  (let* (;; buffer-display-time is not reliable enough.
         ;; The `buffer-terminator--buffer-activity-time' is updated using
         ;; `window-state-change-hook', which is more reliable because it is
         ;; triggered on changes related to the state of the window, including
         ;; buffer changes and resizing.
         (bt-buffer-display-time
          (when (bound-and-true-p buffer-terminator--buffer-activity-time)
            (float-time (time-subtract
                         (current-time)
                         buffer-terminator--buffer-activity-time))))
         (built-in-buffer-display-time
          (when (bound-and-true-p buffer-display-time)
            (float-time (time-subtract (current-time)
                                       buffer-display-time)))))
    (cond
     ((and bt-buffer-display-time built-in-buffer-display-time)
      (if (> bt-buffer-display-time built-in-buffer-display-time)
          built-in-buffer-display-time
        bt-buffer-display-time))

     (bt-buffer-display-time
      bt-buffer-display-time)

     (built-in-buffer-display-time
      built-in-buffer-display-time))))

(defvar buffer-terminator-display-warnings t)

(defun buffer-terminator--warn-obsolete-vars ()
  "Warn the user if any obsolete `buffer-terminator' variables are non-nil."
  (when buffer-terminator-display-warnings
    (dolist (var '(buffer-terminator-keep-buffers-with-process
                   buffer-terminator-keep-special-buffers
                   buffer-terminator-keep-visible-buffers))
      (when (and (boundp var) (not (symbol-value var)))
        (buffer-terminator--message
         (concat "WARNING: The variable `%s` is obsolete. "
                 "Use `buffer-terminator-rules-alist` instead. "
                 "(The obsolete variable will be removed in future versions.)")
         var))))
  (when buffer-terminator-display-warnings
    (dolist (var '(buffer-terminator-keep-buffer-names
                   buffer-terminator-keep-buffer-names-regexps
                   buffer-terminator-keep-file-visiting-buffers
                   buffer-terminator-predicate
                   buffer-terminator-keep-major-modes
                   buffer-terminator-kill-buffer-names
                   buffer-terminator-kill-buffer-names-regexps
                   buffer-terminator-kill-special-buffer-names
                   buffer-terminator-kill-special-buffer-names-regexps))
      (when (and (boundp var) (symbol-value var))
        (buffer-terminator--message
         (concat "WARNING: The variable `%s` is obsolete. "
                 "Use `buffer-terminator-rules-alist` instead. "
                 "(The obsolete variable will be removed in future versions.)")
         var)))))

(defun buffer-terminator--kill-buffer (buffer)
  "Kill BUFFER if it is live.

BUFFER is the buffer to be terminated. If BUFFER is live, it is killed without
prompting the user. If the buffer is successfully killed, a debug message is
logged. If `buffer-terminator-verbose' is non-nil, an additional message is
displayed to the user.

Returns non-nil if the buffer was successfully killed, otherwise nil."
  (when (buffer-live-p buffer)
    (let ((buffer-name (buffer-name buffer))
          (kill-buffer-query-functions nil)
          result)
      (setq result (let ((inhibit-message (not buffer-terminator-verbose)))
                     (kill-buffer buffer)))

      (when result
        (buffer-terminator--debug-message "Terminated the buffer: '%s'"
                                          buffer-name)
        (when buffer-terminator-verbose
          (buffer-terminator--message "Terminated the buffer: '%s'"
                                      buffer-name)))
      result)))

(defun buffer-terminator-apply-rules (&optional rules buffers)
  "Evaluate buffer termination rules and apply them to all buffers.

The function iterates over all existing buffers, evaluating each against the
specified RULES. Buffers matching the conditions in RULES are killed or
retained.

RULES should be a list of conditions formatted similarly to
`buffer-terminator-rules-alist'. If RULES is nil, the default rules defined in
`buffer-terminator-rules-alist' are used.

BUFFERS is a list of buffers or a single buffer to process. If BUFFERS is nil,
all buffers are processed by default."
  (unless rules
    (setq rules buffer-terminator-rules-alist))

  (if (not buffers)
      (setq buffers (buffer-list))
    (cond
     ((bufferp buffers)
      (setq buffers (list buffers)))

     ((not (listp buffers))
      (error "The BUFFERS parameter must be a list of buffers or a single buffer"))))

  (setq buffers (let ((result nil))
                  (dolist (buffer buffers)
                    (when (buffer-live-p buffer)
                      (with-current-buffer buffer
                        (setq buffer-terminator--associated-buffers nil))
                      (push buffer result)))
                  result))

  (let ((result nil)
        (window-buffer (window-buffer)))
    ;; Generate associated buffers
    (dolist (buffer buffers)
      (when buffer
        (with-current-buffer buffer
          (let ((overlay-buffer
                 (cond ((bound-and-true-p edit-indirect--overlay)
                        (overlay-buffer edit-indirect--overlay))

                       ((bound-and-true-p org-src--overlay)
                        (overlay-buffer org-src--overlay)))))

            (cond
             ;; Overlay buffers (Org-src or markdown-mode edit-indirect)
             (overlay-buffer
              (when (buffer-live-p overlay-buffer)
                (push overlay-buffer buffer-terminator--associated-buffers)
                (with-current-buffer overlay-buffer
                  (push buffer buffer-terminator--associated-buffers))))

             ;; Indirect buffers
             (t
              (let ((base-buffer (buffer-base-buffer buffer)))
                (when (and base-buffer
                           (buffer-live-p base-buffer))
                  ;; Indirect buffer
                  (setq buffer-terminator--associated-buffers (list base-buffer))

                  ;; Original buffer
                  (with-current-buffer base-buffer
                    (push buffer buffer-terminator--associated-buffers))))))))))

    ;; Apply rules
    (dolist (buffer buffers)
      (when (buffer-live-p buffer)  ; Always check because buffers can be killed
        (let* ((buffer-name (buffer-name buffer))
               (buffer-info (list (cons 'buffer-name buffer-name)))
               (kill-buffer
                (let ((decision nil))
                  (with-current-buffer buffer
                    (push (cons 'major-mode major-mode) buffer-info)

                    ;; When debug is enabled, always keep the debug buffer
                    (when (and buffer-terminator-debug
                               (string= buffer-name
                                        "*buffer-terminator:debug*"))
                      (setq decision :keep))

                    ;; Pre-flight checks: Modified buffers
                    (unless decision
                      (let* ((base-buffer (or (buffer-base-buffer)
                                              (current-buffer)))
                             (file-name (buffer-file-name base-buffer)))
                        (when (and (not file-name)
                                   (derived-mode-p 'dired-mode))
                          (setq file-name default-directory))

                        (when file-name
                          (push (cons 'file-name file-name) buffer-info))

                        (when (and file-name (buffer-modified-p buffer))
                          (setq decision :keep))))

                    ;; Pre-flight checks: Current buffer
                    (unless decision
                      (when (eq window-buffer buffer)
                        (setq decision :keep)))

                    ;; Rules
                    (when (and (not decision) rules)
                      (setq decision
                            (buffer-terminator--process-buffer-rules rules)))

                    ;; Final decision
                    (eq decision :kill)))))
          (when kill-buffer
            (buffer-terminator--kill-buffer buffer)
            (push buffer-info result)))))
    result))

(defun buffer-terminator--timer-apply-rules ()
  "Apply `buffer-terminator' rules at a specific interval."
  (run-hooks 'buffer-terminator-before-hook)
  (buffer-terminator-apply-rules)
  (run-hooks 'buffer-terminator-after-hook))

(defalias 'buffer-terminator-execute-rules 'buffer-terminator-apply-rules
  "Obsolete. Renamed to `buffer-terminator-apply-rules'.")
(make-obsolete 'buffer-terminator-execute-rules 'buffer-terminator-apply-rules
               "1.1.1")

;;; Obsolete functions

(defun buffer-terminator-find-dired-parent (&optional _kill-buffer)
  "This function is obsolete."
  t)
(make-obsolete 'buffer-terminator-find-dired-parent 'ignore "1.1.0")

(defun buffer-terminator-find-dired-parent-kill-buffer ()
  "This function is obsolete."
  t)
(make-obsolete 'buffer-terminator-find-dired-parent-kill-buffer 'ignore "1.1.0")

(defun buffer-terminator-kill-non-visible-buffers ()
  "This function is obsolete."
  t)
(make-obsolete 'buffer-terminator-kill-non-visible-buffers 'ignore "1.1.0")

;;; Mode

;;;###autoload
(define-minor-mode buffer-terminator-mode
  "Toggle Buffer Terminator mode.
When enabled, this mode automatically kills buffers that have been inactive
and not visible based on a defined timeout."
  :global t
  :lighter " BufTermi"
  :group 'buffer-terminator
  (if buffer-terminator-mode
      ;; Enable
      (progn
        (buffer-terminator--debug-message "Start: buffer-terminator-mode")
        (buffer-terminator--warn-obsolete-vars)
        ;; Add hooks and timers
        (add-hook 'window-state-change-hook
                  #'buffer-terminator--update-buffer-last-view-time)
        (buffer-terminator--cancel-timer)
        (buffer-terminator--start-timer buffer-terminator-interval))
    ;; Disable
    (remove-hook 'window-state-change-hook
                 #'buffer-terminator--update-buffer-last-view-time)
    (buffer-terminator--cancel-timer)))

(provide 'buffer-terminator)
;;; buffer-terminator.el ends here
