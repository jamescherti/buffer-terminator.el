;;; buffer-terminator.el --- Terminate/Kill Inactive Buffers Automatically  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1.0.3
;; URL: https://github.com/jamescherti/buffer-terminator.el
;; Keywords: convenience
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5"))
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
;; The buffer-terminator Emacs package automatically terminates inactive buffers
;; to help maintain a clean and efficient workspace.
;;
;; It provides configurable options to determine which buffers to keep, a
;; timeout for inactivity, and periodic cleanup intervals.
;;
;;
;; Links:
;; ------
;; - buffer-terminator.el @GitHub:
;;   https://github.com/jamescherti/buffer-terminator.el

;;; Code:

(require 'dired)
(require 'cl-lib)

;;; Customizations

(defgroup buffer-terminator nil
  "Terminate inactive buffers automatically."
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

(defcustom buffer-terminator-keep-major-modes nil
  "List of major-modes. Buffers with these major mode are never killed.
This is useful for keeping buffers associated with specific types of
files (e.g., Dired buffers, specific modes, or configuration files) from being
killed automatically.

To keep buffers for specific major modes, set this variable to a list of mode
symbols. For example: \\='(org-mode my-special-mode)

It is recommended to configure this carefully to avoid unintentionally keeping
too many buffers alive."
  :type '(repeat symbol)
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
                        'buffer-terminator-kill-inactive-buffers)))

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

(defcustom buffer-terminator-keep-buffers-with-process t
  "When non-nil, do not kill buffers associated with running processes.
Process buffers are buffers where an active process is running. It is generally
discouraged to set this to nil, as doing so may result in the termination of
such buffers, potentially disrupting active processes."
  :type 'boolean
  :group 'buffer-terminator)

(defcustom buffer-terminator-keep-visible-buffers t
  "When non-nil, `buffer-terminator' will not kill visible buffers.
Visible buffers are those currently displayed in any window.
It is generally discouraged to set this to nil, as doing so may result
in the termination of visible buffers, except for the currently active
buffer in the selected window."
  :type 'boolean
  :group 'buffer-terminator)

(defcustom buffer-terminator-keep-file-visiting-buffers nil
  "When non-nil, `buffer-terminator' will not kill buffers visiting files.
File-visiting buffers are those associated with files, whether the file is
modified or not. It is generally recommended to keep this variable set to t to
avoid terminating buffers that are associated with files you are working on."
  :type 'boolean
  :group 'buffer-terminator)

(defcustom buffer-terminator-predicate nil
  "Function to decide the fate of a buffer.
This function is called (with no parameters) from the buffer.

This function can return one of the following values:

:kill    Indicates that the buffer should be killed.
:keep    Indicates that the buffer should be kept.
nil      Let Buffer Terminator decide. It indicates that the default
         procedure should be followed, using other predicates such as those
         influenced by `buffer-terminator-keep-*` variables.

This function has precedence over all other predicates."
  :group 'buffer-terminator
  :type '(choice (const nil)
                 (function)))

;; DO NOT modify `buffer-terminator-keep-visible-buffers' unless you know what
;; you are doing. If you decide to set it to nil, make sure to update
;; `buffer-terminator-keep-buffer-names' or
;; `buffer-terminator-keep-buffer-names-regexps' to preserve important special
;; buffers.
(defcustom buffer-terminator-keep-special-buffers t
  "If non-nil, `buffer-terminator' will never kill special buffers.
It is generally NOT recommended to set this to nil.
If you choose to set it to nil, ensure that the special buffers you want to keep
are added to `buffer-terminator-keep-buffer-names' and
`buffer-terminator-keep-buffer-names-regexps'."
  :type 'boolean
  :group 'buffer-terminator)

(defcustom buffer-terminator-rules-alist nil
  "Rules for processing buffers.
Each rule is a cons cell where the key is a symbol indicating the rule type, and
the value is either a string or a list of strings."
  :type '(repeat (cons (choice (const :keep-buffer-name)
                               (const :kill-buffer-name)
                               (const :keep-buffer-name-regexp)
                               (const :kill-buffer-name-regexp))
                       (choice string
                               (repeat string))))
  :group 'buffer-terminator)

;;; Obsolete

(defcustom buffer-terminator-keep-buffer-names nil
  "List of buffer names that will never be killed."
  :type '(repeat (string :tag "Buffer Name"))
  :group 'buffer-terminator)

(defcustom buffer-terminator-keep-buffer-names-regexps nil
  "List of regexps that match buffer names that will never be killed."
  :type '(repeat regexp)
  :group 'buffer-terminator)

(defcustom buffer-terminator-kill-buffer-names nil
  "List of buffer names that can be killed.
This setting allows specific buffers to be terminated, overriding the general
`buffer-terminator-keep-buffer-names' and
`buffer-terminator-keep-buffer-names-regexps' . This is useful for excluding
certain special buffers from being preserved when inactive."
  :type '(repeat (string :tag "Buffer Name"))
  :group 'buffer-terminator)

(defcustom buffer-terminator-kill-buffer-names-regexps nil
  "List of regex patterns matching buffer names that can be killed.
This setting allows specific buffers to be terminated, overriding the general
`buffer-terminator-keep-buffer-names' and
`buffer-terminator-keep-buffer-names-regexps' . This is useful for excluding
certain special buffers from being preserved when inactive."
  :type '(repeat regexp)
  :group 'buffer-terminator)

(defvar buffer-terminator-kill-special-buffer-names nil
  "List of special buffer names that can be killed.
This variable is obsolete.")
(defvar buffer-terminator-kill-special-buffer-names-regexps nil
  "List of regex patterns matching special buffer names that can be killed.
This variable is obsolete.")

(make-obsolete-variable 'buffer-terminator-keep-buffer-names
                        'buffer-terminator-rules-alist
                        "1.0.4")

(make-obsolete-variable 'buffer-terminator-keep-buffer-names-regexps
                        'buffer-terminator-rules-alist
                        "1.0.4")

(make-obsolete-variable 'buffer-terminator-kill-buffer-names
                        'buffer-terminator-rules-alist
                        "1.0.4")

(make-obsolete-variable 'buffer-terminator-kill-buffer-names-regexps
                        'buffer-terminator-rules-alist
                        "1.0.4")

(make-obsolete-variable 'buffer-terminator-kill-special-buffer-names
                        'buffer-terminator-rules-alist
                        "1.0.3")

(make-obsolete-variable 'buffer-terminator-kill-special-buffer-names-regexps
                        'buffer-terminator-rules-alist
                        "1.0.3")

;;; Functions

(defun buffer-terminator--message (&rest args)
  "Display a message with '[buffer-terminator]' prepended.
The message is formatted with the provided arguments ARGS."
  (apply #'message (concat "[buffer-terminator] " (car args)) (cdr args)))

(defun buffer-terminator--buffer-visible-p (buffer)
  "Return non-nil if BUFFER is visible in any window on any frame."
  (when buffer
    (or (get-buffer-window buffer 0)
        ;; Tab-bar
        (and (bound-and-true-p tab-bar-mode)
             (fboundp 'tab-bar-get-buffer-tab)
             (funcall 'tab-bar-get-buffer-tab buffer t nil)))))

(defun buffer-terminator--special-buffer-p (&optional buffer)
  "Return non-nil if BUFFER is a special buffer."
  (unless buffer
    (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (let ((buffer-name (buffer-name)))
      (or (string-prefix-p " " buffer-name)
          (and (string-prefix-p "*" buffer-name)
               (string-suffix-p "*" buffer-name))
          (derived-mode-p 'special-mode)))))

(defun buffer-terminator--process-rule (rule value)
  "Run the rule RULE with the value VALUE."
  (let ((buffer-name (buffer-name)))
    (cond
     ((not (symbolp rule))
      (buffer-terminator--message
       "[Warning] Invalid buffer-terminator-rules-alist key: '%s' -> '%s'"
       rule value)
      nil)

     ((not (or (listp value) (stringp value)))
      (buffer-terminator--message
       "[Warning] Invalid buffer-terminator-rules-alist value: '%s' -> '%s'"
       rule value)
      nil)

     ((eq rule 'keep-buffer-name)
      (if (buffer-terminator--match-buffer-p buffer-name value)
          :keep
        nil))

     ((eq rule 'kill-buffer-name)
      (if (buffer-terminator--match-buffer-p buffer-name value)
          :kill
        nil))

     ((eq rule 'keep-buffer-name-regexp)
      (if (buffer-terminator--match-buffer-regexp-p buffer-name value)
          :keep
        nil))

     ((eq rule 'kill-buffer-name-regexp)
      (if (buffer-terminator--match-buffer-regexp-p buffer-name value)
          :kill
        nil))

     (t
      (buffer-terminator--message
       "[Warning] Invalid buffer-terminator-rules-alist entry: '%s' -> '%s'"
       rule value)
      nil)

     ;; TODO: Special buffers.
     ;; (buffer-terminator--match-buffer-p
     ;;  buffer-name
     ;;  buffer-terminator-kill-special-buffer-names)
     ;; (buffer-terminator--match-buffer-regexp-p
     ;;  buffer-name
     ;;  buffer-terminator-kill-special-buffer-names-regexps)
     )))

(defun buffer-terminator--process-buffer-rules ()
  "Process `buffer-terminator-rules-alist'.
Return :kill or :keep or nil."
  (catch 'result
    (dolist (rule buffer-terminator-rules-alist)
      (let ((key (car rule))
            (value (cdr rule)))
        (let ((result (buffer-terminator--process-rule key value)))
          (when result
            (throw 'result result)))))
    ;; Return nil if no rule produces a result
    nil))

(defun buffer-terminator--match-buffer-p (buffer-name match-names)
  "Check if BUFFER-NAME matches one of the names in MATCH-NAMES.
MATCH-NAMES can be a string for a single exact match or a list of strings.
Returns non-nil if BUFFER-NAME matches any of the names."
  (when buffer-name
    (cond
     ((stringp match-names)
      (string-equal buffer-name match-names))

     ((listp match-names)
      (cl-find buffer-name
               match-names
               :test #'string-equal)))))

(defun buffer-terminator--match-buffer-regexp-p (buffer-name match-names-regexp)
  "Check if BUFFER-NAME is matched by one or more regexps in MATCH-NAMES-REGEXP.
MATCH-NAMES-REGEXP can be a string for a single regexp or a list of regexps.
Returns non-nil if BUFFER-NAME matches any of the regexps."
  (when buffer-name
    (cond
     ((stringp match-names-regexp)
      (string-match match-names-regexp buffer-name))

     ((listp match-names-regexp)
      (cl-find buffer-name
               match-names-regexp
               :test (lambda (buffer-name regex)
                       (string-match regex buffer-name)))))))

(defvar-local buffer-terminator--buffer-display-time nil)
(defvar buffer-terminator--disable-buffer-display-time-update nil)

(defun buffer-terminator--keep-buffer-p (buffer &optional ignore-buffers)
  "Check if BUFFER should be excluded from being automatically killed.
Returns non-nil if BUFFER should be kept.
IGNORE-BUFFERS is a list of buffers to ignore."
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((buffer-name (buffer-name))
              (special-buffer (buffer-terminator--special-buffer-p buffer)))
          (and
           ;; Terminate any buffer
           (not (or (buffer-terminator--match-buffer-p
                     buffer-name
                     buffer-terminator-kill-buffer-names)

                    (buffer-terminator--match-buffer-regexp-p
                     buffer-name
                     buffer-terminator-kill-buffer-names-regexps)))
           ;; Terminate special buffers
           (and
            (if special-buffer
                (not (or (buffer-terminator--match-buffer-p
                          buffer-name
                          buffer-terminator-kill-special-buffer-names)
                         (buffer-terminator--match-buffer-regexp-p
                          buffer-name
                          buffer-terminator-kill-special-buffer-names-regexps)))
              t))
           (or
            ;; Keep buffer names or regular expressions
            (buffer-terminator--match-buffer-p
             buffer-name buffer-terminator-keep-buffer-names)

            (buffer-terminator--match-buffer-regexp-p
             buffer-name buffer-terminator-keep-buffer-names-regexps)

            ;; Special buffers
            (and buffer-terminator-keep-special-buffers
                 special-buffer)

            ;; File visiting buffer
            (and (buffer-file-name (buffer-base-buffer))
                 (or buffer-terminator-keep-file-visiting-buffers
                     (buffer-modified-p)))

            (and buffer-terminator-keep-major-modes
                 (cl-find major-mode
                          buffer-terminator-keep-major-modes
                          :test 'eq))

            ;; Keep ignored buffers
            (and ignore-buffers
                 (memq buffer ignore-buffers))

            ;; Keep visible buffers
            (and buffer-terminator-keep-visible-buffers
                 (buffer-terminator--buffer-visible-p buffer))

            ;; Keep buffers that contain processes
            (and buffer-terminator-keep-buffers-with-process
                 (get-buffer-process buffer))))))))

(defun buffer-terminator--update-buffer-last-view-time ()
  "Update the last view time for the current buffer."
  (unless buffer-terminator--disable-buffer-display-time-update
    (setq-local buffer-terminator--buffer-display-time (current-time))))

(defun buffer-terminator--last-display-time (buffer)
  "Return the time in seconds since BUFFER was last displayed.
Return nil when if buffer has never been displayed."
  (if (eq buffer (current-buffer))
      0
    (with-current-buffer buffer
      (let (;; buffer-last-time is is updated on `window-state-change-hook':
            ;; on changes related to the state of the window, including buffer
            ;; changes and resizing.
            (buffer-last-view
             (cond ((bound-and-true-p buffer-terminator--buffer-display-time)
                    (float-time (time-subtract
                                 (current-time)
                                 buffer-terminator--buffer-display-time)))

                   ;; (buffer-display-time
                   ;;  (float-time (time-subtract (current-time)
                   ;;                             buffer-display-time)))
                   )))
        buffer-last-view))))

(defun buffer-terminator--buffer-inactive-p (buffer)
  "Return non-nil when BUFFER is inactive."
  (when buffer
    (let ((last-display-time (buffer-terminator--last-display-time buffer)))
      (cond
       ((not last-display-time)
        t)

       ((>= last-display-time buffer-terminator-inactivity-timeout)
        t)))))

(defun buffer-terminator--kill-buffer-maybe (buffer)
  "Kill BUFFER if it is not visible and not special."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and
             (let ((decision nil))
               ;; Predicate
               (when (and buffer-terminator-predicate
                          (not decision))
                 (cond
                  ;; Function
                  ((functionp buffer-terminator-predicate)
                   (setq decision (funcall buffer-terminator-predicate)))

                  ;; Error: Predicate is not a function
                  (t
                   (buffer-terminator--message
                    (concat "Warning: 'buffer-terminator-predicate' is "
                            "not a function.")))))

               ;; Rules
               (when (and buffer-terminator-rules-alist
                          (not decision))
                 (setq decision (buffer-terminator--process-buffer-rules)))

               ;; Let Buffer-Terminator decide
               (unless decision
                 (setq decision
                       (if (buffer-terminator--keep-buffer-p buffer)
                           :keep
                         :kill)))

               ;; Final decision
               (if (eq decision :kill)
                   t
                 nil)))
        (let ((buffer-name (buffer-name buffer)))
          (ignore-errors
            (let ((kill-buffer-query-functions '()))
              (kill-buffer buffer)))
          (when buffer-terminator-verbose
            (buffer-terminator--message "Terminated the buffer: '%s'"
                                        buffer-name))
          t)))))

;;; Helper functions

(defun buffer-terminator-kill-inactive-buffers ()
  "Kill all buffers that are inactive and not visible."
  (mapc #'(lambda(buffer)
            (when (buffer-terminator--buffer-inactive-p buffer)
              (buffer-terminator--kill-buffer-maybe buffer)))
        (buffer-list)))

(defun buffer-terminator-kill-non-visible-buffers ()
  "Kill all non visible buffers."
  (let ((buffer-killed nil))
    (mapc #'(lambda(buffer)
              (when (buffer-terminator--kill-buffer-maybe buffer)
                (push (buffer-name) buffer-killed)))
          (buffer-list))
    ;; Return the list of killed buffer names
    buffer-killed))

(defalias 'buffer-terminator-kill-all-non-visible-buffers
  'buffer-terminator-kill-non-visible-buffers
  "Renamed to `buffer-terminator-kill-non-visible-buffers'.")
(make-obsolete 'buffer-terminator-kill-all-non-visible-buffers
               'buffer-terminator-kill-non-visible-buffers
               "1.0.3")

(defun buffer-terminator-find-dired-parent (&optional kill-buffer)
  "Open the current directory in a `dired' buffer and select the current file.
The buffer is killed when KILL-BUFFER is set to t."
  (let* ((buffer (or (buffer-base-buffer)
                     (current-buffer)))
         (file-name (buffer-file-name buffer)))
    (if file-name
        (progn
          (dired (file-name-directory file-name))
          (dired-goto-file file-name))
      (dired default-directory))

    (when kill-buffer
      (buffer-terminator--kill-buffer-maybe buffer))))

(defun buffer-terminator-find-dired-parent-kill-buffer ()
  "Open the current directory in a `dired' buffer and select the current file."
  (buffer-terminator-find-dired-parent t))

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
        ;; Initialize the last view time for all buffers
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (unless buffer-terminator--buffer-display-time
              (buffer-terminator--update-buffer-last-view-time))))
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
