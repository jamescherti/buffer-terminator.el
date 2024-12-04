;;; buffer-terminator.el --- Terminate/Kill Inactive Buffers Automatically  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1.0.0
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

(defgroup buffer-terminator nil
  "Terminate inactive buffers automatically."
  :group 'buffer-terminator
  :prefix "buffer-terminator-"
  :link '(url-link
          :tag "Github"
          "https://github.com/jamescherti/buffer-terminator.el"))

(defcustom buffer-terminator-inactivity-timeout (* 30 60)
  "Time in seconds before a buffer is considered inactive.
Default: 30 minutes."
  :type 'integer
  :group 'buffer-terminator)

(defcustom buffer-terminator-keep-buffers-with-process nil
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

(defcustom buffer-terminator-verbose nil
  "Enable verbose mode to log when a buffer is automatically killed."
  :type 'boolean
  :group 'buffer-terminator)

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

;; THIS LIST IS IGNORED BY DEFAULT. It is only useful when
;; `buffer-terminator-keep-special-buffers' is set to nil:
(defcustom buffer-terminator-keep-buffer-names
  '("*scratch*"
    "*Messages*"
    "*Compile-Log*"
    " *eldoc*"
    " *code-conversion-work*"
    "*compile-angel:debug*"
    " *Compiler Input*"
    " *jka-compr-wr-temp*"
    " *consult-async*"
    " *consult-async-stderr*"
    "*Async-native-compile-log*")
  "List of buffer names that will never be killed."
  :type '(repeat (string :tag "Buffer Name")))

;; THIS LIST OF REGEXP IS IGNORED BY DEFAULT. It is only useful when
;; `buffer-terminator-keep-special-buffers' is set to nil:
(defcustom buffer-terminator-keep-buffer-names-regexps
  '("\\` \\*Minibuf-[0-9]+\\*\\'"
    "\\` \\*stderr of "  ; ’ *stderr of elisp-flymake-byte-compile*’
    "\\` \\*eldoc for "  ; ’ *eldoc for NAME, BUFFER_NAME*’
    "\\` \\*flymake-.*\\*\\'"
    "\\` \\?\\*EGLOT .*\\*\\'"
    "\\` \\*markdown-code-fontification:.*\\*\\'"
    "\\` \\*org-src-fontification:.*\\*\\'"
    "\\` \\*Echo Area [0-9]+\\*\\'")
  "List of regexps that match buffer names that will never be killed."
  :type '(repeat
          (choice (regexp :tag "Regexp matching Buffer Name")
                  (function :tag "Predicate function"))))

(defcustom buffer-terminator-keep-file-visiting-buffers nil
  "When non-nil, `buffer-terminator' will not kill buffers visiting files.
File-visiting buffers are those associated with files, whether the file is
modified or not. It is generally recommended to keep this variable set to t to
avoid terminating buffers that are associated with files you are working on."
  :type 'boolean
  :group 'buffer-terminator)

(defcustom buffer-terminator-keep-buffers-with-major-modes nil
  "List of major-modes. Buffers with these major mode are never killed.
This is useful for keeping buffers associated with specific types of
files (e.g., Dired buffers, source code, or configuration files) from being
killed automatically.

To keep buffers for specific major modes, set this variable to
a list of mode symbols.
For example: \\='(org-mode my-special-mode)

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
        (run-with-timer
         seconds
         seconds
         'buffer-terminator--kill-all-non-visible-timed-out-buffers)))

(defcustom buffer-terminator-interval (* 10 60)
  "Frequency in seconds to repeat the buffer cleanup process.
Default: 10 minutes."
  :type 'integer
  :group 'buffer-terminator
  :set (lambda (symbol value)
         (buffer-terminator--cancel-timer)
         (set-default symbol value)
         (buffer-terminator--start-timer value)))

(defun buffer-terminator--message (&rest args)
  "Display a message with '[buffer-terminator]' prepended.
The message is formatted with the provided arguments ARGS."
  (apply #'message (concat "[buffer-terminator] " (car args)) (cdr args)))

(defun buffer-terminator--buffer-visible-p (buffer)
  "Return non-nil if BUFFER is visible in any window on any frame."
  (when buffer
    (or (get-buffer-window buffer 'visible)
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

(defun buffer-terminator--keep-buffer-p (buffer &optional ignore-buffers)
  "Check if BUFFER should be excluded from being automatically killed.
Returns non-nil if BUFFER should be kept.
IGNORE-BUFFERS is a list of buffers to ignore."
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (or
         ;; File visiting buffer
         (and (buffer-file-name (buffer-base-buffer))
              (or buffer-terminator-keep-file-visiting-buffers
                  (buffer-modified-p)))

         ;; Special buffers
         (and buffer-terminator-keep-special-buffers
              (buffer-terminator--special-buffer-p buffer))

         (and buffer-terminator-keep-buffers-with-major-modes
              (cl-find major-mode
                       buffer-terminator-keep-buffers-with-major-modes
                       :test 'eq))

         ;; Keep ignored buffers
         (and ignore-buffers
              (memq buffer ignore-buffers))

         ;; Keep visible buffers
         (and buffer-terminator-keep-visible-buffers
              (buffer-terminator--buffer-visible-p buffer))

         ;; Keep buffers that contain processes
         (and buffer-terminator-keep-buffers-with-process
              (get-buffer-process buffer))

         ;; Keep buffer names or regular expressions
         (let ((buffer-name (buffer-name)))
           (when buffer-name
             (or
              (cl-find buffer-name
                       buffer-terminator-keep-buffer-names
                       :test #'string-equal)

              ;;(cl-some (lambda (regex)
              ;;           (if (functionp regex)
              ;;               (funcall regex buffer-name)
              ;;             (string-match-p regex buffer-name)))
              ;;         buffer-terminator-keep-buffer-names-regexps)

              ;; Keep ignored buffer regexp
              (cl-find buffer-name
                       buffer-terminator-keep-buffer-names-regexps
                       :test (lambda (buffer-name regex)
                               (if (functionp regex)
                                   (funcall regex buffer-name)
                                 (string-match regex buffer-name)))))))))))

(defvar-local buffer-terminator--buffer-display-time nil)

(defun buffer-terminator--update-buffer-last-view-time ()
  "Update the last view time for the current buffer."
  (setq-local buffer-terminator--buffer-display-time (current-time)))

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
                   ))

            ;; The above replaced buffer-display-time because it is not updated
            ;; when switching to the window or the tab where the buffer's window
            ;; is displayed. It only updates when the user switches to a buffer.
            ;; (buffer-display-time
            ;;  (if (boundp 'buffer-display-time)
            ;;      (float-time (time-subtract (current-time)
            ;;                                 buffer-display-time))))
            )
        buffer-last-view))))

(defun buffer-terminator--buffer-inactive-p (buffer)
  "Return non-nil when BUFFER is inactive."
  (when buffer
    (let ((last-display-time (buffer-terminator--last-display-time buffer)))
      (if (not last-display-time)
          ;; Never displayed = inactive
          t
        (when (and last-display-time
                   (> last-display-time buffer-terminator-inactivity-timeout))
          t)))))

(defun buffer-terminator--kill-all-non-visible-timed-out-buffers
    (&optional special)
  "Kill all buffers that are inactive and not visible.
When SPECIAL is set to t, it also kills special buffers."
  (mapc #'(lambda(buffer)
            (when (buffer-terminator--buffer-inactive-p buffer)
              (buffer-terminator--kill-buffer-if-not-visible buffer special)))
        (buffer-list)))

(defun buffer-terminator--kill-buffer-if-not-visible (buffer
                                                      &optional
                                                      kill-special-buffers)
  "Kill BUFFER if it is not visible and not special.
When KILL-SPECIAL-BUFFERS is set to t, it also kills special buffers."
  ;; TODO improve (ignore-buffers (buffer-terminaltor--find-buffers-to-keep))
  ;; unless (buffer-terminator--keep-buffer-p buffer ignore-buffers)
  (when (and (buffer-live-p buffer)
             (or kill-special-buffers
                 (not (buffer-terminator--keep-buffer-p buffer))))
    (let ((buffer-name (buffer-name buffer)))
      (ignore-errors (let ((kill-buffer-query-functions '()))
                       (kill-buffer buffer)))
      (when buffer-terminator-verbose
        (buffer-terminator--message "Terminated the buffer: '%s'" buffer-name)))
    t))

(defun buffer-terminator-kill-all-non-visible-buffers (&optional
                                                       kill-special-buffers)
  "Kill all buffers that are not visible.
When KILL-SPECIAL-BUFFERS is set to t, it also kills special buffers."
  (let ((buffer-killed nil))
    (mapc #'(lambda(buffer)
              (when (buffer-terminator--kill-buffer-if-not-visible
                     buffer kill-special-buffers)
                (setq buffer-killed t)))
          (buffer-list))
    ;; Return non-nil if at least one buffer was killed.
    buffer-killed))

(defun buffer-terminator-find-dired-parent (&optional kill-buffer)
  "Open the current directory in a `dired' buffer and select the current file.
The buffer is killed when KILL-BUFFER is set to t."
  (let* ((buffer (or (buffer-base-buffer)
                     (current-buffer)))
         (file-name (buffer-file-name buffer)))
    (let ((save-silently t))
      (save-buffer))
    (if file-name
        (progn
          (dired (file-name-directory file-name))
          (dired-goto-file file-name))
      (dired default-directory))

    (when kill-buffer
      (buffer-terminator--kill-buffer-if-not-visible buffer))))

(defun buffer-terminator-find-dired-parent-kill-buffer ()
  "Open the current directory in a `dired' buffer and select the current file."
  (let ((kill-buffer t))
    (buffer-terminator-find-dired-parent kill-buffer)))

;;;###autoload
(define-minor-mode buffer-terminator-mode
  "Toggle Buffer Terminator mode.
When enabled, this mode automatically kills buffers that have been inactive
and not visible based on a defined timeout."
  :global t
  :lighter " BufTermi"
  :group 'buffer-terminator
  (if buffer-terminator-mode
      (progn
        ;; Initialize the last view time for all buffers
        (dolist (buffer (buffer-list))
          ;; Update the last view time for the current buffer
          (with-current-buffer buffer
            (buffer-terminator--update-buffer-last-view-time)))

        (add-hook 'after-change-major-mode-hook
                  #'buffer-terminator--update-buffer-last-view-time)

        ;; window-selection-change-functions: Focuses specifically on when the
        ;; user changes the selected window, which is useful for tracking user
        ;; interactions with window selection. For tracking when a user shows a
        ;; buffer after switching tabs or windows,
        ;; window-selection-change-functions is likely the most appropriate
        ;; hook, as it directly captures changes in window selection.
        ;; (add-hook 'window-selection-change-functions
        ;;           #'buffer-terminator--update-buffer-last-view-time)
        ;;
        ;; window-configuration-change-hook: Captures a broad range of changes
        ;; in window configuration, including splits, resizing, and new windows.
        ;; It’s generally used for layout changes rather than content changes.
        ;; window-state-change-hook is also a good choice if you want to capture
        ;; changes related to the buffer displayed in a window, along with other
        ;; state changes.
        ;; (add-hook 'window-configuration-change-hook
        ;;           #'buffer-terminator--update-buffer-last-view-time)
        ;;
        ;; This one works better: window-state-change-hook: More focused on
        ;; changes related to the state of the window, including buffer changes
        ;; and resizing. window-configuration-change-hook might be used if you
        ;; need to track more general configuration changes.
        ;; - Buffer Visibility Changes: It detects when a buffer becomes visible
        ;;   or hidden as a result of changes in the window state.
        ;; - Window Configuration Changes: It captures events like window
        ;;   splits, deletions, and resizing that might affect buffer
        ;;   visibility.
        ;; - Buffer Switches in Windows: It triggers when the displayed buffer
        ;;   in a window changes, which aligns with the goal of tracking buffer
        ;;   activity.
        (add-hook 'window-state-change-hook
                  #'buffer-terminator--update-buffer-last-view-time)
        (buffer-terminator--cancel-timer)
        (buffer-terminator--start-timer buffer-terminator-interval))
    (remove-hook 'window-state-change-hook
                 #'buffer-terminator--update-buffer-last-view-time)
    (buffer-terminator--cancel-timer)))

(provide 'buffer-terminator)
;;; buffer-terminator.el ends here
