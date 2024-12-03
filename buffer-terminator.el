;;; buffer-terminator.el --- Terminate inactive buffers automatically  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 0.9.9
;; URL: https://github.com/jamescherti/buffer-terminator.el
;; Keywords: convenience
;; Package-Requires: ((emacs "24.1"))
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
;; Terminate inactive buffers automatically

;;; Code:

(defgroup buffer-terminator nil
  "Terminate inactive buffers automatically"
  :group 'buffer-terminator
  :prefix "buffer-terminator-"
  :link '(url-link
          :tag "Github"
          "https://github.com/jamescherti/buffer-terminator.el"))

(defun buffer-terminator--message (&rest args)
  "Display a message with '[buffer-terminator]' prepended.
The message is formatted with the provided arguments ARGS."
  (apply #'message (concat "[buffer-terminator] " (car args)) (cdr args)))

(defun buffer-terminator--warning (&rest args)
  "Display a warning message with '[buffer-terminator] Warning: ' prepended.
The message is formatted with the provided arguments ARGS."
  (apply #'message (concat "[buffer-terminator] Warning: " (car args)) (cdr args)))

;;;###autoload
(define-minor-mode buffer-terminator-mode
  "Toggle `buffer-terminator-mode'."
  :global t
  :lighter " buffer-terminator"
  :group 'buffer-terminator
  (if buffer-terminator-mode
      t
    t))

(provide 'buffer-terminator)
;;; buffer-terminator.el ends here
