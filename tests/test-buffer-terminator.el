;;; test-buffer-terminator.el --- Test Buffer Terminator -*- lexical-binding: t -*-

;; Copyright (C) 2024 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1.1.0
;; URL: https://github.com/jamescherti/buffer-terminator.el
;; Keywords: convenience
;; Package-Requires: ((emacs "27.1"))
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
;; This file contains unit tests for the buffer terminator.
;;
;; (The buffer-terminator Emacs package automatically terminates inactive
;; buffers to help maintain a clean and efficient workspace.)

;;; Code:

(require 'ert)
(require 'buffer-terminator)

(defvar test-buffer-terminator--special-buffers
  '(" *test-special*" "*test-special2*" " test-special3"))

(defvar test-buffer-terminator--special-mode-buffer "test-special4")

(defvar test-buffer-terminator--file-buffers
  '("test-file1" "test-file2" "test-file3"))

(defvar test-buffer-terminator--file-buffers-with-asterisks
  '("*test-file1*"))

(defvar test-buffer-terminator--file-buffer-modified
  '"test-file-mod1")

(defvar test-buffer-terminator--process-buffer-name
  "test-process-buffer")

(defvar test-buffer-terminator--func-buffer
  "func-buffer")

(defun test-buffer-terminator--check-special-buffers (exist)
  "Check if special buffers still EXIST or not."
  (if exist
      (dolist (buf test-buffer-terminator--special-buffers)
        (should (get-buffer buf)))
    (dolist (buf test-buffer-terminator--special-buffers)
      (should-not (get-buffer buf)))))

(defun test-buffer-terminator--check-special-mode-buffer (exist)
  "Test `special-mode' buffers EXIST."
  (if exist
      (progn (should (get-buffer test-buffer-terminator--special-mode-buffer))
             (should
              (eq (buffer-local-value
                   'major-mode
                   (get-buffer test-buffer-terminator--special-mode-buffer))
                  'special-mode)))
    (should-not (get-buffer test-buffer-terminator--special-mode-buffer))))

(defun test-buffer-terminator--check-file-buffers-with-asterisks (exist)
  "Test file buffers EXIST."
  (if exist
      (dolist (file test-buffer-terminator--file-buffers-with-asterisks)
        (should (get-buffer file)))
    (dolist (file test-buffer-terminator--file-buffers-with-asterisks)
      (should-not (get-buffer file)))))

(defun test-buffer-terminator--check-file-buffers (exist)
  "Test file buffers EXIST."
  (if exist
      (dolist (file test-buffer-terminator--file-buffers)
        (should (get-buffer file)))
    (dolist (file test-buffer-terminator--file-buffers)
      (should-not (get-buffer file)))))

(defun test-buffer-terminator--check-modified-file-buffer (exist)
  "Test the modified file buffer EXIST."
  (if exist
      (should (and (get-buffer test-buffer-terminator--file-buffer-modified)
                   (buffer-modified-p
                    (get-buffer test-buffer-terminator--file-buffer-modified))))
    (should-not (get-buffer test-buffer-terminator--file-buffer-modified))))

(defun test-buffer-terminator--check-process-buffer (exist)
  "Test the process buffer EXIST."
  (if exist
      (should (get-buffer test-buffer-terminator--process-buffer-name))
    (should-not (get-buffer test-buffer-terminator--process-buffer-name))))

(defun test-buffer-terminator--check-func-buffer (exist)
  "Test the func buffer EXIST."
  (if exist
      (should (get-buffer test-buffer-terminator--func-buffer))
    (should-not (get-buffer test-buffer-terminator--func-buffer))))

(defun test-buffer-terminator--check-test-environment ()
  "Create buffers and process for testing purposes."
  ;;----------------------------------------------------------------------------
  ;; Check buffer existence
  ;;----------------------------------------------------------------------------
  (test-buffer-terminator--check-special-buffers t)
  (test-buffer-terminator--check-special-mode-buffer t)
  (test-buffer-terminator--check-file-buffers t)
  (test-buffer-terminator--check-modified-file-buffer t)
  (test-buffer-terminator--check-process-buffer t)
  (test-buffer-terminator--check-func-buffer t))

(defun test-buffer-terminator--create-test-environment ()
  "Create buffers and process for testing purposes."
  ;; Do not ask any question when killing buffers that contain processes
  (setq kill-buffer-query-functions '())
  (setq buffer-terminator-verbose t)

  ;;----------------------------------------------------------------------------
  ;; Delete existing buffers if they exist
  ;;----------------------------------------------------------------------------
  ;; Delete and create existing test-buffer-terminator--special-buffers
  ;; ------------------------------------------
  (dolist (buf test-buffer-terminator--special-buffers)
    (when (get-buffer buf)
      (kill-buffer buf)))
  (dolist (buf test-buffer-terminator--special-buffers)
    (get-buffer-create buf))

  ;; Create (special-mode) buffer
  ;; ----------------------------
  (when (get-buffer test-buffer-terminator--special-mode-buffer)
    (kill-buffer test-buffer-terminator--special-mode-buffer))
  (with-current-buffer (get-buffer-create
                        test-buffer-terminator--special-mode-buffer)
    (special-mode))

  ;; Create file buffers
  ;; -------------------
  (dolist (file test-buffer-terminator--file-buffers)
    (when (get-buffer file)
      (kill-buffer file)))
  (dolist (file test-buffer-terminator--file-buffers)
    (find-file file))
  (with-current-buffer (get-buffer (car test-buffer-terminator--file-buffers))
    (sh-mode))
  (with-current-buffer (nth 1 test-buffer-terminator--file-buffers)
    (python-mode))
  (with-current-buffer (nth 2 test-buffer-terminator--file-buffers)
    (conf-mode))

  ;; Create file visiting buffers with asterisks
  ;; -------------------------------------------
  (dolist (file test-buffer-terminator--file-buffers-with-asterisks)
    (when (get-buffer file)
      (kill-buffer file)))
  (dolist (file test-buffer-terminator--file-buffers-with-asterisks)
    (find-file file))

  ;; Modified file buffer
  ;; --------------------
  (when (get-buffer test-buffer-terminator--file-buffer-modified)
    (kill-buffer test-buffer-terminator--file-buffer-modified))
  (find-file test-buffer-terminator--file-buffer-modified)
  (with-current-buffer test-buffer-terminator--file-buffer-modified
    ;; Modify test-file2 without saving
    (insert "Modified content"))

  ;; Create buffer that is handled by a function
  ;; -------------------------------------------
  (when (get-buffer test-buffer-terminator--func-buffer)
    (kill-buffer test-buffer-terminator--func-buffer))
  (get-buffer-create test-buffer-terminator--func-buffer)

  ;; Create a process buffer
  ;; -----------------------
  (when (get-buffer test-buffer-terminator--process-buffer-name)
    (kill-buffer test-buffer-terminator--process-buffer-name))
  (start-process-shell-command
   "test-sh-process"
   test-buffer-terminator--process-buffer-name "sh")
  (test-buffer-terminator--check-test-environment)

  (switch-to-buffer (get-buffer-create "*Messages*")))

(defun test-buffer-terminator--is-special-buffer ()
  "Return t if the current buffer is a test special buffer."
  (or (member (buffer-name) test-buffer-terminator--special-buffers)
      (string= (buffer-name) test-buffer-terminator--special-mode-buffer)))

(defun test-buffer-terminator--special-predicate ()
  "Special predicate that prevents deleting other special buffers."
  (let ((buffer-name (buffer-name)))
    (when (and (or (string-prefix-p "*" buffer-name)
                   (string-prefix-p " " buffer-name))
               (not (test-buffer-terminator--is-special-buffer)))
      (message "KEEP EMACS SPECIAL BUFFER: %s" buffer-name)
      :keep)))

(ert-deftest test-buffer-terminator-test01-kill-none ()
  (test-buffer-terminator--create-test-environment)
  (setq buffer-terminator-rules-alist '())
  (buffer-terminator-execute-rules)
  (test-buffer-terminator--check-test-environment))

(ert-deftest test-buffer-terminator-test02-keep-all ()
  (test-buffer-terminator--create-test-environment)
  ;; TODO use variable: test-buffer-terminator--func-buffer
  (setq buffer-terminator-rules-alist '((keep-buffer-name . "func-buffer")
                                        (keep-buffer-property . special)
                                        (keep-buffer-property . process)
                                        (keep-buffer-property . visible)
                                        (keep-buffer-property . file)
                                        (return . :kill)))
  (buffer-terminator-execute-rules)
  (test-buffer-terminator--check-test-environment))

(ert-deftest test-buffer-terminator-test03-kill-all ()
  (test-buffer-terminator--create-test-environment)
  (setq buffer-terminator-rules-alist
        '((call-function . test-buffer-terminator--special-predicate)
          (return . :kill)))
  (buffer-terminator-execute-rules)
  (test-buffer-terminator--check-special-buffers nil)
  (test-buffer-terminator--check-special-mode-buffer nil)
  (test-buffer-terminator--check-func-buffer nil)
  (test-buffer-terminator--check-file-buffers nil)
  (test-buffer-terminator--check-modified-file-buffer t)
  (test-buffer-terminator--check-process-buffer nil))

(ert-deftest test-buffer-terminator-test03-keep-special ()
  ;; Keep
  (test-buffer-terminator--create-test-environment)
  (setq buffer-terminator-rules-alist '((keep-buffer-property . special)
                                        (return . :kill)))
  (buffer-terminator-execute-rules)
  (test-buffer-terminator--check-special-buffers t)
  (test-buffer-terminator--check-special-mode-buffer t)
  (test-buffer-terminator--check-func-buffer nil)
  (test-buffer-terminator--check-file-buffers nil)
  (test-buffer-terminator--check-modified-file-buffer t)
  (test-buffer-terminator--check-process-buffer nil)

  ;; Keep
  (test-buffer-terminator--create-test-environment)
  (setq buffer-terminator-rules-alist '((call-function . test-buffer-terminator--special-predicate)
                                        (kill-buffer-property . special)
                                        (return . :keep)))
  (buffer-terminator-execute-rules)
  (test-buffer-terminator--check-special-buffers nil)
  (test-buffer-terminator--check-special-mode-buffer nil)
  (test-buffer-terminator--check-func-buffer t)
  (test-buffer-terminator--check-file-buffers t)
  (test-buffer-terminator--check-modified-file-buffer t)
  (test-buffer-terminator--check-process-buffer t))

(ert-deftest test-buffer-terminator-test04-keep-func ()
  ;; Kill process buffer
  (defun test-buffer-terminator-predicate-kill ()
    (let ((buffer-name (buffer-name)))
      (if (string= buffer-name test-buffer-terminator--process-buffer-name)
          :kill
        nil)))
  (test-buffer-terminator--create-test-environment)
  (setq buffer-terminator-rules-alist
        '((keep-buffer-property . special)
          (call-function . test-buffer-terminator-predicate-kill)
          (return . :kill)))
  (buffer-terminator-execute-rules)
  (test-buffer-terminator--check-special-buffers t)
  (test-buffer-terminator--check-special-mode-buffer t)
  (test-buffer-terminator--check-func-buffer nil)
  (test-buffer-terminator--check-file-buffers nil)
  (test-buffer-terminator--check-modified-file-buffer t)
  (test-buffer-terminator--check-process-buffer nil)

  ;; Keep process buffer
  (defun test-buffer-terminator-predicate-keep ()
    (let ((buffer-name (buffer-name)))
      (if (string= buffer-name test-buffer-terminator--process-buffer-name)
          :keep
        nil)))
  (test-buffer-terminator--create-test-environment)
  (setq buffer-terminator-rules-alist
        '((keep-buffer-property . special)
          (call-function . test-buffer-terminator-predicate-keep)
          (return . :kill)))
  (buffer-terminator-execute-rules)
  (test-buffer-terminator--check-process-buffer t))

(ert-deftest test-buffer-terminator-test05-keep-file ()
  ;; Keep
  (test-buffer-terminator--create-test-environment)
  (setq buffer-terminator-rules-alist
        '((call-function . test-buffer-terminator--special-predicate)
          (keep-buffer-property . file)
          (return . :kill)))
  (buffer-terminator-execute-rules)
  (test-buffer-terminator--check-special-buffers nil)
  (test-buffer-terminator--check-special-mode-buffer nil)
  (test-buffer-terminator--check-func-buffer nil)
  (test-buffer-terminator--check-file-buffers t)
  (test-buffer-terminator--check-modified-file-buffer t)
  (test-buffer-terminator--check-process-buffer nil)

  ;; Kill
  (test-buffer-terminator--create-test-environment)
  (setq buffer-terminator-rules-alist
        '((call-function . test-buffer-terminator--special-predicate)
          (kill-buffer-property . file)
          (return . :keep)))
  (buffer-terminator-execute-rules)
  (test-buffer-terminator--check-special-buffers t)
  (test-buffer-terminator--check-special-mode-buffer t)
  (test-buffer-terminator--check-func-buffer t)
  (test-buffer-terminator--check-file-buffers nil)
  (test-buffer-terminator--check-modified-file-buffer t)
  (test-buffer-terminator--check-process-buffer t))

(ert-deftest test-buffer-terminator-test06-keep-modified-file ()
  (test-buffer-terminator--create-test-environment)
  (setq buffer-terminator-rules-alist
        '((call-function . test-buffer-terminator--special-predicate)
          (return . :kill)))
  (buffer-terminator-execute-rules)
  (test-buffer-terminator--check-special-buffers nil)
  (test-buffer-terminator--check-special-mode-buffer nil)
  (test-buffer-terminator--check-func-buffer nil)
  (test-buffer-terminator--check-file-buffers nil)
  (test-buffer-terminator--check-modified-file-buffer t)
  (test-buffer-terminator--check-process-buffer nil))

(ert-deftest test-buffer-terminator-test07-keep-process ()
  ;; Keep
  (test-buffer-terminator--create-test-environment)
  (setq buffer-terminator-rules-alist
        '((call-function . test-buffer-terminator--special-predicate)
          (keep-buffer-property . process)
          (return . :kill)))
  (buffer-terminator-execute-rules)
  (test-buffer-terminator--check-special-buffers nil)
  (test-buffer-terminator--check-special-mode-buffer nil)
  (test-buffer-terminator--check-func-buffer nil)
  (test-buffer-terminator--check-file-buffers nil)
  (test-buffer-terminator--check-modified-file-buffer t)
  (test-buffer-terminator--check-process-buffer t)

  ;; Kill
  (test-buffer-terminator--create-test-environment)
  (setq buffer-terminator-rules-alist
        '((call-function . test-buffer-terminator--special-predicate)
          (kill-buffer-property . process)
          (return . :keep)))
  (buffer-terminator-execute-rules)
  (test-buffer-terminator--check-special-buffers t)
  (test-buffer-terminator--check-special-mode-buffer t)
  (test-buffer-terminator--check-func-buffer t)
  (test-buffer-terminator--check-file-buffers t)
  (test-buffer-terminator--check-modified-file-buffer t)
  (test-buffer-terminator--check-process-buffer nil))

(ert-deftest test-buffer-terminator-test08-keep-kill-name ()
  ;; Keep
  (test-buffer-terminator--create-test-environment)
  (setq buffer-terminator-rules-alist
        ;; TODO: use variable test-buffer-terminator--file-buffers
        '((keep-buffer-name . "test-file1")
          (keep-buffer-name . ("test-file2" "test-file3"))
          (call-function . test-buffer-terminator--special-predicate)
          (return . :kill)))
  (buffer-terminator-execute-rules)
  (test-buffer-terminator--check-special-buffers nil)
  (test-buffer-terminator--check-special-mode-buffer nil)
  (test-buffer-terminator--check-func-buffer nil)
  (test-buffer-terminator--check-file-buffers t)
  (test-buffer-terminator--check-modified-file-buffer t)
  (test-buffer-terminator--check-process-buffer nil)

  ;; Kill
  (test-buffer-terminator--create-test-environment)
  (setq buffer-terminator-rules-alist
        ;; TODO: use variable test-buffer-terminator--file-buffers
        '((kill-buffer-name . "test-file1")
          (kill-buffer-name . ("test-file2" "test-file3"))
          (call-function . test-buffer-terminator--special-predicate)
          (return . :keep)))
  (buffer-terminator-execute-rules)
  (test-buffer-terminator--check-special-buffers t)
  (test-buffer-terminator--check-special-mode-buffer t)
  (test-buffer-terminator--check-func-buffer t)
  (test-buffer-terminator--check-file-buffers nil)
  (test-buffer-terminator--check-modified-file-buffer t)
  (test-buffer-terminator--check-process-buffer t))

(ert-deftest test-buffer-terminator-test09-keep-regexp ()
  ;; Keep
  (test-buffer-terminator--create-test-environment)
  (setq buffer-terminator-rules-alist
        ;; TODO: use variable test-buffer-terminator--file-buffers
        '((keep-buffer-name-regexp . "test-process-buffer")
          (keep-buffer-name-regexp . ("test-file" "func-buffer"))
          (call-function . test-buffer-terminator--special-predicate)
          (return . :kill)))
  (buffer-terminator-execute-rules)
  (test-buffer-terminator--check-special-buffers nil)
  (test-buffer-terminator--check-special-mode-buffer nil)
  (test-buffer-terminator--check-func-buffer t)
  (test-buffer-terminator--check-file-buffers t)
  (test-buffer-terminator--check-modified-file-buffer t)
  (test-buffer-terminator--check-process-buffer t)

  ;; Kill
  (test-buffer-terminator--create-test-environment)
  (setq buffer-terminator-rules-alist
        ;; TODO: use variable test-buffer-terminator--file-buffers
        '((kill-buffer-name-regexp . "test-process-buffer")
          (kill-buffer-name-regexp . ("test-file" "func-buffer"))
          (call-function . test-buffer-terminator--special-predicate)
          (return . :keep)))
  (buffer-terminator-execute-rules)
  (test-buffer-terminator--check-special-buffers t)
  (test-buffer-terminator--check-special-mode-buffer t)
  (test-buffer-terminator--check-func-buffer nil)
  (test-buffer-terminator--check-file-buffers nil)
  (test-buffer-terminator--check-modified-file-buffer t)
  (test-buffer-terminator--check-process-buffer nil))

(ert-deftest test-buffer-terminator-test10-mode-kill ()
  (test-buffer-terminator--create-test-environment)
  (setq buffer-terminator-rules-alist
        '((call-function . test-buffer-terminator--special-predicate)
          (return . :kill)))
  (setq buffer-terminator-inactivity-timeout 0.1)
  (setq buffer-terminator-interval 0.1)
  (unwind-protect
      (progn
        (buffer-terminator-mode 1)
        (sleep-for 0.3)
        (test-buffer-terminator--check-special-buffers nil)
        (test-buffer-terminator--check-special-mode-buffer nil)
        (test-buffer-terminator--check-func-buffer nil)
        (test-buffer-terminator--check-file-buffers nil)
        (test-buffer-terminator--check-modified-file-buffer t)
        (test-buffer-terminator--check-process-buffer nil))
    (buffer-terminator-mode 0)))

(ert-deftest test-buffer-terminator-test11-mode-check-before-kill ()
  (test-buffer-terminator--create-test-environment)
  (setq buffer-terminator-rules-alist
        '((call-function . test-buffer-terminator--special-predicate)
          (return . :kill)))
  (setq buffer-terminator-inactivity-timeout 0.1)
  (setq buffer-terminator-interval 0.1)
  (unwind-protect
      (progn
        (buffer-terminator-mode 1)
        (sleep-for 0.3)
        (test-buffer-terminator--check-special-buffers nil)
        (test-buffer-terminator--check-special-mode-buffer nil)
        (test-buffer-terminator--check-func-buffer nil)
        (test-buffer-terminator--check-file-buffers nil)
        (test-buffer-terminator--check-modified-file-buffer t)
        (test-buffer-terminator--check-process-buffer nil))
    (buffer-terminator-mode 0)))

(ert-deftest test-buffer-terminator-test12-visible ()
  ;; Keep visible
  ;; TODO use variable
  (unless (< emacs-major-version 27)
    (test-buffer-terminator--create-test-environment)
    (unwind-protect
        (progn
          (tab-bar-mode 1)
          (pop-to-buffer (get-buffer test-buffer-terminator--special-mode-buffer))
          ;; Current buffer
          (tab-bar-mode 1)
          (pop-to-buffer (get-buffer test-buffer-terminator--process-buffer-name))
          ;; Other split
          (switch-to-buffer (get-buffer test-buffer-terminator--func-buffer))

          (tab-bar-new-tab)
          (setq buffer-terminator-rules-alist
                '((call-function . test-buffer-terminator--special-predicate)
                  (keep-buffer-property . visible)
                  (return . :kill)))
          (buffer-terminator-execute-rules)
          (test-buffer-terminator--check-special-buffers nil)
          (test-buffer-terminator--check-special-mode-buffer t)
          (test-buffer-terminator--check-func-buffer t)
          (test-buffer-terminator--check-file-buffers nil)
          (test-buffer-terminator--check-modified-file-buffer t)
          (test-buffer-terminator--check-process-buffer nil))
      (tab-bar-close-other-tabs)
      (tab-bar-mode 0))

    ;; Kill visible
    ;; TODO fix this one
    (test-buffer-terminator--create-test-environment)
    (unwind-protect
        (progn
          (tab-bar-mode 1)
          (tab-bar-close-other-tabs)
          (switch-to-buffer (get-buffer test-buffer-terminator--special-mode-buffer))
          ;; Current buffer
          (tab-bar-new-tab)
          (switch-to-buffer (get-buffer test-buffer-terminator--process-buffer-name))
          (tab-bar-new-tab)
          ;; Other split
          (switch-to-buffer (get-buffer test-buffer-terminator--func-buffer))
          ;; (tab-bar-new-tab)
          (setq buffer-terminator-rules-alist
                '((call-function . test-buffer-terminator--special-predicate)
                  (kill-buffer-property . visible)
                  (return . :keep)))
          (buffer-terminator-execute-rules)
          ;; TODO broken
          (test-buffer-terminator--check-special-buffers t)
          (test-buffer-terminator--check-special-mode-buffer nil)
          (test-buffer-terminator--check-func-buffer t) ;; Keep current buffer
          (test-buffer-terminator--check-file-buffers t)
          (test-buffer-terminator--check-modified-file-buffer t)
          (test-buffer-terminator--check-process-buffer nil)

          ;; No current buffer
          (tab-bar-new-tab)
          (switch-to-buffer (get-buffer-create "*Messages*"))
          (buffer-terminator-execute-rules)
          (test-buffer-terminator--check-func-buffer nil))
      (tab-bar-mode 0))))

(ert-deftest test-buffer-terminator-test13-major-modes ()
  ;; Kill
  (test-buffer-terminator--create-test-environment)
  (setq buffer-terminator-rules-alist
        ;; TODO: use variable test-buffer-terminator--file-buffers
        '((kill-buffer-major-modes . sh-mode)
          (kill-buffer-major-modes . (python-mode conf-mode))
          (call-function . test-buffer-terminator--special-predicate)
          (return . :keep)))
  (buffer-terminator-execute-rules)
  (should-not
   (get-buffer (car test-buffer-terminator--file-buffers)))
  (should-not
   (get-buffer (nth 1 test-buffer-terminator--file-buffers)))

  ;; Keep
  (test-buffer-terminator--create-test-environment)
  (setq buffer-terminator-rules-alist
        ;; TODO: use variable test-buffer-terminator--file-buffers
        '((keep-buffer-major-modes . sh-mode)
          (keep-buffer-major-modes . (python-mode conf-mode))
          (call-function . test-buffer-terminator--special-predicate)
          (return . :keep)))
  (buffer-terminator-execute-rules)
  (should
   (get-buffer (car test-buffer-terminator--file-buffers)))
  (should
   (get-buffer (nth 1 test-buffer-terminator--file-buffers))))

(ert-deftest test-buffer-terminator-test14-file-names-with-asterisks ()
  ;; Check if killing special buffer do not kill file buffers that start
  ;; and end with an asterisk
  (test-buffer-terminator--create-test-environment)
  (setq buffer-terminator-rules-alist
        '((kill-buffer-property . special)
          (return . :keep)))
  (buffer-terminator-execute-rules)
  (should
   (get-buffer (nth 0 test-buffer-terminator--file-buffers-with-asterisks)))

  ;; Check if keeping file buffers really keep file buffers that start and
  ;; end with an asterisk
  (test-buffer-terminator--create-test-environment)
  (setq buffer-terminator-rules-alist
        '((keep-buffer-property . file)
          (call-function . test-buffer-terminator--special-predicate)
          (return . :kill)))
  (buffer-terminator-execute-rules)
  (should
   (get-buffer (nth 0 test-buffer-terminator--file-buffers-with-asterisks))))

(provide 'test-buffer-terminator)
;;; test-buffer-terminator.el ends here
