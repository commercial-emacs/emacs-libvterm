;;; test-vterm.el --- Tests for vterm -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2020 by Lukas Fürmetz & Contributors

;; This file is not part of GNU Emacs.

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
;;
;; Tests for vterm.

;;; Code:

(require 'ert)
(require 'vterm)

(defvar-local test-vterm/prompt nil)

(defmacro test-vterm/with-session (&rest body)
  (declare (indent defun))
  `(let ((b (vterm--get-buffer nil)))
     (unwind-protect
	 (with-current-buffer b
	   (should (get-buffer-process (current-buffer)))
	   (cl-loop repeat 100
		    until (not (zerop (current-column)))
		    do (sleep-for 0.2)
		    finally
		    (progn (setq test-vterm/prompt
				 (buffer-substring-no-properties
				  (line-beginning-position) (point)))
			   (should-not (zerop (length test-vterm/prompt)))))
	   ,@body)
       (let (kill-buffer-query-functions)
	 (kill-buffer b)
	 (cl-loop repeat 100
		  until (not (get-buffer-process b))
		  do (sleep-for 0.2)
		  finally (should-not (get-buffer-process b)))))))

(defsubst test-vterm/at-prompt ()
  (equal (buffer-substring-no-properties
	  (line-beginning-position) (point))
	 test-vterm/prompt))

(defsubst test-vterm/run (command)
  (should (test-vterm/at-prompt))
  (vterm-send-string command)
  (vterm-send-return)
  (cl-loop repeat 100
	   until (test-vterm/at-prompt)
	   do (sleep-for 0.2)
	   finally (should (test-vterm/at-prompt))))

(ert-deftest basic ()
  (test-vterm/with-session
    (should (eq major-mode 'vterm-mode))))

(ert-deftest ugly-wrap ()
  (test-vterm/with-session
    (should (= (window-width) vterm-min-window-width))
    (let* ((x3 (make-string (* 3 (window-width)) ?x)))
      (test-vterm/run (format "echo %s" x3))
      (save-excursion
	(forward-line -1)
	(let ((pure-text (buffer-substring-no-properties
			  (line-beginning-position) (line-end-position))))
	  (should-not (equal pure-text x3))))
      (call-interactively #'vterm-copy-mode)
      ;; misshapen glom because terminals do not issue explicit
      ;; newlines.  So a non-NUL character in the final column could
      ;; be either a line whose end coincides with the terminal width
      ;; or exceeds it (a continuation).
      (should (equal (buffer-substring-no-properties
		      (line-beginning-position) (point))
		     (concat x3 test-vterm/prompt))))))

(ert-deftest beauty-wrap ()
  (test-vterm/with-session
    (should (= (window-width) vterm-min-window-width))
    (let* ((x3 (make-string (* 3 (1- (window-width))) ?x)))
      (test-vterm/run (format "echo %s" x3))
      (save-excursion
	(forward-line -1)
	(let ((pure-text (buffer-substring-no-properties
			  (line-beginning-position) (line-end-position))))
	  (should-not (equal pure-text x3))))
      (call-interactively #'vterm-copy-mode)
      (should (test-vterm/at-prompt))
      (save-excursion
	(forward-line -1) ;logical
	(should (equal x3 (buffer-substring-no-properties
			   (line-beginning-position) (line-end-position))))))))

(provide 'test-vterm)
;;; test-vterm.el ends here
