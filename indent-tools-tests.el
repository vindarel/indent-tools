;;; indent-tools-test.el --- Tests for indent-tools

;; Copyright (C) 2016  Free Software Foundation, Inc.

;; Author: vindarel

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

(require 'ert)
(load-file "indent-tools.el")
(require 'indent-tools)

(setq text-quoting-style 'grave)
(message "Emacs version: %s" emacs-version)

;; Firs round for unit tests: testing movements
;; --------------------------------------------
;; In a temp buffer, we insert a simple yaml, we put the cursor on a given line, we call some funtions and we check the point is on the expected line.
;;
;; To improve:
;; - test on other modes (like python-mode)
;; - macro ? (no more funcall & lambda)
;; - check the content of the line instead of line nb, so than we can change the yaml sample and the test declaration doesn't move.
;; - test with many and difficult yaml samples

;; - ;TODO: test indentation functions.

(defun indent-tools-test-with-buffer (txt start-line func expected-line &optional mode)
  "Insert the given txt in a temp buffer, run function and check we're on the good line."
  (with-temp-buffer
    (yaml-mode)
    (insert txt)
    (if start-line
        (goto-line start-line)
      (goto-char (point-min)))
    (funcall func)
    (should (= (line-number-at-pos) expected-line))
    ))

;; Our sample yaml file.
(setq indent-test-txt-yaml "parent:
# comment !
  - child: l.3
    foo: 1
  - child: l.5
    foo: 2
  child: l.7
parent2:
  other-family: 1
parent3")

(ert-deftest test-next-sibling ()
  (indent-tools-test-with-buffer
   indent-test-txt-yaml
   1
   (lambda () (indent-tools-goto-next-sibling))
   8))

(ert-deftest test-previous-sibling ()
  (indent-tools-test-with-buffer
   indent-test-txt-yaml
   5
   (lambda () (indent-tools-goto-previous-sibling))
   3))

(ert-deftest test-down ()
  (indent-tools-test-with-buffer
   indent-test-txt-yaml
   1
   (lambda () (indent-tools-goto-child))
   3
   ))

(ert-deftest test-down-twice ()
  (indent-tools-test-with-buffer
   indent-test-txt-yaml
   1
   (lambda () (indent-tools-goto-child) (indent-tools-goto-child))
   4
   ))

(ert-deftest test-up ()
  (indent-tools-test-with-buffer
   indent-test-txt-yaml
   3
   (lambda () (indent-tools-goto-parent))
   1))

(ert-deftest test-up-twice ()
  (indent-tools-test-with-buffer
   indent-test-txt-yaml
   4
   (lambda () (indent-tools-goto-parent) (indent-tools-goto-parent))
   1))

(provide 'indent-tools-tests)

;;; indent-tools-tests.el ends here
