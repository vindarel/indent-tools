;;; indent-tools.el --- Indent, move around etc by indentation units.

;; Copyright (C) 2016 wtf public licence

;; Author: vindarel <ehvince@mailz.org>
;; URL: https://gitlab.com/emacs-stuff/indent-tools/

;; In the current mode, what is the regexp that makes us recognize a valid code node ?
;;
;; In each modes comments are different, in html a line can start with
;; a "<" and not in python, etc.

(defvar indent-tools-node-regexp-of-modes-alist '()
  "For each mode, associate a regexp to reach a line of code.")

(defvar indent-tools-node-regexp-default "\"?[a-zA-Z0-9(\"'-.{]"
  "A regexp to match the beginning of a yaml node.  Should skip comments.") ;; Should be mode specific: skip comments, etc

(setq indent-tools-node-regexp-of-modes-alist
      '(
        (html-mode . "[<{]")
        (lisp-mode . "(")
        (emacs-lisp-mode . "(")
        ))

(defun indent-tools-node-regexp-of-current-mode ()
  "Find the regexp of the current major mode that makes us reach a line of code (node)."
  (let ((mode/regexp (assoc major-mode indent-tools-node-regexp-of-modes-alist)))
    (if mode/regexp
        (cdr mode/regexp)
      indent-tools-node-regexp-default)))

(provide 'indent-tools-node-regexp-of)
