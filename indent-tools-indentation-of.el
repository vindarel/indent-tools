;;; indent-tools.el --- Indent, move around etc by indentation units.

;; Copyright (C) 2016  wtf public licence

;; Author: vindarel <ehvince@mailz.org>
;; URL: https://gitlab.com/emacs-stuff/indent-tools/

;; What is the indentation level of the current mode ?

(defvar indent-tools-indentation-of-modes-alist '() "Given a mode, associate a function that gives this mode's indentation.")

;; A function for every mode.
(defun indent-tools-indentation-of-python ()
  "Return Python's current indentation as an int, usually 4."
  (cond ((and (boundp 'python-indent-offset)
              (numberp python-indent-offset))
         python-indent-offset)))

(defun indent-tools-indentation-of-yaml ()
  "Return Yaml's current indentation as an int."
  (cond ((and (boundp 'yaml-indent-offset)
              (numberp yaml-indent-offset))
         yaml-indent-offset)))

(defun indent-tools-indentation-of-jade ()
  "Return Jade's current indentation as an int."
  (cond ((and (boundp 'jade-tab-width)
              (numberp jade-tab-width))
         jade-tab-width)))

(defun indent-tools-indentation-of-web-mode-code ()
  "In web-mode, indentation of code."
  (cond ((and (boundp 'web-mode-code-indent-offset)
              (numberp web-mode-code-indent-offset))
         web-mode-code-indent-offset)))

;; The alist.
(setq indent-tools-indentation-of-modes-alist
      '(
        (python-mode . indent-tools-indentation-of-python)
        (yaml-mode . indent-tools-indentation-of-yaml)
        (jade-mode . indent-tools-indentation-of-jade)
        (web-mode . indent-tools-indentation-of-web-mode-code)
       ))

(defun indent-tools-indentation-of-current-mode ()
  "Get the current mode's indentation offset by calling the function associated to this mode in the alist `indent-tools-indentation-of-modes-alist'. If not found, return the default `standard-indent'.
Return an int (for python, it's usually 4)."
  (let ((mode-assoc (assoc major-mode indent-tools-indentation-of-modes-alist)))
    (if mode-assoc
        (funcall (cdr mode-assoc))
      ;; if we don't know this major mode, return a default.
      standard-indent)))


(provide 'indent-tools-indentation-of)

;;; indent-tools-indentation-of ends here.
