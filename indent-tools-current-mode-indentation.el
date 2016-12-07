;;; indent-tools.el --- Indent, move around etc by indentation units.

;; Copyright (C) 2016  wtf public licence

;; Author: vindarel <ehvince@mailz.org>
;; URL: https://gitlab.com/emacs-stuff/indent-tools/

;; What is the indentation level of the current mode ?
;; The variable `standart-indent' doesn't change by mode.

(defvar indent-tools-current-mode-indentation-modes-alist '() "Given a mode, associate a function that gives this mode's indentation.")

;; A function for every mode.
(defun indent-tools-current-mode-indentation-python ()
  "Return Python's current indentation as an int, usually 4."
  (cond ((and (boundp 'python-indent-offset)
              (numberp python-indent-offset))
         python-indent-offset)))

(defun indent-tools-current-mode-indentation-yaml ()
  "Return Yaml's current indentation as an int."
  (cond ((and (boundp 'yaml-indent-offset)
              (numberp yaml-indent-offset))
         yaml-indent-offset)))

(defun indent-tools-current-mode-indentation-jade ()
  "Return Jade's current indentation as an int."
  (cond ((and (boundp 'jade-tab-width)
              (numberp jade-tab-width))
         jade-tab-width)))

;; The alist.
(setq indent-tools-current-mode-indentation-modes-alist
      '(
        (python-mode . indent-tools-current-mode-indentation-python)
        (yaml-mode . indent-tools-current-mode-indentation-yaml)
        (jade-mode . indent-tools-current-mode-indentation-jade)
       ))


(provide 'indent-tools-current-mode-indentation)

;;; indent-tools-current-mode-indentation ends here.
