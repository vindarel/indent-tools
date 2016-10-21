;; indent-tools.el --- Indent, move around code based on indentation (yaml, python, etc).

;; Copyright (C) 2016  wtf public licence

;; Author: vindarel <@>
;; URL: https://gitlab.com/emacs-stuff/indent-promote/
;; Version: 0.0.1
;; Keywords: indentation, movements
;; Package-Requires: ((s "0") (hydra "0") (yafolding "0"))

;;; Commentary:
;;
;; Indent, de-indent, move around code based on indentation. Perfect to navigate in a big yaml file or in Python code.
;;; Code:

;; As an answer to https://www.reddit.com/r/emacs/comments/4jb8dj/orgmodelike_promotedemote_tree_for_editing/

(require 'hydra)
(require 'yafolding)

(defvar indent-tools-node-regexp "\"?[a-zA-Z0-9(\"'-.{]" "A regexp to match the beginning of a yaml node. Should skip comments.") ;; Should be mode specific: skip comments, etc
;; (setq indent-tools-node-regexp "\"?[a-zA-Z0-9(\"'-.{]" ) ;; Should be mode specific: skip comments, etc

(defvar indent-tools-indent-offset 4 "default indentation offset, when a mode isnt recognized")

(defun indent-tools-current-line-indentation ()
  "Returns a string containing the spaces that make up the current line's indentation."
  (save-excursion
    (re-search-backward "^\\(\s*\\)" (line-beginning-position))
    (buffer-substring-no-properties (match-beginning 1) (match-end 1))))

(defun indent-tools-on-blank-line-p ()
  "Return true if we are on a blank line"
  (equal (line-beginning-position) (line-end-position)))

(defun indent-tools-end-of-tree-point ()
  "Get the point of the end of the indentend tree."
  (save-excursion
    (indent-tools-goto-end-of-tree)
    (point)))

(defun indent-tools--indentation-offset ()
  "Get the current mode's indentation offset. Return an int (for python, it's usually 4)."
  (let ((current-mode major-mode))
    (cond ((and (equal current-mode 'python-mode)
                (boundp 'python-indent-offset)
                (numberp python-indent-offset))
           python-indent-offset)

          ((and (equal current-mode 'jade-mode)
                (boundp 'jade-tab-width)
                (numberp jade-tab-width))
           jade-tab-width)

          ((and (equal current-mode 'yaml-mode)
                (boundp 'yaml-indent-offset)
                (numberp yaml-indent-offset))
           yaml-indent-offset)

          (t indent-tools-indent-offset))))

(defun indent-tools--on-last-line ()
  (equal (line-number-at-pos) (count-lines (point-min) (point-max))))

(defun indent-tools-goto-end-of-tree ()
  "Go to the end of the indented tree."
  (interactive)
  (let ((goal-column (length (indent-tools-current-line-indentation)))  ;; see next-line doc
        (last-line-reached nil))
    (beginning-of-line-text)
    (next-line)
    (while (and (not last-line-reached)
                (or
                 (indent-tools-on-blank-line-p)
                 (string-equal (char-to-string (following-char)) " ")))
      (if (indent-tools--on-last-line)
          (setq last-line-reached t)
        (next-line)))
    (unless last-line-reached (previous-line))
    (end-of-line)
    ))

(defun indent-tools-goto-parent ()
  "Go to this node's parent, one indentation level up."
  (interactive)
  (beginning-of-line-text)
  (if (not (s-blank? (indent-tools-current-line-indentation)))
      (progn
        (if (search-backward-regexp (concat "^"
                                            (s-left (- (length (indent-tools-current-line-indentation))
                                                       (indent-tools--indentation-offset))
                                                    (indent-tools-current-line-indentation))
                                            indent-tools-node-regexp)
                                    nil t)
            (beginning-of-line-text)
          (message "you don't have more parents")))
    (message "you don't have more parents")))

(defun indent-tools-goto-child ()
  "Go down to the first child (line with greater indentation)."
  (interactive)
  (beginning-of-line-text)
  (unless (search-forward-regexp (concat "^"
                                     (indent-tools-current-line-indentation)
                                     (s-repeat (indent-tools--indentation-offset) " ")
                                     indent-tools-node-regexp)
                             nil
                             t)
    (message "you don't have more children."))
  (beginning-of-line-text))

(defun indent-tools-select-end-of-tree ()
  ""
  (interactive)
  (let ((beg (line-beginning-position))
        (end (save-excursion
               (indent-tools-goto-end-of-tree)
               (point))))
    (goto-char beg)
    (push-mark)
    (activate-mark)
    (goto-char end)
    ))

(defun indent-tools-end-of-level () ;; OK needs more tests MORE TESTS PLZ
  "Go to the end of this indentation level"
  (interactive)
  (let* ((indentation (indent-tools-current-line-indentation))
         (last-line-reached nil))
    (beginning-of-line-text)
    (next-line)
    (while (not last-line-reached)
      (if (indent-tools-on-blank-line-p)
          (next-line))
      (if (< (length (indent-tools-current-line-indentation))
             (length indentation))
          (setq last-line-reached t)
        (next-line)))

    (beginning-of-line-text)))

(defun indent-tools-end-of-level-point ()
  ""
  (save-excursion
    (indent-tools-end-of-level)
    (previous-line)
    (point)))

(defun indent-tools-indent-end-of-level ()
  "Indent until the end of this indentation level."
  (interactive)
  (let ((beg (line-beginning-position))
        (end (indent-tools-end-of-level-point))
        (offset (indent-tools--indentation-offset)))
    (indent-rigidly beg end offset)))

(defun indent-tools-select ()
  "Select the tree (useful to visualize.
   Also useful: highlight-indentation-current-column-mode"
  ; use a red hydra to cancel effects instead ?
  (interactive)
  (let ((beg (save-excursion
               (beginning-of-line-text) (point)))
        (end (indent-tools-end-of-tree-point)))
    (goto-char beg)
    (push-mark)
    (activate-mark)
    (goto-char end)
    ))

(defun indent-tools-indent (&optional select)
  "Indent the current tree."
    (interactive)
    (let ((beg (save-excursion
                (beginning-of-line) (point)))
          (end (indent-tools-end-of-tree-point))
          (indentation-level (indent-tools--indentation-offset)))
    (if select
            (call-interactively 'indent-rigidly t (vector beg end)) ;; hey… hydras do the job of repetition !
            (indent-rigidly beg end indentation-level))))

(defun indent-tools-indent-paragraph ()
  "Indent the current paragraph, i.e. the block of text until a
   new line. The paragraph is the one you would jump with
   forward-paragraph, bound to M-n"
  (interactive)
  (let ((beg (line-beginning-position))
        (end (save-excursion
               (forward-paragraph)
               (point))))
    (indent-rigidly beg end (indent-tools--indentation-offset))))

(defun indent-tools-indent-end-of-defun ()
  "Indent until the end of the current defun."
  (interactive)
  (let ((beg (line-beginning-position))
        (end (save-excursion
               (end-of-defun)
               (point)))
        (indentation-level (indent-tools--indentation-offset)))
    (if (equal beg end)
        ;; case we're at the last defun or in __main__, not a defun.
        (setq end (point-max)))
    (indent-rigidly beg end indentation-level)
    ))

(defun indent-tools-indent-space ()
  "Indent with only a space (specially useful in jade-mode)."
  (interactive)
  (let ((beg (line-beginning-position))
        (end (indent-tools-end-of-tree-point))
        (indentation-level (indent-tools--indentation-offset)))
    (save-excursion
      (replace-regexp "^" " " nil beg end))))

(defun indent-tools-demote ()
  "de-indent the current indented tree"
  ;; todo: factorize
  (interactive)
  (let ((beg (save-excursion
               (beginning-of-line) (point)))
        (end (indent-tools-end-of-tree-point))
        (indentation-level (- (indent-tools--indentation-offset))))
    (indent-rigidly beg end indentation-level)))

(defun indent-tools-interactive ()
  "Set the indentation yourself with the arrow keys."
  ;; that's what M-x indent-rigidly without arg does.
  ;; TO FIX
  (interactive)
  (indent-tools t))

(defun indent-tools-comment ()
  (interactive)
  (let ((beg (line-beginning-position))
        (end (indent-tools-end-of-tree-point)))
    (setq indent-tools--last-beg beg) ;; re-use to uncomment
    (setq indent-tools--last-end end)
    (comment-region beg end)))

(defun indent-tools-goto-next-sibling ()
  "Goes to the next element of the same level."
  (interactive)
  (end-of-line)
  (let ((yaml-regexp indent-tools-node-regexp))
    ;; (setq yaml-element-regexp ".*") ;; should not start by a comment
    (or (search-forward-regexp (concat "^"
                                       (indent-tools-current-line-indentation)
                                       ;; "[^\s-]" ;; exclude following whitespaces
                                       yaml-regexp)
                               nil ; don't bound the search
                               t ; if search fails just return nil, no error
                               )
        (message "We didn't find a next sibling."))
    (beginning-of-line-text)))

(defun indent-tools-goto-previous-sibling ()
  "Go to previous sibling."
  (interactive)
  ;; (beginning-of-line-text)
  (beginning-of-line)
  (or (search-backward-regexp (concat "^"
                                  (indent-tools-current-line-indentation)
                                  ;; "[^\s-]"
                                  indent-tools-node-regexp)
                          nil
                          t)
      (message "We didn't find a previous sibling."))
  (beginning-of-line-text))

;;;;;;; copy
(defun indent-tools-copy (what)
  ""
  (let ((beg (line-beginning-position))
          (end (cond
                 ((equal what "paragraph") (save-excursion
                                             (forward-paragraph)
                                             (point)))
                 ((equal what "tree") (indent-tools-end-of-tree-point))
                 ((equal what "level") (indent-tools-end-of-level-point))
                 )))
     (kill-ring-save beg end)
     (message (format "Copied %s" what))))


(defhydra indent-tools-copy-hydra (:color blue :after-exit (indent-tools-hydra/body))
  "
  "
  (">" (indent-tools-copy "tree") "this indented tree")
  ("l" (indent-tools-copy "level") "all level")
  ("p" (indent-tools-copy "paragraph") "paragraph")
  )


;;;;;;; kill
(defun indent-tools-kill-tree ()
  "Delete the current indentated tree."
  (interactive)
  (let ((beg (save-excursion
               (beginning-of-line-text)
               (point)))
        (end (indent-tools-end-of-tree-point)))
    (kill-region beg end)))

(defun indent-tools-kill-level ()
  ""
  (interactive)
  (let ((beg (line-beginning-position))
        (end (indent-tools-end-of-tree-point)))
    (kill-region beg end)))

(defhydra indent-tools-kill-hydra (:color blue :after-exit (indent-tools-hydra/body))
  "
  "
  (">" indent-tools-kill-tree "indentation tree")
  ("p" kill-paragraph "paragraph")
  ("l" indent-tools-kill-level "level")
  )

;;;;;; General hydra
(defhydra indent-tools-hydra (:color red :hint nil)
  "
 ^Indent^         | ^Navigation^        | ^Actions^
------------------+---------------------+-----------
 _>_ indent       | _j_ v               | _K_ kill
 _<_ de-indent    | _k_ ʌ               | _i_ imenu
 _l_ end of level | _n_ next sibling    | _C_ Copy…
 _E_ defun        | _p_ previous sibling| _c_ comment
 _P_ paragraph    | _u_ up parent       | _f_ fold
 _SPC_ space      | _d_ down child      | _q_ quit
                  | _e_ end of tree
"

  (">" indent-tools-indent)
  ("<" indent-tools-demote)
  ("E" indent-tools-indent-end-of-defun)
  ("c" indent-tools-comment)
  ("P" indent-tools-indent-paragraph)
  ("l" indent-tools-indent-end-of-level)
  ("K" indent-tools-kill-hydra/body :color blue)
  ("C" indent-tools-copy-hydra/body :color blue)
  ("s" indent-tools-select)
  ("e" indent-tools-goto-end-of-tree)
  ("u" indent-tools-goto-parent)
  ("d" indent-tools-goto-child)
  ("S" indent-tools-select-end-of-tree)
  ("n" indent-tools-goto-next-sibling)
  ("p" indent-tools-goto-previous-sibling)
  ("i" helm-imenu)
  ("j" next-line)
  ("k" previous-line)
  ("SPC" indent-tools-indent-space)
  ("f" yafolding-toggle-element)
  ("q" nil)
  )
(defalias 'hydra-indent-tools 'indent-tools-hydra)


(global-set-key (kbd "C-c >") 'indent-tools-hydra/body) ;; overrides in python-mode that only indent the current line

(provide 'indent-tools)

;;; indent-tools.el ends here
