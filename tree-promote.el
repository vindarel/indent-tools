;; tree-promote.el --- Indent, move around code based on indentation (yaml, python, etc).

;; Copyright (C) 2016  wtf public licence

;; Author: vindarel <@>
;; URL: https://gitlab.com/emacs-stuff/indent-promote/
;; Version: 0.0.1
;; Keywords: indentation, movements
;; Package-Requires: ((s "0") (hydra "0"))

;;; Commentary:
;;
;; Indent, de-indent, move around code based on indentation. Perfect to navigate in a big yaml file or in Python code.
;;; Code:

;; As an answer to https://www.reddit.com/r/emacs/comments/4jb8dj/orgmodelike_promotedemote_tree_for_editing/

(require 'hydra)

(defvar tree-promote-node-regexp "\"?[a-zA-Z0-9(\"'-.{]" "A regexp to match the beginning of a yaml node. Should skip comments.") ;; Should be mode specific: skip comments, etc
;; (setq tree-promote-node-regexp "\"?[a-zA-Z0-9(\"'-.{]" ) ;; Should be mode specific: skip comments, etc

(defvar tree-promote-indent-offset 4 "default indentation offset, when a mode isnt recognized")

(defun tree-promote-current-line-indent ()
  "Returns a string containing the spaces that make up the current line's indentation."
  (save-excursion
    (re-search-backward "^\\(\s*\\)" (line-beginning-position))
    (buffer-substring-no-properties (match-beginning 1) (match-end 1))))

(defun tree-promote-on-blank-line-p ()
  "Return true if we are on a blank line"
  (equal (line-beginning-position) (line-end-position)))

(defun tree-promote-end-of-tree-point ()
  "Get the point of the end of the indentend tree."
  (save-excursion
    (tree-promote-goto-end-of-tree)
    (point)))

(defun tree-promote--indentation-offset ()
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

          (t tree-promote-indent-offset))))

(defun tree-promote--on-last-line ()
  (equal (line-number-at-pos) (count-lines (point-min) (point-max))))

(defun tree-promote-goto-end-of-tree ()
  "Go to the end of the indented tree."
  (interactive)
  (let ((goal-column (length (tree-promote-current-line-indent)))  ;; see next-line doc
        (last-line-reached nil))
    (beginning-of-line-text)
    (next-line)
    (while (and (not last-line-reached)
                (or
                 (tree-promote-on-blank-line-p)
                 (string-equal (char-to-string (following-char)) " ")))
      (if (tree-promote--on-last-line)
          (setq last-line-reached t)
        (next-line)))
    (unless last-line-reached (previous-line))
    (end-of-line)
    ))

(defun tree-promote-goto-parent ()
  "Go to this node's parent, one indentation level up."
  (interactive)
  (beginning-of-line-text)
  (if (not (s-blank? (tree-promote-current-line-indent)))
      (progn
        (if (search-backward-regexp (concat "^"
                                            (s-left (- (length (tree-promote-current-line-indent))
                                                       (tree-promote--indentation-offset))
                                                    (tree-promote-current-line-indent))
                                            tree-promote-node-regexp)
                                    nil t)
            (beginning-of-line-text)
          (message "you don't have more parents")))
    (message "you don't have more parents")))

(defun tree-promote-goto-child ()
  "Go down to the first child (line with greater indentation)."
  (interactive)
  (beginning-of-line-text)
  (unless (search-forward-regexp (concat "^"
                                     (tree-promote-current-line-indent)
                                     (s-repeat (tree-promote--indentation-offset) " ")
                                     tree-promote-node-regexp)
                             nil
                             t)
    (message "you don't have more children."))
  (beginning-of-line-text))

(defun tree-promote-select-end-of-tree ()
  ""
  (interactive)
  (let ((beg (line-beginning-position))
        (end (save-excursion
               (tree-promote-goto-end-of-tree)
               (point))))
    (goto-char beg)
    (push-mark)
    (activate-mark)
    (goto-char end)
    ))

(defun tree-promote-end-of-level () ;; OK needs more tests MORE TESTS PLZ
  "Go to the end of this indentation level"
  (interactive)
  (let* ((indentation (tree-promote-current-line-indent))
         (last-line-reached nil))
    (beginning-of-line-text)
    (next-line)
    (while (not last-line-reached)
      (if (tree-promote-on-blank-line-p)
          (next-line))
      (if (< (length (tree-promote-current-line-indent))
             (length indentation))
          (setq last-line-reached t)
        (next-line)))

    (beginning-of-line-text)))

(defun tree-promote-end-of-level-point ()
  ""
  (save-excursion
    (tree-promote-end-of-level)
    (previous-line)
    (point)))

(defun tree-promote-indent-end-of-level ()
  "Indent until the end of this indentation level."
  (interactive)
  (let ((beg (line-beginning-position))
        (end (tree-promote-end-of-level-point))
        (offset (tree-promote--indentation-offset)))
    (indent-rigidly beg end offset)))

(defun tree-promote-select ()
  "Select the tree (useful to visualize.
   Also useful: highlight-indentation-current-column-mode"
  ; use a red hydra to cancel effects instead ?
  (interactive)
  (let ((beg (save-excursion
               (beginning-of-line-text) (point)))
        (end (tree-promote-end-of-tree-point)))
    (goto-char beg)
    (push-mark)
    (activate-mark)
    (goto-char end)
    ))

(defun tree-promote-indent (&optional select)
  "Indent the current tree."
    (interactive)
    (let ((beg (save-excursion
                (beginning-of-line) (point)))
          (end (tree-promote-end-of-tree-point))
          (indentation-level (tree-promote--indentation-offset)))
    (if select
            (call-interactively 'indent-rigidly t (vector beg end)) ;; hey… hydras do the job of repetition !
            (indent-rigidly beg end indentation-level))))

(defun tree-promote-indent-paragraph ()
  "Indent the current paragraph, i.e. the block of text until a
   new line. The paragraph is the one you would jump with
   forward-paragraph, bound to M-n"
  (interactive)
  (let ((beg (line-beginning-position))
        (end (save-excursion
               (forward-paragraph)
               (point))))
    (indent-rigidly beg end (tree-promote--indentation-offset))))

(defun tree-promote-indent-end-of-defun ()
  "Indent until the end of the current defun."
  (interactive)
  (let ((beg (line-beginning-position))
        (end (save-excursion
               (end-of-defun)
               (point)))
        (indentation-level (tree-promote--indentation-offset)))
    (if (equal beg end)
        ;; case we're at the last defun or in __main__, not a defun.
        (setq end (point-max)))
    (indent-rigidly beg end indentation-level)
    ))

(defun tree-promote-indent-space ()
  "Indent with only a space (specially useful in jade-mode)."
  (interactive)
  (let ((beg (line-beginning-position))
        (end (tree-promote-end-of-tree-point))
        (indentation-level (tree-promote--indentation-offset)))
    (save-excursion
      (replace-regexp "^" " " nil beg end))))

(defun tree-promote-demote ()
  "de-indent the current indented tree"
  ;; todo: factorize
  (interactive)
  (let ((beg (save-excursion
               (beginning-of-line) (point)))
        (end (tree-promote-end-of-tree-point))
        (indentation-level (- (tree-promote--indentation-offset))))
    (indent-rigidly beg end indentation-level)))

(defun tree-promote-interactive ()
  "Set the indentation yourself with the arrow keys."
  ;; that's what M-x indent-rigidly without arg does.
  ;; TO FIX
  (interactive)
  (tree-promote t))

(defun tree-promote-comment ()
  (interactive)
  (let ((beg (line-beginning-position))
        (end (tree-promote-end-of-tree-point)))
    (setq tree-promote--last-beg beg) ;; re-use to uncomment
    (setq tree-promote--last-end end)
    (comment-region beg end)))

(defun tree-promote-goto-next-sibling ()
  "Goes to the next element of the same level."
  (interactive)
  (end-of-line)
  (let ((yaml-regexp tree-promote-node-regexp))
    ;; (setq yaml-element-regexp ".*") ;; should not start by a comment
    (or (search-forward-regexp (concat "^"
                                       (tree-promote-current-line-indent)
                                       ;; "[^\s-]" ;; exclude following whitespaces
                                       yaml-regexp)
                               nil ; don't bound the search
                               t ; if search fails just return nil, no error
                               )
        (message "We didn't find a next sibling."))
    (beginning-of-line-text)))

(defun tree-promote-goto-previous-sibling ()
  "Go to previous sibling."
  (interactive)
  ;; (beginning-of-line-text)
  (beginning-of-line)
  (or (search-backward-regexp (concat "^"
                                  (tree-promote-current-line-indent)
                                  ;; "[^\s-]"
                                  tree-promote-node-regexp)
                          nil
                          t)
      (message "We didn't find a previous sibling."))
  (beginning-of-line-text))

;;;;;;; copy
(defun tree-promote-copy-paragraph ()
  ""
  (interactive)
  (let ((beg (line-beginning-position))
        (end (save-excursion
               (forward-paragraph)
               (point))))
    (kill-ring-save beg end)
    (message "Copied paragraph")))

(defhydra tree-promote-copy-hydra (:color blue :after-exit (tree-promote-hydra/body))
  "
  "
  ("p" tree-promote-copy-paragraph "paragraph")
  )


;;;;;;; kill
(defun tree-promote-kill-tree ()
  "Delete the current indentated tree."
  (interactive)
  (let ((beg (save-excursion
               (beginning-of-line-text)
               (point)))
        (end (tree-promote-end-of-tree-point)))
    (kill-region beg end)))

(defun tree-promote-kill-level ()
  ""
  (interactive)
  (let ((beg (line-beginning-position))
        (end (tree-promote-end-of-tree-point)))
    (kill-region beg end)))

(defhydra tree-promote-kill-hydra (:color blue :after-exit (tree-promote-hydra/body))
  "
  "
  (">" tree-promote-kill-tree "indentation tree")
  ("p" kill-paragraph "paragraph")
  ("l" tree-promote-kill-level "level")
  )

;;;;;; General hydra
(defhydra tree-promote-hydra (:color red :hint nil)
  "
 ^Indent^         | ^Navigation^        | ^Actions^
------------------+---------------------+-----------
 _>_ indent       | _j_ v               | _K_ kill
 _<_ de-indent    | _k_ ʌ               | _i_ imenu
 _l_ end of level | _n_ next sibling    | _C_ Copy…
 _E_ defun        | _p_ previous sibling
 _P_ paragraph    | _u_ up parent
 _SPC_ space      | _d_ down child
                  | _e_ end of tree
"

  (">" tree-promote-indent)
  ("<" tree-promote-demote)
  ("E" tree-promote-indent-end-of-defun)
  ("c" tree-promote-comment)
  ("P" tree-promote-indent-paragraph)
  ("l" tree-promote-indent-end-of-level)
  ("K" tree-promote-kill-hydra/body :color blue)
  ("C" tree-promote-copy-hydra/body :color blue)
  ("s" tree-promote-select)
  ("e" tree-promote-goto-end-of-tree)
  ("u" tree-promote-goto-parent)
  ("d" tree-promote-goto-child)
  ("S" tree-promote-select-end-of-tree)
  ("n" tree-promote-goto-next-sibling)
  ("p" tree-promote-goto-previous-sibling)
  ("i" helm-imenu)
  ("j" next-line)
  ("k" previous-line)
  ("SPC" tree-promote-indent-space)
  )

(defalias 'hydra-tree-promote 'tree-promote-hydra)


(global-set-key (kbd "C-c >") 'tree-promote-hydra/body) ;; overrides in python-mode that only indent the current line

(provide 'tree-promote)

;;; tree-promote.el ends here
