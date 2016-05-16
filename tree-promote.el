;; indent-promote.el

;; Promote (and demote) a tree for code editing (think org-mode).
;;
;; This snippet is meant for indentation-based languages. Tested quickly with python.


;; As an answer to https://www.reddit.com/r/emacs/comments/4jb8dj/orgmodelike_promotedemote_tree_for_editing/

;; yaml-utils: https://gitlab.com/emacs-stuff/my-elisp/blob/master/yaml-utils.el

(defun current-line ()
  "returns the current line."
  ;; http://ergoemacs.org/emacs/elisp_all_about_lines.html
         (let ( (p1 (line-beginning-position))
                (p2 (line-end-position)))
           (buffer-substring-no-properties p1 p2)
           ))

(defun current-line-indentation ()
  "returns the str of the current indentation (spaces)."
  ;; https://github.com/magnars/s.el#s-match-strings-all-regex-string
  (car (car (s-match-strings-all "^\s+" (current-line)) ) )
  )


(defun my-yaml-next-sibling ()
  "Goes to the next element of the same level (defined by the current line indentation)."
  (interactive)
  (end-of-line)
  (setq my-yaml-element-regexp "\"?[a-zA-Z0-9]")
  (or (search-forward-regexp (concat "^"
                                 (current-line-indentation)
                                 my-yaml-element-regexp)
                         nil ; don't bound the search
                         t ; if search fails just return nil, no error
                         )
      (goto-char (point-max)))
  (beginning-of-line-text))

(defun my-indent (reg-beg reg-end)
  "Indent a region with spaces (should be replaced with a
   built-in one, but I observed evil's is buggy in some modes, like
   jade-mode."
  (interactive "r")
  (save-excursion
    (replace-regexp "^" "    " nil reg-beg reg-end)))

(defun my-tree-promote ()
  "Indent the current tree (based on indentation)."
         (interactive)
         (let (( beg (save-excursion
                       (beginning-of-line-text) (point)))
               ( end (save-excursion
                       (my-yaml-next-sibling)
                       (previous-line)
                       (point)))
               )
           (my-indent beg end)
         ))
