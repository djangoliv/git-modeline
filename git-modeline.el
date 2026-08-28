;;; git-modeline.el --- Show the git status of the current file in the modeline -*- lexical-binding: t -*-

;; Copyright (C) 2022 xl666
;; Copyright (C) 2022-2026 djangoliv

;; Author: djangoliv <olivier.giorgis@quantstack.net>
;; Maintainer: djangoliv <olivier.giorgis@quantstack.net>
;; URL: https://github.com/djangoliv/git-modeline
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: vc, tools, convenience

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `git-modeline-mode' is a global minor mode that prepends a colored
;; mark to `mode-line-format' in every buffer visiting a file tracked
;; by, or living inside, a git repository.  The color of the mark tells
;; the git status of that file at a glance:
;;
;;   GreenYellow  up to date        yellow  staged
;;   tomato       modified          blue    added
;;   red          deleted           purple  unmerged
;;   gray         untracked
;;
;; Usage:
;;
;;   (require 'git-modeline)
;;   (git-modeline-mode 1)
;;
;; The mark is refreshed when a file is visited and after each save.
;; `git-modeline-decoration' selects how it is drawn: a large dot (the
;; default), a small dot, a status letter, a colored status letter, or
;; any function of one argument returning a mode-line construct.
;;
;; The status collection code is derived from git-emacs
;; (https://github.com/tsgates/git-emacs), reduced to what the modeline
;; display needs.

;;; Code:

;;; General

(require 'cl-lib)
(require 'vc)                           ; vc
(require 'vc-git)                       ; vc-git advises

(defgroup git-modeline nil
  "Display git status as a mark in the modeline."
  :group 'vc
  :prefix "git-modeline-")

;;-----------------------------------------------------------------------------
;; Internal variables.
;;-----------------------------------------------------------------------------

(defvar git-modeline-executable "git" "Name of, or path to, the git executable.")
(defconst git-modeline--reg-status  "\\([A-Z?]\\)")
(defconst git-modeline--reg-blank   "[\t\0 ]+")
(defconst git-modeline--reg-eof     "\0")
(defconst git-modeline--reg-perm    "\\([0-7]\\{6\\}\\)")
(defconst git-modeline--reg-sha1    "\\([0-9a-f]\\{40\\}\\)")
(defconst git-modeline--reg-file    "\\([^\0]+\\)")

;;-----------------------------------------------------------------------------
;; Low-level execution functions.
;;-----------------------------------------------------------------------------

(defsubst git-modeline--exec (cmd outbuf infile &rest args)
  "Low level function for calling git.
CMD is the main git subcommand, ARGS are the remaining args.  See
`call-process' for the meaning of OUTBUF and INFILE.  Returns git's
exit code."
  (apply #'call-process git-modeline-executable infile outbuf nil (cons cmd args)))

(defsubst git-modeline--exec-buffer (cmd &rest args)
  "Run git subcommand CMD with ARGS in the current buffer.
Return the exit code."
  (apply #'git-modeline--exec cmd t nil args))

(defsubst git-modeline--interpret-to-state-symbol (stat)
  "Interpret STAT, a one-letter git state string, as a state symbol."
  (cl-case (string-to-char stat)
    (?H 'uptodate )
    (?M 'modified )
    (?? 'unknown  )
    (?A 'added    )
    (?D 'deleted  )
    (?U 'unmerged )
    (?T 'modified )
    (?K 'killed   )
    (t nil)))

(defsubst git-modeline--build-reg (&rest args)
  "Concatenate ARGS into a regexp matching one NUL-terminated record."
  (apply #'concat (append args (list "\0"))))

(defun git-modeline--status-index (&rest files)
  "Return the index status of FILES as a list of state symbols."

  (let ((states nil)
        (regexp (git-modeline--build-reg ":"
                                git-modeline--reg-perm    ; matched-1: HEAD perms
                                git-modeline--reg-blank
                                git-modeline--reg-perm    ; matched-2: index perms
                                git-modeline--reg-blank
                                git-modeline--reg-sha1    ; matched-3: HEAD sha1
                                git-modeline--reg-blank
                                git-modeline--reg-sha1    ; matched-4: index sha1
                                git-modeline--reg-blank
                                git-modeline--reg-status  ; matched-5
                                git-modeline--reg-eof
                                git-modeline--reg-file    ; matched-6
                                )))

    (with-temp-buffer
      (apply #'git-modeline--diff-raw (list "HEAD") files)

      (goto-char (point-min))

      (while (re-search-forward regexp nil t)
        (let ((stat (git-modeline--interpret-to-state-symbol (match-string 5))))
          ;; modified vs. staged: the latter has a nonzero sha1
          (when (and (eq stat 'modified)
                     (not (equal (match-string 4)
                                 "0000000000000000000000000000000000000000")))
            (setq stat 'staged))
          (push stat states))))

    states))

(defsubst git-modeline--diff-raw (args &rest files)
  "Execute \\='git diff --raw\\=' with ARGS and FILES at current buffer.
This gives, essentially, file status."
  ;; git-diff abbreviates by default, and also produces a diff.
  (apply #'git-modeline--exec-buffer "diff" "-z" "--full-index" "--raw" "--abbrev=40"
         (append args (list "--") files)))

(defun git-modeline--ls-files (&rest args)
  "Run \\='git ls-files\\=' with ARGS, returning a list of state symbols.
Does not differentiate between `modified' and `staged'."
  (let (states)
    (with-temp-buffer
      (apply #'git-modeline--exec-buffer "ls-files" "-t" "-z" args)
      (goto-char (point-min))

      (let ((regexp (git-modeline--build-reg git-modeline--reg-status ; matched-1
                                    git-modeline--reg-blank
                                    git-modeline--reg-file))) ; matched-2

        (while (re-search-forward regexp nil t)
          (push (git-modeline--interpret-to-state-symbol (match-string 1))
                states))))
    states))

;;-----------------------------------------------------------------------------
;; git application
;;-----------------------------------------------------------------------------

(defun git-modeline--status-file (file)
  "Return the git status of FILE, as a state symbol."
  (let ((states (git-modeline--status-index file)))
    (unless states
      (setq states
            (git-modeline--ls-files "-c" "-o" "--exclude-standard" file)))
    (when (= 1 (length states))
      (car states))))

;;-----------------------------------------------------------------------------
;; vc-git integration
;;-----------------------------------------------------------------------------

(defsubst git-modeline--in-repo-p ()
  "Return non-nil if the current buffer's file lives inside a git repo.
Unlike a `vc-mode' check, this also matches untracked files."
  (and buffer-file-name (vc-git-root buffer-file-name)))

(defun git-modeline--update ()
  "Update the current's buffer modeline state display."
  ;; mark depending on the fileinfo state
  (when (git-modeline--in-repo-p)
    (git-modeline--update-state-mark
     (git-modeline--status-file (file-relative-name buffer-file-name)))))

;;;###autoload
(define-minor-mode git-modeline-mode
  "Toggle display of git status as a colored dot in the modeline.
When enabled, every buffer visiting a file under git shows a dot
at the start of `mode-line-format'.  The color reflects the git
status of the file."
  :global t
  :group 'git-modeline
  (cond
   (git-modeline-mode
    (advice-add 'vc-after-save :after #'git-modeline--update)
    (add-hook 'find-file-hook #'git-modeline--update t)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (git-modeline--update))))
   (t
    (advice-remove 'vc-after-save #'git-modeline--update)
    (remove-hook 'find-file-hook #'git-modeline--update)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (git-modeline--uninstall-state-mark))))))

;;-------------------------------------------------------------------------
;; modeline
;;---------------------------------------------------------------------------

;; Modeline decoration customization
(defcustom git-modeline-decoration
  'git-modeline-decoration-large-dot
  "How to indicate the status of files in the modeline.
The value must be a function that takes a single arg: a symbol denoting
file status, e.g. `unmerged'.  The return value of the function will be
added at the beginning of `mode-line-format'."
  :type '(choice (function-item :tag "Small colored dot"
                                git-modeline-decoration-small-dot)
                 (function-item :tag "Large colored dot"
                                git-modeline-decoration-large-dot)
                 (function-item :tag "Status letter"
                                git-modeline-decoration-letter)
                 (function-item :tag "Colored status letter"
                                git-modeline-decoration-colored-letter)
                 (const :tag "No decoration" nil)
                 (function :tag "Other"))
  :group 'git-modeline
)

(defun git-modeline--interpret-state-mode-color (stat)
  "Return a mode line status color appropriate for STAT (a state symbol)."
  (cl-case stat
    (modified  "tomato"      )
    (unknown   "gray"        )
    (added     "blue"        )
    (deleted   "red"         )
    (unmerged  "purple"      )
    (uptodate  "GreenYellow" )
    (staged    "yellow"      )
    (t "gray")))


;; Modeline decoration options
(defun git-modeline-decoration-small-dot (stat)
  "Return a small colored dot for the state symbol STAT."
  (git-modeline--state-mark-dot
   (git-modeline--interpret-state-mode-color stat) stat
"/* XPM */
static char * data[] = {
\"14 7 3 1\",
\" 	c None\",
\"+	c #202020\",
\".	c %s\",
\"      +++     \",
\"     +...+    \",
\"    +.....+   \",
\"    +.....+   \",
\"    +.....+   \",
\"     +...+    \",
\"      +++     \"};"))

(defun git-modeline-decoration-large-dot (stat)
  "Return a large colored dot for the state symbol STAT."
  (git-modeline--state-mark-dot
   (git-modeline--interpret-state-mode-color stat) stat
"/* XPM */
static char * data[] = {
\"18 13 3 1\",
\" 	c None\",
\"+	c #000000\",
\".	c %s\",
\"                  \",
\"       +++++      \",
\"      +.....+     \",
\"     +.......+    \",
\"    +.........+   \",
\"    +.........+   \",
\"    +.........+   \",
\"    +.........+   \",
\"    +.........+   \",
\"     +.......+    \",
\"      +.....+     \",
\"       +++++      \",
\"                  \"};"))

(defun git-modeline--interpret-state-mode-letter (stat)
  "Return the one-letter mode line abbreviation for the state symbol STAT."
   (cl-case stat
     (modified  "M")
     (unknown   "?")
     (added     "A")
     (deleted   "D")
     (unmerged  "!")
     (uptodate  "U")
     (staged    "S")
     (t "")))

(defsubst git-modeline--state-mark-tooltip (stat)
  "Return the tooltip text describing the state symbol STAT."
  (format "File status in git: %s" stat))

(defun git-modeline-decoration-letter (stat)
  "Return the status letter for the state symbol STAT."
  (propertize
   (concat (git-modeline--interpret-state-mode-letter stat) " ")
   'help-echo (git-modeline--state-mark-tooltip stat)))

(defun git-modeline-decoration-colored-letter (stat)
  "Return the status letter for the state symbol STAT, colored."
  (propertize
   (concat
    (propertize
     (git-modeline--interpret-state-mode-letter stat)
     'face (list ':foreground (git-modeline--interpret-state-mode-color stat)))
    " ")
   'help-echo (git-modeline--state-mark-tooltip stat)))

;; Modeline decoration implementation
(defvar git-modeline--state-mark t
  "Marker symbol for our entry in `mode-line-format'.
Must remain non-nil: `mode-line-format' evaluates each `(SYMBOL . VALUE)'
cell and only renders VALUE when SYMBOL's value is non-nil.")

(defun git-modeline--dot-displayable-p ()
  "Return non-nil if the current frame can display the XPM dot."
  (and (display-graphic-p) (image-type-available-p 'xpm)))

(defun git-modeline--state-mark-dot (color stat img)
  "Return a mode line image built from the XPM template IMG.
COLOR fills the dot and STAT is used for the tooltip.  On a terminal,
where no image can be shown, fall back to the colored status letter."
  (if (not (git-modeline--dot-displayable-p))
      (git-modeline-decoration-colored-letter stat)
    (propertize "    "
                'help-echo (git-modeline--state-mark-tooltip stat)
                'display
                `(image :type xpm
                        :data ,(format img color)
                        :ascent center))))

(defun git-modeline--decoration-dispatch (stat)
  "Render the state symbol STAT with `git-modeline-decoration'."
  (if (functionp git-modeline-decoration)
      (funcall git-modeline-decoration stat)))

(defun git-modeline--install-state-mark (stat)
  "Prepend the mark for the state symbol STAT to `mode-line-format'."
  (push `(git-modeline--state-mark
          ,(git-modeline--decoration-dispatch stat))
        mode-line-format)
  )

(defun git-modeline--uninstall-state-mark ()
  "Remove our mark from `mode-line-format' in the current buffer."
  (setq mode-line-format
        (delq nil (mapcar #'(lambda (mode)
                              (unless (eq (car-safe mode)
                                          'git-modeline--state-mark)
                                mode))
                   mode-line-format)))
  )

(defun git-modeline--update-state-mark (stat)
  "Refresh the mode line mark so that it shows the state symbol STAT."
  (git-modeline--uninstall-state-mark)
  (git-modeline--install-state-mark stat))

(provide 'git-modeline)

;;; git-modeline.el ends here
