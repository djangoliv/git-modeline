;;; git-modeline.el --- display git status as mark in modeline -*- lexical-binding: t -*-

;; Copyright (C) 2022 xl666

;; Author: djangoliv (ogiorgis)
;; URL: https://github.com/djangoliv/git-modeline
;; Version: 0.1

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
;; Add a dot in the modeline that indicates the state of the source file.

;;; Code:

;;; General

(require 'cl-lib)
(require 'vc)                           ; vc
(require 'vc-git)                       ; vc-git advises
;;-----------------------------------------------------------------------------
;; Internal variables.
;;-----------------------------------------------------------------------------

(defvar git--executable "git" "Main git executable")
(defconst git--reg-status  "\\([A-Z?]\\)")
(defconst git--reg-blank   "[\t\0 ]+")
(defconst git--reg-eof     "\0")
(defconst git--reg-perm    "\\([0-7]\\{6\\}\\)")
(defconst git--reg-sha1    "\\([0-9a-f]\\{40\\}\\)")
(defconst git--reg-file    "\\([^\0]+\\)")

;;-----------------------------------------------------------------------------
;; Low-level execution functions.
;;-----------------------------------------------------------------------------

(defsubst git--exec (cmd outbuf infile &rest args)
  "Low level function for calling git.
CMD is the main git subcommand, ARGS are the remaining args.  See
`call-process' for the meaning of OUTBUF and INFILE.  Returns git's
exit code."
  (apply #'call-process git--executable infile outbuf nil (cons cmd args)))

(defsubst git--exec-buffer (cmd &rest args)
  "Execute \\='git\\=' within the buffer.  Return the exit code."
  (apply #'git--exec cmd t nil args))

(defsubst git--interpret-to-state-symbol (stat)
  "Interpret a one-letter git state string to our state symbols."
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

(defsubst git--build-reg (&rest args)
  (apply #'concat (append args (list "\0"))))

;;-----------------------------------------------------------------------------
;; fileinfo structure
;;-----------------------------------------------------------------------------

;; ewoc file info structure for each list element
(cl-defstruct (git--fileinfo
            (:copier nil)
            (:constructor git--create-fileinfo
                          (name type &optional sha1 perm marked
                                               stat size refresh))
            (:conc-name git--fileinfo->))
  marked   ;; t/nil
  expanded ;; t/nil
  refresh  ;; t/nil
  stat     ;; 'unknown/'modified/'uptodate/'staged  etc.
  type     ;; 'blob/'tree/'commit (i.e. submodule)
  name     ;; filename
  size     ;; size
  perm     ;; permission
  sha1)    ;; sha1

(defsubst git--fileinfo-is-dir (info)
  "Returns true if a file info is directory-like (expandable, sorted first)"
  (not (eq 'blob (git--fileinfo->type info))))

(defsubst git--fileinfo-dir (info)
  "Returns the directory component of a fileinfo's path. If the fileinfo is
directory-like, the directory is the path itself, with a slash appended."
  (if (git--fileinfo-is-dir info)
      (file-name-as-directory (git--fileinfo->name info))
    (or (file-name-directory (git--fileinfo->name info)) "")))

(defun git--fileinfo-lessp (info1 info2)
  "Sorting rule for git--fileinfos, such that the ordering in git-status is
right. The rule is rather complicated, but it basically results in a
properly expanded tree."
  (let ((info1-dir (git--fileinfo-dir info1))
        (info2-dir (git--fileinfo-dir info2)))
    (let ((cmp (compare-strings info1-dir 0 nil info2-dir 0 nil)))
      (if (not (eq t cmp))
          ;; A file in a subdirectory should always come before a file
          ;; in the parent directory.
          (if (< cmp 0)
              ;; info1-dir < info2-dir
              (if (eq (length info1-dir) (- -1 cmp))
                  ;; info1-dir is a subdir of info2-dir. less == false,
                  ;; unless info1 is a directory itself.
                  (git--fileinfo-is-dir info1)
                t)
            ;; info1-dir > info2-dir
            (if (eq (length info2-dir) (- cmp 1))
                ;; info2-dir is a subdir of info1-dir. less == true, unless
                ;; info2 is a directory itself.
                (not (git--fileinfo-is-dir info2))
              nil))
        ;; same directory, straight-up comparison
        (string< (git--fileinfo->name info1)
                 (git--fileinfo->name info2))))))

(defun git--status-index (&rest files)
  "Execute \\='git-status-index\\=' and return a list of `git--fileinfo'."

  ;; update fileinfo -> unmerged index
  (let ((fileinfo nil)
        (unmerged-info (make-hash-table :test 'equal))
        (regexp (git--build-reg ":"
                                git--reg-perm    ; matched-1: HEAD perms
                                git--reg-blank
                                git--reg-perm    ; matched-2: index perms
                                git--reg-blank
                                git--reg-sha1    ; matched-3: HEAD sha1
                                git--reg-blank
                                git--reg-sha1    ; matched-4: index sha1
                                git--reg-blank
                                git--reg-status  ; matched-5
                                git--reg-eof
                                git--reg-file    ; matched-6
                                )))

    (with-temp-buffer
      (apply #'git--diff-raw (list "HEAD") files)

      (goto-char (point-min))

      (while (re-search-forward regexp nil t)
        (let ((perm (match-string 2))
              (stat (git--interpret-to-state-symbol (match-string 5)))
              (file (match-string 6)))

          ;; if unmerged file
          (when (gethash file unmerged-info) (setq stat 'unmerged))
          ;; modified vs. staged: the latter has a nonzero sha1
          (when (and (eq stat 'modified)
                     (not (equal (match-string 4)
                                 "0000000000000000000000000000000000000000")))
            (setq stat 'staged))

          ;; assume all listed elements are 'blob
          (push (git--create-fileinfo file 'blob nil perm nil stat) fileinfo))))

    fileinfo))

(defsubst git--diff-raw (args &rest files)
  "Execute \\='git diff --raw\\=' with ARGS and FILES at current buffer.
This gives, essentially, file status."
  ;; git-diff abbreviates by default, and also produces a diff.
  (apply #'git--exec-buffer "diff" "-z" "--full-index" "--raw" "--abbrev=40"
         (append args (list "--") files)))

(defun git--ls-files (&rest args)
  "Execute \\='git-ls-files\\=' with ARGS and return the list of
`git--fileinfo'.  Does not differentiate between `modified' and
`staged'."

  (let (fileinfo)
    (with-temp-buffer
      (apply #'git--exec-buffer "ls-files" "-t" "-z" args)
      (goto-char (point-min))

      (let ((regexp (git--build-reg git--reg-status ; matched-1
                                    git--reg-blank
                                    git--reg-file))) ; matched-2

        (while (re-search-forward regexp nil t)
          (let* ((stat (match-string 1))
                 (name (match-string 2))
                 (file-name (directory-file-name name)))
            ;; Files listed with e.g "-o" might be directories
            (push (git--create-fileinfo file-name
                                        (if (equal name file-name) 'blob
                                          'tree)
                                        nil nil nil
                                        (git--interpret-to-state-symbol stat))
                  fileinfo)))))
    (sort fileinfo 'git--fileinfo-lessp)))

;;-----------------------------------------------------------------------------
;; git application
;;-----------------------------------------------------------------------------

(defun git--status-file (file)
  "Return the git status of FILE, as a symbol."
  (let ((fileinfo (git--status-index file)))
    (unless fileinfo
      (setq fileinfo
            (git--ls-files "-c" "-o" "--exclude-standard" file)))
    (when (= 1 (length fileinfo))
      (git--fileinfo->stat (car fileinfo)))))

;;-----------------------------------------------------------------------------
;; vc-git integration
;;-----------------------------------------------------------------------------

(defsubst git--in-git-repo? ()
  "Return non-nil if the current buffer's file lives inside a git repo.
Unlike a `vc-mode' check, this also matches untracked files."
  (and buffer-file-name (vc-git-root buffer-file-name)))

(defun git--update-modeline ()
  "Update the current's buffer modeline state display."
  ;; mark depending on the fileinfo state
  (when (git--in-git-repo?)
    (git--update-state-mark
     (git--status-file (file-relative-name buffer-file-name)))))

;;;###autoload
(define-minor-mode git-modeline-mode
  "Toggle display of git status as a colored dot in the modeline.
When enabled, every buffer visiting a file under git shows a dot
at the start of `mode-line-format'.  The color reflects the git
status of the file."
  :global t
  :group 'git-emacs
  (cond
   (git-modeline-mode
    (advice-add 'vc-after-save :after #'git--update-modeline)
    (add-hook 'find-file-hook #'git--update-modeline t)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (git--update-modeline))))
   (t
    (advice-remove 'vc-after-save #'git--update-modeline)
    (remove-hook 'find-file-hook #'git--update-modeline)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (git--uninstall-state-mark-modeline))))))

;;-------------------------------------------------------------------------
;; modeline
;;---------------------------------------------------------------------------

(defgroup git-emacs nil
  "Display git status as a mark in the modeline."
  :group 'vc)

;; Modeline decoration customization
(defcustom git-state-modeline-decoration
  'git-state-decoration-large-dot
  "How to indicate the status of files in the modeline.
The value must be a function that takes a single arg: a symbol denoting
file status, e.g. `unmerged'.  The return value of the function will be
added at the beginning of `mode-line-format'."
  :type '(choice (function-item :tag "Small colored dot"
                                git-state-decoration-small-dot)
                 (function-item :tag "Large colored dot"
                                git-state-decoration-large-dot)
                 (function-item :tag "Status letter"
                                git-state-decoration-letter)
                 (function-item :tag "Colored status letter"
                                git-state-decoration-colored-letter)
                 (const :tag "No decoration" nil)
                 (function :tag "Other"))
  :group 'git-emacs
)

(defun git--interpret-state-mode-color (stat)
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
(defun git-state-decoration-small-dot(stat)
  (git--state-mark-modeline-dot
   (git--interpret-state-mode-color stat) stat
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

(defun git-state-decoration-large-dot(stat)
  (git--state-mark-modeline-dot
   (git--interpret-state-mode-color stat) stat
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

(defun git--interpret-state-mode-letter(stat)
   (cl-case stat
     (modified  "M")
     (unknown   "?")
     (added     "A")
     (deleted   "D")
     (unmerged  "!")
     (uptodate  "U")
     (staged    "S")
     (t "")))

(defsubst git--state-mark-tooltip(stat)
  (format "File status in git: %s" stat))

(defun git-state-decoration-letter(stat)
  (propertize
   (concat (git--interpret-state-mode-letter stat) " ")
   'help-echo (git--state-mark-tooltip stat)))

(defun git-state-decoration-colored-letter(stat)
  (propertize
   (concat 
    (propertize 
     (git--interpret-state-mode-letter stat)
     'face (list ':foreground (git--interpret-state-mode-color stat)))
    " ")
   'help-echo (git--state-mark-tooltip stat)))

;; Modeline decoration implementation
(defvar git--state-mark-modeline t
  "Marker symbol for our entry in `mode-line-format'.
Must remain non-nil: `mode-line-format' evaluates each `(SYMBOL . VALUE)'
cell and only renders VALUE when SYMBOL's value is non-nil.")

(defun git--state-mark-modeline-dot (color stat img)
  (propertize "    "
              'help-echo (git--state-mark-tooltip stat)
              'display
              `(image :type xpm
                      :data ,(format img color)
                      :ascent center)))

(defun git--state-decoration-dispatch(stat)
  (if (functionp git-state-modeline-decoration)
      (funcall git-state-modeline-decoration stat)))

(defun git--install-state-mark-modeline (stat)
  (push `(git--state-mark-modeline
          ,(git--state-decoration-dispatch stat))
        mode-line-format)
  )

(defun git--uninstall-state-mark-modeline ()
  (setq mode-line-format
        (delq nil (mapcar #'(lambda (mode)
                              (unless (eq (car-safe mode)
                                          'git--state-mark-modeline)
                                mode))
                   mode-line-format)))
  )

(defun git--update-state-mark (stat)
  (git--uninstall-state-mark-modeline)
  (git--install-state-mark-modeline stat))

(provide 'git-modeline)
