;;; git-modeline.el --- Show the git status of the current file in the modeline -*- lexical-binding: t -*-

;; Copyright (C) 2022 xl666
;; Copyright (C) 2022-2026 djangoliv

;; Author: djangoliv <olivier.giorgis@quantstack.net>
;; Maintainer: djangoliv <olivier.giorgis@quantstack.net>
;; Assisted-by: Claude:claude-opus-5
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
(require 'subr-x)                       ; string-trim

(declare-function file-notify-add-watch "filenotify" (file flags callback))
(declare-function file-notify-rm-watch "filenotify" (descriptor))
(defvar magit-post-refresh-hook)

(defgroup git-modeline nil
  "Display git status as a mark in the modeline."
  :group 'vc
  :prefix "git-modeline-")

(defcustom git-modeline-executable "git"
  "Name of, or path to, the git executable."
  :type 'string
  :group 'git-modeline)

;;-----------------------------------------------------------------------------
;; Internal variables.
;;-----------------------------------------------------------------------------

(defconst git-modeline--reg-status  "\\([A-Z?]\\)")
(defconst git-modeline--reg-blank   "[\t\0 ]+")
(defconst git-modeline--reg-eof     "\0")
(defconst git-modeline--reg-perm    "\\([0-7]\\{6\\}\\)")
(defconst git-modeline--reg-sha1    "\\([0-9a-f]\\{40\\}\\)")
(defconst git-modeline--reg-file    "\\([^\0]+\\)")

;;-----------------------------------------------------------------------------
;; Low-level execution functions.
;;-----------------------------------------------------------------------------

(defvar git-modeline--executable-checked nil
  "Last value of `git-modeline-executable' found to be runnable.")

(defun git-modeline--executable-p ()
  "Return non-nil if `git-modeline-executable' can be run.
The answer is remembered so that the lookup does not run for every
visited file, and redone whenever the option changes."
  (or (equal git-modeline--executable-checked git-modeline-executable)
      (when (executable-find git-modeline-executable)
        (setq git-modeline--executable-checked git-modeline-executable))))

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
  (let ((root (and (git-modeline--executable-p)
                   (git-modeline--in-repo-p))))
    (when root
      (git-modeline--watch-repository root)
      (git-modeline--update-state-mark
       (git-modeline--status-file (file-relative-name buffer-file-name))))))

;;-----------------------------------------------------------------------------
;; Refresh on external changes
;;-----------------------------------------------------------------------------

(defcustom git-modeline-watch-index t
  "Whether to watch each repository index for changes made outside Emacs.
When non-nil, a file notification watch on the git directory refreshes
the mark after e.g. a `git add\=' or a commit run from a terminal.  Set
to nil to rely only on `find-file\=', saving and
`git-modeline-refresh\='."
  :type 'boolean
  :group 'git-modeline)

(defcustom git-modeline-refresh-delay 0.5
  "Seconds to wait before refreshing after the git index changed.
A single git operation writes the index several times; waiting coalesces
those writes into one refresh, and leaves git time to finish."
  :type 'number
  :group 'git-modeline)

(defvar git-modeline--watchers (make-hash-table :test 'equal)
  "Hash table mapping a repository root to its file notification watch.")

(defvar git-modeline--refresh-timer nil
  "Timer coalescing the refreshes triggered by index changes.")

(defun git-modeline-refresh (&optional root)
  "Refresh the mark of every buffer visiting a file under ROOT.
With ROOT nil, refresh every buffer visiting a file."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and buffer-file-name
                 (or (null root) (file-in-directory-p buffer-file-name root)))
        (git-modeline--update))))
  ;; Buffers refreshed from a timer are not necessarily the current one:
  ;; ask for their mode line to be redrawn.
  (force-mode-line-update t))

(defun git-modeline--schedule-refresh (root)
  "Refresh the buffers under ROOT after `git-modeline-refresh-delay\='.
A plain timer is used rather than an idle one: the index changes while
Emacs waits, so an idle timer armed from the notification callback is not
guaranteed to run."
  (when (timerp git-modeline--refresh-timer)
    (cancel-timer git-modeline--refresh-timer))
  (setq git-modeline--refresh-timer
        (run-with-timer git-modeline-refresh-delay nil
                        #'git-modeline-refresh root)))

(defconst git-modeline--index-files '("index" "index.lock" "HEAD")
  "Names, in the git directory, whose change can change a file status.")

(defun git-modeline--index-event-p (event)
  "Return non-nil if file notification EVENT touches the git index.
Both names carried by EVENT are checked: git does not write the index in
place, it writes `index.lock\=' and renames it over `index\=', and a
`renamed\=' event holds the old name in third position and the new one
in fourth."
  (and (memq (nth 1 event)
             '(created changed renamed renamed-from renamed-to
                       attribute-changed))
       (cl-some (lambda (file)
                  (and (stringp file)
                       (member (file-name-nondirectory file)
                               git-modeline--index-files)))
                (list (nth 2 event) (nth 3 event)))))

(defun git-modeline--git-dir (root)
  "Return the absolute git directory of the repository at ROOT, or nil.
ROOT may be a worktree or a submodule, whose \=`.git\=' is a file."
  (with-temp-buffer
    (let ((default-directory root))
      (when (zerop (git-modeline--exec-buffer "rev-parse" "--absolute-git-dir"))
        (let ((dir (string-trim (buffer-string))))
          (and (file-directory-p dir) dir))))))

(defun git-modeline--watch-repository (root)
  "Watch the git directory of the repository at ROOT for index changes.
Do nothing if ROOT is already watched, or if this Emacs has no file
notification support."
  (when (and git-modeline-watch-index
             ;; A nil value means "tried, and no watch could be set": keep it,
             ;; so that we do not run rev-parse again for that repository.
             (eq 'missing (gethash root git-modeline--watchers 'missing))
             (require 'filenotify nil t))
    (let ((git-dir (git-modeline--git-dir root)))
      (when git-dir
        ;; Watch the directory rather than the index itself: git replaces
        ;; the index by renaming index.lock over it, which would drop a
        ;; watch set on the file.
        (puthash root
                 (ignore-errors
                   (file-notify-add-watch
                    git-dir '(change)
                    (lambda (event)
                      (when (git-modeline--index-event-p event)
                        (git-modeline--schedule-refresh root)))))
                 git-modeline--watchers)))))

(defun git-modeline--unwatch-all ()
  "Stop watching every repository and cancel any pending refresh."
  (when (timerp git-modeline--refresh-timer)
    (cancel-timer git-modeline--refresh-timer)
    (setq git-modeline--refresh-timer nil))
  (maphash (lambda (_root descriptor)
             (when descriptor (ignore-errors (file-notify-rm-watch descriptor))))
           git-modeline--watchers)
  (clrhash git-modeline--watchers))

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
    (advice-add 'vc-refresh-state :after #'git-modeline--update)
    (add-hook 'find-file-hook #'git-modeline--update t)
    (add-hook 'after-revert-hook #'git-modeline--update)
    ;; Magit knows when it changed the index; ask it to tell us.
    (add-hook 'magit-post-refresh-hook #'git-modeline-refresh)
    (unless (git-modeline--executable-p)
      (message "git-modeline: %s not found in `exec-path', no mark will be shown"
               git-modeline-executable))
    (git-modeline-refresh))
   (t
    (advice-remove 'vc-after-save #'git-modeline--update)
    (advice-remove 'vc-refresh-state #'git-modeline--update)
    (remove-hook 'find-file-hook #'git-modeline--update)
    (remove-hook 'after-revert-hook #'git-modeline--update)
    (remove-hook 'magit-post-refresh-hook #'git-modeline-refresh)
    (git-modeline--unwatch-all)
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
  :group 'git-modeline)

(defface git-modeline-uptodate
  '((t :foreground "GreenYellow"))
  "Face for a tracked file with no local change."
  :group 'git-modeline)

(defface git-modeline-modified
  '((t :foreground "tomato"))
  "Face for a tracked file with unstaged changes."
  :group 'git-modeline)

(defface git-modeline-staged
  '((t :foreground "yellow"))
  "Face for a file with staged changes."
  :group 'git-modeline)

(defface git-modeline-added
  '((t :foreground "blue"))
  "Face for a new file added to the index."
  :group 'git-modeline)

(defface git-modeline-deleted
  '((t :foreground "red"))
  "Face for a file removed from the working tree."
  :group 'git-modeline)

(defface git-modeline-unmerged
  '((t :foreground "purple"))
  "Face for a file with a merge conflict."
  :group 'git-modeline)

(defface git-modeline-unknown
  '((t :foreground "gray"))
  "Face for an untracked file, and for any unrecognized state."
  :group 'git-modeline)

(defun git-modeline--interpret-state-mode-face (stat)
  "Return the face used to render the state symbol STAT."
  (cl-case stat
    (modified  'git-modeline-modified )
    (unknown   'git-modeline-unknown  )
    (added     'git-modeline-added    )
    (deleted   'git-modeline-deleted  )
    (unmerged  'git-modeline-unmerged )
    (uptodate  'git-modeline-uptodate )
    (staged    'git-modeline-staged   )
    (t         'git-modeline-unknown  )))

(defun git-modeline--interpret-state-mode-color (stat)
  "Return a mode line status color appropriate for STAT (a state symbol).
The color is the foreground of the face matching STAT, so that themes
can override it.  Used to fill the XPM dot, which needs a color string."
  (or (face-foreground (git-modeline--interpret-state-mode-face stat) nil t)
      "gray"))


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
     'face (git-modeline--interpret-state-mode-face stat))
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
        mode-line-format))

(defun git-modeline--uninstall-state-mark ()
  "Remove our mark from `mode-line-format' in the current buffer."
  (setq mode-line-format
        (delq nil (mapcar #'(lambda (mode)
                              (unless (eq (car-safe mode)
                                          'git-modeline--state-mark)
                                mode))
                   mode-line-format))))

(defun git-modeline--update-state-mark (stat)
  "Refresh the mode line mark so that it shows the state symbol STAT."
  (git-modeline--uninstall-state-mark)
  (git-modeline--install-state-mark stat))

(provide 'git-modeline)

;;; git-modeline.el ends here
