# git-modeline

Display the git status of the file visited by the current buffer as a
colored dot at the beginning of Emacs' modeline.

## Color legend

| Color        | State    | Meaning                                           |
|--------------|----------|---------------------------------------------------|
| GreenYellow  | uptodate | Tracked, no local changes                         |
| tomato       | modified | Tracked with unstaged changes                     |
| yellow       | staged   | Staged changes ready to commit                    |
| blue         | added    | New file added to the index                       |
| red          | deleted  | Removed from the working tree                     |
| purple       | unmerged | Conflict during merge                             |
| gray         | unknown  | Untracked file inside a git repo                  |

Each state has its own face — `git-modeline-uptodate`,
`git-modeline-modified`, `git-modeline-staged`, `git-modeline-added`,
`git-modeline-deleted`, `git-modeline-unmerged`, `git-modeline-unknown` —
so a theme or `M-x customize-face` can override the colors above. The
foreground of the face fills the dot as well as the letter.

## Installation

### Manual

```elisp
(add-to-list 'load-path "/path/to/git-modeline")
(require 'git-modeline)
(git-modeline-mode 1)
```

### With [straight.el](https://github.com/radian-software/straight.el)

```elisp
(straight-use-package
 '(git-modeline :type git :host github :repo "djangoliv/git-modeline"))
(git-modeline-mode 1)
```

Or with `use-package` integration:

```elisp
(use-package git-modeline
  :straight (:host github :repo "djangoliv/git-modeline")
  :config (git-modeline-mode 1))
```

Toggle the dot on or off interactively with `M-x git-modeline-mode`.

## Refreshing

The mark is refreshed when a file is visited, when it is saved or
reverted, and on `M-x vc-refresh-state`. Changes made outside the buffer
are picked up too:

- a file notification watch on each repository's git directory catches a
  `git add`, `git reset` or commit run from a terminal;
- `magit-post-refresh-hook` catches the same operations run from Magit.

Set `git-modeline-watch-index` to nil to disable the watches, and
`git-modeline-refresh-delay` to change how long index writes are
coalesced (0.5s by default). `M-x git-modeline-refresh` refreshes every
buffer by hand.

## Customization

`git-modeline-decoration` controls how the state is rendered.
Pick one with `M-x customize-variable RET git-modeline-decoration`:

| Value                                 | Look                             |
|---------------------------------------|----------------------------------|
| `git-modeline-decoration-large-dot`      | large filled dot (default)       |
| `git-modeline-decoration-small-dot`      | small filled dot                 |
| `git-modeline-decoration-letter`         | single status letter (U/M/A/…)   |
| `git-modeline-decoration-colored-letter` | same, colored                    |
| `nil`                                 | no decoration                    |

On a terminal, where images cannot be displayed, both dot variants fall
back to the colored status letter automatically.

You can also set it to any function of one argument (the state symbol)
that returns a string suitable for `mode-line-format`.

## Requirements

- Emacs with `vc`/`vc-git` (built-in)
- `git` on `$PATH`

No external Emacs packages.
