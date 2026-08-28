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

You can also set it to any function of one argument (the state symbol)
that returns a string suitable for `mode-line-format`.

## Requirements

- Emacs with `vc`/`vc-git` (built-in)
- `git` on `$PATH`

No external Emacs packages.
