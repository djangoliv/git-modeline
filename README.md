git-modeline: display the git status as dot in the modeline
-----------------------------------------------------------


The dot in the modeline indicates the state of this source file

green: uptodate
gray: unknown
tomato: modified
red: deleted
purple: unmerged

Installation instruction
------------------------

```
  (add-to-list 'load-path "/path/to/git-modeline")
  (require 'git-modeline)
  (git-modeline-mode 1)
```

Toggle the dot on/off interactively with `M-x git-modeline-mode`.
