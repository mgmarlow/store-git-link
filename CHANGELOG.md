## 04/30/23

- New command: `store-git-link-commit`. Copies a link to the commit hash associated responsible for the current line of code, determined via git blame.

## 04/29/23

- New custom variable: `sgl-prefer-current-branch`. Defaults to `t`. When `non-nil`, always selects the current branch when generating links. When `nil`, opens a minibuffer prompt for branch selection.
