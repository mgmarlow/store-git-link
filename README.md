# store-git-link

A command to easily share code links with colleagues. Currently supports Github and Sourcehut.

Example:

```
M-x store-git-link

> Copied "https://github.com/mgmarlow/store-git-link/blob/main/store-git-link-test.el#L12" to clipboard.
```

## Installation

With Emacs 29:

```elisp
(unless (package-installed-p 'store-git-link)
  (package-vc-install "https://git.sr.ht/~mgmarlow/store-git-link"))
```
