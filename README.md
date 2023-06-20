# store-git-link

An emacs package for sharing code links with colleagues. Currently supports Github and Sourcehut.

## Commands

- `M-x store-git-link`: Copies a link to the current line of code.
- `M-x store-git-link-commit`: Copies a link to the commit hash associated responsible for the current line of code, determined via `git blame`.

## Installation

With Emacs 29:

```elisp
(unless (package-installed-p 'store-git-link)
  (package-vc-install "https://git.sr.ht/~mgmarlow/store-git-link"))
```

## License

Released under the [GPL-3.0 license](./LICENSE).
