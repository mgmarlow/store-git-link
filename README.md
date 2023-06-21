# store-git-link

An emacs package for sharing code links with colleagues. Currently supports Github and Sourcehut.

## Commands

- `M-x store-git-link`: Copies a link to the current line of code.
- `M-x store-git-link-commit`: Copies a link to the commit hash associated responsible for the current line of code, determined via `git blame`.

## Installation

With Emacs 29:

```elisp
(use-package store-git-link
  :vc (:fetcher sourcehut :repo mgmarlow/store-git-link))
```

## License

Released under the [GPL-3.0 license](./LICENSE).

## Contributing

Please contribute improvements via email to [the mailing list](https://lists.sr.ht/~mgmarlow/public-inbox) using [git send-email](https://git-send-email.io/). When posting patches, edit the `[PATCH]` line to include `store-git-link`:

```
[PATCH store-git-link] Add thing to stuff
```

Learn more about contributing via email from [Sourcehut's documentation](https://man.sr.ht/lists.sr.ht/etiquette.md).
