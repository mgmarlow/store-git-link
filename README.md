# store-git-link

An emacs package for sharing code links with colleagues. Currently supports Github and Sourcehut.

## Installation

With Emacs 29:

```elisp
(use-package store-git-link
  :vc (:fetcher sourcehut :repo mgmarlow/store-git-link))
```

Alternatively, clone the repo and update your load path:

```
git clone https://git.sr.ht/~mgmarlow/store-git-link /path/to/store-git-link
```

```
(add-to-list 'load-path "/path/to/store-git-link")
(require 'store-git-link)
```

## Commands

- `M-x store-git-link`: Copies a URL to the current line of code.
- `M-x store-git-link-commit`: Copies a URL to the commit responsible for the current line of code, determined via `git blame`.

### Configuration

- `sgl-open-links-in-browser`: When `t`, opens links in your browser when copied.
- `sgl-prefer-current-branch`: When `t`, always selects the current branch when generating a link.

## Contributing

Please direct bug reports or patches to the [the mailing list](https://lists.sr.ht/~mgmarlow/public-inbox).

## License

Licensed under [GPL-3.0](./LICENSE).
