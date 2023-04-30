;;; store-git-link.el --- Stores a web link to git repository at current point  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Graham Marlow

;; Author: Graham Marlow <info@mgmarlow.com>
;; Keywords: vc, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; An emacs package for sharing code links with colleagues.
;;
;; Provides the command `store-git-link' that saves a link to the current
;; point as a URI to the remote host. This makes it easy to share reference
;; links with colleagues when digging through code on your local machine.
;;
;; E.g. M-x store-code-link ->
;;   https://github.com/mgmarlow/store-git-link/blob/main/store-git-link-test.el#L12

;;; Code:

(require 'vc)
(require 'vc-git)

(defgroup store-git-link ()
  "Generate links to git repositories directly from source."
  :group 'tools)

(defcustom sgl-prefer-current-branch t
  "If non-nil, always selects the current branch when generating links.

When nil, opens a minibuffer prompt for branch selection."
  :group 'store-git-link
  :type 'boolean)

(defun sgl--format-sourcehut (basename branch rel-filename loc)
  "Formats a Sourcehut link."
  (format "https://%s/tree/%s/item/%s"
          basename
          branch
          (concat rel-filename "#L" (number-to-string loc))))

(defun sgl--format-github (basename branch rel-filename loc)
  "Formats a Github link."
  (format "https://%s/blob/%s/%s"
          basename
          branch
          (concat rel-filename "#L" (number-to-string loc))))

(defun sgl--format (basename branch rel-filename loc)
  "Root formatter. Assigns format function based on BASENAME."
  (cond ((string-match-p "github.com" basename)
         (sgl--format-github basename branch rel-filename loc))
        ((string-match-p "git.sr.ht" basename)
         (sgl--format-sourcehut basename branch rel-filename loc))
        (t (error "Unsupported git remote passed to `sgl--format'."))))

(defun sgl--maybe-remove-extension (uri)
  "Removes '.git' from a repo URI, if it exists."
  (if (string-suffix-p ".git" uri)
      (substring uri 0 (* (length ".git") -1))
    uri))

(defun sgl--https-basename (repo-uri)
  "Extracts basename from HTTPS repository URI.

Examples:
https://github.com/user/repo.git -> github.com/user/repo"
  (sgl--maybe-remove-extension (substring repo-uri (length "https://"))))

(defun sgl--ssh-basename (repo-uri)
  "Extracts basename from SSH repository URI.

Examples:
git@github.com:user/repo.git -> github.com/user/repo
git@git.sr.ht:~user/repo     -> git.sr.ht/~user/repo"
  (sgl--maybe-remove-extension
   (replace-regexp-in-string ":" "/" (substring repo-uri (length "git@")))))

(defun sgl--repo-basename (repo-uri)
  "Extracts basename from repository URI."
  (if (string-prefix-p "https" repo-uri)
        (sgl--https-basename repo-uri)
      (sgl--ssh-basename repo-uri)))

(defun sgl--branch-prompt ()
  "When `sgl-prefer-current-branch' is nil, prompt for branch selection
if there's more than one choice. Otherwise use the current branch."
  (let ((branches (vc-git-branches)))
    (if (or sgl-prefer-current-branch (= (length branches) 1))
        (car branches)
      (completing-read "Pick a branch: " branches nil t))))

(defun sgl--generate-link (filename)
  "Generates a link to the repository of current buffer at current line number."
  (let ((basename (sgl--repo-basename (vc-git-repository-url filename)))
        (branch (sgl--branch-prompt))
        (rel-filename (file-relative-name filename (vc-root-dir))))
    (sgl--format basename branch rel-filename (line-number-at-pos))))

(defun sgl--copy-link (link)
  (kill-new link)
  (message (concat "Copied " link " to clipboard.")))

;;;###autoload
(defun store-git-link ()
  "Store a web link to git repository at current point in the kill ring."
  (interactive)
  (let ((filename (buffer-file-name (current-buffer))))
    (unless (and filename (vc-git-registered filename))
      (error "File must be version controlled by git to use `store-git-link'."))
    (sgl--copy-link (sgl--generate-link filename))))

(defun sgl--format-commit (basename commit)
  (format "https://%s/commit/%s" basename commit))

(defun sgl--blame (filename loc)
  "Calls git blame on filename with LOC, converting output to a string."
  (let* ((loc (number-to-string loc))
         (command (concat "git blame " filename " -L " loc "," loc))
         (result (shell-command-to-string command)))
    (when (string-prefix-p "fatal" result)
      (error result))
    result))

(defun sgl--extract-commit (blame-string)
  "Extracts commit hash from blame string, stripping leading ^ if present."
  (let ((hash (car (string-split blame-string " "))))
    (if (string-prefix-p "^" hash)
        (substring hash 1)
      hash)))

(defun sgl--commit (filename loc)
  "Returns commit hash from git blame for filename at LOC."
  (sgl--extract-commit (sgl--blame filename loc)))

;;;###autoload
(defun store-git-link-commit ()
  "Store a link to the commit associated with current point in the kill ring."
  (interactive)
  (let ((filename (buffer-file-name (current-buffer))))
    (unless (and filename (vc-git-registered filename))
      (error "File must be version controlled by git to use `store-git-link-commit'."))
    (let ((basename (sgl--repo-basename (vc-git-repository-url filename)))
          (commit (sgl--commit filename (line-number-at-pos))))
      (sgl--copy-link (sgl--format-commit basename commit)))))

(provide 'store-git-link)
;;; store-git-link.el ends here
