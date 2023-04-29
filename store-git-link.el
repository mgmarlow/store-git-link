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

;;

;;; Code:

(require 'vc)

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
  (cond ((s-contains? "github.com" basename)
         (sgl--format-github basename branch rel-filename loc))
        ((s-contains? "git.sr.ht" basename)
         (sgl--format-sourcehut basename branch rel-filename loc))
        (t (error "Unsupported git remote passed to `sgl--format'."))))

(defun sgl--maybe-remove-extension (uri)
  "Removes '.git' from a repo URI, if it exists."
  (if (s-ends-with? ".git" uri)
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
  (if (s-starts-with? "https" repo-uri)
        (sgl--https-basename repo-uri)
      (sgl--ssh-basename repo-uri)))

(defun sgl--generate-link (filename &optional branchname)
  "Returns a git link.

Uses BRANCHNAME or defaults to first result of `vc-git-branches'."
  (let ((basename (sgl--repo-basename (vc-git-repository-url filename)))
        (branch (or branchname (car (vc-git-branches))))
        (rel-filename (file-relative-name filename (vc-root-dir))))
    (sgl--format basename branch rel-filename (line-number-at-pos))))

(defun store-git-link ()
  "Store a web link to git repository at current point in the kill ring."
  (interactive)
  (let ((filename (buffer-file-name (current-buffer))))
    (unless (vc-git-registered filename)
      (error "File not tracked by git."))
    (let ((code-link (sgl--generate-link filename)))
      (kill-new code-link)
      (message (concat "Copied " code-link " to clipboard.")))))

(provide 'store-git-link)
;;; store-git-link.el ends here
