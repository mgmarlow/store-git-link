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

(defun store-code-link--generate-link (filename)
  (let ((rel-filename (file-relative-name filename (vc-root-dir))))))

(vc-git-registered (buffer-file-name (current-buffer)))

(defun store-git-link ()
  "Store a web link to git repository at current point in the kill ring."
  (interactive)
  (let ((filename (buffer-file-name (current-buffer))))
    (unless (vc-git-registered filename)
      (error "File not tracked by git."))
    (let ((code-link (store-git-link--generate-link filename)))
      (kill-new code-link)
      (message (concat "Copied " code-link " to clipboard.")))))

(provide 'store-git-link)
;;; store-git-link.el ends here
