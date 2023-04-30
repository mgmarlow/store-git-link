;;; store-git-link-test.el --- Unit tests for store-git-link  -*- lexical-binding: t; -*-

(require 'store-git-link)
(require 'ert)

(ert-deftest sgl-test-repo-basename ()
  "Tests the extraction of basename from a repo URI."
  (should (equal (sgl--repo-basename "git@github.com:user/repo.git") "github.com/user/repo"))
  (should (equal (sgl--repo-basename "git@git.sr.ht:~user/repo") "git.sr.ht/~user/repo"))
  (should (equal (sgl--repo-basename "https://github.com/user/repo.git") "github.com/user/repo")))

(ert-deftest sgl-test-maybe-remove-extension ()
  "Tests removal of .git from URIs"
  (should (equal (sgl--maybe-remove-extension "github.com/user/repo.git") "github.com/user/repo"))
  (should (equal (sgl--maybe-remove-extension "github.com/user/repo") "github.com/user/repo")))

(ert-deftest sgl-test-format ()
  "Tests formatting of URI placed on kill ring."
  (should (equal (sgl--format "github.com/user/repo" "main" "foo/bar.el" 323)
                 "https://github.com/user/repo/blob/main/foo/bar.el#L323"))
  (should (equal (sgl--format "git.sr.ht/~user/repo" "main" "foo/bar.el" 323)
                 "https://git.sr.ht/~user/repo/tree/main/item/foo/bar.el#L323")))

(ert-deftest sgl-test-extract-commit ()
  "Test commit extraction from git blame."
  (should (equal (sgl--extract-commit "002c05b6 (mgmarlow 2023-04-29 10:09:12 -0700 77)   \"Extracts basename from HTTPS repository URI.")
                 "002c05b6"))
  (should (equal (sgl--extract-commit "^002c05b6 (mgmarlow 2023-04-29 10:09:12 -0700 77)   \"Extracts basename from HTTPS repository URI.")
                 "002c05b6")))

(ert-deftest sgl-test-format-commit ()
  "Tests commit link formatting."
  (should (equal (sgl--format-commit "github.com/user/repo" "002c05b6")
                 "https://github.com/user/repo/commit/002c05b6"))
  (should (equal (sgl--format-commit "git.sr.ht/~user/repo" "002c05b6")
                 "https://git.sr.ht/~user/repo/commit/002c05b6")))
