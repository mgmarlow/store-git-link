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
