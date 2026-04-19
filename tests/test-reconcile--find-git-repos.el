;;; test-reconcile--find-git-repos.el --- Tests for cj/find-git-repos -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for recursive git repository discovery in cj/find-git-repos.
;; Uses real temporary directory trees with fake .git directories.

;;; Code:

(require 'ert)
(require 'testutil-reconcile-open-repos)
(require 'reconcile-open-repos)

;;; Normal Cases

(ert-deftest test-find-git-repos-normal-flat-repos ()
  "Finds multiple git repos at the same level."
  (reconcile-test-with-temp-dirs
   ("repo-a/.git/" "repo-b/.git/" "repo-c/.git/")
   (let ((repos (cj/find-git-repos test-root)))
     (should (= (length repos) 3)))))

(ert-deftest test-find-git-repos-normal-nested-repo ()
  "Finds a repo nested inside a non-repo directory."
  (reconcile-test-with-temp-dirs
   ("parent/child/.git/")
   (let ((repos (cj/find-git-repos test-root)))
     (should (= (length repos) 1))
     (should (string-suffix-p "child" (car repos))))))

(ert-deftest test-find-git-repos-normal-repo-with-nested-subrepo ()
  "Finds both a parent repo and a sub-repo inside it."
  (reconcile-test-with-temp-dirs
   ("deepsat/.git/" "deepsat/frontend/.git/" "deepsat/backend/.git/")
   (let ((repos (cj/find-git-repos test-root)))
     (should (= (length repos) 3)))))

(ert-deftest test-find-git-repos-normal-mixed-repos-and-dirs ()
  "Finds repos while skipping plain directories."
  (reconcile-test-with-temp-dirs
   ("repo-a/.git/" "not-a-repo/readme.txt" "repo-b/.git/")
   (let ((repos (cj/find-git-repos test-root)))
     (should (= (length repos) 2)))))

(ert-deftest test-find-git-repos-normal-deeply-nested ()
  "Finds a repo several levels deep."
  (reconcile-test-with-temp-dirs
   ("a/b/c/deep-repo/.git/")
   (let ((repos (cj/find-git-repos test-root)))
     (should (= (length repos) 1))
     (should (string-suffix-p "deep-repo" (car repos))))))

;;; Boundary Cases

(ert-deftest test-find-git-repos-boundary-empty-directory ()
  "Returns empty list for directory with no children."
  (reconcile-test-with-temp-dirs
   ()
   (let ((repos (cj/find-git-repos test-root)))
     (should (= (length repos) 0)))))

(ert-deftest test-find-git-repos-boundary-no-git-repos ()
  "Returns empty list when no directories contain .git."
  (reconcile-test-with-temp-dirs
   ("dir-a/file.txt" "dir-b/file.txt")
   (let ((repos (cj/find-git-repos test-root)))
     (should (= (length repos) 0)))))

(ert-deftest test-find-git-repos-boundary-hidden-dirs-skipped ()
  "Skips hidden directories (starting with dot) per the regex filter."
  (reconcile-test-with-temp-dirs
   (".hidden-repo/.git/" "visible-repo/.git/")
   (let ((repos (cj/find-git-repos test-root)))
     (should (= (length repos) 1))
     (should (string-suffix-p "visible-repo" (car repos))))))

(provide 'test-reconcile--find-git-repos)
;;; test-reconcile--find-git-repos.el ends here
