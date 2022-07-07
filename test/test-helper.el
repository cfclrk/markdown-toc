;;; test-helper.el --- Tests for markdown-toc.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Ert-runner evaluates this file before running tests.

;;; Code:

(require 'projectile)

(defun proj-file (rel-path)
  "Return the absolute path to the project file at REL-PATH.
REL-PATH is a path relative to this project's root."
  (f-join (projectile-project-root) rel-path))

(defun read-test-file (rel-path)
  "Return the contents of the file at REL-PATH.
REL-PATH is a path relative to the test/files directory in this
project."
  (f-read-text
   (proj-file (f-join "test/files" rel-path))))

(defmacro with-test-file (rel-path &rest forms)
  "Read file at REL-PATH and execute FORMS."
  (declare (indent 1))
  `(with-temp-buffer
     (markdown-mode)
     (insert (read-test-file ,rel-path))
     (goto-char (point-min))
     ,@forms
     (buffer-string)))

(require 'markdown-toc)

;;; test-helper.el ends here
