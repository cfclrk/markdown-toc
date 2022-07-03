;;; test-helper.el --- Tests for markdown-toc.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Ert-runner evaluates this file before running tests.

;;; Code:

(require 'projectile)

(defun proj-file (path)
  (f-join (projectile-project-root) path))

(defun read-test-file (path)
  "Return the contents of the file at PATH.
If PATH is relative, it is considered relative to the test/files
directory in this project."
  (f-read-text
   (proj-file (f-join "test/files" path))))

(defmacro with-test-file (rel-path &rest forms)
  "Read file at REL-PATH and execute FORMS."
  (declare (indent 1))
  `(with-temp-buffer
     (markdown-mode)
     (insert (read-test-file ,rel-path))
     (goto-char (point-min))
     ,@forms
     (buffer-string)))

(defmacro markdown-toc-with-temp-buffer-and-return-buffer-content (text body-test)
  "A `markdown-toc' test macro to ease testing.
TEXT is the content of the buffer.
BODY-TEST is the assertion to test on the buffer.
NB-LINES-FORWARD is the number of lines to get back to."
  `(with-temp-buffer
     (markdown-mode)
     (insert ,text)
     (progn
       (goto-char (point-min))
       ,body-test
       (buffer-substring-no-properties (point-min) (point-max)))))

(defun generate-test-files ()
  "Regenerate test files."

  ;; Set everything to their defaults
  (let ((markdown-toc-title "**Table of Contents**")
        (markdown-toc-start "<!-- toc start -->")
        (markdown-toc-end "<!-- toc end -->")
        (markdown-toc-indent 2)
        (markdown-toc-transform-fn (lambda (level-to-heading-list) level-to-heading-list)))

    (dolist (test-name '("basic" "duplicate-titles"))
      (let ((start-file (concat test-name ".md"))
            (end-file   (concat test-name "-toc.md")))
        (f-write
         (with-test-file start-file (markdown-toc-generate))
         'utf-8
         (proj-file (f-join "test/files" end-file)))))))

(require 'markdown-toc)

;;; test-helper.el ends here
