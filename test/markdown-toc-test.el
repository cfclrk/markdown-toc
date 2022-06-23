;;; markdown-toc-test.el --- Tests for markdown-toc  -*- lexical-binding: t;

;;; Commentary:

;; Tests for markdown-toc.el

;;; Code:

(require 'ert)
(require 'el-mock)
(require 'markdown-toc)
(require 'mocker)

(ert-deftest markdown-toc-generate-basic ()
  (should (equal
           (read-test-file "basic-toc.md")
           (with-test-file "basic.md"
                           (markdown-toc-generate)))))

(ert-deftest markdown-toc--to-link ()
  (should (equal
           "[a/b Д~d (e).](#ab-дd-e-2)"
           (markdown-toc--to-link "a/b Д~d (e)." 2)))

  (should (equal
           "[ under_score](#under_score)"
           (markdown-toc--to-link " under_score"))))

(ert-deftest markdown-toc--to-markdown-toc ()
  (let ((toc (markdown-toc--to-markdown-toc
              '((0 . "some markdown page title")
                (0 . "main title")
                (1 . "Sources")
                (2 . "Marmalade (recommended)")
                (2 . "Melpa-stable")
                (2 . "Melpa (~snapshot)")
                (1 . "Install")
                (2 . "Load org-trello")
                (2 . "Alternative")
                (3 . "Git")
                (3 . "Tar")
                (0 . "another title")
                (1 . "with")
                (1 . "some")
                (1 . "heading")))))
    (should (equal
             (read-test-file "to-markdown-toc.md")
             toc))))

(ert-deftest markdown-toc--compute-toc-structure-from-level ()
  (should (equal
           '((0 . "Sources")
             (1 . "Marmalade (recommended)")
             (1 . "Melpa-stable"))
           (markdown-toc--compute-toc-structure-from-level
            0
            '("Sources"
              ("." . 130)
              ("Marmalade (recommended)" . 311)
              ("Melpa-stable" . 552)))))

  (should (equal
           '((0 . "Install")
             (1 . "Load org-trello")
             (1 . "Alternative")
             (2 . "Git")
             (2 . "Tar"))
           (markdown-toc--compute-toc-structure-from-level
            0
            '("Install"
              ("." . 1184)
              ("Load org-trello" . 1277)
              ("Alternative"
               ("." . 1563)
               ("Git" . 1580)
               ("Tar" . 1881))))))

  (should (equal
           '((0 . "some markdown page title"))
           (markdown-toc--compute-toc-structure-from-level
            0
            '("some markdown page title" . 1)))))

(ert-deftest markdown-toc--compute-toc-structure ()
  (should (equal
           '((0 . "some markdown page title")
             (0 . "main title")
             (1 . "Sources")
             (2 . "Marmalade (recommended)")
             (2 . "Melpa-stable")
             (2 . "Melpa (~snapshot)")
             (1 . "Install")
             (2 . "Load org-trello")
             (2 . "Alternative")
             (3 . "Git")
             (3 . "Tar")
             (0 . "another title")
             (1 . "with")
             (1 . "some")
             (1 . "heading"))
           (markdown-toc--compute-toc-structure
            '(("some markdown page title" . 1)
              ("main title"
               (#1="." . 52)
               ("Sources"
                (#1# . 130)
                ("Marmalade (recommended)" . 311)
                ("Melpa-stable" . 552)
                ("Melpa (~snapshot)" . 792))
               ("Install" (#1# . 1184)
                ("Load org-trello" . 1277)
                ("Alternative" (#1# . 1563)
                 ("Git" . 1580)
                 ("Tar" . 1881))))
              ("another title"
               (#1# . 2044)
               ("with" . 2061)
               ("some" . 2070)
               ("heading" . 2079)))))))

(ert-deftest markdown-toc--compute-full-toc ()
  (should (equal
           (read-test-file "full-toc.md")
           (markdown-toc--compute-full-toc "some-toc"))))

(ert-deftest markdown-toc-generate--with-duplicate-titles ()
  (should (equal
           (read-test-file "duplicate-title-toc.md")
           (markdown-toc-with-temp-buffer-and-return-buffer-content
            (read-test-file "duplicate-titles.md")
            (markdown-toc-generate)))))

(ert-deftest markdown-toc-generate--with-customs ()
  (should (equal
           (read-test-file "customs.md")
           (let ((markdown-toc-start "<!-- markdown-toc start -->")
                 (markdown-toc-title "** foobar **")
                 (markdown-toc-end "<!-- toc end -->"))
             (markdown-toc-with-temp-buffer-and-return-buffer-content
              (read-test-file "custom-toc.md")
              (markdown-toc-generate))))))

(ert-deftest markdown-toc-generate--first-toc-with-user-override ()
  (should (equal
           (read-test-file "first-with-user-override-toc.md")
           (let ((markdown-toc-transform-fn 'cdr))
             (markdown-toc-with-temp-buffer-and-return-buffer-content
              (read-test-file "first-with-user-override.md")
              (markdown-toc-generate))))))

(ert-deftest markdown-toc-generate--replace-old-toc-if-already-present ()
  (should (equal
           (read-test-file "replace-old-if-present-toc.md")
           (markdown-toc-with-temp-buffer-and-return-buffer-content
            (read-test-file "replace-old-if-present.md")
            (markdown-toc-generate)))))

(ert-deftest markdown-toc-generate--replace-old-toc ()
  ;; Update an existing TOC
  (should (equal
           (read-test-file "replace-old-toc.md")
           (markdown-toc-with-temp-buffer-and-return-buffer-content
            (read-test-file "replace-old.md")
            (markdown-toc-generate 'replace-old-toc)))))

(ert-deftest test-markdown-toc--refresh-toc--with-existing-toc ()
  ;; Update an existing TOC
  (should (equal
           (read-test-file "refresh-with-existing-toc.md")
           (markdown-toc-with-temp-buffer-and-return-buffer-content
            (read-test-file "refresh-with-existing.md")
            (markdown-toc-refresh)))))

(ert-deftest test-markdown-toc-refresh--without-existing-toc ()
  ;; Refresh on a document without a ToC should yield the same document.
  (should (equal
           (read-test-file "refresh-without-existing-toc.md")
           (markdown-toc-with-temp-buffer-and-return-buffer-content
            (read-test-file "refresh-without-existing-toc.md")
            (markdown-toc-refresh)))))

(ert-deftest test-markdown-toc-delete ()
  (should (equal
           (read-test-file "first.md")
           (markdown-toc-with-temp-buffer-and-return-buffer-content
            (read-test-file "first-toc.md")
            (markdown-toc-delete)))))

(ert-deftest markdown-toc--read-title-out-of-link ()
  (should (string=
           "this is the title"
           (markdown-toc--read-title-out-of-link
            "  - [this is the title](#this-is-the-link)   ")))

  (should (string=
           "another title"
           (markdown-toc--read-title-out-of-link
            "  - [another title](#this-is-the-link)
with multiple line
should not matter "))))

(ert-deftest markdown-toc--title-level ()
  (should (eq 1
              (markdown-toc--title-level
               "- [this is the title](#this-is-the-link)")))

  (should (eq 4
              (let ((markdown-toc-indent 4))
                (markdown-toc--title-level
                 "            - [this is the title](#this-is-the-link)"))))

  (should (eq 2
              (let ((markdown-toc-indent 2))
                (markdown-toc--title-level
                 "  - [another title](#this-is-the-link)
with multiple line
should not matter "))))

  (should (eq 2
              (let ((markdown-toc-indent 3))
                (markdown-toc--title-level "   - [another title](#this-is-the-link)
with multiple line
should not matter "))))
  ;; no - as prefix so considered not a title
  (should-not (markdown-toc--title-level "[this is the title](#this-is-the-link)"))
  ;; prefixed with a dash but misaligned, title should be indented with a
  ;; multiple of `markdown-toc-indent` blank spaces
  (should-not (markdown-toc--title-level " - [title](#this-is-the-link)")))

(ert-deftest markdown-toc-follow-link-at-point()
  "Follow a correct toc link should follow to the title"
  (should (string= "## Sources"
                   (with-temp-buffer
                     (insert "- [some markdown page title](#some-markdown-page-title)
- [main title](#main-title)
  - [Sources](#sources)
    - [Marmalade (recommended)](#marmalade-recommended)

# main title
## Sources
### marmalade
...
")
                     (search-backward "- [Sources]")
                     (call-interactively 'markdown-toc-follow-link-at-point)
                     (buffer-substring-no-properties (point-at-bol) (point-at-eol))))))

(ert-deftest markdown-toc-follow-link-at-point-failures()
  "Follow a misindented toc link should do nothing"
  (should
   ;; not move
   (string= "   - [Sources](#sources)  <- misindented 3 instead of 4 here"
            (with-temp-buffer
              (insert "- [some markdown page title](#some-markdown-page-title)
- [main title](#main-title)
   - [Sources](#sources)  <- misindented 3 instead of 4 here

# main title
## Sources
...
")
              (search-backward "- [Sources]")
              (call-interactively 'markdown-toc-follow-link-at-point)
              (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))

  (should
   ;; not move as well because
   (string= "not a title"
            (with-temp-buffer
              (insert "- [some markdown page title](#some-markdown-page-title)
- [main title](#main-title)
   - [Sources](#sources)
        - [Marmalade (recommended)](#marmalade-recommended)

# main title
## Sources
### marmalade
not a title
...
")
              (search-backward "not a title")
              (call-interactively 'markdown-toc-follow-link-at-point)
              (buffer-substring-no-properties (point-at-bol) (point-at-eol))))))

(provide 'markdown-toc-tests)
;;; markdown-toc-test.el ends here
