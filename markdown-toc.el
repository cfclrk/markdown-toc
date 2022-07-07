;;; markdown-toc.el --- A TOC generator for markdown -*- lexical-binding: t;

;; Package-Requires: ((markdown-mode) (dash) (s))
;; Version: 0

;;; Commentary:

;; An Emacs mode for creating a table of contents in markdown files.
;;
;; This package provides four interactive functions:
;;
;; - `markdown-toc-generate' Create a toc at point
;; - `markdown-toc-refresh' Find and update an existing toc
;; - `markdown-toc-follow-link' Jump to a section from the toc

;;; Code:

(require 's)
(require 'dash)
(require 'markdown-mode)

;;;; Variables

(defgroup markdown-toc nil
  "A simple TOC generator for markdown file."
  :group 'markdown)

(defcustom markdown-toc-list-item
  "-"
  "List item marker that should be used.
Example: '-' for unordered lists or '1.' for ordered lists."
  :type '(choice
          (string :tag "Unordered list header" "-")
          (string :tag "Ordered list header" "1."))
  :group 'markdown-toc)

(defcustom markdown-toc-title
  "**Table of Contents**"
  "Title for a table of contents."
  :group 'markdown-toc
  :type 'string)

(defcustom markdown-toc-start
  "<!-- toc start -->"
  "Beginning delimiter comment."
  :group 'markdown-toc
  :type 'string)

(defcustom markdown-toc-end
  "<!-- toc end -->"
  "Ending delimiter comment."
  :group 'markdown-toc
  :type 'string)

(defcustom markdown-toc-indent 2
  "Indentation offset."
  :group 'markdown-toc
  :type 'integer)

(defcustom markdown-toc-transform-fn
  (lambda (level-to-heading-list) level-to-heading-list)
  "A function to transform the toc structure.

The `level-to-heading-list' is an ordered list of all document
headings and their levels. It has a form like the following:

  '((0 . \"H1 heading\")
    (1 . \"H2 heading\")
    (2 . \"H3 heading\")
    (2 . \"Another H3 heading\")
    (1 . \"H2 heading\")
    (2 . \"H3 heading\")
    (3 . \"H4 heading\"))

The car of each item is a heading level; the cdr of each item is
the heading text.

To e.g. remove the first heading (often the title) from the toc:

  (setq markdown-toc-transform-fn 'cdr)

The default is the identity function (no transformation)."
  :group 'markdown-toc
  :type 'function)

;;;; Generate and refresh

;;;###autoload
(defun markdown-toc-generate ()
  "Generate a TOC at point."
  (interactive)
  (save-excursion
    (->> (funcall imenu-create-index-function)
         markdown-toc--compute-toc-structure
         (funcall markdown-toc-transform-fn)
         markdown-toc--generate-toc
         insert)))

;;;###autoload
(defun markdown-toc-refresh ()
  "Refresh an existing TOC."
  (interactive)
  (save-excursion
    (when (markdown-toc--present?)
      (markdown-toc-delete))
    (markdown-toc-generate)))

;;;; Create toc structure using imenu

(defun markdown-toc--compute-toc-structure-from-level (level menu-index)
  "Given a LEVEL and a MENU-INDEX, compute the toc structure."
  (when menu-index
    (let* ((fst   (car menu-index))
           (tail  (cdr menu-index))
           (ttail (if (integerp tail) nil (cdr tail))))
      (cons `(,level . ,fst)
            (--mapcat
             (markdown-toc--compute-toc-structure-from-level (+ 1 level) it)
             ttail)))))

(defun markdown-toc--compute-toc-structure (imenu-index)
  "Given a IMENU-INDEX, compute the TOC structure."
  (--mapcat
   (markdown-toc--compute-toc-structure-from-level 0 it)
   imenu-index))

;;;; Wrap

(defun markdown-toc--add-header-title-footer (toc)
  "Wrap the given string TOC with a title and start/end comments.
While the start/end comments are optional, note that without
them, this package cannot find the toc in a document."
  (-as-> "" str
         (if markdown-toc-start
             (concat str markdown-toc-start "\n")
           str)
         (if markdown-toc-title
             (concat str markdown-toc-title "\n\n")
           str)
         (concat str toc)
         (if markdown-toc-end
             (concat str markdown-toc-end "\n")
           str)))

;;;; Filters

(defun markdown-toc-remove-first-heading (x) x)

(defun markdown-toc-remove-toc-heading (x) x)

(defun markdown-toc-remove-headings-before-point (x) x)

;;;; Links

(defconst
  markdown-toc--dash-protection-symbol
  "904c3144b0494520ae42dcd0466611b1"
  "Random string to protect - characters.")

(defconst
  markdown-toc--underscore-protection-symbol
  "1e2be169e3fa4c689bb01c486dafb1a2"
  "Random string to protect _ characters.")

;;;###autoload
(defun markdown-toc-follow-link ()
  "On a given toc link, navigate to the current markdown header.
If the toc is misindented (according to markdown-toc-indent`)
or if not on a toc link, this does nothing."
  (interactive)
  (let* ((full-title (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
         (level (markdown-toc--title-level full-title)))
    (if level ;; nil if misindented or not on a title
        (let ((title (markdown-toc--link-title full-title)))
          (goto-char (point-min))
          (search-forward-regexp (format "%s %s" (s-repeat level "#") title)))
      (message "markdown-toc: Not on a link (or misindented), nothing to do"))))

(defun markdown-toc--to-link (title &optional count)
  "Return a markdown link formatted from string TITLE.
If COUNT is provided, append it to the link part. Example:

(markdown-toc--to-link \"Foo bar\" 2)
;; => [Foo bar](#foo-bar-2)"
  (let ((count (if count count 0)))
    (format "[%s](#%s%s)"
            title
            (->> title
                 s-trim
                 downcase
                 (s-replace "-" markdown-toc--dash-protection-symbol)
                 (s-replace "_" markdown-toc--underscore-protection-symbol)
                 (replace-regexp-in-string "[[:punct:]]" "")
                 (s-replace markdown-toc--dash-protection-symbol "-")
                 (s-replace markdown-toc--underscore-protection-symbol "_")
                 (s-replace " " "-"))
            (if (> count 0)
                (concat "-" (number-to-string count))
              ""))))

(defun markdown-toc--link-title (link)
  "Extract the title from the given markdown LINK."
  (->> link
       s-trim
       (s-chop-prefix "- [")
       (s-split "]")
       car))

(defun markdown-toc-count-duplicate-titles (toc-structure)
  "Counts the number of times each title appeared in the toc structure and adds
it to the TOC structure."
  (-map-indexed
   (lambda (index n)
     (let* ((indent (car n))
            (title (cdr n))
            (count (--count (string= title (cdr it))
                            (-take (+ index 1) toc-structure))))
       (list indent title (- count 1))))
   toc-structure))

(defun markdown-toc--to-markdown-toc (level-title-toc-list)
  "Given LEVEL-TITLE-TOC-LIST, a list of pair level, title, return a TOC string."
  (->> level-title-toc-list
       markdown-toc-count-duplicate-titles
       (--map (let ((num-spaces (* markdown-toc-indent (car it)))
                    (title      (car (cdr it)))
                    (count      (car (cdr (cdr it)))))
                (format "%s%s %s"
                        (s-repeat num-spaces " ")
                        markdown-toc-list-item
                        (markdown-toc--to-link title count))))
       (s-join "\n")
       (s-append "\n")))

;;;; helpers

(defun markdown-toc-delete ()
  "Find a TOC and delete it.
Return the starting position of the old TOC. Not for interactive
use. This function moves point."
  (let ((region-start (markdown-toc--start-pos))
        (region-end   (markdown-toc--end-pos)))
    (delete-region region-start (1+ region-end))
    (goto-char region-start)))

(defun markdown-toc--present? ()
  "Determine if a toc is present in the current buffer.
Reutrns the toc's end position if it exists, nil otherwise."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward markdown-toc-start nil t)))

(defun markdown-toc--start-pos ()
  "Find a toc and return the start position."
  (save-excursion
    (goto-char (markdown-toc--present?))
    (point-at-bol)))

(defun markdown-toc--end-pos ()
  "Find a toc and return the end position."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward markdown-toc-end nil t)))

(defun markdown-toc--generate-toc (toc-structure)
  "Given a TOC-STRUCTURE, compute a new toc."
  (-> toc-structure
      markdown-toc--to-markdown-toc
      markdown-toc--add-header-title-footer))

(defun markdown-toc--title-level (link)
  "Determine the markdown title LINK out of its indentation.
If misindented or not prefixed by `-`, it's considered not a link
and returns nil. Otherwise, returns the level number."
  (when (s-prefix? "-" (-> link s-trim)) ;; if not, it's not a link title
    (let ((indent (->> link
                       (s-split "-")
                       car  ;; first string contains a string with empty spaces
                       ;; which should be a multiple of
                       ;; `markdown-toc-indent`
                       length)))
      (when (zerop (% indent markdown-toc-indent))
        (+ 1 (/ indent markdown-toc-indent))))))

(defvar markdown-toc-mode-map
  nil
  "Default Bindings map for markdown-toc mode.")

(setq markdown-toc-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c m .") 'markdown-toc-follow-link)
        (define-key map (kbd "C-c m g") 'markdown-toc-generate)
        (define-key map (kbd "C-c m r") 'markdown-toc-refresh)
        map))

;;;; minor mode

;;;###autoload
(define-minor-mode markdown-toc-mode
  "Minor mode for generating toc in markdown file.
With no argument, the mode is toggled on/off. Non-nil argument
turns mode on. Nil argument turns mode off.

Commands:
\\{markdown-toc-mode-map}"
  :init-value nil
  :lighter " mt"
  :group 'markdown-toc
  :keymap markdown-toc-mode-map)

(provide 'markdown-toc)
;;; markdown-toc.el ends here
