# markdown-toc

An Emacs mode for creating a table of contents in markdown files.

<!--ts-->
- [Usage](#usage)
  - [Change the toc structure](#change-the-toc-structure)
  - [Configuration](#configuration)
  - [Minor mode](#minor-mode)
- [Development](#development)
<!--te-->

# Usage

This package provides three interactive functions:

- `markdown-toc-generate`: Create a toc at point
- `markdown-toc-refresh`: Find and update an existing toc
- `markdown-toc-delete`: Find and delete an existing toc

## Change the toc structure

You can transform the structure of the ToC using `markdown-toc-transform-fn`. It defaults to the identity function (no transformation).

It expects as argument the toc-structure markdown-toc uses to generate
the toc. The remaining code expects a similar structure.

Example:

```emacs-lisp
'((0 . "H1 Document Title")
  (0 . "Another H1 heading")
  (1 . "H2 heading")
  (2 . "H3 heading")
  (1 . "H2 heading")
  (0 . "H1 heading")
  (1 . "H2 heading"))
```

The car of each item is a heading level; the cdr of each item is the heading
text.

The first element is often a document title. To drop the title from the table of
contents, set `markdown-toc-transform-fn` to a function that removes the first
element from a list:

```emacs-lisp
(setq markdown-toc-transform-fn 'cdr)
```

Or drop all h1 titles:

```emacs-lisp
(require 'dash)

(custom-set-variables
 '(markdown-toc-transform-fn
   (lambda (toc-structure)
     (-filter
      (lambda (l) (let ((index (car l)))
                    (<= 1 index)))
      toc-structure)))
```

## Configuration

The following variables can be customized:

```emacs-lisp
(setq
 markdown-toc-start "<!-- toc start -->"
 markdown-toc-end "<!-- toc end -->"
 markdown-toc-title "**Table of Contents**"
 markdown-toc-indent 2
 markdown-toc-transform-fn (lambda
                             (level-to-heading-list)
                             level-to-heading-list))
```

## Minor mode

`markdown-toc-mode` is a minor mode with the following default key bindings:

```emacs-lisp
(setq markdown-toc-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c m .") 'markdown-toc-follow-link-at-point)
        (define-key map (kbd "C-c m d") 'markdown-toc-delete)
        map))
```

To (de)activate this in an org file: /M-x markdown-toc-mode/

# Development

Run tests:

```sh
cask install
cask exec ert-runner
```
