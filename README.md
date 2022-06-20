# markdown-toc

Create and update a Table of Contents in markdown files.

<!--ts-->
- [markdown-toc](#markdown-toc)
- [Use](#use)
  - [Create](#create)
  - [User toc manipulation](#user-toc-manipulation)
  - [Update](#update)
  - [Create elsewhere](#create-elsewhere)
  - [Remove](#remove)
  - [Customize](#customize)
  - [Minor mode](#minor-mode)
- [Install](#install)
  - [emacs package repository](#emacs-package-repository)
    - [Setup](#setup)
      - [melpa stable](#melpa-stable)
      - [melpa](#melpa)
    - [Install](#install-1)
  - [emacs-lisp file](#emacs-lisp-file)
- [Inspiration](#inspiration)
<!--te-->

# Usage

## Create a ToC

Inside a markdown file, the first time, place yourself where you want to insert the TOC:

<kbd>M-x markdown-toc-generate</kbd>

This will compute the TOC and insert it at current position.

You can also execute: <kbd>M-x markdown-toc-generate-or-refresh</kbd> to either
gnerate a TOC when none exists or refresh the currently existing one.

## Structure manipulation

You can transform the structure of the ToC using `markdown-toc-transform-fn`. It defaults to the identity function (no transformation).

It expects as argument the toc-structure markdown-toc uses to generate
the toc. The remaining code expects a similar structure.

Example:

```emacs-lisp
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
```

So for example, as asked in #16, one could drop the first element:

```emacs-lisp
(custom-set-variables '(markdown-toc-transform-fn 'cdr))
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

## Update

To update an existing TOC:

<kbd>M-x markdown-toc-refresh-toc</kbd>

## Create elsewhere

To create another updated TOC elsewhere, execute <kbd>M-x
markdown-toc-generate</kbd> again, this will remove the old TOC and insert the
updated one from where you stand.

## Remove

To remove a TOC, execute <kbd>M-x markdown-toc-delete-toc</kbd>.

## Customize

Currently, you can customize the following:

- `markdown-toc-start`
- `markdown-toc-title`
- `markdown-toc-end`
- `markdown-toc-indent`

Customize them as following format:

```emacs-lisp
(custom-set-variables
 '(markdown-toc-start "<!-- customized start-->")
 '(markdown-toc-title "**customized title**")
 '(markdown-toc-end "<!-- customized end -->")
 '(markdown-toc-indent 4))
```

## Minor mode

markdown-toc-mode provides a minor mode with the following default binding:

```emacs-lisp
(setq markdown-toc-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c m .") 'markdown-toc-follow-link-at-point)
        (define-key map (kbd "C-c m t") 'markdown-toc-generate-or-refresh)
        (define-key map (kbd "C-c m d") 'markdown-toc-delete-toc)
        (define-key map (kbd "C-c m v") 'markdown-toc-version)
        map))
```

To (de)activate this in an org file: /M-x markdown-toc-mode/

# Development

Run tests:

```sh
cask install
cask exec ert-runner
```
