;;; test-helper.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Antoine Romain Dumont

;; Author: Antoine Romain Dumont <antoine.romain.dumont@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Ert-runner evaluates this file before running tests.

;;; Code:

(require 'projectile)
(require 'undercover)


(defun read-test-file (rel-path)
  "Return the contents of the file at REL-PATH.
REL-PATH is a path relative to the test/files directory in this
project."
  (f-read-text
   (f-join (projectile-project-root) "test/files" rel-path)))

(undercover "*.el"
            (:exclude "*-test.el")
            (:report-file "/tmp/undercover-report.json"))

(require 'markdown-toc)

;;; test-helper.el ends here
