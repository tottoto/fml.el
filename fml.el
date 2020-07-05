;;; fml.el --- Form markup language to generate HTML

;; Copyright (C) 2020 tottoto

;; Author: tottoto <tottotodev@gmail.com>
;; Maintainer: tottoto <tottotodev@gmail.com>
;; Keywords: lisp html
;; Homepage: https://github.com/tottoto/fml.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defgroup fml nil
  "Form markup language to generate HTML."
  :group 'lisp
  :prefix "fml-")

(defcustom fml-empty-element-list
  '(area base br col embed hr img input
         link meta param source track wbr)
  "List of empty element."
  :type 'sexp)

(defcustom fml-enable-multiline t
  "Flag to enable multiline."
  :type 'symbol)

(defun fml-translate-attr (attr)
  (let ((key (symbol-name (car attr)))
        (value (cdr attr)))
    (format "%s=\"%s\"" key value)))

(defun fml-translate-attrs (attrs)
  (mapconcat 'fml-translate-attr attrs " "))

(defun fml-start-tag (element &optional attrs)
  (if attrs
      (format "<%s %s>" element (fml-translate-attrs attrs))
    (format "<%s>" element)))

(defun fml-end-tag (element)
  (format "</%s>" element))

(defun fml (form)
  (cond
   ((stringp form) form)
   ((not (null form))
    (let* ((element (nth 0 form))
           (attrs (nth 1 form))
           (start-tag (fml-start-tag element attrs)))
      (if (memq element fml-empty-element-list)
          start-tag
        (let* ((end-tag (fml-end-tag element))
               (contents (nthcdr 2 form))
               (delim (if fml-enable-multiline "\n" ""))
               (inner (mapconcat 'fml contents delim)))
          (setq delim (if (and fml-enable-multiline
                               (string-match (rx "<") inner))
                          "\n" ""))
          (mapconcat 'identity `(,start-tag ,inner ,end-tag) delim)))))))

(defun fml-oneline (form)
  (let ((fml-enable-multiline nil))
    (fml form)))

(defun fml-multiline (form)
  (let ((fml-enable-multiline t))
    (fml form)))

(provide 'fml)
