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
