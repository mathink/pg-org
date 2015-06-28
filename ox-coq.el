(require 'files)
(require 'ox)
(require 'ox-publish)
(require 'format-spec)

(eval-when-compile (require 'cl))

(setq org-structure-template-alist
      (append
       '(("coq" "#+BEGIN_SRC coq\n?\n#+END_SRC" "<src lang=\"coq\">\n?\n</src>"))
       org-structure-template-alist))

(org-export-define-backend
    'coqdoc
  '((bold . org-coq-bold)
    (code . org-coq-code)
    (example-block . org-coq-example-block)
    (headline . org-coq-headline)
    (latex-environment . org-coq-latex-environment)
    (latex-fragment . org-coq-latex-fragment)
    (link . org-coq-link)
    (inline-src-block . org-coq-inline-src-block)
    (inner-template . org-coq-inner-template)
    (item . org-coq-item)
    (paragraph . org-coq-paragraph)
    (plain-list . org-coq-plain-list)
    (plain-text . org-coq-plain-text)
    (section . org-coq-section)
    (special-block . org-coq-special-block)
    (src-block . org-coq-src-block)
    (template . org-coq-template)
    (verbatim . org-coq-verbatim))
  :export-block "COQDOC"
  :menu-entry
  '(?v "Export to Coq Script"
       (
	(?v "As Coq file" org-coq-to-vernacular)
	(?V "As Coq Buffer" org-coq-to-buffer)
	))
  ())

;;
;; --------------------------------
;;  Custom Variables
;;
(defgroup po:export nil
  "Options for exporting Org mode files to Coq Script."
  :tag "Org Export Coq"
  :group 'po:export)

(defcustom po:coding-system 'utf-8
  "Coding system for Coq export.
Use utf-8 as the default value."
  :group 'po:export
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'coding-system)

;; --------------------------------
;; Filters
;;

(defun org-coq-filter-headline (headline back-end info)
  (format "%s\n" headline))

;; --------------------------------
;; Transcoders
;;


(defun org-coq-template (contents info)
  (concat
   ;; (org-coq-title info)
   "(** "
   (org-coq-doc-info info)
   contents
   "*)"))

(defun org-coq-inner-template (contents info)
  (concat
   contents))

(defun org-coq-title (info)
  (let ((fname (file-name-sans-extension
		(file-name-nondirectory (buffer-file-name)))))
    (format "(** * %s : %s *)\n" fname (car (plist-get info :title)))))

(defun org-coq-doc-info (info)
  (format "* %s *)\n(** "
	  (car (plist-get info :title))
	  ;; (car (plist-get info :author))
	  ))

(defun org-coq-bold (bold contents info)
  (format "%s" contents))

(defun org-coq-code (code contents info)
  (format "%s" (org-element-property :value code)))

(defun org-coq-example-block (example-block contents info)
  (let* ((text (org-element-property :value example-block))
	 (attrs (org-export-read-attribute :attr_coq example-block))
	 (name (plist-get attrs :name))
	 (type (plist-get attrs :type)))
    (format "#<div class=\"example\" type=\"%s\">%s#\n<<\n%s>>\n#</div>#"
		   type
		   (if name (format "\n<span class=\"name\">%s</span>" name) "")
		   text)
    ))

(defun org-coq-verbatim (verbatim contents info)
  (let* ((parent (org-export-get-parent-element verbatim))
	 (ptype (org-element-type parent)))
    (if (memq ptype '(special-block)) contents
      (format "\[%s\]" (org-element-property :value verbatim)))))

(defun org-coq-headline (headline contents info)
  (let ((level (org-export-get-relative-level headline info))
	(text (org-export-data (org-element-property :title headline) info)))
    (format "%s %s\n%s"
	    (make-string level ?*) text
	    ;; (replace-regexp-in-string "^
	    ;; $" "" contents)
	    contents
	    )))

(defun org-coq-paragraph (paragraph contents info)
  (let* ((parent (org-export-get-parent-element paragraph))
	 (ptype (org-element-type parent)))
    (if (memq ptype '(special-block item)) contents
      (format "%s" (replace-regexp-in-string "
$" "" contents)))))

(defun org-coq-src-block (src-block contents info)
  (let ((lang (org-element-property :language src-block))
	(caption (org-export-get-caption src-block))
	(code (car (org-export-unravel-code src-block)))
	;; (code (car (org-export-format-code-default src-block info)))
	)
    (if (string= "coq" lang)
	;; code
	(concat
	  "*)\n"
	  code
	  "(** ")
      (format "<<\n%s>>\n" code))))

(defun org-coq-inline-src-block (inline-src-block contents info)
  (let ((code (org-element-property :value inline-src-block)))
    (format "[%s]" code)))

(defun org-coq-special-block (special-block contents info)
  (let* ((block-name (org-element-property :name special-block))
	 (block-type (downcase (org-element-property :type special-block)))
	 (contents (or contents ""))
	 (attrs (org-export-read-attribute :attr_coq special-block)))
    (format "%s#<div class=\"%s\">%s#\n<<\n%s\n>>\n#</div>#"
	    (if attrs attrs "")
	    block-type
	    (if block-name (format "\n<span class=\"name\">%s </span>" block-name) "")
	    contents
	    )))

(defun org-coq-latex-environment (latex-environment contents info)
  (let ((code (org-element-property :value latex-environment)))
    (format "#<div class=\"latex\" keyword=\"env\">#\n$$\n%s$$\n#</div>#" code)))

(defun org-coq-latex-fragment (latex-fragment contents info)
  (let ((latex-frag (org-element-property :value latex-fragment)))
    (format "$%s$" latex-frag)))

(defun org-coq-plain-text (text info)
  text)

(defun org-coq-plain-list (plain-list contents info)
  (let* ((parent (org-export-get-parent-element plain-list))
	 (in-p (progn
		 (while (and parent (not (eq (org-element-type parent) 'item)))
		   (setq parent (org-export-get-parent-element parent)))
		 (and parent (eq (org-element-type parent) 'item)))))
    (if in-p (format "%s"  contents)
     (format "%s" contents))))

(defun org-coq-item (item contents info)
  (let* ((parent (org-export-get-parent-element item))
	 (nest 0))
    (while parent
      (when (eq (org-element-type parent) 'item)
	(setq nest (+ 1 nest)))
      (setq parent (org-export-get-parent-element parent)))
    (format "%s- %s" (make-string nest ? ) contents)))

(defun org-coq-section (section contents info)
  (let ((parent (org-export-get-parent-headline section)))
    (if (not parent) contents
      (let ((section-number
	     (and (org-export-numbered-headline-p parent info)
		  (mapconcat
		    #'number-to-string
		    (org-export-get-headline-number parent info) "-"))))
	(format "%s" contents)
	))))

(defun org-coq-almost (almost contents info)
  contents)

(defun org-coq-link (link desc info)
  (let* ((type (org-element-property :type link))
	 (raw-path (org-element-property :path link))
	 (path (if (member type '("http" "https" "ftp" "mailto"))
		   (concatenate 'string type ":" raw-path)
		 raw-path)))
    (format "#<a href=\"%s\">#%s#</a>#"
	    (org-link-escape (org-link-unescape path) '(32 34 35))
	    (if desc desc path))))

;; --------------------------------
;;  Exporter
;; 

(defun org-coq-to-vernacular (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let* ((extension ".v")
	 (file (org-export-output-file-name extension subtreep))
	 (org-export-coding-system po:coding-system))
    (org-export-to-file 'coqdoc file
      async subtreep visible-only body-only ext-plist)))


(defun org-coq-to-buffer (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (org-export-to-buffer 'coqdoc "*Org Coq Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (coq-mode))))

(provide 'ox-coq)
