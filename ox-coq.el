(require 'ox)
(require 'ox-publish)
(require 'format-spec)

(eval-when-compile (require 'cl) (require 'table nil 'noerror))


(org-export-define-backend
 'coqdoc
 '(
   (bold . org-export-coq-bold)
   (code . org-export-coq-code)
   (headline . org-export-coq-headline)
   (latex-fragment . org-export-coq-latex-fragment)
   (paragraph . org-export-coq-paragraph)
   (src-block . org-export-coq-src-block)
   (section . org-export-coq-section)
   (template . org-export-coq-template)
   )
 :export-block "COQDOC"
 :menu-entry
 '(?v "Export to Coq Script"
      (
       (?v "As Coq file" org-export-coq-to-vernacular)
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
;; Transcoders
;;

(defun org-export-coq-template (contents info)
  (concat
   (org-export-coq-doc-info info)
   contents))

(defun org-export-coq-doc-info (info)
  (format "\n(* authored by: %s *)\n"
	  (and (plist-get info :with-author)
	       (let ((auth (plist-get info :author)))
	  	 (and auth
	  	      ;; Return raw Org syntax, skipping non
	  	      ;; exportable objects.
	  	      (org-element-interpret-data
	  	       (org-element-map auth
	  		   (cons 'plain-text org-element-all-objects)
	  		 'identity info)))))))


(defun org-export-coq-bold (bold contents info)
  (format "_%s_" contents))

(defun org-export-coq-code (code contents info)
  (format "code:\n%s\n" contents))

(defun org-export-coq-headline (headline contents info)
  (let ((level (org-export-get-relative-level headline info))
	(text (org-export-data (org-element-property :title headline) info)))
    (format "%s(** %s %s *)\n%s\n"
	    (if (= level 1) "(* section *)\n" "")
	    (make-string level ?*) text contents)))

(defun org-export-coq-paragraph (paragraph contents info)
  (format "(**\n%s\n*)" contents))

(defun org-export-coq-src-block (src-block contents info)
  (let ((lang (org-element-property :language src-block))
	(caption (org-export-get-caption src-block))
	(code (car (org-export-unravel-code src-block))))
    (if (string= "coq" lang) code
      (format "(**\n<<\n%s>>\n*)\n" code))))

(defun org-export-coq-inner-src-block (inner-src-block contents info)
  (format "[%s]" contents))

(defun org-export-coq-section (section contents info)
  (let ((parent (org-export-get-parent-headline section)))
    (if (not parent) contents
      (let ((section-number
	     (and (org-export-numbered-headline-p parent info)
		  (mapconcat
		    #'number-to-string
		    (org-export-get-headline-number parent info) "-"))))
	(format "(* SECTION:%s *)\n%s" section-number contents)))))

(defun org-export-coq-latex-fragment (latex-fragment contents info)
  (let ((latex-frag (org-element-property :value latex-fragment)))
    (format "%s" latex-frag)))


(defun org-export-coq-almost (almost contents info)
  contents)


;; --------------------------------
;;  Exporter
;; 

(defun org-export-coq-to-vernacular
  (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let* ((extension ".v")
	 (file (org-export-output-file-name extension subtreep))
	 (org-export-coding-system po:coding-system))
    (org-export-to-file 'coqdoc file
      async subtreep visible-only body-only ext-plist)))

(provide 'ox-coq)
