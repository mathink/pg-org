(require 'ox)
(require 'ox-publish)
(require 'format-spec)

(eval-when-compile (require 'cl))

(defun org-coq-cons (p)
  (cons p (intern (concat "org-export-coq-" (symbol-name p)))))

(org-export-define-backend
    'coqdoc
  (mapcar
   #'org-coq-cons
   '(bold code verbatim headline latex-fragment paragraph src-block inline-src-block section template))
  ;; '(
  ;;    (bold . org-export-coq-bold)
  ;;    (code . org-export-coq-code)
  ;;    (headline . org-export-coq-headline)
  ;;    (latex-fragment . org-export-coq-latex-fragment)
  ;;    (paragraph . org-export-coq-paragraph)
  ;;    (src-block . org-export-coq-src-block)
  ;;    (section . org-export-coq-section)
  ;;    (template . org-export-coq-template)
  ;;    )
  :export-block "COQDOC"
  :menu-entry
  '(?v "Export to Coq Script"
       (
	(?v "As Coq file" org-export-coq-to-vernacular)
	(?V "As Coq Buffer" org-export-coq-to-buffer)
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
  (format "%s" contents))

(defun org-export-coq-code (code contents info)
  (format "%s" (org-element-property :value code)))

(defun org-export-coq-verbatim (verbatim contents info)
  (format "\[%s\]" (org-element-property :value verbatim)))

(defun org-export-coq-headline (headline contents info)
  (let ((level (org-export-get-relative-level headline info))
	(text (org-export-data (org-element-property :title headline) info)))
    (format "%s(** %s %s *)\n%s\n"
	    (if (= level 1) "\n" "")
	    (make-string level ?*) text contents)))

(defun org-export-coq-paragraph (paragraph contents info)
  (format "(**\n%s\n*)" contents))

(defun org-export-coq-src-block (src-block contents info)
  (let ((lang (org-element-property :language src-block))
	(caption (org-export-get-caption src-block))
	(code (car (org-export-unravel-code src-block))))
    (if (string= "coq" lang) code
      (format "(**\n<<\n%s>>\n*)\n" code))))

(defun org-export-coq-inline-src-block (inline-src-block contents info)
  (let ((code (org-element-property :value inline-src-block)))
    (format "[%s]" code)))

(defun org-export-coq-section (section contents info)
  (let ((parent (org-export-get-parent-headline section)))
    (if (not parent) contents
      (let ((section-number
	     (and (org-export-numbered-headline-p parent info)
		  (mapconcat
		    #'number-to-string
		    (org-export-get-headline-number parent info) "-"))))
	(format "\n%s" contents)))))

(defun org-export-coq-latex-fragment (latex-fragment contents info)
  (let ((latex-frag (org-element-property :value latex-fragment)))
    (format "%s" latex-frag)))


(defun org-export-coq-almost (almost contents info)
  contents)


;; --------------------------------
;;  Exporter
;; 

(defun org-export-coq-to-vernacular (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let* ((extension ".v")
	 (file (org-export-output-file-name extension subtreep))
	 (org-export-coding-system po:coding-system))
    (org-export-to-file 'coqdoc file
      async subtreep visible-only body-only ext-plist)))


(defun org-export-coq-to-buffer (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (org-export-to-buffer 'coqdoc "*Org Coq Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (coq-mode))))


(provide 'ox-coq)
