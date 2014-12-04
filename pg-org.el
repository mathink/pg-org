
(define-derived-mode pg-org-mode org-mode
  "PG-Org mode"
  "ProofGeneral Plugin (?) for Org-mode."

  ;; sorry, general
  (setq proof-splash-time 0)

  (defvar po:update-point 1)

  (define-key pg-org-mode-map
    (kbd "C-c C-<return>") 'po:update-here)
  (define-key pg-org-mode-map
    (kbd "C-c C-;") 'po:insert)
  (define-key pg-org-mode-map
    (kbd "C-c C-n") 'po:update-next)
  (define-key pg-org-mode-map
    (kbd "C-c C-u") 'po:update-prev)
  (define-key pg-org-mode-map
    (kbd "C-c C-x") 'po:exit))

(defun po:update-here ()
  (interactive)
  (let ((here (point)))
    (po:update t)
    (goto-char here)))

(defun po:update-next ()
  (interactive)
  (goto-char po:update-point)
  (org-babel-next-src-block)
  (po:update t))

(defun po:update-prev ()
  (interactive)
  (goto-char po:update-point)
  (org-babel-previous-src-block)
  (org-babel-previous-src-block)
  (po:update nil))

(defun po:insert ()
  (interactive)
  (when proof-shell-last-output
    (insert "\n")
    (org-cycle)
    (insert "#+BEGIN_EXAMPLE\n")
    (insert (proof-shell-strip-output-markup proof-shell-last-output))
    (insert "#+END_EXAMPLE")
    (org-cycle)
    (insert "\n")))

(defun po:exit ()
  (interactive)
  (let* ((src-buf (current-buffer))
         (fname (file-name-nondirectory (buffer-file-name src-buf)))
         (cname (concat "*pg-org: " fname "*"))
         (c (get-buffer cname))
         (dir (file-name-directory (buffer-file-name src-buf))))
    (with-current-buffer c
      (proof-shell-exit t))
    (kill-buffer c)
    (setq po:update-point 1)))

(defun org-babel-where-is-src-block-tail ()
  "Goto End of src block."
  (interactive)
  (let ((head (org-babel-where-is-src-block-head)))
    (when head
      (save-excursion
        (goto-char head)
        (looking-at org-babel-src-block-regexp)
        (goto-char (match-end 5))
        (point-at-eol)))))

(defun po:update (eval-p)
  (let ((cbuf (current-buffer))
        (begin 1)
        (end nil))
    (unless (org-babel-when-in-src-block) (org-babel-previous-src-block))
    (setf end (org-babel-where-is-src-block-tail))
    (goto-char end)
    (setf po:update-point end)
    (po:update-and-eval (current-buffer) eval-p begin end)
    (switch-to-buffer cbuf)))

(defun po:update-and-eval (buffer eval-p begin end)
  (let* ((src-buf buffer)
         (temp-buf (generate-new-buffer "*temp*"))
         (fname (file-name-nondirectory (buffer-file-name src-buf)))
         (cname (concat "*pg-org: " fname "*"))
         (c (get-buffer cname))
         (dir (file-name-directory (buffer-file-name src-buf))))
    ;; 
    ;; org-babel-tangle 用ファイルの生成
    (with-current-buffer temp-buf
      (insert-buffer-substring src-buf begin end)
      (write-file "/tmp/pgorg_temp.org")
      (org-babel-tangle nil "/tmp/pgorg_temp.v" "coq"))
    (kill-buffer temp-buf)

    ;; 
    ;; 既に pg-org で処理をしているか？
    (unless c
      (setf c (generate-new-buffer cname))
      (with-current-buffer c
        (setf buffer-offer-save nil)
        (coq-mode)))

    ;; 
    ;; バッファの更新
    (let ((now (with-current-buffer c (buffer-string))))
      (with-temp-buffer
        (insert-file-contents "/tmp/pgorg_temp.v")
        (let* ((new (buffer-string))
               (pos (compare-strings now nil nil new nil nil)))
          (when (numberp pos)
            (setf pos (abs pos))
            (with-current-buffer c
              (goto-char pos)
              (delete-region pos (point-max))
              (setf buffer-offer-save nil))
            (append-to-buffer c pos (point-max)))))
      (with-current-buffer c
        (goto-char (point-max))
        (when eval-p (proof-goto-point))))))
