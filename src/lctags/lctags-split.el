(defvar lctags-sub-ret-type nil)

(defun lctags-split-get-args (info)
  (xml-get-children (car (xml-get-children info 'args)) 'arg)
  )

(defun lctags-split-can-directRet (info)
  (car (xml-node-children (car (xml-get-children info 'directRet))))
  )


(defun lctags-split-arg-isAddressAccess (arg)
  (equal (car (xml-node-children (car (xml-get-children arg 'addressAccess))))
	 "true"))

(defun lctags-split-arg-get-name (arg)
  (car (xml-node-children (car (xml-get-children arg 'name)))))

(defun lctags-split-arg-get-argSym (arg)
  (car (xml-node-children (car (xml-get-children arg 'argSymbol)))))


(defun lctags-split-get-call (info)
  (let ((call-txt (car (xml-node-children (car (xml-get-children info 'call))))))
    (when (string-match "^[\n\t]+" call-txt)
      (setq call-txt (replace-match "" nil nil call-txt)))
    call-txt
    ))
(defun lctags-split-get-subroutine (info)
  (let ((txt (car (xml-node-children (car (xml-get-children info 'sub_routine))))))
    (when (string-match "^[\n\t]+" txt)
      (setq txt (replace-match "" nil nil txt)))
    txt
    ))


(defvar lctags-split-buffer nil)
(defvar lctags-split-target-buffer nil)
(defvar lctags-split-target-pos-mark nil)


(defun lctags-execute-split-op (src-buf lctags-buf input lctags-opts)
  (let (info)
    (if (eq (lctags-execute-op src-buf lctags-buf input nil lctags-opts) 0)
	(progn
	  (setq lctags-split-info (lctags-xml-get lctags-buf 'refactoring_split))
	  (if (assoc 'candidate lctags-split-info)
	      (setq lctags-diag-info nil)
	    (setq lctags-diag-info (lctags-xml-get-diag lctags-buf))))
      (setq lctags-split-info nil)
      (with-current-buffer lctags-buf
	(setq lctags-diag-info `((message nil ,(buffer-string))))))
    info
    ))

(defun lctags-execute-split (src-buf lctags-buf input &rest lctags-opts)
  (let (split-info subroutine-pos)
    (setq split-info (lctags-execute-split-op src-buf lctags-buf input lctags-opts))
    (if lctags-diag-info
	(lctags-helm-display-diag)
      (setq lctags-split-target-buffer src-buf)
      (when lctags-split-target-pos-mark
	(set-marker lctags-split-target-pos-mark nil)
	(setq lctags-split-target-pos-mark nil))
      (setq lctags-split-target-pos-mark (point-marker))
      (when (not (buffer-live-p lctags-split-buffer))
	(setq lctags-split-buffer (get-buffer-create "*lctags-split*")))
      (lctags-switch-to-buffer-other-window lctags-split-buffer)
      (setq split-info (lctags-xml-get lctags-buf 'refactoring_split))

      (with-current-buffer lctags-split-buffer
	(erase-buffer)
	(insert "/* please edit 'x' or 'o' and symbol and order of following items,\n    and push C-c C-c to update.\n")
	(when (lctags-split-can-directRet split-info)
	  (insert (format ":%s:indirect-return\n"
			  (if (equal (lctags-split-can-directRet split-info)
				     "true")
			      "x" "o"))))
	(dolist (arg (lctags-split-get-args split-info))
	  (insert (format ":%s:%s:%s\n"
			  (if (lctags-split-arg-isAddressAccess arg)
			      "o" "x")
			  (lctags-split-arg-get-argSym arg)
			  (lctags-split-arg-get-name arg)
			  arg)))
	(c++-mode)
	(insert "*/\n")
	(setq subroutine-pos (point))
	(insert "//======= call ======\n")
	(insert (lctags-split-get-call split-info))
	(insert "\n//======= sub routine ======\n")
	(insert (lctags-split-get-subroutine split-info))
	(local-set-key (kbd "C-c C-c") 'lctags-split-retry)
	(indent-region subroutine-pos (point))
	(beginning-of-buffer))
      )))



(defun lctags-split-at (&optional direct-return &rest ignore-list)
  (interactive)
  (let ((buffer (lctags-get-process-buffer t)))
    (lctags-execute-split (current-buffer) buffer (buffer-string) "split-at"
			  (buffer-file-name (current-buffer))
			  (number-to-string (lctags-get-line))
			  (number-to-string (- (lctags-get-column) 1)) "-i"
			  (when lctags-sub-ret-type "--lctags-subRet")
			  (when lctags-sub-ret-type lctags-sub-ret-type)
			  direct-return
			  (when ignore-list "-split-param-list")
			  (mapconcat (lambda (X) X) 
				     ignore-list "," ))
  ))

(defun lctags-split-retry ()
  (interactive)
  (let ((back-pos (point))
	ignore-list direct-return)
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward "^:[xo]:" nil t)
	(let (symbol pos param endpos)
	  (setq pos (point))
	  (end-of-line)
	  (setq endpos (point))
	  (setq param (buffer-substring-no-properties (- pos 2) endpos))
	  (re-search-backward ":" nil t)
	  (setq symbol (buffer-substring-no-properties (1+ (point)) endpos))
	  (if (equal symbol "indirect-return")
	      (when (string-match "^x:" param)
		(setq direct-return "--lctags-directRet"))
	    (setq ignore-list (append ignore-list (list param)))))))
    (with-current-buffer lctags-split-target-buffer
      (goto-char lctags-split-target-pos-mark)
      (apply 'lctags-split-at direct-return ignore-list)
      )
    (goto-char back-pos)
    ))

(provide 'lctags-split)
