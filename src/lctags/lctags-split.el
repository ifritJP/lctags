(defun lctags-split-get-args (info)
  (xml-get-children (car (xml-get-children info 'args)) 'arg)
  )

(defun lctags-split-arg-isAddressAccess (arg)
  (equal (car (xml-node-children (car (xml-get-children arg 'addressAccess))))
	 "true"))

(defun lctags-split-arg-get-name (arg)
  (car (xml-node-children (car (xml-get-children arg 'name)))))

(defun lctags-split-get-call (info)
  (let ((call-txt (car (xml-node-children (car (xml-get-children info 'call))))))
    (when (string-match "[\n\t]" call-txt)
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


(defun lctags-execute-split (src-buf lctags-buf input &rest lctags-opts)
  (if (eq (lctags-execute-op src-buf lctags-buf input nil lctags-opts) 0)
      (let (split-info subroutine-pos)
	
	(setq lctags-split-target-buffer src-buf)
	(when lctags-split-target-pos-mark
	  (set-marker lctags-split-target-pos-mark nil)
	  (setq lctags-split-target-pos-mark nil))
	(setq lctags-split-target-pos-mark (point-marker))
	(if (and lctags-split-buffer (buffer-live-p lctags-split-buffer))
	    (switch-to-buffer lctags-split-buffer)
	  (setq lctags-split-buffer (get-buffer-create "*lctags-split*"))
	  (switch-to-buffer-other-window lctags-split-buffer))
	(setq split-info (lctags-xml-get lctags-buf 'refactoring_split))
	(erase-buffer)
	(insert "/* please edit 'x' or 'o' of following items,\n    and push C-c C-c to update.\n")
	(dolist (arg (lctags-split-get-args split-info))
	  (insert (format "%s: %s\n"
			  (if (lctags-split-arg-isAddressAccess arg)
			      "o" "x")
			  (lctags-split-arg-get-name arg)
			  arg)))
	(insert "*/\n")
	(insert "//======= call ======\n")
	(insert (lctags-split-get-call split-info))
	(insert "\n//======= sub routine ======\n")
	(setq subroutine-pos (point))
	(insert (lctags-split-get-subroutine split-info))
	(c++-mode)
	(local-set-key (kbd "C-c C-c") 'lctags-split-retry)
	(indent-region subroutine-pos (point))
	(beginning-of-buffer)
	)))



(defun lctags-split-at (&rest ignore-list)
  (interactive)
  (let ((buffer (lctags-get-process-buffer t)))
    (lctags-execute-split (current-buffer) buffer (buffer-string) "split-at"
			  (buffer-file-name (current-buffer))
			  (number-to-string (lctags-get-line))
			  (number-to-string (- (lctags-get-column) 1)) "-i"
			  (when ignore-list "-ignore-sym-list")
			  (mapconcat (lambda (X) X) 
				     ignore-list "," ))
  ))

(defun lctags-split-retry ()
  (interactive)
  (let (ignore-list pos)
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward "^x: " nil t)
	(setq pos (point))
	(end-of-line)
	(setq ignore-list (cons (buffer-substring-no-properties pos (point))
				ignore-list))))
    (with-current-buffer lctags-split-target-buffer
      (message "%s" ignore-list)
      (goto-char lctags-split-target-pos-mark)
      (apply 'lctags-split-at ignore-list)
      )))

(provide 'lctags-split)
