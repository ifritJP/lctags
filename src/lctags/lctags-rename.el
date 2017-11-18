(require 'lctags-highlight)

(defun lctags-rename-at ()
  (interactive)
  (let ((buffer (lctags-get-process-buffer t))
	location-info location-list kind new-symbol locationSet-list
	pos-list)
    (lctags-execute-xml (current-buffer) buffer
			(buffer-string)
			'location-info 'ref 'location
			"ref-at-all"
			(buffer-file-name)
			(number-to-string (lctags-get-line))
			(number-to-string (lctags-get-column)) "-i")
    (setq locationSet-list (lctags-xml-get-list location-info 'locationSet))
    (cond
     (lctags-diag-info
      (lctags-helm-display-diag))
     ((not (eq 1 (length locationSet-list)))
      (message "not support to rename function"))
     (t
      (setq location-list (lctags-xml-get-list (car locationSet-list) 'location))
      (setq kind (lctags-location-item-get-kind (car location-list)))
      (when (or (equal kind "ParmDecl") (equal kind "VarDecl"))
	(setq new-symbol (read-string "rename?: "
				      (lctags-location-item-get-symbol
				       (car location-list))))
	(save-excursion
	  ;; 編集すると位置が変ってしまって正常に編集できなくなるので、
	  ;; 位置が変わらないように marker を作る。
	  (dolist (location (lctags-xml-get-list (car locationSet-list) 'location))
	    (let (begin-mark)
	      (goto-char (lctags-location-item-get-point location))
	      (setq begin-mark (point-marker))
	      (goto-char (lctags-location-item-get-end-point location))
	      (setq pos-list (append pos-list `((:mark ,begin-mark
						     :end-mark ,(point-marker)))))))
	  ;; マーカーを元に編集する
	  (dolist (pos-info pos-list)
	    (goto-char (plist-get pos-info :mark))
	    (delete-region (point)
			   (plist-get pos-info :end-mark))
	    (insert new-symbol))
	  (backward-char)
	  (lctags-highlight-at-op lctags-search-token-color-default)
	  ))))))


(provide 'lctags-rename)
