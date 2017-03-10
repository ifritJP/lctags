(defun lctags-heml-select (item)
  (message (format "%s" item))
  )


(defun lctags-heml-complete-at ()
  (interactive)
  (let ((buffer (lctags-get-process-buffer t))
	(filename (buffer-file-name (current-buffer)))
	(lineno (lctags-get-line))
	(column (- (lctags-get-column) 2))
	candidates
	)
    (lctags-execute (current-buffer) buffer
		    (buffer-string) "comp-at" filename
		    (number-to-string lineno) (number-to-string column)
		    "--lctags-log" "0" "-i")
    (with-current-buffer buffer
      (setq candidates
	    (delq nil (mapcar (lambda (X) 
				(when (and (listp X) 
					   (eq (car X) 'candidate))
				  (nth 2 X)))
			      (assoc 'complete
				     (xml-parse-region (point-min) (point-max)))))))
    (helm :sources (list (cons 'name (format "comp-at:%s:%d:%d" filename lineno column))
			 (cons 'candidates candidates)
			 (cons 'action 'lctags-heml-select)))))

;;(cdr (assoc 'candidate '((candidate . ["::Test1::member" "::Test1::sub" "::Test1::func" ""]) (prefix . ""))))

(mapcar (lambda (X) (when (not (equal X "")) X)) [a b c ""])
