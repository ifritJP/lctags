(defvar lctags-candidate-info nil)


(defun lctags-helm-select (item)
  (let ((prefix (lctags-candidate-get-prefix))
	simple )
    (setq simple (lctags-candidate-item-get-simple item))
    (insert (substring simple (length prefix)))
  ))

(defun lctags-candidate-item-get-simple (item)
  (nth 2 (assoc 'simple item))
  )

(defun lctags-candidate-item-get-type (item)
  (nth 2 (assoc 'type item))
  )

(defun lctags-candidate-item-get-kind (item)
  (nth 2 (assoc 'kind item))
  )

(defun lctags-candidate-item-get-canonical (item)
  (nth 2 (assoc 'canonical item))
  )

(defun lctags-candidate-get-item-from-canonical (canonical)
  (car (delq nil (mapcar (lambda (X)
			   (when (listp X)
			     (when (equal (nth 2 (assoc 'canonical X)) canonical)
			       X)))
			 lctags-candidate-info
			 ))))

(defun lctags-candidate-map-candidate (func info)
  (mapcar (lambda (X)
	    (when (and (listp X) (eq (car X) 'candidate) )
	      (funcall func X)))
	  info
	  ))

(defun lctags-candidate-get-prefix ()
  (let ((prefix-info
	 (assoc 'prefix lctags-candidate-info)))
    (when prefix-info
      (nth 2 prefix-info))))


(defun lctags-helm-complete-at ()
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
      (setq lctags-candidate-info (assoc 'complete
				  (xml-parse-region (point-min) (point-max))))
      (setq candidates
	    (delq nil (lctags-candidate-map-candidate
		       (lambda (X)
			 (cons (format "(%s) %s %s"
				       (lctags-candidate-item-get-kind X)
				       (lctags-candidate-item-get-canonical X)
				       (if (lctags-candidate-item-get-type X)
					   (format "<%s>"
						   (lctags-candidate-item-get-type X))
					 ""
					 ))
			       X))
		       lctags-candidate-info))))
    (helm :sources (list (cons 'name (format "comp-at:%s:%d:%d" filename lineno column))
			 (cons 'candidates candidates)
			 (cons 'action 'lctags-helm-select)))))

(provide 'lctags-helm)
