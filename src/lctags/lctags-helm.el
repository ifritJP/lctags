(defvar lctags-candidate-info nil)

(defvar lctags-anything nil)


(defun lctags-helm-select (item)
  (let ((prefix (lctags-candidate-get-prefix))
	simple )
    (setq simple (lctags-candidate-item-get-simple item))
    (insert (substring simple (length prefix)))
  ))

(defun lctags-helm-select-swap (item)
  (let (simple )
    (setq simple (lctags-candidate-item-get-simple item))
    (lctags-remove-current-token)
    (insert simple ))
  )

(defun lctags-candidate-item-get-simple (item)
  (nth 2 (assoc 'simple item))
  )

(defun lctags-candidate-item-get-type (item)
  (nth 2 (assoc 'type item))
  )

(defun lctags-candidate-item-get-kind (item)
  (nth 2 (assoc 'kind item))
  )

(defun lctags-candidate-item-get-val (item)
  (nth 2 (assoc 'val item))
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

(defface lctags-candidate-face
  '((t
     :foreground "orange"))
  "candidate face")
(defvar lctags-candidate-face 'lctags-candidate-face)


(defun lctags-helm-complete-at ()
  (interactive)
  (let ((buffer (lctags-get-process-buffer t))
	(filename (buffer-file-name (current-buffer)))
	(lineno (lctags-get-line))
	(column (- (lctags-get-column) 2))
	candidates
	lctags-params
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
      			 (cons (format
      				"(%s) %s%s %s"
      				(lctags-candidate-item-get-kind X)
      				(propertize
      				 (lctags-candidate-item-get-canonical X)
      				 'face 'lctags-candidate-face)
				(if (lctags-candidate-item-get-val X)
				    (format " => %s"
					    (lctags-candidate-item-get-val X))
				  "")
      				(if (lctags-candidate-item-get-type X)
      				    (format "<%s>"
      					    (lctags-candidate-item-get-type X))
      				  ""
      				  ))
      			       X))
      		       lctags-candidate-info)))
      )
    (setq lctags-params
	  `((name . ,(format "comp-at:%s:%d:%d"
			   (file-name-nondirectory filename)
			   lineno column))
	    (candidates . ,candidates)
	    (action . lctags-helm-select)))
    (if lctags-anything
	(let ((anything-candidate-number-limit 9999))
	  (anything '(lctags-params)))
      (let ((helm-candidate-number-limit 9999))
	(helm :sources lctags-params)))))



(defun lctags-helm-change-enum-at ()
  (interactive)
  (let ((buffer (lctags-get-process-buffer t))
	(filename (buffer-file-name (current-buffer)))
	(lineno (lctags-get-line))
	(column (- (lctags-get-column) 1))
	(token (lctags-get-current-token))
	candidates
	lctags-params
	)
    (lctags-execute (current-buffer) buffer
		    (buffer-string) "inq-at" filename
		    (number-to-string lineno) (number-to-string column)
		    "--lctags-log" "0" "-i")
    (with-current-buffer buffer
      (setq lctags-candidate-info (assoc 'complete
				  (xml-parse-region (point-min) (point-max))))
      (setq candidates
      	    (delq nil (lctags-candidate-map-candidate
      		       (lambda (X)
      			 (cons (format
      				"(%s) %s%s %s"
      				(lctags-candidate-item-get-kind X)
      				(propertize
      				 (lctags-candidate-item-get-canonical X)
      				 'face 'lctags-candidate-face)
				(if (lctags-candidate-item-get-val X)
				    (format " => %s"
					    (lctags-candidate-item-get-val X))
				  "")
      				(if (lctags-candidate-item-get-type X)
      				    (format "<%s>"
      					    (lctags-candidate-item-get-type X))
      				  ""
      				  ))
      			       X))
      		       lctags-candidate-info)))
      )
    (setq lctags-params
	  `((name . ,(format "comp-at:%s:%d:%d"
			   (file-name-nondirectory filename)
			   lineno column))
	    (candidates . ,candidates)
	    (action . lctags-helm-select-swap)))
    (if lctags-anything
	(let ((anything-candidate-number-limit 9999))
	  (anything :sources lctags-params :preselect token ))
      (let ((helm-candidate-number-limit 9999))
	(helm :sources lctags-params
	      :preselect (format "%s =>" token))))))



(defvar lctags-token-pattern "[a-zA-Z0-9_]")
(defvar lctags-no-token-pattern "[^a-zA-Z0-9_]")

(defun lctags-get-current-token ()
  (interactive)
  (save-excursion
    (let (symstart symend)
      (setq symstart (1+ (re-search-backward lctags-no-token-pattern)))
      (forward-char)
      (setq symend (1- (re-search-forward lctags-no-token-pattern)))
      (when (and symstart symend)
	(buffer-substring symstart symend)))))

(defun lctags-remove-current-token ()
  (let (symstart symend)
    (setq symstart (1+ (re-search-backward lctags-no-token-pattern)))
    (forward-char)
    (setq symend (1- (re-search-forward lctags-no-token-pattern)))
    (when (and symstart symend)
      (delete-region symstart symend)
      (backward-char))))




(provide 'lctags-helm)
