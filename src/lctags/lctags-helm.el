(defvar lctags-candidate-info nil)
(defvar lctags-candidate-history nil)

(defvar lctags-anything nil)

(when (not lctags-anything)
  (require 'helm))
  

(defvar lctags-heml-map
  (let (map)
    (if lctags-anything
	(setq map (copy-keymap anything-map))
      (setq map (copy-keymap helm-map)))
    (define-key map (kbd "C-M-f")   'lctags-helm-type-forward)
    (define-key map (kbd "C-M-b")   'lctags-helm-type-backward)
    (delq nil map))
  "Keymap")

(defun lctags-helm-select (item)
  (case lctags-candidate-expand
    (:back)
    (nil)
    (t
     (let ((prefix (lctags-candidate-get-prefix))
	   (pos (point))
	   simple info )
       (if (eq lctags-candidate-expand :forward)
	   (setq info (nth 1 lctags-candidate-history))
	 (setq info (nth 0 lctags-candidate-history)))
       (setq simple
	     (concat
	      (lctags-candidate-item-get-expand (plist-get info :select))
	      (lctags-candidate-item-get-simple item)))
       (insert (substring simple (length prefix)))
       (when (equal (lctags-candidate-item-get-kind item) "f")
	 (goto-char pos)
	 (search-forward "(")
	 (backward-char)
	 )
       )))
  )

(defun lctags-helm-select-swap (item)
  (let (simple )
    (setq simple (lctags-candidate-item-get-expand item))
    (lctags-remove-current-token)
    (insert simple ))
  )

(defun lctags-candidate-item-get-simple (item)
  (nth 2 (assoc 'simple item))
  )
(defun lctags-candidate-item-get-expand (item)
  (nth 2 (assoc 'expand item))
  )

(defun lctags-candidate-item-get-type (item)
  (nth 2 (assoc 'type item))
  )

(defun lctags-candidate-item-get-hash (item)
  (nth 2 (assoc 'hash item))
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

(defun lctags-candidate-get-typeInfo (info hash)
  (delq nil (mapcar (lambda (X)
		      (when (and (listp X) (eq (car X) 'typeInfo) )
			(if (equal (nth 2 (assoc 'hash X)) hash)
			    X)))
		    info
		    )))

;; (lctags-candidate-get-typeInfo lctags-candidate-info "a11b879aa4eeb015a456aa89da4a9926")
;; (lctags-candidate-map-candidate (symbol-function 'lctags-helm-make-candidates) (car (lctags-candidate-get-typeInfo lctags-candidate-info "a11b879aa4eeb015a456aa89da4a9926")))

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

;; (assoc 'candidate (plist-get (car lctags-candidate-history) :typeInfo))
(defun lctags-helm-wrap (src keymap preselect)
  (setq lctags-candidate-history `((:candidate ,src)))
  (let ((lctags-candidate-expand t))
    (while lctags-candidate-expand
      (setq lctags-candidate-expand nil)
      (if lctags-anything
	  (let ((anything-candidate-number-limit 9999))
	    (anything :sources '(src) :keymap keymap :preselect preselect))
	(let ((helm-candidate-number-limit 9999))
	  (helm :sources src :keymap keymap :preselect preselect)))
      (setq preselect nil)
      (when (eq lctags-candidate-expand :back)
	(let ((prev (car lctags-candidate-history)))
	  (when (plist-get prev :point)
	    (setq preselect
		  (car (lctags-helm-make-candidates
			(plist-get (car lctags-candidate-history) :select))))
	    (with-current-buffer (lctags-get-helm-current-buffer)
	      (delete-region (plist-get prev :point) (point)))
	    (setq lctags-candidate-history (cdr lctags-candidate-history))
	    ))
	)
      (when lctags-candidate-expand
	(setq src (plist-get (car lctags-candidate-history) :candidate))
	)
      ))
  ;;(message "ok")
  )

(defun lctags-helm-make-candidates ( info )
  (cons (format
	 "(%s) %s%s %s"
	 (lctags-candidate-item-get-kind info)
	 (propertize
	  (lctags-candidate-item-get-canonical info)
	  'face 'lctags-candidate-face)
	 (if (lctags-candidate-item-get-val info)
	     (format " => %s"
		     (lctags-candidate-item-get-val info))
	   "")
	 (if (lctags-candidate-item-get-type info)
	     (format "<%s>"
		     (lctags-candidate-item-get-type info))
	   ""
	   ))
	info))


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
      (setq lctags-candidate-info
	    (assoc 'complete
		   (xml-parse-region (point-min) (point-max))))
      (setq candidates
      	    (delq nil (lctags-candidate-map-candidate
		       (symbol-function 'lctags-helm-make-candidates)
		       lctags-candidate-info)))
      )
    (setq lctags-params
	  `((name . ,(format "comp-at:%s:%d:%d"
			   (file-name-nondirectory filename)
			   lineno column))
	    (candidates . ,candidates)
	    (action . lctags-helm-select)))
    (lctags-helm-wrap lctags-params lctags-heml-map nil)
    ))



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
	  (anything :sources lctags-params :keymap lctags-heml-map
		    :preselect token))
      (let ((helm-candidate-number-limit 9999))
	(helm :sources lctags-params :keymap lctags-heml-map
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


(when lctags-anything
  (defalias helm-get-selection anything-get-selection)
  (defalias helm-exit-minibuffer anything-exit-minibuffer)
  )

(defun lctags-get-helm-current-buffer ()
  (if lctags-anything
      anything-current-buffer
    helm-current-buffer))



(defun* lctags-helm-type-forward (&optional (attr 'persistent-action) onewindow)
  (interactive)

  (let ((info (helm-get-selection))
	hash typeInfo)
    (setq hash (lctags-candidate-item-get-hash info))
    (setq typeInfo (lctags-candidate-get-typeInfo
		    lctags-candidate-info hash))
    (when typeInfo
      (setq typeInfo (delq nil (lctags-candidate-map-candidate
				(symbol-function 'lctags-helm-make-candidates)
				(car typeInfo))))
      (setq lctags-candidate-expand :forward)
      (with-current-buffer (lctags-get-helm-current-buffer)
	(setq lctags-candidate-history
	      (append `((:candidate ((name . ,(format "expand:%s"
						     (lctags-candidate-item-get-type info)))
				     (action . lctags-helm-select)
				     (candidates . ,typeInfo))
				    :select ,info
				    :point ,(point)
				    ))
		      lctags-candidate-history))))
    (helm-exit-minibuffer)
    )
  )

(defun* lctags-helm-type-backward (&optional (attr 'persistent-action) onewindow)
  (interactive)
  (setq lctags-candidate-expand :back)
  (helm-exit-minibuffer)
  )



(provide 'lctags-helm)
