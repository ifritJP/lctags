(defvar lctags-candidate-info nil)
(defvar lctags-candidate-history nil)
(defvar lctags-diag-info nil)

(defvar lctags-anything nil)

(defvar lctags-diag-buf-name "*lctags-diag*" )

(when (not lctags-anything)
  (require 'helm))

(require 'cl)
  
;; [C-return]
(defvar lctags-heml-map
  (let (map)
    (if lctags-anything
	(setq map (copy-keymap anything-map))
      (setq map (copy-keymap helm-map)))
    (define-key map (kbd "C-M-f") 'lctags-helm-type-forward)
    (define-key map (kbd "C-M-b") 'lctags-helm-type-backward)
    (define-key map [C-return] 'lctags-helm-select-all)
    (delq nil map))
  "Keymap")

(defun lctags-execute-heml (src-buf lctags-buf input &rest lctags-opts)
  (if (eq (lctags-execute-op src-buf lctags-buf input nil lctags-opts) 0)
      (progn
	(setq lctags-candidate-info (lctags-xml-get lctags-buf 'complete))
	(if (assoc 'candidate lctags-candidate-info)
	    (setq lctags-diag-info nil)
	  (setq lctags-diag-info (lctags-xml-get-diag lctags-buf))))
    (setq lctags-candidate-info nil)
    (with-current-buffer lctags-buf
      (setq lctags-diag-info `((message nil ,(buffer-string))))))
  )

(defun lctags-helm-select (item)
  (case lctags-candidate-select-mode
    (:back)
    (nil)
    (t
     (let ((prefix (lctags-candidate-get-prefix))
	   (front (lctags-candidate-get-frontExpr))
	   (pos (point))
	   (hist-expr "")
	   simple info items)
       (if (eq lctags-candidate-select-mode :forward)
	   (setq info (nth 1 lctags-candidate-history))
	 (setq info (nth 0 lctags-candidate-history)))
       (when (> (length (helm-marked-candidates)) 1)
	 (setq item (car (helm-marked-candidates)))
	 (setq items (cdr (helm-marked-candidates))))
       (when (eq lctags-candidate-select-mode :all)
	 (setq lctags-candidate-select-mode nil)
	 (setq items (cdr (assoc 'candidates (plist-get info :candidate))))
	 (setq item (car items))
	 (setq items (cdr items))
	 )
       (setq simple
	     (concat
	      (lctags-candidate-item-get-expand (plist-get info :select))
	      (lctags-candidate-item-get-simple item)))
       (insert (substring simple (length prefix)))
       (when (equal (lctags-candidate-item-get-kind item) "f")
	 ;; (not (string-match "\\.\\|->" front))
	 (if (string-match "(" simple)
	     (progn
	       (goto-char pos)
	       (search-forward "(")
	       (backward-char))
	   ;; メンバでないと simple に引数情報が含まれないので、
	   ;; 別途関数の問い合わせをして引数情報を展開する。
	   (let ((buffer (lctags-get-process-buffer t))
		 (filename (buffer-file-name (current-buffer)))
		 (lineno (lctags-get-line))
		 (column (- (lctags-get-column) 2))
		 inq-info
		 args
		 )
	     (lctags-execute (current-buffer) buffer
			     (buffer-string) "inq-at" filename
			     (number-to-string lineno) (number-to-string column)
			     "--lctags-log" "0" "-i")
	     (with-current-buffer buffer
	       (setq inq-info (lctags-xml-get buffer 'candidate)))
	     (setq args (lctags-candidate-item-get-simple inq-info))
	     (when (string-match (concat simple "(") args)
	       (setq pos (point))
	       (insert (substring args (length (concat simple))))
	       (goto-char pos))
	     )
	 ))
       (when (>= (length items) 1)
	 (dolist (info lctags-candidate-history)
	   (setq hist-expr
		 (concat hist-expr
			 (lctags-candidate-item-get-simple (plist-get info :select))
			 (lctags-candidate-item-get-expand (plist-get info :select)))))
	 (dolist (marked items)
	   (insert (concat "\n" front hist-expr
			   (lctags-candidate-item-get-simple marked)))
	 )))
     ))
  )

(defun lctags-helm-select-swap (item)
  (let (simple )
    (setq simple (lctags-candidate-item-get-simple item))
    (lctags-remove-current-token)
    (insert simple ))
  )

(defun lctags-xml-get (buf symbol)
  (with-current-buffer buf
    (assoc symbol (assoc 'lctags_result (xml-parse-region (point-min) (point-max))) )))

(defun lctags-xml-get-diag (buf)
  (delq nil (mapcar (lambda (X)
		      (when (listp X)
			(when (eq (car X) 'message)
			  X)))
		    (lctags-xml-get buf 'diagnostics))))

(defun lctags-diag-get-message (diag)
  (nth 2 diag))


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

(defun lctags-candidate-map-candidate (func info all-info)
  (mapcar (lambda (X)
	    (when (and (listp X) (eq (car X) 'candidate) )
	      (funcall func X all-info)))
	  info
	  ))

(defun lctags-candidate-get-typeInfo (info hash)
  (delq nil (mapcar (lambda (X)
		      (when (and (listp X) (eq (car X) 'typeInfo) )
			(if (equal (nth 2 (assoc 'hash X)) hash)
			    X)))
		    info
		    )))

(defun lctags-candidate-get-prefix (&optional candidate-info)
  (let ((prefix-info
	 (assoc 'prefix
		(if candidate-info candidate-info lctags-candidate-info))))
    (when prefix-info
      (nth 2 prefix-info))))

(defun lctags-candidate-get-frontExpr (&optional candidate-info)
  (let ((info
	 (assoc 'frontExpr
		(if candidate-info candidate-info lctags-candidate-info))))
    (when info
      (nth 2 info))))

(defface lctags-candidate-face
  '((t
     :foreground "gold"))
  "candidate face")
(defvar lctags-candidate-face 'lctags-candidate-face)

(defface lctags-expandable-candidate-face
  '((t
     :foreground "dark orange"))
  "expandable candidate face")
(defvar lctags-expandable-candidate-face 'lctags-expandable-candidate-face)

;; (assoc 'candidate (plist-get (car lctags-candidate-history) :typeInfo))
(defun lctags-helm-wrap (src keymap preselect)
  (setq lctags-candidate-history `((:candidate ,src)))
  (let ((lctags-candidate-select-mode t))
    (while lctags-candidate-select-mode
      (setq lctags-candidate-select-mode nil)
      (if lctags-anything
	  (let ((anything-candidate-number-limit 9999))
	    (anything :sources '(src) :keymap keymap :preselect preselect))
	(let ((helm-candidate-number-limit 9999))
	  (helm :sources src :keymap keymap :preselect preselect)))
      (setq preselect nil)
      (when (eq lctags-candidate-select-mode :back)
	(let ((prev (car lctags-candidate-history)))
	  (when (plist-get prev :point)
	    (setq preselect
		  (car (lctags-helm-make-candidates
			(plist-get (car lctags-candidate-history) :select)
			lctags-candidate-info)))
	    (with-current-buffer (lctags-get-helm-current-buffer)
	      (delete-region (plist-get prev :point) (point)))
	    (setq lctags-candidate-history (cdr lctags-candidate-history))
	    ))
	)
      (when lctags-candidate-select-mode
	(setq src (plist-get (car lctags-candidate-history) :candidate))
	)
      ))
  ;;(message "ok")
  )

(defun lctags-helm-make-candidates ( info candidates-info)
  (let* ((hash (lctags-candidate-item-get-hash info))
	 type-info
	 expandable)
    (when (< (length candidates-info) 300)
      (setq type-info (lctags-candidate-get-typeInfo candidates-info hash)))
    (setq expandable (assoc 'candidate (car type-info)))
    (cons (format
	   "(%s) %s%s %s %s"
	   (lctags-candidate-item-get-kind info)
	   (propertize
	    (lctags-candidate-item-get-canonical info)
	    'face
	    (if expandable
		'lctags-expandable-candidate-face
	      'lctags-candidate-face))
	   (if (lctags-candidate-item-get-val info)
	       (format " => %s"
		       (lctags-candidate-item-get-val info))
	     "")
	   (if (lctags-candidate-item-get-type info)
	       (format "<%s>"
		       (lctags-candidate-item-get-type info))
	     ""
	     )
	   (if expandable
	       (propertize "=>" 'face 'lctags-expandable-candidate-face)
	     "")
	   )
	  info)))


(defun lctags-helm-display-diag ()
  (if lctags-diag-info
      (progn
	(switch-to-buffer-other-window lctags-diag-buf-name)
	(setq buffer-read-only nil)
	(erase-buffer)
	(dolist (diag lctags-diag-info)
	  (let* ((token-list (split-string (lctags-diag-get-message diag) ":" ))
		 (path (file-relative-name (car token-list) default-directory))
		 (message path))
	    (dolist (token (cdr token-list))
	      (setq message (format "%s:%s" message token)))
	    (insert message)
	    )
	  (insert "\n"))
	(compilation-mode)
	(goto-char (point-min)))
    (message "none diagnostics message")
    (when (get-buffer lctags-diag-buf-name)
      (kill-buffer lctags-diag-buf-name))))
  

(defun lctags-helm-complete-at ()
  (interactive)
  (let ((buffer (lctags-get-process-buffer t))
	(filename (buffer-file-name (current-buffer)))
	(lineno (lctags-get-line))
	(column (- (lctags-get-column) 2))
	candidates
	lctags-params
	)
    (lctags-execute-heml (current-buffer) buffer
			 (buffer-string) "comp-at" filename
			 (number-to-string lineno) (number-to-string column)
			 "--lctags-log" "0" "-i")
    (if lctags-diag-info
	(lctags-helm-display-diag)
      (with-current-buffer buffer
	(setq candidates
	      (delq nil (lctags-candidate-map-candidate
			 (symbol-function 'lctags-helm-make-candidates)
			 lctags-candidate-info lctags-candidate-info)))
	)
      (setq lctags-params
	    `((name . ,(format "comp-at:%s:%d:%d"
			       (file-name-nondirectory filename)
			       lineno column))
	      (candidates . ,candidates)
	      (action . lctags-helm-select)))
      (lctags-helm-wrap lctags-params lctags-heml-map nil)
      )))

(defun lctags-get-candidate-at ( command )
  (interactive)
  (let ((buffer (lctags-get-process-buffer t))
	(filename (buffer-file-name (current-buffer)))
	(lineno (lctags-get-line))
	(column (- (lctags-get-column) 2))
	)
    (lctags-execute-heml (current-buffer) buffer
			 (buffer-string) command filename
			 (number-to-string lineno) (number-to-string column)
			 "--lctags-log" "0" "-i")
    )
  lctags-candidate-info
  )


(defun lctags-get-inq-at ()
  (interactive)
  (lctags-get-candidate-at "inq-at"))

(defun lctags-get-expand-at ()
  (interactive)
  (lctags-get-candidate-at "expand"))

(defun lctags-generate-to-dump-member-at ()
  (interactive)
  (let ((info (lctags-get-inq-at))
	(line-num (line-number-at-pos))
	(pos (point))
	expr search-token log-func)
    (if lctags-diag-info
	(lctags-helm-display-diag)
      (setq log-func (read-string "log function?: " "printf(" ))
      (setq expr (lctags-candidate-get-frontExpr info))
      (when (string-match "->$" expr)
	(setq search-token (replace-match "" t nil expr)))
      (when (string-match "\\.$" expr)
	(setq search-token (replace-match "" t nil expr)))
      (save-excursion
	(beginning-of-line-text)
	(if (search-forward search-token)
	    (replace-match "")))
      (lctags-candidate-map-candidate
       (lambda (X XX)
	 (let ((val (concat expr (lctags-candidate-item-get-simple X) )))
	   (insert (format
		    "%s \"%s = %%p\\n\", %s%s );\n"
		    log-func
		    val
		    (if (> (length val) 30)
			"\n"
		      "")
		    val))))
       info info)
      (indent-region pos (point))
      )))


(defun lctags-generate-to-convert-enumName-at ()
  (interactive)
  (let ((info (lctags-get-expand-at))
	(line-num (line-number-at-pos))
	(search-token (lctags-get-current-token))
	(pos (point))
	)
    (if lctags-diag-info
	(lctags-helm-display-diag)
      (save-excursion
	(beginning-of-line-text)
	(if (search-forward search-token)
	    (replace-match "")))
      (insert (format "switch (%s) {\n" search-token))
      (lctags-candidate-map-candidate
       (lambda (X XX)
	 (let ((val (lctags-candidate-item-get-simple X) ))
	   (insert (format
		    "case %s:\nreturn \"%s\";\n"
		    val val))))
       info info)
      (insert "default:\nreturn NULL;\n")
      (insert (format "}" search-token))
      (indent-region pos (point))
      )))


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
    (lctags-execute-heml (current-buffer) buffer
			 (buffer-string) "expand" filename
			 (number-to-string lineno) (number-to-string column)
			 "--lctags-log" "0" "-i")
    (if lctags-diag-info
	(lctags-helm-display-diag)
      (with-current-buffer buffer
	(setq candidates
	      (delq nil (lctags-candidate-map-candidate
			 (lambda (X XX)
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
			 lctags-candidate-info lctags-candidate-info)))
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
		:preselect (format "%s =>" token)))))))



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
  (defalias 'helm-get-selection 'anything-get-selection)
  (defalias 'helm-exit-minibuffer 'anything-exit-minibuffer)
  (defalias 'helm-marked-candidates 'anything-marked-candidates)
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
				(car typeInfo) lctags-candidate-info)))
      (setq lctags-candidate-select-mode :forward)
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
  (setq lctags-candidate-select-mode :back)
  (helm-exit-minibuffer)
  )

(defun* lctags-helm-select-all (&optional (attr 'persistent-action) onewindow)
  (interactive)
  (let (candidates)
    (setq candidates (plist-get (nth (1- (length lctags-candidate-history))
				     lctags-candidate-history)
				:candidate))
    (setq lctags-candidate-select-mode :all)
    (helm-exit-minibuffer)    
    ))

(defun lctags-display-diag ()
  (interactive)
  (let ((buffer (lctags-get-process-buffer t)))
    (lctags-execute-heml (current-buffer) buffer
			 (buffer-string) "diag" (buffer-file-name)
			 "--lctags-log" "0" "-i")
    (lctags-helm-display-diag)
    )
  )

(defun lctags-gtags-select (item)
  (with-temp-buffer
    (insert (format "sym %d %s "
		    (plist-get item :line)
		    (plist-get item :path)))
    (beginning-of-line)
    (gtags-select-it nil)))

(defun lctags-gtags-select-mode ()
  (let (candidate-list lctags-params)
    (while (not (eobp))
      (beginning-of-line)
      (when (not (looking-at "[^ \t]+[ \t]+\\([0-9]+\\)[ \t]\\([^ \t]+\\)[ \t]\\(.*\\)$"))
	(error "illegal format"))
      (let* ((line (string-to-number (gtags-match-string 1)))
	     (path (gtags-match-string 2))
	     (txt (gtags-match-string 3))
	     (relative-path (file-relative-name path default-directory)))
	(setq candidate-list
	      (cons (cons (format "%s:%d:%s"
				  (if (> 40 (length relative-path))
				      relative-path
				    (concat "..." (substring relative-path -37)))
				  line txt)
			  (list :line line :path path))
		    candidate-list)))
      (next-line))
    (setq lctags-params
	  `((name . ,(buffer-name (current-buffer)))
	    (candidates . ,candidate-list)
	    (action . lctags-gtags-select)))
    (if lctags-anything
	(let ((anything-candidate-number-limit 9999))
	  (anything :sources lctags-params
		    :buffer (concat "*lctags gtags*"
				    (substring (buffer-name (current-buffer)) 15))))
      (let ((helm-candidate-number-limit 9999))
	(helm :sources lctags-params
	      :buffer (concat "*lctags gtags*"
			      (substring (buffer-name (current-buffer)) 15)))))))

(defun lctags-gtags-resume ()
  (interactive)
  (if lctags-anything
      (anything-resume nil "*lctags gtags*")
    (helm-resume nil "*lctags gtags*")))



(provide 'lctags-helm)
