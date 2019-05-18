(defvar lctags-diag-ignore-pattern nil)
(defvar lctags-anything nil)
(defvar lctags-path-length 60)
(defvar lctags-after-save-check-diag nil)

(defvar lctags-decide-completion-func nil)


(defvar lctags-diag-buf-name "*lctags-diag*" )

(defvar lctags-candidate-info nil)
(defvar lctags-candidate-history nil)
(defvar lctags-diag-info nil)

(defvar lctags-default-expand-candidate-p nil
  "")

(defvar lctags-helm-expand-ignore-pattern nil
  "")


(when (not lctags-anything)
  (require 'helm))

(require 'cl)


(defun lctags-xml-get-child (xml symbol)
  (car (xml-get-children xml symbol)))
(defun lctags-xml-get-val (xml symbol)
  (car (xml-node-children (lctags-xml-get-child xml symbol))))

;; [C-return]
(defvar lctags-heml-map
  (let (map)
    (if lctags-anything
	(setq map (copy-keymap anything-map))
      (setq map (copy-keymap helm-map)))
    (define-key map (kbd "C-M-f") 'lctags-helm-type-forward)
    (define-key map (kbd "C-M-b") 'lctags-helm-type-backward)
    (define-key map (kbd "C-z") 'lctags-helm-execute-persistent-action)
    (define-key map [C-return] 'lctags-helm-select-all)
    (delq nil map))
  "Keymap")

(defun lctags-helm-execute-persistent-action (&optional attr onewindow)
  (interactive)
  (let ((lctags-candidate-select-mode :persistent))
    (helm-execute-persistent-action attr onewindow)
    ))
  

(defun lctags-execute-heml (src-buf lctags-buf input &rest lctags-opts)
  (if (eq (lctags-execute-op src-buf lctags-buf input nil lctags-opts) 0)
      (progn
	(setq lctags-candidate-info (lctags-xml-get lctags-buf 'completion))
	(if (lctags-xml-get-child lctags-candidate-info 'candidateList)
	    (setq lctags-diag-info nil)
	  (setq lctags-diag-info (lctags-xml-get-diag lctags-buf))))
    (setq lctags-candidate-info nil)
    (with-current-buffer lctags-buf
      (setq lctags-diag-info `((message nil ,(buffer-string))))))
  )

(defun lctags-expand-function-arg (func-name pos input lineno column)
  (if (string-match "(" func-name)
      (progn
	(goto-char pos)
	(search-forward "(")
	(backward-char))
    ;; メンバでないと simple に引数情報が含まれないので、
    ;; 別途関数の問い合わせをして引数情報を展開する。
    (let ((buffer (lctags-get-process-buffer t))
	  (filename (buffer-file-name (current-buffer)))
	  inq-info
	  args
	  )
      (lctags-execute (current-buffer) buffer
		      (or input (buffer-string)) "inq-at" filename
		      (number-to-string lineno) (number-to-string column)
		      "--lctags-log" "0" "-i")
      (with-current-buffer buffer
	(setq inq-info (lctags-xml-get buffer 'candidate)))
      (setq args (lctags-candidate-item-get-simple inq-info))
      (when (string-match (concat func-name "(") args)
	(setq pos (point))
	(insert (substring args (length (concat func-name))))
	(insert (concat " => " (lctags-candidate-item-get-result inq-info)))
	(goto-char pos)
	)
      )
    ))

(defun lctags-helm-select (item)
  (case lctags-candidate-select-mode
    (:persistent
     (lctags-candidate-item-open-file item))
    (:back)
    (nil)
    (t
     (let ((prefix (lctags-candidate-get-prefix))
	   (front (lctags-candidate-get-frontExpr))
	   (compMode (lctags-candidate-get-compMode))
	   (pos (point))
	   (hist-expr "")
	   (suffix "")
	   simple info items)
       (when (equal compMode "case")
	 (setq front "case " )
	 (setq suffix ":\nbreak;"))
       (if (eq lctags-candidate-select-mode :forward)
	   (setq info (nth 1 lctags-candidate-history))
	 (setq info (nth 0 lctags-candidate-history)))
       (when (> (length (helm-marked-candidates)) 1)
	 (setq item (car (helm-marked-candidates)))
	 (setq items (cdr (helm-marked-candidates))))
       (when (eq lctags-candidate-select-mode :all)
	 (setq lctags-candidate-select-mode nil)
	 (setq items (cdr (xml-get-children (plist-get info :candidate)
					    'candidates)))
	 (setq item (car items))
	 (setq items (cdr items))
	 )
       (setq simple
	     (concat
	      (lctags-candidate-item-get-expand (plist-get info :select))
	      (lctags-candidate-item-get-simple item)))
       (when lctags-decide-completion-func
	 (setq simple (funcall (symbol-function lctags-decide-completion-func)
			       simple front lctags-candidate-select-mode
			       lctags-candidate-history)))
       (insert (substring simple (length prefix)) suffix)
       (when (equal (lctags-candidate-item-get-kind item) "f")
	 ;; (not (string-match "\\.\\|->" front))
	 (when (string-match "(" simple)
	   (insert (concat " => " (lctags-candidate-item-get-result item))))
	 (lctags-expand-function-arg simple pos (buffer-string)
				     (lctags-get-line) (- (lctags-get-column) 2)))
       (when (>= (length items) 1)
	 (dolist (info lctags-candidate-history)
	   (setq hist-expr
		 (concat hist-expr
			 (lctags-candidate-item-get-simple (plist-get info :select))
			 (lctags-candidate-item-get-expand (plist-get info :select)))))
	 (dolist (marked items)
	   (insert (concat "\n" front hist-expr
			   (lctags-candidate-item-get-simple marked) suffix)))
	   )
       (indent-region pos (point))
       (when (equal compMode "case")
	 ;; case の場合 break の位置にカーソルを移動させる
	 (goto-char pos)
	 (next-line)
	 (end-of-line)
	 (search-backward "break;"))
       ))
     ))

(defun lctags-helm-select-swap (item)
  (let (simple )
    (setq simple (lctags-candidate-item-get-simple item))
    (lctags-remove-current-token)
    (insert simple ))
  )

(defun lctags-json-get (buf symbol)
  (with-current-buffer buf
    (let ((json-object-type 'plist)
	  (json-array-type 'list)
	  info)
      (setq info (json-read-from-string (buffer-string)))
      (plist-get (plist-get info :lctags_result) symbol))))

(defun lctags-json-val (json symbol)
  (plist-get json symbol))


(defun lctags-xml-get (buf symbol)
  (with-current-buffer buf
    (let (info)
      (setq info (xml-parse-region (point-min) (point-max)))
      (lctags-xml-get-child (assoc 'lctags_result info) symbol))))

(defun lctags-xml-get-list (item symbol)
  (delq nil (mapcar (lambda (X)
		      (when (listp X)
			(when (eq (car X) symbol)
			  X)))
		    item)))

(defun lctags-xml-get-list-root (item)
  (delq nil (mapcar (lambda (X)
		      (when (listp X)
			X)) item)))


(defun lctags-xml-get-diag (buf)
  (delq nil (mapcar (lambda (X)
		      (when (listp X)
			(when (eq (car X) 'message)
			  X)))
		    (lctags-xml-get buf 'diagnostics))))

(defun lctags-diag-get-message (diag)
  (nth 2 diag))

(defun lctags-candidate-item-get-simple (item)
  (lctags-xml-get-val item 'simple)
  )
(defun lctags-candidate-item-get-expand (item)
  (lctags-xml-get-val item 'expand)
  )

(defun lctags-candidate-item-get-type (item)
  (lctags-xml-get-val item 'type)
  )

(defun lctags-candidate-item-get-result (item)
  (lctags-xml-get-val item 'result)
  )

(defun lctags-candidate-item-get-hash (item)
  (lctags-xml-get-val item 'hash)
  )

(defun lctags-candidate-item-get-kind (item)
  (lctags-xml-get-val item 'kind)
  )

(defun lctags-candidate-item-get-val (item)
  (lctags-xml-get-val item 'val)
  )

(defun lctags-candidate-item-get-canonical (item)
  (lctags-xml-get-val item 'canonical)
  )

(defun lctags-candidate-item-open-file (item)
  (when (lctags-xml-get-val item 'file)
    (find-file (lctags-xml-get-val item 'file))
    (goto-line (string-to-number (lctags-xml-get-val (lctags-xml-get-child
						      item 'startPos) 'line)))))

(defun lctags-candidate-item-rename (item newname)
  (mapcar (lambda (X)
	    (if (and (listp X)
		     (or (eq (car X) 'canonical)
			 (eq (car X) 'simple)))
		(list (car X) nil newname)
	      X))
	  item))


(defun lctags-candidate-get-item-from-canonical (canonical)
  (car (delq nil (mapcar (lambda (X)
			   (when (listp X)
			     (when (equal (lctags-xml-get-val X 'canonical)
					  canonical)
			       X)))
			 lctags-candidate-info
			 ))))

(defun lctags-candidate-map-candidate (func info all-info)
  (lctags-candidate-map func (assoc 'candidateList info) all-info 'candidate))

(defun lctags-candidate-map (func info all-info symbol)
  (mapcar (lambda (X)
	    (when (and (listp X) (eq (car X) symbol) )
	      (funcall func X all-info)))
	  info
	  ))



(defun lctags-candidate-get-typeInfo (info hash)
  (delq nil (mapcar (lambda (X)
		      (when (and (listp X) (eq (car X) 'typeInfo) )
			(if (equal (lctags-xml-get-val X 'hash) hash)
			    X)))
		    (assoc 'typeInfoList info)
;;		    info
		    )))

(defun lctags-candidate-get-prefix (&optional candidate-info)
  (lctags-xml-get-val
   (if candidate-info candidate-info lctags-candidate-info) 'prefix))

(defun lctags-candidate-get-frontExpr (&optional candidate-info)
  (lctags-xml-get-val
   (if candidate-info candidate-info lctags-candidate-info) 'frontExpr))

(defun lctags-candidate-get-compMode (&optional candidate-info)
  (lctags-xml-get-val
   (if candidate-info candidate-info lctags-candidate-info) 'compMode))

(defface lctags-candidate-face
  '((t
     :foreground "gold"))
  "candidate face")
(defvar lctags-candidate-face 'lctags-candidate-face)

(defface lctags-candidate-path-face
  '((t
     :foreground "green"))
  "candidate path face")
(defvar lctags-candidate-path-face 'lctags-candidate-path-face)


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
    (setq expandable (lctags-xml-get-child (car type-info) 'candidateList))
    (setq expandable (lctags-xml-get-child expandable 'candidate))
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


(defun lctags-diag-get-path (diag)
  (if (lctags-diag-get-message diag)
      (car (split-string (lctags-diag-get-message diag) ":"))
    nil
  ))

(defun lctags-helm-display-diag (switch-flag)
  (setq lctags-diag-info
	(delq nil (mapcar (lambda (diag)
			    (if (not lctags-diag-ignore-pattern)
				diag
			      (if (string-match lctags-diag-ignore-pattern
						(lctags-diag-get-path diag))
				  nil
				diag)))
			  lctags-diag-info)))
  (if lctags-diag-info
      (let ((buf (lctags-get-buffer lctags-diag-buf-name t))
	    (cur-window (selected-window)))
	;;(lctags-switch-to-buffer-other-window buf)
	(pop-to-buffer buf)
	(dolist (diag lctags-diag-info)
	  (let* ((token-list (split-string (lctags-diag-get-message diag) ":" ))
		 (path (file-relative-name (car token-list) default-directory))
		 (message path))
	    (dolist (token (cdr token-list))
	      (setq message (format "%s:%s" message token)))
	    (insert message)
	    )
	  (insert "\n")
	  (compilation-mode)
	  (goto-char (point-min)))
	(when (not switch-flag)
	  (select-window cur-window))
	)
    (message "none diagnostics message")
    (when (get-buffer lctags-diag-buf-name)
      (kill-buffer lctags-diag-buf-name))))

(defun lctags-helm-completion-at (&optional toggle-expand)
  (interactive "P")
  (let ((buffer (lctags-get-process-buffer t))
	(filename (buffer-file-name (current-buffer)))
	(lineno (lctags-get-line))
	(column (- (lctags-get-column) 2))
	(expand-flag lctags-default-expand-candidate-p)
	candidates
	lctags-params
	)
    (lctags-execute-heml (current-buffer) buffer
			 (buffer-string) "comp-at" filename
			 (number-to-string lineno) (number-to-string column)
			 "--lctags-log" "0" "-i")
    (when toggle-expand
      (setq expand-flag (not expand-flag)))
    (if lctags-diag-info
	(lctags-helm-display-diag t)
      (with-current-buffer buffer
	(setq candidates
	      (delq nil (lctags-candidate-map-candidate
			 (symbol-function 'lctags-helm-make-candidates)
			 (if expand-flag
			     (lctags-helm-toggle-expand-candidate lctags-candidate-info)
			   lctags-candidate-info)
			 ;;lctags-candidate-info
			 lctags-candidate-info)))
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
	(lctags-helm-display-diag t)
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
	(lctags-helm-display-diag t)
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
				      (let* ((val (lctags-candidate-item-get-val X))
					     (hex_val (calc-eval `(,val calc-number-radix 16))))
					(format " => %s(0x%s)"
						val (substring hex_val 3)))
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
  (defalias 'helm 'anything)
  (defalias 'helm-get-selection 'anything-get-selection)
  (defalias 'helm-exit-minibuffer 'anything-exit-minibuffer)
  (defalias 'helm-marked-candidates 'anything-marked-candidates)
  (defalias 'helm-execute-persistent-action 'anything-execute-persistent-action)
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

(defun lctags-after-save-hook ()
  (when (and (local-variable-p 'lctags-mode (current-buffer))
	     lctags-after-save-check-diag)
    (lctags-display-diag t)))

(defun lctags-display-diag (&optional no-switch-flag)
  (interactive)
  (let ((buffer (lctags-get-process-buffer t)))
    (lctags-execute-heml (current-buffer) buffer
			 (buffer-string) "diag" (buffer-file-name)
			 "--lctags-log" "0" "-i")
    (lctags-helm-display-diag (not no-switch-flag))
    )
  )

(defun lctags-gtags-select (item)
  (with-temp-buffer
    (insert (format "sym %d %s "
		    (plist-get item :line)
		    (plist-get item :path)))
    (beginning-of-line)
    (gtags-select-it nil)
    (recenter)
    ))

(defun lctags-conv-disp-path (path omit &optional base-dir)
  (when (string-match "^!" path)
    (setq path (substring path 1)))
  (let* ((relative-path (file-relative-name path
					    (or base-dir default-directory)))
	 (abs-path (expand-file-name path))
	 (disp-path relative-path))
    (when (< (length abs-path) (length relative-path))
      (setq disp-path abs-path))
    (if (or (not omit)
	    (not lctags-path-length)
	    (> lctags-path-length (length disp-path)))
	disp-path
      (concat "..."
	      (substring disp-path
			 (+ (* -1 lctags-path-length) 3))))))

(defun lctags-gtags-create-candidate-list (&optional output-symbol-flag)
  (let (projDir candidate-list)
    (setq projDir (lctags-get-projDir (current-buffer)))
    (while (not (eobp))
      (beginning-of-line)
      (when (not (looking-at "^\\([^ \t]+\\)[ \t]+\\([0-9]+\\)[ \t]\\([^ \t]+\\)[ \t]\\(.*\\)$"))
	(error "illegal format"))
      (let* ((symbol (gtags-match-string 1))
	     (line (string-to-number (gtags-match-string 2)))
	     (path (gtags-match-string 3))
	     (txt (gtags-match-string 4)))
	(setq candidate-list
	      (cons (cons (concat
			   (when output-symbol-flag
			     (concat (propertize symbol 'face lctags-candidate-face)
				     ":\t"))
			   (format "%s:%d:%s"
				   (propertize
				    (lctags-conv-disp-path path t projDir)
				    'face lctags-candidate-path-face)
				    line txt))
			  (list :line line :path path))
		    candidate-list)))
      (next-line))
    candidate-list
    ))

(defun lctags-gtags-select-mode (select-func create-candidate-list-func
					     &optional header-name)
  (let (candidate-list lctags-params)
    (setq candidate-list (funcall create-candidate-list-func))
    (if (eq (length candidate-list) 1)
	(funcall select-func (cdr (car candidate-list)))
      (setq candidate-list
	    (sort candidate-list
		  (lambda (X Y)
		    (let* ((infoX (cdr X))
			   (infoY (cdr Y))
			   (pathX (plist-get infoX :path))
			   (pathY (plist-get infoY :path)))
		      (if (string< pathX pathY)
			  t
			(if (string= pathX pathY)
			    (if (< (plist-get infoX :line) (plist-get infoY :line))
				t
			      nil)))))))
      (when (not header-name)
	(setq header-name (buffer-name (current-buffer))))
      (setq lctags-params
	    `((name . ,header-name)
	      (candidates . ,candidate-list)
	      (action . ,select-func)))
      (if lctags-anything
	  (let ((anything-candidate-number-limit 9999))
	    (anything :sources lctags-params
		      :buffer (concat "*lctags gtags*"
				      (substring (buffer-name (current-buffer)) 15))))
	(let ((helm-candidate-number-limit 9999))
	  (helm :sources lctags-params
		:buffer (concat "*lctags gtags*"
				(substring (buffer-name (current-buffer)) 15))))))))

(defun lctags-gtags-resume ()
  (interactive)
  (if lctags-anything
      (anything-resume nil "*lctags gtags*")
    (helm-resume "*lctags gtags*")))


(defun lctags-helm-select-from-list (message list)
  (let (item)
    (helm :sources `((name . ,message)
		     (candidates . ,list)
		     (action . (lambda (X)
				 (setq item X)))))
    item))


(defun lctags-helm-toggle-expand-candidate (candidate-info)
  (interactive)
  (lctags-helm-toggle-expand-candidate-sub "" nil candidate-info
					   candidate-info nil)
  )

(defun lctags-helm-toggle-expand-candidate-sub (prefix parent candidate-info
						       all-info checked-hash-list)
  (let (expand-list)
    (when (not (and prefix
		    lctags-helm-expand-ignore-pattern
		    (string-match lctags-helm-expand-ignore-pattern prefix)))
      (setq expand-list
	    (lctags-candidate-map-candidate
	     (lambda (item XX)
	       (when item
		 (let ((hash (lctags-candidate-item-get-hash item))
		       expr typeInfo)
		   (setq expr (concat prefix
				      (lctags-candidate-item-get-expand parent)
				      (lctags-candidate-item-get-simple item)))
		   (if (or (not hash)
			   (not (member hash checked-hash-list)))
		       (progn
			 (setq typeInfo (car (lctags-candidate-get-typeInfo all-info hash)))
			 (setq checked-hash-list (cons hash checked-hash-list))
			 (lctags-helm-toggle-expand-candidate-sub
			  expr item typeInfo all-info checked-hash-list))
		     (list (lctags-candidate-item-rename item expr))
		     ))))
	     candidate-info all-info)))
    (setq expand-list (delq nil expand-list))
    ;; (if expand-list
    ;; 	  (apply 'append expand-list)
    ;; 	(list (lctags-candidate-item-rename parent prefix)))
    (apply 'append (list (lctags-candidate-item-rename parent prefix))
	   expand-list)
    ))
;; (lctags-helm-toggle-expand-candidate lctags-candidate-info)

(provide 'lctags-helm)
