(require 'lctags-helm)

(defun lctags-function-item-get-name (item)
  (lctags-xml-get-val item 'name)
  )

(defun lctags-function-item-get-include (item)
  (lctags-xml-get-val item 'include)
  )

(defun lctags-function-item-get-declaration (item)
  (lctags-xml-get-val item 'declaration)
  )




(defun lctags-function-make-candidates (info candidates-info)
  (cons (format "%-30s\t%s" (concat (lctags-function-item-get-name info) ":")
		(lctags-conv-disp-path 
		 (lctags-function-item-get-declaration info) t))
	info))

(defun lctags-execute-insert-func (src-buf lctags-buf input &rest lctags-opts)
  (if (eq (lctags-execute-op src-buf lctags-buf input nil lctags-opts) 0)
      (progn
	(setq lctags-insert-func-info (lctags-xml-get lctags-buf 'functionList))
	(if (lctags-xml-get-child lctags-insert-func-info 'function)
	    (setq lctags-diag-info nil)
	  (setq lctags-diag-info (lctags-xml-get-diag lctags-buf))))
    (setq lctags-insert-func-info nil)
    (with-current-buffer lctags-buf
      (setq lctags-diag-info `((message nil ,(buffer-string))))))
  )

(defun lctags-insert-inc-decide (item)
  (let ((path (plist-get item :path)))
    (when (string-match "^!" path)
      ;; ソート用の ! を除外する
      (setq path (substring path 1)))
    (setq lctags-insert-inc path)
    ))

(defun lctags-insert-select (item)
  (let ((filename (buffer-file-name (current-buffer)))
	(name (lctags-function-item-get-name item))
	(include (lctags-function-item-get-include item))
	(buffer-txt (buffer-string))
	lineno column txt pos bak-pos mark lctags-insert-inc)
    ;; 関数の情報を取得するために、関数を参照するソースを作成
    (with-temp-buffer
      (when include
	(insert (format "#include <%s>\n" include)))
      (insert buffer-txt)
      (insert "\nstatic void ___tmpfunc___( void ) { \n" )
      (setq pos (point))
      (setq lineno (lctags-get-line))
      (setq column 1)
      (insert (format "%s; }" name ))
      (setq txt (buffer-string)))
    (setq bak-pos (point))
    ;; 関数の展開
    (insert name)
    (lctags-expand-function-arg name pos txt lineno column)
    (setq mark (point-marker))
    (end-of-line)
    ;; 必要な include の挿入
    (when include
      (lctags-execute-heml (current-buffer) (lctags-get-process-buffer t) nil
			   "list" "incSrcHeader" include)
      (with-current-buffer (lctags-get-process-buffer nil)
	(goto-char 1)
	;; ソートで先頭になるように パスに ! を付加する
	(insert (format "%-16s %4d !%-16s \n" "path" 1 include))
	;;(end-of-buffer)
	(beginning-of-buffer)
	)
      (lctags-select-gtags (lctags-get-process-buffer nil) "select include file"
			   'lctags-gtags-select-mode 'lctags-insert-inc-decide)
      
      (insert (format "\n// #include <%s>"
		      (lctags-conv-disp-path lctags-insert-inc nil)))
      (end-of-line))
    (indent-region bak-pos (point))
    (goto-char mark)
    ))


(defun lctags-insert-call-func ()
  (interactive)
  (let ((pattern (read-string "funcname-pattern: "))
	(buffer (lctags-get-process-buffer t))
	candidates)
    (lctags-execute-insert-func (current-buffer) buffer
				(buffer-string) "call-func"
				(buffer-file-name) pattern "-i")
    (if lctags-diag-info
	(lctags-helm-display-diag)
      (with-current-buffer buffer
	(setq candidates
	      (delq nil (lctags-candidate-map
			 (symbol-function 'lctags-function-make-candidates)
			 lctags-insert-func-info lctags-insert-func-info
			 'function )))
	)
      (setq lctags-params
	    `((name . "insert function")
	      (candidates . ,candidates)
	      (action . lctags-insert-select)))
      (lctags-helm-wrap lctags-params lctags-heml-map nil)
      )))



(provide 'lctags-insert-func)
