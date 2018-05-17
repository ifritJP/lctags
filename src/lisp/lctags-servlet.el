(require 'lctags)
(require 'simple-httpd)

;; (httpd-start)

;; http://xxx.xxx.xxx.xxx:8080/lctags/start?db=path&target=target&conf=conf
;; http://xxx.xxx.xxx.xxx:8080/lctags/inq
;; ((file-name-directory (find-library-name "lctags"))
;; (locate-library "lctags")

(defvar lctags-servlet-content-dir nil)

;; lctags.el の html 以下を /lctags/contents にマッピング
(let ((dir (file-name-directory (locate-library "lctags")))
      content)
  (setq lctags-servlet-content-dir (expand-file-name "html" dir))
  (httpd-def-file-servlet lctags/contents lctags-servlet-content-dir))

(defservlet lctags text/json (path query req)
  (lctags-servlet-gen "/lctags/gen/index.html" query req)
  )


(defservlet lctags/start text/json (path query req)
  (lctags-servlet-start path query req)
  )

(defservlet lctags/inq text/json (path query req)
  (lctags-servlet-handle path query req)
  )

(defservlet lctags/gen text/json (path query req)
  (lctags-servlet-gen path query req)
  )


;; (clrhash lctags-servlet-cookie-hash)
;; (clrhash lctags-servlet-cookie-hash-key)
(defvar lctags-servlet-cookie-hash (make-hash-table)
  "cookie -> val map")
(defvar lctags-servlet-cookie-hash-key (make-hash-table :test 'equal)
  "key -> val map")
(defvar lctags-servlet-last-cookie nil)


;;(gethash 0 lctags-servlet-cookie-hash)

(defun lctags-servlet-set-cookie-of-symbol-at ()
  (interactive)
  (let ((src-buf (current-buffer))
	tmp-buf result symbol)
    (with-temp-buffer
      (setq tmp-buf (current-buffer))
      (with-current-buffer src-buf
	(lctags-execute-op (current-buffer) tmp-buf (buffer-string) nil 
			   (list "cursor-at" (buffer-file-name)
				 (number-to-string (lctags-get-line))
				 (number-to-string (lctags-get-column)) "-i")))
      (setq result (lctags-xml-get (current-buffer) 'cursor)))
    (setq symbol (list (lctags-xml-get-val result 'fullName)
		       (string-to-number (lctags-xml-get-val result 'nsId))))
    (when symbol
      (lctags-servlet-set-cookie symbol))
  ))

(defun lctags-servlet-set-cookie (&optional targetSymbol)
  (interactive)
  (let ((db-info (lctags-get-db (current-buffer))))
    (setq lctags-servlet-last-cookie
	  (lctags-servlet-make-cookie (plist-get db-info :db)
				      (plist-get db-info :target)
				      (plist-get db-info :conf)
				      targetSymbol))))

(defun lctags-servlet-hash-key (db-path target conf-path)
  (format "%s$%s$%s" db-path target conf-path))

(defun lctags-servlet-make-cookie (db-path target conf-path &optional targetSymbol)
  (when (not db-path)
    (let ((src-buf (current-buffer)))
      (with-temp-buffer
	(lctags-execute-op2 src-buf (current-buffer) nil nil "dump" "dbPath")
	(beginning-of-buffer)
	(when (re-search-forward "^\\(.+\\)$")
	  (setq db-path (match-string-no-properties 1)))
	)))
  (let ((key (lctags-servlet-hash-key db-path target conf-path))
	cookie val)
    (setq val (gethash key lctags-servlet-cookie-hash-key))
    (if val
	(progn
	  (plist-put val :targetSymbol targetSymbol)
	  (setq cookie (plist-get val :cookie)))
      (setq cookie (hash-table-count lctags-servlet-cookie-hash))
      (setq val (list :db db-path :table target :conf conf-path :cookie cookie
		      :targetSymbol targetSymbol))
      (puthash cookie val lctags-servlet-cookie-hash)
      (puthash key val lctags-servlet-cookie-hash-key)

      ;; lctags の準備
      (with-temp-buffer
	(let ((lctags-db db-path)
	      (lctags-target target)
	      (lctags-conf conf-path))
	  (lctags-execute-op2 (current-buffer) (current-buffer) nil nil
			      "inq" "projDir" "--lctags-form" "json")
	  (plist-put val :projDir 
		     (plist-get (car (lctags-json-get (current-buffer) :projDir)) :path))
	  (lctags-execute-op2 (current-buffer) (current-buffer) nil nil "prepare"))))
    cookie))

(defun lctags-servlet-start (path query req)
  (let ((db (cadr (assoc "db" query)))
	(target (cadr (assoc "target" query)))
	(conf (cadr (assoc "conf" query)))
	(cookie (cadr (assoc "cookie" query)))
	(jumpCallGraph (cadr (assoc "jumpCallGraph" query)))
	(jumpLastSymbol (cadr (assoc "jumpLastSymbol" query)))
	location conf-info symbol)
    (when cookie
      (setq cookie (string-to-number cookie)))
    (when jumpLastSymbol
      (setq jumpCallGraph t)
      (setq cookie lctags-servlet-last-cookie))
    (if (not cookie)
	(setq cookie (lctags-servlet-make-cookie db target conf))
      (setq conf-info (gethash cookie lctags-servlet-cookie-hash))
      (setq symbol (plist-get conf-info :targetSymbol)))
    (if (and jumpCallGraph symbol)
	(setq location (format "/lctags/gen/func-call-graph.html?confId=%s&nsId=%d&name=%s"
			       cookie (cadr symbol) (car symbol)))
      (setq location (format "/lctags/gen/file-list.html?confId=%s" cookie)))
    (httpd-send-header t "text/plain" 302
		       ;;:Set-Cookie (format "confId=%s; path=/lctags;" cookie)
		       :Location location)
    ))

(defun lctags-servlet-display-at ()
  (interactive)
  (lctags-servlet-set-cookie)
  
  
  )

(defun lctags-servlet-open-pos ( sym &rest arg )
  (apply 'lctags-execute-op2 (current-buffer) (current-buffer) nil nil
		      "--lctags-form json" arg)
  (let* ((result (lctags-json-get (current-buffer)
				  (intern (format ":%s" sym))))
	 (info (lctags-json-val (car result) :info))
	 (path (lctags-json-val info :path))
	 (line (lctags-json-val info :line))
	 (column (lctags-json-val info :column)))
    (when path
      (find-file path)
      (lctags-goto-line-column line column)
      (recenter)
      ))
  )

(defun lctags-servlet-handle (path query req)
  (let* ((cookie (string-to-number
		  (or (cadr (assoc "confId" query)) "-1")))
	 (conf-info (gethash cookie lctags-servlet-cookie-hash))
	 (command (cadr (assoc "command" query)))
	 result)
    (with-temp-buffer
      (let ((lctags-db (plist-get conf-info :db))
	    (lctags-target (plist-get conf-info :target))
	    (lctags-conf (plist-get conf-info :conf)))
	(cond ((equal command "dumpDir")
	       (lctags-execute-op2 (current-buffer) (current-buffer) nil nil
				   "inq" command "--lctags-form json"))
	      ((equal command "matchFile")
	       (lctags-execute-op2 (current-buffer) (current-buffer) nil nil
				   "inq" command
				   (cadr (assoc "pattern" query))
				   "--lctags-form json" ))
	      ((equal command "defAtFileId")
	       (lctags-execute-op2 (current-buffer) (current-buffer) nil nil
				   "inq" command
				   (cadr (assoc "fileId" query))
				   "--lctags-form json" ))
	      ((equal command "callee")
	       (lctags-execute-op2 (current-buffer) (current-buffer) nil nil
				   "inq" command
				   (cadr (assoc "nsId" query))
				   "--lctags-form json" ))
	      ((equal command "caller")
	       (lctags-execute-op2 (current-buffer) (current-buffer) nil nil
				   "inq" command
				   (cadr (assoc "nsId" query))
				   "--lctags-form json" ))
	      ((equal command "refSym")
	       (lctags-execute-op2 (current-buffer) (current-buffer) nil nil
				   "inq" command
				   (cadr (assoc "nsId" query))
				   "--lctags-form json" ))
	      ((equal command "defBody")
	       (lctags-servlet-open-pos 'defBody
					"inq" command
					(cadr (assoc "nsId" query))))
	      ((equal command "callPair")
	       (lctags-servlet-open-pos 'callPair
					"inq" command
					(cadr (assoc "nsId" query))
					(cadr (assoc "belongNsId" query))))
	      ((equal command "refPair")
	       (lctags-servlet-open-pos 'refPair
					"inq" command
					(cadr (assoc "nsId" query))
					(cadr (assoc "belongNsId" query))))
	      ((equal command "cookies")
	       (let* (obj list)
		 (maphash (lambda (cookie val)
			    (setq obj (json-new-object))
			    (setq obj (json-add-to-object obj "db"
							  (plist-get val :db)))
			    (setq obj (json-add-to-object obj "target"
							  (plist-get val :target)))
			    (setq obj (json-add-to-object obj "conf"
							  (plist-get val :conf)))
			    (setq obj (json-add-to-object obj "targetSymbol"
							  (car (plist-get val :targetSymbol))))
			    (setq obj (json-add-to-object obj "targetNsId"
							  (cadr (plist-get val :targetSymbol))))
			    (setq obj (json-add-to-object obj "cookie" cookie))
			    (setq list (cons obj list))
			    )
			  lctags-servlet-cookie-hash)
		 
		 (insert (json-encode (json-add-to-object (json-new-object)
							  "list" (vconcat list))))
		 ))
	      (t
	       (httpd-error t 500)
	       ))
	(setq result (buffer-string))))
    (insert result)
    (httpd-send-header t "text/json" 200)
    ))


(defun lctags-servlet-gen (path query req)
  (let* ((command (cadr (assoc "command" query)))
	 (content-path (expand-file-name (substring path (length "/lctags/gen/"))
					 lctags-servlet-content-dir))
	 (cookie (string-to-number
		  (or (cadr (assoc "confId" query)) "-1")))
	 (conf-info (gethash cookie lctags-servlet-cookie-hash))
	 (proj-dir (plist-get conf-info :projDir))
	 result)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents content-path)
      (dolist (rep-info (cons (list 'projDir proj-dir)
				(copy-sequence query)))
	(beginning-of-buffer)
	(replace-string (format "$%s$" (car rep-info))
			(cadr rep-info) nil (point-min) (point-max)))
      (setq result (buffer-string)))
    (insert result)
    (httpd-send-header t (httpd-get-mime (file-name-extension content-path))
		       200
		       :Last-Modified
		       (httpd-date-string (nth 4 (file-attributes content-path)))))
  )

(provide 'lctags-servlet)
