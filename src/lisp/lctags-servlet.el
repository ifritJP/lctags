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


;;(gethash 0 lctags-servlet-cookie-hash)

(defun lctags-servlet-set-cookie ()
  (interactive)
  (let ((src-buf (current-buffer)))
    (with-temp-buffer
      (lctags-execute-op2 src-buf (current-buffer) nil nil "prepare")))
  (let ((db-info (lctags-get-db (current-buffer))))
    (lctags-servlet-make-cookie (plist-get db-info :db)
				(plist-get db-info :target)
				(plist-get db-info :conf))))

(defun lctags-servlet-hash-key (db-path target conf-path)
  (format "%s$%s$%s" db-path target conf-path))

(defun lctags-servlet-make-cookie (db-path target conf-path)
  (when (not db-path)
    (let ((src-buf (current-buffer)))
      (with-temp-buffer
	(lctags-execute-op2 src-buf (current-buffer) nil nil "dump" "dbPath")
	(beginning-of-buffer)
	(when (re-search-forward "^\\(.+\\)$")
	  (setq db-path (match-string-no-properties 1)))
	)))
  (let ((cookie (hash-table-count lctags-servlet-cookie-hash))
	(key (lctags-servlet-hash-key db-path target conf-path))
	val)
    (when (not (gethash key lctags-servlet-cookie-hash-key))
      (setq val (list :db db-path :table target :conf conf-path :cookie cookie))
      (puthash cookie val lctags-servlet-cookie-hash)
      (puthash key val lctags-servlet-cookie-hash-key)
      )
    cookie))

(defun lctags-servlet-start (path query req)
  (let ((db (cadr (assoc "db" query)))
	(target (cadr (assoc "target" query)))
	(conf (cadr (assoc "conf" query)))
	(cookie (cadr (assoc "cookie" query))))
    (when (not cookie)
      (setq cookie (lctags-servlet-make-cookie db target conf)))
    (httpd-send-header t "text/plain" 302
		       :Set-Cookie (format "confId=%s; path=/lctags;" cookie)
		       :Location "contents/file-list.html"
		       )
    ))


(defun lctags-servlet-handle (path query req)
  (let* ((cookie (string-to-number
		  (cadr (split-string (or (cadr (assoc "Cookie" req)) "=-1") "="))))
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
			    (setq obj (json-add-to-object obj "cookie" cookie))
			    (setq list (cons obj list))
			    )
			  lctags-servlet-cookie-hash)
		 
		 (insert (json-encode (json-add-to-object (json-new-object)
							  "list" (vconcat list))))
		 ))
	      ((equal command "defBody")
	       (lctags-execute-op2 (current-buffer) (current-buffer) nil nil
				   "inq" command
				   (cadr (assoc "nsId" query))
				   "--lctags-form json" )
	       (let* ((result (cdr (lctags-json-get (current-buffer) 'defBody)))
		      (info (lctags-json-val (aref result 0) 'info))
		      (path (lctags-json-val info 'path))
		      (line (lctags-json-val info 'line))
		      (column (lctags-json-val info 'column)))
		 (when path
		   (find-file path)
		   (lctags-goto-line-column line column)
		   (recenter)
		   ))
	       )
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
	 result)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents content-path)
      (dolist (rep-info query)
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
