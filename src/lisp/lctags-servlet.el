(require 'lctags)

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
  (lctags-servlet path query req)
)

(defservlet lctags/gen text/json (path query req)
  (lctags-servlet-gen path query req)
)


(defvar lctags-servlet-cookie-hash (makehash))

;;(gethash 3 lctags-servlet-cookie-hash)

(defun lctags-servlet-start (path query req)
  (let ((db (cadr (assoc "db" query)))
	(target (cadr (assoc "target" query)))
	(conf (cadr (assoc "conf" query)))
	(key (hash-table-count lctags-servlet-cookie-hash)))
    (puthash key (list :db db :table target :conf conf) lctags-servlet-cookie-hash)
    (httpd-send-header t "text/plain" 302
		       :Set-Cookie (format "confId=%s; path=/lctags;" key)
		       :Location "contents/file-list.html"
		       )
    ))


(defun lctags-servlet (path query req)
  (let* ((key (string-to-number
	       (cadr (split-string (cadr (assoc "Cookie" req)) "="))))
	 (conf-info (gethash key lctags-servlet-cookie-hash))
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
        (httpd-send-header t (httpd-get-mime (file-name-extension content-path))
                           200
			   :Last-Modified
			   (httpd-date-string (nth 4 (file-attributes content-path)))))
    ))

(provide 'lctags-servlet)
