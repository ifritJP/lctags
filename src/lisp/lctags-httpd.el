;; (httpd-start)

;; http://xxx.xxx.xxx.xxx:8080/lctags/symbolDefine?db=path&target=target&conf=conf

(defservlet lctags/symbolDefine text/plain (path query req)
  (lctags-servlet path query req)
)

(defun lctags-servlet (path query req)
  (let (result)
    (with-temp-buffer
      (let ((lctags-db (cadr (assoc "db" query)))
	    (lctags-target (cadr (assoc "target" query)))
	    (lctags-conf (cadr (assoc "conf" query))))
	(lctags-execute-op2 (current-buffer) (current-buffer) nil nil
			    "inq" "dumpDir" "--lctags-form json")
	(setq result (buffer-string))))
    (insert result)))

(provide 'lctags-httpd)
