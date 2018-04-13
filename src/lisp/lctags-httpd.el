;; (httpd-start)


(defservlet lctags/symbolDefine text/plain (path query req)
  
  (insert "hello, " (format "%s %s" path query)))

(provide 'lctags-httpd)
