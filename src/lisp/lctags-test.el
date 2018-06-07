(require 'lctags-const)

(defun lctags-servlet-api-info-2-python ()
  "lctags-servlet-api-info-table から httpy.py の apiInfoMap を生成する。"
  ;;(lctags-switch-to-buffer-other-window (generate-new-buffer "*lctags-python*"))
  (with-temp-buffer
    (insert "apiInfoMap = {\n")
    (dolist (item lctags-servlet-api-info-table)
      (let ((first t)
	    (param-list (plist-get (cadr item) :param)))
	(if (symbolp (car param-list))
	    nil
	  (insert (format "\"%s\": { " (car item)))
	  (insert "\"param\": [")
	  (dolist (param param-list)
	    (if first
		(setq first nil)
	      (insert ", "))
	    (cond ((stringp param)
		   (insert (format "\"%s\"" param)))
		  ((listp param)
		   (insert (format "[\"%s\", (lambda val: val.replace( \"_\", \"$_\" ))"
				   (car param)))
		   (insert "]"))
		  ))
	  (insert "] },\n")))
	)
    (insert "}\n")
    (buffer-string)
    ))

;; (lctags-servlet-api-info-2-python)

(with-temp-buffer
  (insert (lctags-servlet-api-info-2-python))
  (write-region (point-min) (point-max) "servlet-api-info"))

(provide 'lctags-test)
