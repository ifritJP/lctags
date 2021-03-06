(defvar lctags-dispatch-prev-window-conf nil)

(defvar lctags-dispatch-buf-name "lctags dispatcher")

(defvar lctags-dispatch-menu-info
  '((:name "list" :bind "l"
	   :submenu
	   ((:name "def-at" :bind "d" :action lctags-def-at)
	    (:name "def-search" :bind "s" :action lctags-def-pickup-symbol)
	    (:name "ref-at" :bind "r" :action lctags-ref-at)
	    (:name "call-at" :bind "c" :action lctags-call-at)
	    (:name "callee-at" :bind "C" :action lctags-callee-at)
	    (:name "inc" :bind "i" :action lctags-list-inc-this-file)
	    (:name "incSrc" :bind "I" :action lctags-list-incSrc-this-file)
	    (:name "file" :bind "f" :action lctags-find-file)
	    ))
    (:name "graph" :bind "g"
	   :submenu
	   ((:name "setCookie" :bind "c" :action lctags-servlet-set-cookie)
	    (:name "setCookie-at" :bind "a" :action lctags-servlet-set-cookie-of-symbol-at)
	    (:name "caller" :bind "r" :action lctags-graph-caller-at)
	    (:name "callee" :bind "e" :action lctags-graph-callee-at)
	    (:name "symbol" :bind "s" :action lctags-graph-symbol-at)
	    (:name "inc" :bind "i" :action lctags-graph-inc)
	    (:name "incSrc" :bind "I" :action lctags-graph-incSrc)
	    ))
    (:name "generate" :bind "G"
	   :submenu
	   ((:name "dump-member-at" :bind "m"
		   :action lctags-generate-to-dump-member-at)
	    (:name "convert-enumName-at" :bind "e"
		   :action lctags-generate-to-convert-enumName-at)
	    (:name "form-enum" :bind "E"
		   :action lctags-expand-enum-format-at)
	   ))
    (:name "insert" :bind "i"
	   :submenu
	   ((:name "insert-to-call-function" :bind "c"
		   :action lctags-insert-call-func)
	   ))
    (:name "highlight" :bind "h"
	   :submenu
	   ((:name "highlight-at" :bind "h" :action lctags-highlight-at)
	    (:name "rescan" :bind "r" :action lctags-highlight-rescan)
	    (:name "grep" :bind "g" :action lctags-highlight-grep-at)
	    (:name "clear" :bind "c" :action lctags-highlight-clear)
	   ))
    (:name "refactoring" :bind "r"
	   :submenu
	   ((:name "sub-routine" :bind "s"
		   :action lctags-split-at)
	    (:name "rename" :bind "r"
		   :action lctags-rename-at)
	   ))
    (:name "misc" :bind "m" :submenu
	   ((:name "expand-macro" :bind "e" :action lctags-expand-macro)
	    (:name "grep-cursor" :bind "g" :action lctags-grep-cursor)))
    (:name "update" :bind "u" :submenu
	   ((:name "this file" :bind "u" :action lctags-update-this-file)
	    (:name "this directory" :bind "d" :action lctags-update-this-directory)
	    (:name "all" :bind "A" :action lctags-update-all)))))


(defvar lctags-dispatch-menu-info-current nil)


(defun lctags-dispatch-menu-get-name (info)
  (plist-get info :name))

(defun lctags-dispatch-menu-get-bind (info)
  (plist-get info :bind))

(defun lctags-dispatch-menu-get-submenu (info)
  (plist-get info :submenu))

(defun lctags-dispatch-menu-get-action (info)
  (plist-get info :action))

(defun lctags-dispatch-menu-get-keymap (info)
  (plist-get info :keymap))

(defun lctags-dispatch-menu-set-keymap (info keymap)
  (plist-put info :keymap keymap))

(defun lctags-dispatch-mode-exit (&optional action param)
  (set-window-configuration lctags-dispatch-prev-window-conf)
  (kill-buffer (get-buffer lctags-dispatch-buf-name))
  (when action
    (if param
	(funcall action param)
      (funcall action)))
  )

(defun lctags-dispatch-build-keymap (menu-info)
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-g")
      (lambda ()
	(interactive)
	(lctags-dispatch-mode-exit nil)
	))
    (mapcar
     (lambda (menu)
       (define-key map (eval `(kbd ,(lctags-dispatch-menu-get-bind menu)))
	 (if (lctags-dispatch-menu-get-submenu menu)
	     (eval `(lambda ()
		      (interactive)
		      (lctags-dispatch-redraw ',(lctags-dispatch-menu-get-submenu menu))))
	   (eval `(lambda (param)
		    (interactive "P")
		    (lctags-dispatch-mode-exit
		     ',(lctags-dispatch-menu-get-action menu) param))))))
       menu-info)
    map
    ))


(defun lctags-dispatch-redraw (menu-info)
  (font-lock-mode nil)
  (let ((buffer-read-only nil)
        (old-point (point))
        (is-first (zerop (buffer-size)))
        (actions-p nil))
    (erase-buffer)
    (use-local-map (lctags-dispatch-build-keymap menu-info))

    (lctags-dispatch-draw-menu lctags-dispatch-menu-info menu-info)
    
    (delete-trailing-whitespace)
    (setq mode-name "lctags-dispatcher" major-mode 'lctags-dispatcher)
    )
  (setq buffer-read-only t)
  (fit-window-to-buffer)
  (beginning-of-buffer))


(defun lctags-dispatch-mode ()
  ""
  (interactive)
  (setq lctags-dispatch-prev-window-conf
        (current-window-configuration))
  (let ((buf (get-buffer-create lctags-dispatch-buf-name)))
    (delete-other-windows)
    (split-window-vertically)
    (other-window 1)
    (switch-to-buffer buf)
    (setq lctags-dispatch-menu-info-current lctags-dispatch-menu-info)
    (lctags-dispatch-redraw lctags-dispatch-menu-info))
  )

(defface lctags-dispatch-valid-bind-face
  '((t
     :foreground "red"))
  "valid bind face")
(defvar lctags-dispatch-valid-bind-face 'lctags-dispatch-valid-bind-face)

(defface lctags-dispatch-invalid-bind-face
  '((t
     :foreground "gray30"))
  "valid bind face")
(defvar lctags-dispatch-invalid-bind-face 'lctags-dispatch-invalid-bind-face)


(defun lctags-dispatch-draw-menu (menu-info current-info &optional prefix )
  (mapcar (lambda (menu)
	    (when (> (+ (current-column)
			(length prefix)
			(length (lctags-dispatch-menu-get-name menu)) 5)
		     (window-width))
	      (insert "\n"))
	    (insert (format "%s%s: %s   "
			    (or prefix "")
			    (if (eq menu-info current-info)
				(propertize 
				 (lctags-dispatch-menu-get-bind menu)
				 'face 'lctags-dispatch-valid-bind-face)
			      (propertize 
			       (lctags-dispatch-menu-get-bind menu)
			       'face 'lctags-dispatch-invalid-bind-face))
			    (lctags-dispatch-menu-get-name menu)
			    ))
	    (when (lctags-dispatch-menu-get-submenu menu)
	      (insert "\n")
	      (lctags-dispatch-draw-menu
	       (lctags-dispatch-menu-get-submenu menu)
	       current-info
	       (concat prefix "    "))
	      (insert "\n")
	      ))
	  menu-info))


(provide 'lctags-dispatch)
