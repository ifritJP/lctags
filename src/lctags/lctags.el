;;-*- coding:utf-8; mode:emacs-lisp-*-
;;
;; Copyright (C) 2017 ifritJP
;;
;;
;; 
;; (add-hook 'lctags-mode-hook
;;       '(lambda ()
;;          (local-set-key "\M-t" 'lctags-def)
;;          (local-set-key "\M-r" 'lctags-ref)
;;          (local-set-key "\C-cld" 'lctags-def-at)
;;          (local-set-key "\C-clr" 'lctags-ref-at)
;;          (local-set-key "\C-clc" 'lctags-ref-at)
;;          (local-set-key "\C-clli" 'lctags-list-inc-this-file)
;;          (local-set-key "\C-cllI" 'lctags-list-incSrc-this-file)
;;          (local-set-key "\C-clgr" 'lctags-graph-caller-at)
;;          (local-set-key "\C-clge" 'lctags-graph-callee-at)
;;          (local-set-key "\C-clgs" 'lctags-graph-symbol-at)
;;          (local-set-key "\C-clu" 'lctags-update-this-file)
;;          (local-set-key "\C-t" 'gtags-pop-stack)))


(require 'gtags)

(defvar lctags-process-buf-name "*lctags-process*"
  "")

(defvar lctags-command
  (expand-file-name "lctags")
  "lctags command")

(defvar lctags-mode-map (make-sparse-keymap)
  "lctags-mode Keymap.")

(defvar lctags-db nil
  "lcatags.sqlite's path. nil is default.
This parameter can set function and string.
")

(defvar lctags-target nil
  "lcatags.sqlite's target. nil is default.
This parameter can set function and string.
")

(defvar lctags-conf nil
  "lcatags.sqlite's config. nil is default.
This parameter can set function and string.
")


(setq lctags-target-buf nil)


(define-minor-mode lctags-mode
  "LibClang Tag System mode.
"
  ;; 初期値
  nil
  ;; mode-line 文字列
  " lctags"
  ;; マイナモード用キーマップの初期値
  lctags-mode-map
  ;; body
  (run-hooks 'lctags-mode-hook)
  )

(defun execute-lctags (src-buf lctags-buf &rest lctags-opts)
  (let ((db-path lctags-db)
	(target lctags-target)
	(config lctags-conf)
	command dir)
    (with-current-buffer src-buf
      (when (and lctags-db (functionp lctags-db))
	(setq db-path (funcall lctags-db)))
      (when (and lctags-target (functionp lctags-target))
	(setq target (funcall lctags-target)))
      (when (and lctags-conf (functionp lctags-conf))
	(setq config (funcall lctags-conf)))
      (setq dir default-directory))
    
    (with-current-buffer lctags-buf
      (setq default-directory dir)
      (setq command
	    (delq nil
		  (append lctags-opts
			  (list "--lctags-quiet")
			  (when db-path
			    (list "--lctags-db" db-path))
			  (when target
			    (list "--lctags-target" target))
			  (when config
			    (list "--lctags-conf" config))
			  )))
      (setq command
	    (append (list lctags-command nil lctags-buf t )
		    command))
	    
      (apply 'call-process command)
      (goto-char 1))))



(defun lctags-pos-at ( mode &optional tag &rest lctags-opt-list)
  (let ((save (current-buffer))
	(line (1- (current-line)))
	(column (+ (- (point) (point-at-bol)) 1))
	buffer lineNum select-name lctags-opt lctags-opt2 opt-list)
    (cond
     ((equal mode "def-at")
      (setq lctags-opt "def-ata")
      (setq tag buffer-file-name)
      (setq select-name
	    (format "(D)%s:%d:%d"
		    (file-name-nondirectory buffer-file-name) line column)))
     ((equal mode "ref-at")
      (setq lctags-opt "ref-ata")
      (setq tag buffer-file-name)
      (setq select-name
	    (format "(R)%s:%d:%d"
		    (file-name-nondirectory buffer-file-name) line column)))
     ((equal mode "call-at")
      (setq lctags-opt "call-ata")
      (setq tag buffer-file-name)
      (setq select-name
	    (format "(C)%s:%d:%d"
		    (file-name-nondirectory buffer-file-name) line column)))
     ((equal mode "def")
      (setq lctags-opt "-xta")
      (setq select-name (format "(D)%s" tag)))
     ((equal mode "ref")
      (setq lctags-opt "-xra")
      (setq select-name (format "(R)%s" tag)))
     ((equal mode "inc")
      (setq lctags-opt "list")
      (setq lctags-opt2 "inc")
      (setq tag buffer-file-name)
      (setq select-name (format "(i)%s" tag)))
     ((equal mode "incSrc")
      (setq lctags-opt "list")
      (setq lctags-opt2 "incSrc")
      (setq tag buffer-file-name)
      (setq select-name (format "(I)%s" tag)))
     )
    (setq buffer (generate-new-buffer
		  (concat "GTAGS SELECT* " select-name)))
    (setq opt-list
	  (append (list save buffer lctags-opt lctags-opt2)
		  lctags-opt-list
		  (list tag
			(number-to-string line)
			(number-to-string column))))
    (apply 'execute-lctags opt-list)

    (with-current-buffer buffer
      ;;(message (buffer-string))
      (setq lineNum (count-lines (point-min) (point-max)))
      (cond
       ((= lineNum 0)
	(message "not found")
	(gtags-pop-context)
	(kill-buffer buffer)
	nil
	)
       ((= lineNum 1)
	(gtags-select-it nil)
	t)
       (t
	(gtags-select-mode)
	t)
       ))))

(defun lctags-ref-at ()
  (interactive)
  (gtags-push-context)
  (lctags-pos-at "ref-at"))

(defun lctags-def-at ()
  (interactive)
  (gtags-push-context)
  (lctags-pos-at "def-at"))

(defun lctags-call-at ()
  (interactive)
  (gtags-push-context)
  (lctags-pos-at "call-at"))

(defun lctags-list-inc-this-file (&optional depth)
  (interactive "P")
  (gtags-push-context)
  (lctags-pos-at "inc" nil
		 (when depth "-d")
		 (when depth (number-to-string depth))))

(defun lctags-list-incSrc-this-file (&optional depth)
  (interactive)
  (gtags-push-context)
  (lctags-pos-at "incSrc" nil
		 (when depth "-d")
		 (when depth (number-to-string depth))))


(defun lctags-graph-caller-at ()
  (interactive)
  (lctags-graph-at "caller"))

(defun lctags-graph-callee-at ()
  (interactive)
  (lctags-graph-at "callee"))

(defun lctags-graph-symbol-at ()
  (interactive)
  (lctags-graph-at "symbol"))

(defun lctags-graph-at ( graph )
  (let ((org-buf (current-buffer))
	(nsId (lctags-namespaceId-at))
	(line (1- (current-line)))
	(column (+ (- (point) (point-at-bol)) 1))
	)
    
    (with-temp-buffer
      (execute-lctags org-buf (current-buffer)
		      "graph-at" graph (buffer-file-name org-buf)
		      (number-to-string line) (number-to-string column) "-b"))))


(defun lctags-update-this-file ()
  (interactive)
  (let ((buffer (get-buffer-create lctags-process-buf-name))
	(org-buf (current-buffer))
	(file-name (buffer-file-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (execute-lctags org-buf buffer "update" file-name "--lctags-log" "2" ))))

(defun lctags-get-namespace-at ()
  (let ((line (1- (current-line)))
	(column (+ (- (point) (point-at-bol)) 1))
	buffer namespace )
    (setq buffer (generate-new-buffer "lctags temp"))
    (execute-lctags (current-buffer) buffer
		    "ns-at" (buffer-file-name)
		    (number-to-string line)
		    (number-to-string column))
    (setq namespace (with-current-buffer buffer
		      (string-match "\n$" (buffer-string))
		      (replace-match ""  t nil (buffer-string))))
    (kill-buffer buffer)
    namespace))

(defun lctags-namespaceId-at ()
  (let ((namespace (lctags-get-namespace-at)))
    (car (split-string namespace))))
  

(defun lctags-namespace-at ()
  (interactive)
  (message (lctags-get-namespace-at)))


(defun lctags-gtags-goto-tag-func (tag flag)
  (let (flag-char)
    (setq flag-char (string-to-char flag))
    (cond
     ((char-equal flag-char ?R)
      (lctags-pos-at "ref" tag))
     ((char-equal flag-char ?S)
      (lctags-pos-at "def" tag))
     (t
      (lctags-pos-at "def" tag)))))

(defadvice gtags-goto-tag (around lctags-gtags-goto-tag activate)
  (if lctags-target-buf
      (when (not (lctags-gtags-goto-tag-func (ad-get-arg 0) (ad-get-arg 1)))
	ad-do-it)
    ad-do-it))


(defun lctags-def (&optional mode)
  (interactive "P")
  (let ((gtags-symbol-regexp "[:A-Za-z_][@:A-Za-z_0-9]*"))
    (cond
     ((equal mode '(4))
      (lctags-def-at))
     ((equal mode '(16))
      (let ((lctags-target-buf nil))
	(gtags-find-tag)))
     (t
      (let ((lctags-target-buf (current-buffer)))
	(gtags-find-tag)))
     )))

(defun lctags-ref (&optional mode)
  (interactive "P")
  (cond
   ((equal mode '(4))
    (lctags-ref-at))
   ((equal mode '(16))
    (let ((lctags-target-buf nil))
      (gtags-find-rtag)))
   (t
    (let ((lctags-target-buf (current-buffer)))
      (gtags-find-rtag)))
   ))


(defun lctags-call-process-func (arg-list)
  ""
  (if (or (not (equal (car arg-list) "global"))
	  (not lctags-target-buf))
      arg-list
    (let ((db-path lctags-db)
	  (target lctags-target)
	  (config lctags-conf)
	  new-arg-list)
      (setq new-arg-list
	    (append (list lctags-command)
		    (cdr arg-list)))
      (with-current-buffer lctags-target-buf
	(when (and lctags-db (functionp lctags-db))
	  (setq db-path (funcall lctags-db)))
	(when (and lctags-target (functionp lctags-target))
	  (setq target (funcall lctags-target)))
	(when (and lctags-conf (functionp lctags-conf))
	  (setq config (funcall lctags-conf))))

      (when db-path
	(setq new-arg-list
	      (append new-arg-list
		      (list "--lctags-db" db-path))))
      (when target
	(setq new-arg-list
	      (append new-arg-list
		      (list "--lctags-target" target))))
      (when config
	(setq new-arg-list
	      (append new-arg-list
		      (list "--lctags-conf" config))))
      new-arg-list)))

(defadvice call-process (around lctags-call-process activate)
  (let ((index 0))
    (ad-set-args 0 (lctags-call-process-func (ad-get-args 0)))
    ad-do-it))


(provide 'lctags)
