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
;;          (local-set-key "\C-\M-t" 'lctags-def-at)
;;          (local-set-key "\C-\M-r" 'lctags-ref-at)
;;          (local-set-key "\C-t" 'gtags-pop-stack)))


(require 'gtags)


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


(setq lctags-hack-gtags nil)


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



(defun lctags-pos-at ( mode &optional tag )
  (let ((save (current-buffer))
	(dir default-directory)
	(line (1- (current-line)))
	(column (+ (- (point) (point-at-bol))))
	(db-path lctags-db)
	(target lctags-target)
	(config lctags-conf)
	buffer lineNum select-name lctags-opt)
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
     )
    (setq buffer (generate-new-buffer
		  (concat "GTAGS SELECT* " select-name)))
    (with-current-buffer save
      (when (and lctags-db (functionp lctags-db))
	(setq db-path (funcall lctags-db)))
      (when (and lctags-target (functionp lctags-target))
	(setq target (funcall lctags-target)))
      (when (and lctags-conf (functionp lctags-conf))
	(setq config (funcall lctags-conf))))
    
    (with-current-buffer buffer
      (setq default-directory dir)
      (call-process lctags-command nil buffer t lctags-opt
		    tag
		    (number-to-string line) (number-to-string column)
		    (if db-path "--lctags-db" "")
		    (if db-path db-path "")
		    (if target "--lctags-target" "")
		    (if target target "" )
		    (if config "--lctags-conf" "")
		    (if config config "" )
		    )
      (goto-char 1)
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
  (if lctags-hack-gtags
      (when (not (lctags-gtags-goto-tag-func (ad-get-arg 0) (ad-get-arg 1)))
	ad-do-it)
    ad-do-it))

(defun lctags-def (&optional use-gtags)
  (interactive "P")
  (let ((lctags-hack-gtags (if use-gtags nil 1)))
    (gtags-find-tag)))

(defun lctags-ref (&optional use-gtags)
  (interactive "P")
  (let ((lctags-hack-gtags (if use-gtags nil 1)))
    (gtags-find-rtag)))


(defun lctags-call-process-func (arg-list)
  ""
  (if (or (not (equal (car arg-list) "global"))
	  (not lctags-mode))
      arg-list
    (let ((db-path lctags-db)
	  (target lctags-target)
	  (config lctags-conf)
	  new-arg-list)
      (setq new-arg-list
	    (append (list lctags-command)
		    (cdr arg-list)))
      (with-current-buffer save
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
