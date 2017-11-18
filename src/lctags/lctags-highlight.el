(defvar lctags-search-token-color-list
  '("OrangeRed4" "OrangeRed3" "dark green" "dark magenta" "dark blue"
    "yellow4" "DeepSkyBlue4" "gray34"))

(defface lctags-search-token-color-default
  `((t
     :background ,(car lctags-search-token-color-list)))
  "default highlight face")

(defvar lctags-search-token-color-default 'lctags-search-token-color-default)


(setq lctags-search-token-color nil)
(setq lctags-search-token-color-first t)

(setq lctags-highlight-locationSet-list nil)
(setq lctags-highlight-overlay-list nil)
(setq lctags-highlight-overlay-mark-list nil)


(defun lctags-goto-line-column (line column)
  (let (pos)
    (goto-line line)
    (setq pos (point))
    (forward-char (1- column))
    (while (not (eq (lctags-get-column) column))
      (forward-char -1))
  ))

(defun lctags-get-point-at-line-column (line column)
  (let ((now (point)) pos)
    (setq now (point))
    (lctags-goto-line-column line column)
    (setq pos (point))
    (goto-char now)
    pos))

(defun lctags-location-item-get-kind (item)
  (lctags-xml-get-val item 'kind))

(defun lctags-location-item-get-symbol (item)
  (lctags-xml-get-val item 'symbol))

(defun lctags-location-item-get-file (item)
  (lctags-xml-get-val item 'file))

(defun lctags-location-item-get-line (item)
  (string-to-number (lctags-xml-get-val item 'line))
  )

(defun lctags-location-item-get-column (item)
  (string-to-number (lctags-xml-get-val item 'column))
  )

(defun lctags-location-item-get-end-line (item)
  (string-to-number (lctags-xml-get-val item 'endLine))
  )

(defun lctags-location-item-get-end-column (item)
  (string-to-number (lctags-xml-get-val item 'endColumn))
  )

(defun lctags-location-item-get-point (item)
  (lctags-get-point-at-line-column (lctags-location-item-get-line item)
				   (lctags-location-item-get-column item)))

(defun lctags-location-item-get-end-point (item)
  (lctags-get-point-at-line-column (lctags-location-item-get-end-line item)
				   (lctags-location-item-get-end-column item)))



(defun lctags-highlight-clear ()
  (interactive)
  (dolist (overlay lctags-highlight-overlay-list)
    (delete-overlay overlay))
  (setq lctags-highlight-overlay-list nil)
  (setq lctags-highlight-overlay-mark-list nil)
  (setq lctags-search-token-color nil)
  (setq lctags-highlight-locationSet-list nil)
  (setq lctags-search-token-color-first t)
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (when (string-match "^lctags-" (symbol-name (overlay-get overlay 'face)))
      (delete-overlay overlay))))


(defun lctags-highlight-at ()
  (interactive)
  (lctags-highlight-at-op))

(defun lctags-highlight-make-face (&optional color)
  (when (not lctags-search-token-color)
    (setq lctags-search-token-color lctags-search-token-color-list)
    (setq lctags-search-token-color-first (not lctags-search-token-color-first)))
  (when (not color)
    (setq color (read-color (format "face-color?(default %s): "
				    (car lctags-search-token-color) )
			    nil t nil)))
  (when (or (eq color t) (equal color ""))
    (setq color (car lctags-search-token-color))
    (setq lctags-search-token-color (cdr lctags-search-token-color)))
  (setq face (make-face (make-symbol (format "lctags-search-token-face-%s" color))))
  (set-face-attribute face nil
		      :background color
		      ;;:overline lctags-search-token-color-first
		      :underline lctags-search-token-color-first)
  face
  )

(defun lctags-highlight-at-op (&optional face)
  (let ((buffer (lctags-get-process-buffer t))
	highlight-info overlay color mark locationSet-list)
    (when (or (not face) (eq face 'auto))
      (setq face (lctags-highlight-make-face nil)))
    (lctags-execute-xml (current-buffer) buffer
			(buffer-string)
			'highlight-info 'ref 'locationSet
			"ref-at-all"
			(buffer-file-name)
			(number-to-string (lctags-get-line))
			(number-to-string (lctags-get-column)) "-i")
    (cond
     (lctags-diag-info
      (lctags-helm-display-diag))
     (t
      (setq locationSet-list (lctags-xml-get-list highlight-info 'locationSet))
      (dolist (locationSet locationSet-list)
	(dolist (location (lctags-xml-get-list locationSet 'location))
	  (when (equal (lctags-location-item-get-file location)
		       (buffer-file-name))
	    (setq overlay
		  (make-overlay (lctags-get-point-at-line-column
				 (lctags-location-item-get-line location)
				 (lctags-location-item-get-column location))
				(lctags-get-point-at-line-column
				 (lctags-location-item-get-end-line location)
				 (lctags-location-item-get-end-column location))))
	    (setq mark (point-marker))
	    (if (not lctags-highlight-overlay-list)
		(progn
		  (setq lctags-highlight-overlay-list (list overlay))
		  (setq lctags-highlight-overlay-mark-list
			(list (list mark face)))
		  )
	      (add-to-list 'lctags-highlight-overlay-list overlay)
	      (add-to-list 'lctags-highlight-overlay-mark-list
			   (list mark face))
	      )
	    (overlay-put overlay 'face face)
	    ))
	(setq face (lctags-highlight-make-face t)))
      (setq lctags-highlight-locationSet-list
	    (append locationSet-list lctags-highlight-locationSet-list))
      )
     )))


(defun lctags-highlight-rescan ()
  (interactive)
  (save-excursion
    (let ((list (copy-sequence lctags-highlight-overlay-mark-list)))
      (lctags-highlight-clear)
      (dolist (info list)
	(let ((mark (nth 0 info))
	      (face (nth 1 info)))
	  (goto-char mark)
	  (lctags-highlight-at-op face))))))

(defun lctags-highlight-grep-at ()
  "カーソル位置のハイライト箇所の grep バッファを作成する"
  (interactive)
  (let ((pos (point))
	(dir default-directory)
	(cur-buf (current-buffer))
	target-locationSet)
    (dolist (locationSet lctags-highlight-locationSet-list)
      (dolist (location (lctags-xml-get-list locationSet 'location))
	(when (and (equal (lctags-location-item-get-file location)
			  (buffer-file-name))
		   (when (and (<= (lctags-location-item-get-point location) pos)
			      (> (lctags-location-item-get-end-point location) pos))
		     (setq target-locationSet locationSet)
		   )))))
    (if (not target-locationSet)
	(message "not found")
      (let ((buffer (get-buffer-create "*lctags-highlight-grep*"))
	    line begin-pos)
	(with-current-buffer buffer
	  (setq buffer-read-only nil)
	  (setq default-directory dir)
	  (erase-buffer)
	  (insert (format "grep for \"%s\".\n\n"
			  (lctags-location-item-get-symbol
			   (car (lctags-xml-get-list target-locationSet 'location)))))
	  (dolist (location (lctags-xml-get-list target-locationSet 'location))
	    (setq line (lctags-location-item-get-line location))
	    (insert (format "%s:%d: %s\n"
			    (file-relative-name (lctags-location-item-get-file location)
						default-directory)
			    line
			    (with-current-buffer cur-buf
			      (save-excursion
				(goto-line line)
				(beginning-of-line)
				(setq begin-pos (point))
				(end-of-line)
				(buffer-substring begin-pos (point)))
			      )
			    )))
	  (grep-mode)
	  )
	(lctags-switch-to-buffer-other-window buffer)
	(with-current-buffer buffer
	  (fit-window-to-buffer nil (/ (frame-height) 3)))
	  (beginning-of-buffer)
	))))


(provide 'lctags-highlight)
