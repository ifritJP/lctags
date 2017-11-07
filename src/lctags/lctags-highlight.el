(defface lctags-search-token-face
  '((t
     :background "OrangeRed4"))
  "face")
(defvar lctags-search-token-face 'lctags-search-token-face)



(defun lctags-location-item-get-line (item)
  (lctags-xml-get-val item 'line)
  )

(defun lctags-location-item-get-column (item)
  (lctags-xml-get-val item 'column)
  )

(defun lctags-location-item-get-end-line (item)
  (lctags-xml-get-val item 'endLine)
  )

(defun lctags-location-item-get-end-colmun (item)
  (lctags-xml-get-val item 'endColumn)
  )



(defun lctags-clear-highlight ()
  (interactive)
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (when (string-match "^lctags-" (symbol-name (overlay-get overlay 'face)))
      (delete-overlay overlay))))


(defun lctags-highlight-at ()
  (interactive)
  (let ((buffer (lctags-get-process-buffer t)))
    (lctags-execute-xml (current-buffer) buffer
			(buffer-string)
			'lctags-highlight-info 'ref 'location
			"ref-at-all"
			(buffer-file-name)
			(number-to-string (lctags-get-line))
			(number-to-string (1- (lctags-get-column))) "-i")
  
  ))