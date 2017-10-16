(add-hook 'lctags-mode-hook
      '(lambda ()
         (local-set-key (kbd "M-t") 'lctags-def)
         (local-set-key (kbd "M-r") 'lctags-ref)
         (local-set-key (kbd "C-c l") 'lctags-dispatch-mode)
         (local-set-key (kbd "C-c C-/") 'lctags-helm-complete-at)
         (local-set-key (kbd "C-c C-x") 'lctags-helm-change-enum-at)
	 (local-set-key (kbd "C-c C-f") 'lctags-display-diag)
	 (local-set-key (kbd "M-m") 'lctags-gtags-resume)
         (local-set-key "\C-t" 'gtags-pop-stack)))

(cond ((featurep 'helm)
       (require 'lctags-helm))
      ((featurep 'anything)
       (require 'lctags-anything))
      (t
       (error "please set helm or anything")))

(add-hook 'c-mode-common-hook
          '(lambda()
	     (lctags-mode 1)))

(provide 'lctags-conf)
