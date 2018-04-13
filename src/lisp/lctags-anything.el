(require 'anything)
(setq lctags-anything t)

(setq anything-c-source-gtags-select
  '((name . "GTAGS")
    (init
     . (lambda ()
	 (call-process-shell-command
	  "lctags -c" nil (anything-candidate-buffer 'global))))
    (candidates-in-buffer)
    (action
     ("Goto the location" . (lambda (candidate)
                              (gtags-push-context)
                              (gtags-goto-tag candidate "")))
     ("Goto the location (other-window)" . (lambda (candidate)
                                             (gtags-push-context)
                                             (gtags-goto-tag candidate "" t)))
     ("Move to the referenced point" . (lambda (candidate)
                                         (gtags-push-context)
                                         (gtags-goto-tag candidate "r"))))))

(require 'lctags-helm)

(provide 'lctags-anything)
