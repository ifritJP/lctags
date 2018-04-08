;;-*- coding:utf-8; mode:emacs-lisp-*-
;;
;; Copyright (C) 2017 ifritJP
;;
;;
;; 
;; (add-hook 'lctags-mode-hook
;;       '(lambda ()
;;          (local-set-key (kbd "M-t") 'lctags-def)
;;          (local-set-key (kbd "M-r") 'lctags-ref)
;;          (local-set-key (kbd "C-c l") 'lctags-dispatch-mode)
;;          (local-set-key (kbd "C-c C-/") 'lctags-helm-completion-at)
;;          (local-set-key (kbd "C-c C-x") 'lctags-helm-change-enum-at)
;;          (local-set-key (kbd "C-c C-f") 'lctags-display-diag)
;;          (local-set-key (kbd "C-t") 'gtags-pop-stack)))
;;
;; (require 'lctags-helm)  or  (require 'lctags-anything)
;;



(require 'gtags)

(require 'lctags-dispatch)
(require 'lctags-split)
(require 'lctags-insert-func)
(require 'lctags-highlight)
(require 'lctags-rename)



(defvar lctags-cursor-buf-name "*lctags-cursor*"
  "")

(defvar lctags-process-buf-name "*lctags-process*"
  "")

(defvar lctags-graph-process-buf-name "*lctags-graph-process*"
  "")

(defvar lctags-command
  "lctags"
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

(defvar lctags-use-global nil
  )


(defvar lctags-cursor-kind-list
  '( "UnexposedDecl" "StructDecl" "UnionDecl" "ClassDecl" "EnumDecl"
     "FieldDecl" "EnumConstantDecl" "FunctionDecl" "VarDecl" "ParmDecl"
     "ObjCInterfaceDecl" "ObjCCategoryDecl" "ObjCProtocolDecl"
     "ObjCPropertyDecl" "ObjCIvarDecl" "ObjCInstanceMethodDecl"
     "ObjCClassMethodDecl" "ObjCImplementationDecl" "ObjCCategoryImplDecl"
     "TypedefDecl" "CXXMethod" "Namespace" "LinkageSpec" "Constructor"
     "Destructor" "ConversionFunction" "TemplateTypeParameter"
     "NonTypeTemplateParameter" "TemplateTemplateParameter" "FunctionTemplate"
     "ClassTemplate" "ClassTemplatePartialSpecialization"
     "NamespaceAlias" "UsingDirective" "UsingDeclaration" "TypeAliasDecl"
     "ObjCSynthesizeDecl" "ObjCDynamicDecl" "CXXAccessSpecifier"
     "ObjCSuperClassRef" "ObjCProtocolRef" "ObjCClassRef" "TypeRef"
     "CXXBaseSpecifier" "TemplateRef" "NamespaceRef" "MemberRef"
     "LabelRef" "OverloadedDeclRef" "VariableRef" "InvalidFile"
     "NoDeclFound" "NotImplemented" "InvalidCode" "UnexposedExpr"
     "DeclRefExpr" "MemberRefExpr" "CallExpr" "ObjCMessageExpr"
     "BlockExpr" "IntegerLiteral" "FloatingLiteral" "ImaginaryLiteral"
     "StringLiteral" "CharacterLiteral" "ParenExpr" "UnaryOperator"
     "ArraySubscriptExpr" "BinaryOperator" "CompoundAssignOperator"
     "ConditionalOperator" "CStyleCastExpr" "CompoundLiteralExpr"
     "InitListExpr" "AddrLabelExpr" "StmtExpr" "GenericSelectionExpr"
     "GNUNullExpr" "CXXStaticCastExpr" "CXXDynamicCastExpr"
     "CXXReinterpretCastExpr" "CXXConstCastExpr" "CXXFunctionalCastExpr"
     "CXXTypeidExpr" "CXXBoolLiteralExpr" "CXXNullPtrLiteralExpr"
     "CXXThisExpr" "CXXThrowExpr" "CXXNewExpr" "CXXDeleteExpr"
     "UnaryExpr" "ObjCStringLiteral" "ObjCEncodeExpr" "ObjCSelectorExpr"
     "ObjCProtocolExpr" "ObjCBridgedCastExpr" "PackExpansionExpr"
     "SizeOfPackExpr" "LambdaExpr" "ObjCBoolLiteralExpr" "ObjCSelfExpr"
     "UnexposedStmt" "LabelStmt" "CompoundStmt" "CaseStmt" "DefaultStmt"
     "IfStmt" "SwitchStmt" "WhileStmt" "DoStmt" "ForStmt" "GotoStmt"
     "IndirectGotoStmt" "ContinueStmt" "BreakStmt" "ReturnStmt"
     "GCCAsmStmt" "AsmStmt" "ObjCAtTryStmt" "ObjCAtCatchStmt"
     "ObjCAtFinallyStmt" "ObjCAtThrowStmt" "ObjCAtSynchronizedStmt"
     "ObjCAutoreleasePoolStmt" "ObjCForCollectionStmt" "CXXCatchStmt"
     "CXXTryStmt" "CXXForRangeStmt" "SEHTryStmt" "SEHExceptStmt"
     "SEHFinallyStmt" "MSAsmStmt" "NullStmt" "DeclStmt" "OMPParallelDirective"
     "TranslationUnit" "UnexposedAttr" "IBActionAttr" "IBOutletAttr"
     "IBOutletCollectionAttr" "CXXFinalAttr" "CXXOverrideAttr"
     "AnnotateAttr" "AsmLabelAttr" "PackedAttr" "PreprocessingDirective"
     "MacroDefinition" "MacroExpansion" "MacroInstantiation"
     "InclusionDirective" "ModuleImportDecl" ) )


(defvar lctags-decl-cursor-kind-list
  '( "FunctionDecl" "UnexposedDecl" "StructDecl" "UnionDecl" "ClassDecl"
     "EnumDecl" "FieldDecl" "EnumConstantDecl" "VarDecl" "ParmDecl"
     "ObjCInterfaceDecl" "ObjCCategoryDecl" "ObjCProtocolDecl"
     "ObjCPropertyDecl" "ObjCIvarDecl" "ObjCInstanceMethodDecl"
     "ObjCClassMethodDecl" "ObjCImplementationDecl" "ObjCCategoryImplDecl"
     "TypedefDecl" "CXXMethod" "Namespace" "LinkageSpec" "Constructor"
     "Destructor" "ConversionFunction" "TemplateTypeParameter"
     "NonTypeTemplateParameter" "TemplateTemplateParameter" "FunctionTemplate"
     "ClassTemplate" "ClassTemplatePartialSpecialization"
     "NamespaceAlias" "UsingDirective" "UsingDeclaration" "TypeAliasDecl"
     "ObjCSynthesizeDecl" "ObjCDynamicDecl""PreprocessingDirective"
     "MacroDefinition" "InclusionDirective" "ModuleImportDecl" ) )



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

(defun lctags-get-projDir (buffer)
  (with-temp-buffer
    (lctags-execute-op2 buffer (current-buffer) nil nil "dump" "projDir")
    (car (split-string (buffer-string)))))


(defun lctags-execute-op2 (src-buf lctags-buf input async &rest lctags-opts)
  (lctags-execute-op src-buf lctags-buf input async lctags-opts))

(defun lctags-execute-op (src-buf lctags-buf input async lctags-opts)
  (let ((db-path lctags-db)
	(target lctags-target)
	(config lctags-conf)
	command dir exit-code)
    (with-current-buffer src-buf
      (when (and lctags-db (functionp lctags-db))
	(setq db-path (funcall lctags-db)))
      (when (and lctags-target (functionp lctags-target))
	(setq target (funcall lctags-target)))
      (when (and lctags-conf (functionp lctags-conf))
	(setq config (funcall lctags-conf)))
      (setq dir default-directory))
    
    (with-temp-buffer
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
      (when input
	(insert input))
      (if async
	  (let (process)
	    (setq command
		  (append (list "lctags-process" lctags-buf lctags-command)
			  command))
	    (setq process (apply 'start-process command))
	    (setq exit-code process)
	    (process-send-string process (buffer-string))
	    (run-with-timer 1 nil 'lctags-process-scroll lctags-buf) )
	(setq command
	      (append (list (point-min) (point-max) lctags-command
			    nil lctags-buf nil )
		      command))
	(setq exit-code (apply 'call-process-region command))))
    (with-current-buffer lctags-buf
      (setq default-directory dir)
      (goto-char (point-min))
      )
    exit-code))

(defun lctags-execute (src-buf lctags-buf input &rest lctags-opts)
  (lctags-execute-op src-buf lctags-buf input nil lctags-opts))

(defun lctags-process-scroll (lctags-buf)
  (with-current-buffer lctags-buf
    (goto-char (point-max))
    (recenter)
    ))

(defun lctags-select-gtags (buffer header-name select-func decide-func
				   &optional create-candidate-list)
  (with-current-buffer buffer
    ;;(message (buffer-string))
    (let ((lineNum (count-lines (point-min) (point-max))))
      (cond
       ((string-match "^lctags:" (buffer-string))
	(switch-to-buffer buffer)
	)
       ((= lineNum 0)
	(message "not found")
	(gtags-pop-context)
	(kill-buffer buffer)
	nil
	)
       (t
	(funcall select-func decide-func
		 (if create-candidate-list
		     create-candidate-list
		   'lctags-gtags-create-candidate-list)
		 header-name)
	t)
       ))  
    ))

(defun lctags-pos-at ( mode &optional tag &rest lctags-opt-list)
  (let ((save (current-buffer))
	(line (lctags-get-line))
	(column (+ (- (point) (point-at-bol)) 1))
	(use-global lctags-use-global)
	buffer select-name lctags-opt lctags-opt2 opt-list input )
    (cond
     ((equal mode "def-at")
      (setq lctags-opt "def-ata")
      (setq tag buffer-file-name)
      (setq input (buffer-string))
      (setq lctags-opt2 '( "-i" ))
      (setq select-name
	    (format "(D)%s<%s:%d:%d>"
		    (lctags-get-current-token)
		    (file-name-nondirectory buffer-file-name) line column)))
     ((equal mode "ref-at")
      (setq lctags-opt "ref-ata")
      (setq tag buffer-file-name)
      (setq input (buffer-string))
      (setq lctags-opt2 '( "-i" ))
      (setq select-name
	    (format "(R)%s<%s:%d:%d>"
		    (lctags-get-current-token)
		    (file-name-nondirectory buffer-file-name) line column)))
     ((equal mode "call-at")
      (setq lctags-opt "call-ata")
      (setq tag buffer-file-name)
      (setq input (buffer-string))
      (setq lctags-opt2 '( "-i" ))
      (setq select-name
	    (format "(C)%s<%s:%d:%d>"
		    (lctags-get-current-token)
		    (file-name-nondirectory buffer-file-name) line column)))
     ((equal mode "file")
      (setq lctags-opt "-xP")
      (setq select-name (format "(F)%s" tag)))
     ((equal mode "def")
      (setq lctags-opt "-xa")
      (setq select-name (format "(D)%s" tag)))
     ((equal mode "ref")
      (setq lctags-opt "-xra")
      (setq select-name (format "(R)%s" tag)))
     ((equal mode "inc")
      (setq use-global nil)
      (setq lctags-opt "list")
      (setq lctags-opt2 '( "inc" ))
      (setq tag buffer-file-name)
      (setq select-name (format "(i)%s" tag)))
     ((equal mode "incSrc")
      (setq use-global nil)
      (setq lctags-opt "list")
      (setq lctags-opt2 '( "incSrc" ))
      (setq tag buffer-file-name)
      (setq select-name (format "(I)%s" tag)))
     )
    (setq buffer (generate-new-buffer
		  (concat "GTAGS SELECT* " select-name)))
    (setq opt-list
	  (append (list save buffer input lctags-opt)
		  (when use-global
		      '("--use-global"))
		  lctags-opt2
		  lctags-opt-list
		  (list tag
			(when (not (posix-string-match "^-x" lctags-opt))
			  (when (and tag (not (eq tag "")))
			    (number-to-string line)))
			(when (not (posix-string-match "^-x" lctags-opt))
			  (when (and tag (not (eq tag "")))
			    (number-to-string column)))
			)))
    (apply 'lctags-execute opt-list)
    (lctags-select-gtags buffer nil
			 'lctags-gtags-select-mode 'lctags-gtags-select)
    ))

(defun lctags-find-file ()
  (interactive)
  (let (tagname prompt)
    (setq prompt "Find files: ")
    (setq tagname (read-string prompt))
    (gtags-push-context)
    (let ((lctags-target-buf (current-buffer)))
      (gtags-goto-tag tagname "P"))))

  

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
  (interactive "P")
  (gtags-push-context)
  (lctags-pos-at "incSrc" nil
		 (when depth "-d")
		 (when depth (number-to-string depth))))


(defun lctags-graph-caller-at (&optional depth)
  (interactive "P")
  (lctags-graph-at "caller" depth))

(defun lctags-graph-callee-at (&optional depth)
  (interactive "P")
  (lctags-graph-at "callee" depth))

(defun lctags-graph-symbol-at (&optional depth)
  (interactive "P")
  (lctags-graph-at "symbol" depth))

(defun lctags-graph-inc (&optional depth)
  (interactive "P")
  (lctags-graph "inc" depth))

(defun lctags-graph-incSrc (&optional depth)
  (interactive "P")
  (lctags-graph "incSrc" depth))


(defun lctags-get-line ()
  (interactive)
  (if (eq (point) (point-max))
      (if (eq (current-column) 0)
	  (1+ (count-lines 1 (point)))
	(count-lines 1 (point)))
    (count-lines 1 (1+ (point)))))

(defun lctags-get-column ()
  (interactive)
  (1+ (string-bytes (buffer-substring (point-at-bol) (point)))))
  

(defun lctags-graph-at ( graph depth )
  (let ((org-buf (current-buffer))
	(nsId (lctags-namespaceId-at))
	(line (lctags-get-line))
	(column (lctags-get-column))
	)
    
    (with-current-buffer (generate-new-buffer lctags-graph-process-buf-name)
      (lctags-execute-op2 org-buf (current-buffer) nil t
			  "graph-at" graph (buffer-file-name org-buf)
			  (number-to-string line) (number-to-string column) "-b"
			  (when depth "-d")
			  (when depth (number-to-string depth))
			  ))))

(defun lctags-graph-at ( graph depth )
  (lctags-graph-op "graph-at" graph depth))

(defun lctags-graph ( graph depth )
  (lctags-graph-op "graph" graph depth))

(defun lctags-graph-op ( command graph depth )
  (let ((org-buf (current-buffer))
	(nsId (lctags-namespaceId-at))
	(line (lctags-get-line))
	(column (lctags-get-column))
	)
    
    (with-current-buffer (generate-new-buffer lctags-graph-process-buf-name)
      (lctags-execute-op2 org-buf (current-buffer) nil t
			  command graph (buffer-file-name org-buf)
			  (number-to-string line) (number-to-string column) "-b"
			  (when depth "-d")
			  (when depth (number-to-string depth))
			  ))))


(defun lctags-get-buffer (name &optional init)
  (let ((buffer (get-buffer-create name)))
    (when init
      (with-current-buffer buffer
	(setq buffer-read-only nil)
	(erase-buffer)))
    buffer))

(defun lctags-get-process-buffer (init)
  (lctags-get-buffer lctags-process-buf-name init))

(defun lctags-update-for (path)
  (let ((buffer (lctags-get-process-buffer t))
	(org-buf (current-buffer)))
    (switch-to-buffer-other-window buffer)
    (lctags-execute-op org-buf buffer nil t
		       (list "update" path "--lctags-log" "2" ))))

(defun lctags-update-this-file ()
  (interactive)
  (lctags-update-for (buffer-file-name)))

(defun lctags-update-this-directory ()
  (interactive)
  (lctags-update-for (file-name-directory (buffer-file-name))))

(defun lctags-update-all ()
  (interactive)
  (lctags-update-for (lctags-get-projDir (current-buffer))))


(defun lctags-get-namespace-at ()
  (let ((line (lctags-get-line))
	(column (+ (- (point) (point-at-bol)) 1))
	buffer namespace )
    (setq buffer (generate-new-buffer "lctags temp"))
    (lctags-execute (current-buffer) buffer nil
		    "ns-at" (buffer-file-name)
		    (number-to-string line)
		    (number-to-string column))
    (setq namespace (with-current-buffer buffer
		      (if (equal (buffer-string) "" )
			  "nothing"
			(string-match "\n$" (buffer-string))
			(replace-match ""  t nil (buffer-string)))))
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
     ((char-equal flag-char ?P)
      (lctags-pos-at "file" tag))
     (t
      (lctags-pos-at "def" tag)))))

(defadvice gtags-goto-tag (around lctags-gtags-goto-tag activate)
  (if lctags-target-buf
      (when (not (lctags-gtags-goto-tag-func (ad-get-arg 0) (ad-get-arg 1)))
	ad-do-it)
    ad-do-it))


(defun lctags-def (&optional mode)
  (interactive "P")
  (let ((gtags-symbol-regexp "[:A-Za-z_][@:A-Za-z_0-9]*[A-Za-z_0-9]"))
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
	  (use-global lctags-use-global)
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
	  (setq config (funcall lctags-conf)))
	(when (and lctags-use-global (functionp lctags-use-global))
	  (setq use-global (funcall lctags-use-global)))
	)

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
      (when (and use-global
      		 (or (posix-string-match "^-c" (nth 4 arg-list))
		     (posix-string-match "^-x" (nth 4 arg-list))))
	(setq new-arg-list
      	      (append new-arg-list
      		      (list "--use-global"))))
      new-arg-list)))

(defadvice call-process (around lctags-call-process activate)
  (let ()
    (ad-set-args 0 (lctags-call-process-func (ad-get-args 0)))
    ad-do-it))


(defun lctags-execute-xml (src-buf lctags-buf input set-symbol 
				   xml-element xml-sub-element
				   &rest lctags-opts)
  (if (eq (lctags-execute-op src-buf lctags-buf input nil lctags-opts) 0)
      (progn
	(set set-symbol (lctags-xml-get lctags-buf xml-element))
	(if (lctags-xml-get-child (eval set-symbol) xml-sub-element)
	    (setq lctags-diag-info nil)
	  (setq lctags-diag-info (lctags-xml-get-diag lctags-buf))))
    (set set-symbol nil)
    (with-current-buffer lctags-buf
      (setq lctags-diag-info `((message nil ,(buffer-string))))))
  )


(defun lctags-switch-to-buffer-other-window (buf)
  (if (not (get-buffer-window buf))
      (switch-to-buffer-other-window buf)
    (select-window (get-buffer-window buf))
    ))


(defun lctags-def-pickup-symbol ()
  (interactive)
  (let (pattern)
    (setq pattern (read-string "symbol (^match_front or match_tail$ or match_any): "
			       (lctags-get-current-token) ))
    (lctags-def-pickup-symbol-op pattern)
  ))

(defun lctags-def-pickup-symbol-op (pattern)
  (let ((buffer (generate-new-buffer
		 (concat "GTAGS SELECT* (T)" pattern)))
	kind)
    (setq kind (lctags-helm-select-from-list "Cursor Kind?"
					     (cons "*all*"
						   lctags-decl-cursor-kind-list )))
    (when (string= kind "*all*")
      (setq kind nil))
    (cond
     ((string-match "$$" pattern)
      t)
     ((string-match "^\\^" pattern)
      (setq pattern (concat "::" (substring pattern 1 ))))
     (t 
      (setq pattern (concat "%" pattern ))))
    (lctags-execute-op2 (current-buffer) buffer nil nil
			"-xTa" pattern
			(when kind "--lctags-cursorKind")
			(when kind kind))
    (gtags-push-context)
    (lctags-select-gtags buffer pattern
			 'lctags-gtags-select-mode 'lctags-gtags-select
			 (lambda ()
			   (lctags-gtags-create-candidate-list t))
			 )
    ))

(defun lctags-grep-cursor ()
  (interactive)
  (let (symbol buf kind)
    (setq kind (lctags-helm-select-from-list "Cursor Kind?"
					     lctags-cursor-kind-list ))
    (setq symbol (read-string "text?: "))
    (setq buf (lctags-get-buffer "*lctags-grep*" t))
    
    (lctags-execute-op2 (current-buffer) buf nil nil
			"grep-cursor" buffer-file-name kind symbol)
    (lctags-switch-to-buffer-other-window buf)
    (grep-mode)
    (run-hooks 'grep-setup-hook)
    (with-current-buffer buf
      (goto-char 0))
  ))

(defun lctags-get-unique-buffer (symbol name init)
  (if (not (buffer-live-p (eval symbol)))
      (set symbol (lctags-get-buffer name init))
    (with-current-buffer (eval symbol)
      (erase-buffer)))
  (eval symbol))
  

(setq lctags-expand-macro-buf nil)
(defun lctags-expand-macro ()
  (interactive)
  (let (symbol kind cpp-mode-flag)
    (lctags-get-unique-buffer 'lctags-expand-macro-buf
			      "*expand-macro*" t)
    (setq cpp-mode-flag (eq major-mode 'c++-mode))
    (lctags-execute-op2 (current-buffer) lctags-expand-macro-buf nil nil
			"expand-macro" buffer-file-name)
    (lctags-switch-to-buffer-other-window lctags-expand-macro-buf)
    (if cpp-mode-flag
	(c++-mode)
      (c-mode))
    (with-current-buffer lctags-expand-macro-buf
      (goto-char 0))
  ))


(defun lctags-expand-enum-format-at (&optional form)
  (interactive)
  (when (not form)
    (setq form (read-string "form?: " "%s;\n")))
  (lctags-expand-enum-and-replace-text
   (lambda (enum)
     (insert (format form enum enum enum enum enum enum enum)))
   nil nil))


(defun lctags-generate-to-convert-enumName-at ()
  (interactive)
  (lctags-expand-enum-and-replace-text
   (lambda (enum)
     (insert (format "case %s:\nreturn \"%s\";\n" enum enum)))
   (lambda (token)
     (insert (format "switch (%s) {\n" token)))
   (lambda (token)
     (insert "default:\nreturn NULL;\n}"))))

(defun lctags-expand-enum-and-replace-text ( enum-func
					     &optional first-func end-func )
  "This function expand value of enum type at current cursor,
and replace text with specified function.
enum-func parameter is function, and
it is called with enum value name argument per enum value.
first-func parameter is function, and
it is called with symbol at current cursor.
end-func parameter is function, and
it is called with symbol at current cursor.
"
  (let ((token (lctags-get-current-token))
	(first-flag t)
	pos)
    (lctags-expand-enum-map-at
     (lambda (enum)
       (when first-flag
	 (setq first-flag nil)
	 (lctags-remove-current-token)
	 (setq pos (point))
	 (when first-func
	   (funcall first-func token)))
       (funcall enum-func enum)))
    (when end-func
      (funcall end-func token))
    (indent-region pos (point))))



(defun lctags-expand-enum-map-at ( func )
  "This function expand value of enum type at current cursor.
func parameter is called with enum value name argument.
"
  (let ((src-buf (current-buffer))
	tmp-buf result enum-list)
    (with-temp-buffer
      (setq tmp-buf (current-buffer))
      (with-current-buffer src-buf
	(lctags-execute-op (current-buffer) tmp-buf (buffer-string) nil 
			   (list "expand" (buffer-file-name)
				 (number-to-string (lctags-get-line))
				 (number-to-string (lctags-get-column)) "-i")))
      (setq result (lctags-xml-get (current-buffer) 'completion))
      )
    (setq enum-list
	  (delq nil
		(lctags-candidate-map-candidate
		 (lambda (X XX) (lctags-candidate-item-get-simple X))
		 result result)))
    (dolist (enum enum-list)
      (funcall func enum))
    ))


(defun lctags-cursor-format-val (val)
  (when val
    (if (string-match "^[0-9]" val)
	(format "%s(0x%X)" val (string-to-number val))
      val)))

(defun lctags-cursor-at ()
  (interactive)
  (let ((src-buf (current-buffer))
	tmp-buf result enum-list cursor-buf)
    (with-temp-buffer
      (setq tmp-buf (current-buffer))
      (with-current-buffer src-buf
	(lctags-execute-op (current-buffer) tmp-buf (buffer-string) nil 
			   (list "cursor-at" (buffer-file-name)
				 (number-to-string (lctags-get-line))
				 (number-to-string (lctags-get-column)) "-i")))
      (setq result (lctags-xml-get (current-buffer) 'cursor))
      )
    (setq cursor-buf (lctags-get-buffer lctags-cursor-buf-name t))
    (save-selected-window
      (delete-other-windows)
      (split-window-vertically)
      (other-window 1)
      (switch-to-buffer cursor-buf)
      (let ((base-key-list '(spelling kindName type typeSize))
	    key-list)
	(dolist (item base-key-list)
	  (insert (format "%s: %s\n"
			  item
			  (lctags-cursor-format-val 
			   (lctags-xml-get-val result item)))))
	(dolist (item (lctags-xml-get-list-root result))
	  (setq key-list (cons (xml-node-name item) key-list)))
	(dolist (key (sort key-list 'string< ))
	  (let (val)
	  (when (not (member key base-key-list))
	    (setq val (lctags-xml-get-val result key))
	    (when val 
	      (insert (format "%s: %s\n" key (lctags-cursor-format-val val))))))))
      (beginning-of-buffer)
      (fit-window-to-buffer)
      )
    ))


(provide 'lctags)
