(defconst lctags-servlet-api-info-table
  `(("dumpDir" (:param ("--lctags-form" "json")))
    ("matchFile" (:param ("?pattern" "?option" "--lctags-form" "json" )))
    ("searchFile" (:param (("?path" ,(lambda (val)
				       (lctags-replace-txt val "_" "$_")))
			   "--lctags-form" "json"
			   "--lctags-candidateLimit" "?limit")))
    ("searchDecl" (:param (("?name" ,(lambda (val)
				       (lctags-replace-txt val "_" "$_")))
			   "--lctags-form" "json"
			   "--lctags-candidateLimit" "?limit")))
    ("defAtFileId" (:param ("?fileId" "--lctags-form" "json" )))
    ("callee" (:param ("?nsId" "--lctags-form" "json" )))
    ("caller" (:param ("?nsId" "--lctags-form" "json" )))
    ("refSym" (:param ("?nsId" "--lctags-form" "json" )))
    ("refDir" (:param ("?path" "--lctags-form" "json" )))
    ("refFile" (:param ("?fileId" "?path" "--lctags-form" "json" )))
    ("reqDir" (:param ("?path" "--lctags-form" "json" )))
    ("reqFile" (:param ("?fileId" "?path" "--lctags-form" "json" )))
    ("decl" (:param ("?nsId" "--lctags-form" "json" )))
    ("openDecl" (:param (decl "inq" "decl" "?nsId")
			:func lctags-servlet-open-pos))
    ("callPair" (:param (callPair "inq" "callPair" "?nsId" "?belongNsId")
			:func lctags-servlet-open-pos))
    ("refPair" (:param (refPair "inq" "refPair" "?nsId" "?belongNsId")
		       :func lctags-servlet-open-pos)))
  "REST API information table.
'(API (:param PARAM-LIST :func FUNC))

- API: API name
- PARAM-LIST: parameter list.
   if type of parameter is string and the parameter starts with ?, use from query.
   if type of parameter is string and the parameter does not starts with ?, use raw.
   if type of parameter is the list, the list consists of (val func).
- FUNC: if FUNC is specified, servlet process the form (apply func param).
")

(provide 'lctags-const)
