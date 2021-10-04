(provide 'daylight-org-override)

;;;###autoload
(defun org-babel-execute-src-block (&optional arg info params)
  "Execute the current source code block.
Insert the results of execution into the buffer.  Source code
execution and the collection and formatting of results can be
controlled through a variety of header arguments.

With prefix argument ARG, force re-execution even if an existing
result cached in the buffer would otherwise have been returned.

Optionally supply a value for INFO in the form returned by
`org-babel-get-src-block-info'.

Optionally supply a value for PARAMS which will be merged with
the header arguments specified at the front of the source code
block."
  (interactive)
  (let* ((org-babel-current-src-block-location
	  (or org-babel-current-src-block-location
	      (nth 5 info)
	      (org-babel-where-is-src-block-head)))
	 (info (if info (copy-tree info) (org-babel-get-src-block-info))))
    ;; Merge PARAMS with INFO before considering source block
    ;; evaluation since both could disagree.
    (cl-callf org-babel-merge-params (nth 2 info) params)
    (when (org-babel-check-evaluate info)
      (cl-callf org-babel-process-params (nth 2 info))
      (let* ((params (nth 2 info))
	     (cache (let ((c (cdr (assq :cache params))))
		      (and (not arg) c (string= "yes" c))))
	     (new-hash (and cache (org-babel-sha1-hash info :eval)))
	     (old-hash (and cache (org-babel-current-result-hash)))
	     (current-cache (and new-hash (equal new-hash old-hash))))
	(cond
	 (current-cache
	  (save-excursion		;Return cached result.
	    (goto-char (org-babel-where-is-src-block-result nil info))
	    (forward-line)
	    (skip-chars-forward " \t")
	    (let ((result (org-babel-read-result)))
	      (message (replace-regexp-in-string "%" "%%" (format "%S" result)))
	      result)))
	 ((org-babel-confirm-evaluate info)
	  (let* ((lang (nth 0 info))
		 (result-params (cdr (assq :result-params params)))
		 (body (org-babel--expand-body info))
		 (dir (cdr (assq :dir params)))
		 (mkdirp (cdr (assq :mkdirp params)))
		 (default-directory
		   (cond
		    ((not dir) default-directory)
		    ((member mkdirp '("no" "nil" nil))
		     (file-name-as-directory (expand-file-name dir)))
		    (t
		     (let ((d (file-name-as-directory (expand-file-name dir))))
		       (make-directory d 'parents)
		       d))))
		 (cmd (intern (concat "org-babel-execute:" lang)))
		 result)
	    (unless (fboundp cmd)
	      (error "No org-babel-execute function for %s!" lang))
	    (message "executing %s code block%s..."
		     (capitalize lang)
		     (let ((name (nth 4 info)))
		       (if name (format " (%s)" name) "")))
	    (if (member "none" result-params)
		(progn (funcall cmd body params)
		       (message "result silenced"))
	      (setq result
		    (let ((r (funcall cmd body params)))
		      (if (and (eq (cdr (assq :result-type params)) 'value)
			       (or (member "vector" result-params)
				   (member "table" result-params))
			       (not (listp r)))
			  (list (list r))
			r)))
	      (let ((file (and (member "file" result-params)
			       (cdr (assq :file params)))))
		;; If non-empty result and :file then write to :file.
		(when file
		  ;; If `:results' are special types like `link' or
		  ;; `graphics', don't write result to `:file'.  Only
		  ;; insert a link to `:file'.
		  (when (and result
			     (not (or (member "link" result-params)
				      (member "graphics" result-params))))
		    (with-temp-file file
		      (insert (org-babel-format-result
			       result
			       (cdr (assq :sep params)))))
		    ;; Set file permissions if header argument
		    ;; `:file-mode' is provided.
		    (when (assq :file-mode params)
		      (set-file-modes file (cdr (assq :file-mode params)))))
		  (setq result file))
		;; Possibly perform post process provided its
		;; appropriate.  Dynamically bind "*this*" to the
		;; actual results of the block.
		(let ((post (cdr (assq :post params))))
		  (when post
		    (let ((*this* (if (not file) result
				    (org-babel-result-to-file
				     file
				     (org-babel--file-desc params result)))))
		      (setq result (org-babel-ref-resolve post))
		      (when file
			(setq result-params (remove "file" result-params))))))
		(org-babel-insert-result
		 result result-params info new-hash lang)))
	    (run-hooks 'org-babel-after-execute-hook)
	    result)))))))
