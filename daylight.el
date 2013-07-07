(provide 'daylight)

(require 'ox)
(require 'ox-html)
(require 'json)
(require 'assoc)
(require 'cl)

(defvar daylight-head "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Backend and end-user functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(org-export-define-derived-backend 'daylight 'html
  :translate-alist
  '((link . daylight-trans-link)
    (latex-fragment . daylight-trans-latex-fragment))
  :filters-alist
  '((:filter-final-output . daylight-final-function))
  :options-alist
  '((:daylight-date-created "DAYLIGHT_CREATED" nil nil)
    (:daylight-bibliography-url "DAYLIGHT_BIBLIOGRAPHY" nil nil)
    (:daylight-license-url "DAYLIGHT_LICENSE" nil nil)))

(defun daylight-export-to-file (file)
  (let (
        (org-html-text-markup-alist
        '((bold . "<em>%s</em>")
          (code . "<code>%s</code>")
          (italic . "<i>%s</i>")
          (strike-through . "(RESERVED: %s)")
          (underline . "<var>%s</var>")
          (verbatim . "(RESERVED: %s)")))
        (org-export-with-sub-superscripts '{})
        (org-html-inline-images t)
        (org-html-allow-name-attribute-in-anchors nil)
        (org-html-footnote-format "%s")
        (org-html-footnote-separator "<span class='footref'>, </span>")
        (org-html-footnotes-section "<div id=\"footnotes\"><h2 class=\"footnotes\">%s</h2><div id=\"text-footnotes\">%s</div></div>")
        (org-export-dictionary (mapcar (lambda (x) `(,(car x) ("en" :default ,(cdr x)))) '(
          ("Table of Contents" . "Contents")
          ("Footnotes" . "Notes")
          ("Figure %d:" . "Figure %d.")
          ("Table %d:" . "Table %d.")
          ("Listing %d:" . "Listing %d.")))))
  (daylight-aliasing 'org-export-solidify-link-text 'daylight-solidify-link-text
    '(org-export-to-file 'daylight file nil nil nil (list
      :html-doctype "html5"
      :time-stamp-file nil
      :html-head daylight-head
      :html-head-include-default-style nil
      :html-head-include-scripts nil
      :html-html5-fancy t
      :section-numbers nil
      :with-latex 'daylight
        ; Most of the effect of this option is negated by our
        ; LaTeX processing, but we set it to something to keep
        ; ox-html from inserting the MathJax JavaScript.
      :html-postamble nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Miscellaneous hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'org-mode-hook 'daylight-set-org-table-number-regexp)
(defun daylight-set-org-table-number-regexp ()
  (setq org-table-number-regexp
    "^\\([$<>]?[-+^.0-9]*[0-9][-+^.0-9eEdDx()%:]*\\|[<>]?[-+]?0[xX][0-9a-fA-F.]+\\|[<>]?[-+]?[0-9]+#[0-9a-zA-Z.]+\\|nan\\|[-+u]?inf\\)$"))
      ; This is the same as the default except that leading
      ; dollar signs are also allowed.

(add-hook 'org-export-before-parsing-hook 'daylight-add-fig-names)
(defun daylight-add-fig-names (backend)
"For each code block that produces graphics, adds a #+NAME to the
results block matching the file name (but without the file extension)."
  (when (org-export-derived-backend-p backend 'daylight)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "\n#+RESULTS:\n[[file:" nil t)
        (save-excursion
          (let ((file (replace-regexp-in-string "\\.[^.][^.]?[^.]?[^.]?[^.]?\\'" ""
              (buffer-substring-no-properties (point)
                (progn (skip-chars-forward "^]") (point))))))
            (forward-line -1)
            (insert "#+NAME: fig--" file "\n")))))))

(defvar daylight-html-cleanup-path nil)
(defun daylight-final-function (contents backend info)
  (message "Cleaning up...")
  (let (info2 plist k v)
    ; `info2' will be `info' (eventually, encoded in JSON), but
    ; only the parts thereof with string or numeric values
    ; (there's no need to send, e.g., the whole parse tree).
    (setq plist info)
    (while plist
      (setq k (car plist)
        v (cadr plist)
        plist (cddr plist))
      (when (or (stringp v) (numberp v))
        (push (cons k v) info2)))
    (unless (and
        (= (length (plist-get info :author)) 1)
        (stringp (car (plist-get info :author))))
      (error "#+AUTHOR must be plain text"))
    (push (cons :author (car (plist-get info :author))) info2)
    (unless (and
        (= (length (plist-get info :title)) 1)
        (stringp (car (plist-get info :title))))
      (error "non-plain-text #+TITLE not yet implemented"))
    (push (cons :title (car (plist-get info :title))) info2)
    (daylight-proc-on-string daylight-html-cleanup-path contents
      (json-encode info2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Links
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun daylight-trans-link (link desc info)
  (let (
      (type (org-element-property :type link))
      (raw-link (org-element-property :raw-link link)))
    (cond
      ((org-export-inline-image-p link org-html-inline-image-rules)
        ; It's an image. Pass the buck to `org-html-link'.
        (org-html-link link desc info))
      ((and (equal type "file") (string-match "\\`\\.?/" raw-link))
        ; This link's path has no protocol and begins
        ; with "/" or "./", so assume it's a link to a document.
        (format "<a href=\"%s\">%s</a>"
          (daylight-escape-url
            (replace-regexp-in-string "\\`./" "" raw-link))
          desc))
      ((and (not desc) (member type '("http" "https" "ftp" "mailto")))
        ; It's a literal URL. Give it a special class.
        (format "<a class=\"url\" href=\"%s\">%s</a>"
          (daylight-escape-url raw-link)
          (daylight-escape-url raw-link)))
      (t
        ; Pass the buck to `org-html-link'.
        (org-html-link link desc info)))))

(org-add-link-type "cls"
  'ignore
  (lambda (path desc format)
    (cond
      ((eq format 'html)
        (format "<span class='%s'>%s</span>"
          (org-html-encode-plain-text path)
          desc))
      (t
        ""))))

(org-add-link-type "m"
; Markup applied to the description. The path specifies the
; kind of markup.
  'ignore
  (lambda (path desc format)
    (unless (string= path "i")
      (error "'m' link: Unknown type %S" path))
    (cond
      ((eq format 'html)
        (format "<i>%s</i>" desc))
      ((eq format 'odt)
        (org-odt-format-fontify (org-html-encode-plain-text path) "Emphasis"))
      (t
        ""))))

(org-add-link-type "var"
  'ignore
  (lambda (path desc format)
    (cond
      ((eq format 'html)
        (format "<var>%s</var>" (org-html-encode-plain-text path)))
      ((eq format 'odt)
        (org-odt-format-fontify (org-html-encode-plain-text path) "Emphasis"))
      (t
        ""))))

(defun daylight-translate-wp-link (input)
  (format "http://en.wikipedia.org/wiki/%s"
    (daylight-ucfirst (replace-regexp-in-string " " "_" input))))

(org-add-link-type "wp"
  (lambda (path)
    (browse-url (daylight-translate-wp-link path)))
  (lambda (path desc format)
    (cond
      ((eq format 'html)
        (format "<a class='wikipedia' title='Wikipedia: %s' href='%s'>%s</a>"
          (org-html-encode-plain-text (daylight-ucfirst path))
          (daylight-escape-url (daylight-translate-wp-link path))
          (or desc (org-html-encode-plain-text path))))
      (t
        ""))))

(org-add-link-type "doi"
  (lambda (path)
    (browse-url (concat org-doi-server-url path)))
  (lambda (path desc format)
    (cond
      ((eq format 'html)
        (format "<a class='doi' href='http://dx.doi.org/%s'>doi:%s</a>"
          (daylight-escape-url path)
          (org-html-encode-plain-text path)))
      ((eq format 'odt)
        (org-html-encode-plain-text (concat "doi:" path)))
      (t
        ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Math
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar daylight-simplify-math-path nil)

(add-hook 'org-export-before-parsing-hook 'daylight-simplify-math)
(defun daylight-simplify-math (backend)
  (when (and
      (org-export-derived-backend-p backend 'daylight)
      (save-excursion
        (goto-char (point-min))
        (search-forward-regexp "\\$\\|\\\\(" nil t)))
          ; This regex matches '$' or '\('.
    (message "Simplifying math...")
    (daylight-proc-buffer daylight-simplify-math-path)))

(defun daylight-trans-latex-fragment (latex-fragment _ info)
  ; Just format it so daylight-html-cleanup can find it easily.
  (format "<latexfrag>%s</latexfrag>"
    (org-element-property :value latex-fragment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Code blocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar daylight-ess nil)

(add-hook 'org-mode-hook 'daylight-set-babel-default)
(defun daylight-set-babel-default ()
  (when (daylight-buffer-is-daylit)
    (make-local-variable 'org-babel-default-header-args)
    (aput 'org-babel-default-header-args :session "*R*")
    (aput 'org-babel-default-header-args :colnames "yes")
    (aput 'org-babel-default-header-args :rownames "yes")
    (aput 'org-babel-default-header-args :width 800)
    (aput 'org-babel-default-header-args :height 500)
    (aput 'org-babel-default-header-args :exports "both")))

(add-hook 'ess-R-post-run-hook 'daylight-ess-load-initial-R)
(defconst daylight-ess-initial-R
; Load Kodi.R and set the ggplot2 theme to kodi_theme().
"local(
   {if ('net.arfer:Kodi.R' %in% search())
       {e = globalenv()
        while (!identical(attr(e, 'name'), 'net.arfer:Kodi.R'))
            {e = parent.env(e)}}
    else
       {e = new.env()
        suppressPackageStartupMessages(
            source(getOption('Kodi.R.path'), local = e))}
    assign('Daylight.Kodi', pos = 'ESSR', e)
    library(ggplot2)
    theme_set(e$kodi.theme())})")

(defun daylight-ess-load-initial-R ()
  (when daylight-ess
    (ess-eval-linewise
      daylight-ess-initial-R t nil nil t)))

(add-hook 'ess-tracebug-enter-hook (lambda ()
  ; `ess-post-run-hook' runs too late.
  (when daylight-ess
    (ess-process-put 'source-file-function 'daylight-R-source-current-file))))

(defun daylight-R-source-current-file (&optional filename)
; This is derivative of ESS 13.05's `ess--tb-R-source-current-file'.
; Its only purpose (compared to just using `ess--tb-R-source-current-file'
; is to use 'ksource' instead of 'source'.
  (ess-force-buffer-current "R process to use: ")
  (let (
      (proc (get-process ess-local-process-name))
      (file (or filename buffer-file-name)))
    (when (process-get proc 'developer)
      (error "Daylight: ess-developer not supported"))
    (unless file
      (error "Daylight: ESS source buffers must have an associated file"))
    (when (buffer-modified-p)
      (save-buffer))
    (save-selected-window
      (ess-switch-to-ESS t))
    (ess-send-string
      (get-process ess-current-process-name)
      (format "\ninvisible(get('Daylight.Kodi', 'ESSR')$ksource(file = %S))"
        file))))

(defadvice org-babel-R-graphical-output-file (before infer-graphics-results activate)
"Assume ':results graphics' when ':file' is given and has
the file extension of an image."
  (when (and
      daylight-ess
      (assq :file params)
      (string-match ".+\\.\\([^.]+\\)" (cdr (assoc :file params)))
      (assq
        (intern (concat ":" (match-string 1 (cdr (assoc :file params)))))
        org-babel-R-graphics-devices))
    (push "graphics" (cdr (assq :result-params params)))))

(defadvice org-babel-expand-body:R (before print-graphics activate)
"Wrap code in a 'print' when graphics are requested. This is
necessary for ggplot2, for which (unlike, e.g., graphics::plot)
printing only happens automatically at top level."
  (when (and daylight-ess (or graphics-file (org-babel-R-graphical-output-file params)))
    (setq body (format "print({\n%s\n})" body))))

(defadvice org-babel-R-evaluate-session (around use-kodi-eval activate)
"Do two things:
- Wrap the code in 'kodi.eval'. Besides the compartmentalizing
  effect of 'ksource', 'kodi.eval' replaces invisible return values
  with the empty string, to keep Babel from trying to print
  assignments of huge objects.
- Keep Babel from messing up numbers that were formatted at the
  R level. Otherwise, Babel may, e.g., throw away some trailing 0s."
  (if daylight-ess
    (let ((body (format "get('Daylight.Kodi', 'ESSR')$kodi.eval({\n%s\n}, %S)"
        body
        (directory-file-name (file-name-directory (buffer-file-name))))))
      (daylight-aliasing 'org-babel-number-p (lambda (string) nil)
        'ad-do-it))
    ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Citematic integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar daylight-bibgen-path nil)

(defun daylight-translate-bib-link (input)
  (unless (string-match "\\(.+?\\)\\([12][0-9][0-9][0-9]\\)\\(?: \"?\\([^\"]+?\\)\"?\\)?\\'" input)
    (error "Malformed bib link"))
  (let (
      (names (match-string 1 input))
      (year (match-string 2 input))
      (title (match-string 3 input)))
    (setq names (split-string (chomp names) ", \\(?:& \\)?\\| & " t))
    (format "%s::KEY: %s %s%s" (getenv "DAYLIGHT_BIB_PATH")
      (mapconcat 'identity names ", ")
      year
      (if title (format " \"%s\"" title) ""))))

(dolist (linktype '("bib" "bibp"))
  (org-add-link-type linktype
    (lambda (path)
      (org-open-link-from-string (format "[[%s]]" (daylight-translate-bib-link path))))
    (lambda (path desc format)
      (let ((text (or desc
            (org-html-encode-plain-text
              (replace-regexp-in-string " \".+\"" "" path)))))
        (cond
          ((eq format 'html)
            (format "<a class='bibref' href='#bibref--%s'>%s</a>"
              (daylight-escape-url
                (replace-regexp-in-string " +" "_"
                (replace-regexp-in-string "[^-[:alnum:] ]" ""
                path)))
              text))
          (t
            (org-html-encode-plain-text text)))))))

(add-hook 'org-export-before-parsing-hook 'daylight-add-bib)
(defun daylight-add-bib (backend)
  (when (and
      (org-export-derived-backend-p backend 'daylight)
      (save-excursion
        (search-forward-regexp "\\[\\[bibp?:" nil t)))
    (message "Generating bibliography...")
    (daylight-proc-buffer daylight-bibgen-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun daylight-ucfirst (str)
  (format "%c%s" (upcase (elt str 0)) (substring str 1)))

(defun daylight-escape-url (s)
  (replace-regexp-in-string  " " "%20"
    (org-html-encode-plain-text s)))

(defun daylight-proc-on-string (program s &rest args)
  (with-temp-buffer
    (insert s)
    (let ((exit-status (apply 'call-process-region
          (point-min) (point-max) program t t nil args)))
      (unless (zerop exit-status)
        (with-output-to-temp-buffer "*Daylight External Error*"
          (princ (format "Program: %s\n" program))
          (princ (format "Exit status: %d\n" exit-status))
          (princ "\n------------------------------------------------------------\n\n")
          (princ (buffer-substring-no-properties (point-min) (point-max))))
        (error "Program returned nonzero exit status")))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun daylight-proc-region (program start end &rest args)
  (let ((output (apply 'daylight-proc-on-string program
        (buffer-substring-no-properties start end)
        args)))
    (save-excursion
      (goto-char start)
      (delete-region start end)
      (insert output))))

(defun daylight-proc-buffer (program &rest args)
  (apply 'daylight-proc-region program (point-min) (point-max) args))

(defun daylight-buffer-is-daylit ()
  (save-excursion (goto-char (point-min))
    (let ((case-fold-search t))
      (search-forward "\n#+daylight_" nil t))))

(defun daylight-solidify-link-text (s)
  "Like `org-solidify-link-text', but hyphens are considered safe."
  (save-match-data
    (mapconcat 'identity (org-split-string s "[^-a-zA-Z0-9_.-:]+") "-")))

(defun daylight-aliasing (symbol f form)
"Execute FORM with the function cell of SYMBOL temporarily
replaced with F."
  (let ((daylight-aliasing:saved-f (symbol-function symbol)))
    (unwind-protect
      (fset symbol f)
      (eval form))
    (fset symbol daylight-aliasing:saved-f)))
