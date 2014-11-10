(provide 'daylight)

(require 'ox)
(require 'ox-html)
(require 'json)
(require 'url-util)

(defvar daylight-css-href "http://arfer.net/daylight.css")
(defvar daylight-apa-css-href "http://arfer.net/daylight-apa.css")
(defvar daylight-slideshow-css-href "http://arfer.net/daylight-slideshow.css")

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
    (:daylight-license-url "DAYLIGHT_LICENSE" nil nil)
    (:daylight-include-meta nil "daylight-include-meta" t)
    (:daylight-apa nil "daylight-apa" nil t)
    (:daylight-slideshow nil "daylight-slideshow" nil t)))

(defun daylight-export-to-file (file &optional ext-plist)
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
    '(org-export-to-file 'daylight file nil nil nil nil (append
      ext-plist
      (list
        :html-doctype "html5"
        :time-stamp-file nil
        :html-head-include-default-style nil
        :html-head-include-scripts nil
        :html-html5-fancy t
        :section-numbers nil
        :headline-levels 5
        :with-latex 'daylight
          ; Most of the effect of this option is negated by our
          ; LaTeX processing, but we set it to something to keep
          ; ox-html from inserting the MathJax JavaScript.
        :html-postamble nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Miscellaneous hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'org-mode-hook 'daylight-set-org-table-number-regexp)
(defun daylight-set-org-table-number-regexp ()
  (setq org-table-number-regexp
    "^\\([$<>]?[-+^.0-9]*[0-9][-+^.0-9eEdDx()%:]*\\|[<>]?[-+]?0[xX][0-9a-fA-F.]+\\|[<>]?[-+]?[0-9]+#[0-9a-zA-Z.]+\\|nan\\|[-+u]?inf\\)$"))
      ; This is the same as the default except that leading
      ; dollar signs are also allowed.

(add-hook 'org-export-before-processing-hook 'daylight-strip-comments)
(defun daylight-strip-comments (backend)
"Remove '# …'-style comments. The intended effect is to allow
such comments in the middle of paragraphs (otherwise, they split
the paragraph into two)."
  (when (org-export-derived-backend-p backend 'daylight)
    (save-excursion
      (goto-char (point-min))
      (let (in-src (case-fold-search t))
        (while (progn (forward-line 1) (not (eobp)))
          (cond
            ((looking-at "#\\+BEGIN_SRC ")
              (setq in-src t))
            ((looking-at "#\\+END_SRC")
              (setq in-src nil))
            ((and (looking-at "# ") (not in-src))
              (while (not (or (eobp) (= (char-after) ?\n)))
                (delete-char 1))
              (unless (eobp)
                (delete-char 1)
                (forward-line -1)))))))))
                  ; The last forward-line is to compensate for
                  ; the loop and how we just deleted a line.

(defvar daylight-postproc nil)
(add-hook 'org-export-before-processing-hook 'daylight-get-postproc)
(defun daylight-get-postproc (backend)
; Save and remove the POSTPROC block, if there is one.
  (when (org-export-derived-backend-p backend 'daylight)
    (save-excursion
      (goto-char (point-min))
      (setq daylight-postproc (and
        (search-forward "\n* POSTPROC\n" nil t)
        (progn
          (backward-char)
          (search-forward "\n#+BEGIN_SRC python\n" nil t))
        (buffer-substring-no-properties (point) (progn
          (search-forward "\n#+END_SRC\n")
          (forward-line -1)
          (backward-char)
          (point)))))
      (when daylight-postproc
        (delete-region
          (progn (outline-back-to-heading t) (point))
          (progn (outline-next-heading) (point)))))))

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

(defadvice org-html-footnote-reference (after FOO activate)
; Prepend to each footnote reference some fake HTML with the
; footnote label, so daylight-html-cleanup can replace numeric
; footnote IDs with labels.
  (if (org-export-derived-backend-p backend 'daylight)
    (let ((label (org-element-property :label footnote-reference)))
      (setq ad-return-value (concat
        "<footenotelabel " label ">" ad-return-value)))))

(defadvice org-html-template (before add-to-html-head activate)
  (when (daylight-buffer-is-daylit)
    (plist-put info :html-head (concat
      (plist-get ext-plist :html-head)
      (and daylight-css-href (format
        "\n<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\">"
        (daylight-escape-html daylight-css-href)))
      (and daylight-apa-css-href (plist-get info :daylight-apa) (format
        "\n<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\">"
        (daylight-escape-html daylight-apa-css-href)))
      (and daylight-slideshow-css-href (plist-get info :daylight-slideshow) (format
        "\n<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\">"
        (daylight-escape-html daylight-slideshow-css-href)))))))

(defvar daylight-html-cleanup-path nil)
(defun daylight-final-function (contents backend info)
  (message "Cleaning up...")
  (let (info2 plist k v)
    ; `info2' will be `info' (eventually, encoded in JSON), but
    ; only the parts thereof with simple values (there's no need
    ; to send, e.g., the whole parse tree).
    (setq plist info)
    (while plist
      (setq k (car plist)
        v (cadr plist)
        plist (cddr plist))
      (when (or (stringp v) (numberp v) (null v) (eq v t))
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
    (push (cons :postproc daylight-postproc) info2)
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
          (daylight-escape-html
            (replace-regexp-in-string "\\`./" "" raw-link))
          desc))
      ((and (not desc) (member type '("http" "https" "ftp" "mailto")))
        ; It's a literal URL. Give it a special class.
        (format "<a class=\"url\" href=\"%s\">%s</a>"
          (daylight-escape-html raw-link)
          (daylight-escape-html raw-link)))
      (t
        ; Pass the buck to `org-html-link'.
        (let ((output (org-html-link link desc info)))
          (if (string-match "\\`<i>" output)
            (error "Couldn't translate link: %s / %s" raw-link (or desc "[nil]"))
            output))))))

(org-add-link-type "cls"
  'ignore
  (lambda (path desc format)
    (cond
      ((eq format 'html)
        (format "<span class='%s'>%s</span>"
          (daylight-escape-html path)
          desc))
      (t
        ""))))

(org-add-link-type "m"
; Markup applied to the description. The path specifies the
; kind of markup.
  'ignore
  (lambda (path desc format)
    (unless (member path '("i" "code"))
      (error "'m' link: Unknown type %S" path))
    (cond
      ((eq format 'html)
        (format "<%s class='mcolon'>%s</%s>" path desc path))
          ; The class is currently necessary to distinguish m:i
          ; from the failure mode of `org-html-link'.
      ((eq format 'odt)
        (org-odt-format-fontify (daylight-escape-html path) "Emphasis"))
      (t
        ""))))

(org-add-link-type "var"
  'ignore
  (lambda (path desc format)
    (cond
      ((eq format 'html)
        (format "<var>%s</var>" (daylight-escape-html path)))
      ((eq format 'odt)
        (org-odt-format-fontify (daylight-escape-html path) "Emphasis"))
      (t
        ""))))

(defun daylight-translate-wp-link (input)
  (format "http://en.wikipedia.org/wiki/%s"
    (daylight-hexify-string (daylight-ucfirst
      (replace-regexp-in-string " " "_" input)))))

(org-add-link-type "wp"
  (lambda (path)
    (browse-url (daylight-translate-wp-link path)))
  (lambda (path desc format)
    (cond
      ((eq format 'html)
        (format "<a class='wikipedia' title='Wikipedia: %s' href='%s'>%s</a>"
          (daylight-escape-html (daylight-ucfirst path))
          (daylight-escape-html (daylight-translate-wp-link path))
          (or desc (daylight-escape-html path))))
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
          (daylight-escape-html path)))
      ((eq format 'odt)
        (daylight-escape-html (concat "doi:" path)))
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
    (daylight-update-alist 'org-babel-default-header-args '(    
      (:session . "*R*")
      (:colnames . "yes")
      (:rownames . "yes")
      (:width . 800)
      (:height . 500)
      (:exports . "both")))))

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

(defadvice org-babel-execute:R (before auto-implies-silent activate)
"Make ':auto t' imply ':results silent'."
  (when (and
      daylight-ess
      (assoc :auto params)
      (cdr (assoc :auto params)))
    (setcar (cdr (assoc :result-params params)) "silent")))

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
"Do three things:
- Wrap the code in 'kodi.eval'. Besides the compartmentalizing
  effect of 'ksource', 'kodi.eval' replaces invisible return values
  with the empty string, to keep Babel from trying to print
  assignments of huge objects.
- Keep Babel from messing up numbers that were formatted at the
  R level. Otherwise, Babel may, e.g., throw away some trailing 0s.
- For output, use org.write.table (from Kodi.R) instead of
  write.table."
  (if daylight-ess
    (let (
        (body (format "get('Daylight.Kodi', 'ESSR')$kodi.eval({\n%s\n}, %S)"
          body
          (directory-file-name (file-name-directory (buffer-file-name)))))
        (org-babel-R-write-object-command
          (replace-regexp-in-string
            "\\bwrite\\.table("
            "get('Daylight.Kodi', 'ESSR')$org.write.table("
            org-babel-R-write-object-command
            t t)))
      (daylight-aliasing 'org-babel-number-p (lambda (string) nil)
        'ad-do-it))
    ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Citematic integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar daylight-bibgen-path nil)

(defun daylight-translate-bib-link (input)
  (setq input (substring-no-properties input))
  (let (key names year title)
    (setq key input)
    (when (string-match "\\(.+?\\)\\([12][0-9][0-9][0-9]\\)\\(?: \"?\\([^\"]+?\\)\"?\\)?\\'" input)
      (setq names (match-string 1 input))
      (setq year (match-string 2 input))
      (setq title (match-string 3 input))
      (setq names (split-string (chomp names) ", \\(?:& \\)?\\| & " t))
      (setq key (format "%s %s%s"
        (mapconcat 'identity names ", ")
        year
        (if title (format " \"%s\"" title) ""))))
    (setq key (concat "KEY: " key))
    (find-file (getenv "DAYLIGHT_BIB_PATH"))
    (goto-char (point-min))
    (search-forward key)))

(dolist (linktype '("bib" "bibp"))
  (org-add-link-type linktype
    'daylight-translate-bib-link
    (lambda (path desc format)
      (let ((text (or desc
            (daylight-escape-html
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
            (daylight-escape-html text)))))))

(add-hook 'org-export-before-parsing-hook 'daylight-add-bib)
(defun daylight-add-bib (backend)
  (when (and
      (org-export-derived-backend-p backend 'daylight)
      (save-excursion
        (search-forward-regexp "\\[\\[bibp?:" nil t)))
    (message "Generating bibliography...")
    (daylight-proc-buffer daylight-bibgen-path
      (format "%s" (plist-get ext-plist :daylight-apa)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun daylight-ucfirst (str)
  (format "%c%s" (upcase (elt str 0)) (substring str 1)))

(defun daylight-escape-html (s)
  (dolist (pair
        '(
          ("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;")
          ("\"" . "&quot;") ("'" . "&#39;"))
        s)
    (setq s (replace-regexp-in-string (car pair) (cdr pair) s t t))))

(defconst daylight-url-unreserved-chars
  (cons ?/ url-unreserved-chars))
(defun daylight-hexify-string (s)
"Like `url-hexify-string', but doesn't hexify slashes." 
  (let ((url-unreserved-chars daylight-url-unreserved-chars))
    (url-hexify-string s)))

(defun daylight-escape-url (s)
  (daylight-escape-html (daylight-hexify-string s)))

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
  "Allow all but a few characters."
  (save-match-data
    (mapconcat 'identity (org-split-string s "[\"'&<>#?]+") "-")))

(defun daylight-update-alist (symbol updates)
"Each supplied cons cell is added to the alist in SYMBOL.
If the alist already has a cell for the given key,
its cdr is replaced with the cdr of the updated cell."
  (dolist (x updates)
    (let ((cell (assoc (car x) (symbol-value symbol))))
      (if cell
        (setcdr cell (cdr x))
        (set symbol (cons x (symbol-value symbol)))))))

(defun daylight-aliasing (symbol f form)
"Execute FORM with the function cell of SYMBOL temporarily
replaced with F."
  (let ((daylight-aliasing:saved-f (symbol-function symbol)))
    (unwind-protect
      (fset symbol f)
      (eval form))
    (fset symbol daylight-aliasing:saved-f)))
