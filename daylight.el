(provide 'daylight)

(require 'ox)
(require 'ox-html)
(require 'ob-R)
(require 'json)
(require 'url-util)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Backend and end-user functions
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
    (:daylight-now-copyright "DAYLIGHT_NOW_COPYRIGHT" nil nil)
    (:daylight-citation-meta "DAYLIGHT_CITATION_META" nil nil)
    (:daylight-include-meta nil "daylight-include-meta" t)
    (:daylight-show-mdate nil "daylight-show-mdate" t)
    (:daylight-apa nil "daylight-apa" nil t)
    (:daylight-slideshow nil "daylight-slideshow" nil t)))

(defun daylight-export-to-file (file &optional ext-plist)
  (let (
        (exporting-daylight t)
        (org-html-text-markup-alist
        '((bold . "<em>%s</em>")
          (code . "<code>%s</code>")
          (italic . "<i>%s</i>")
          (strike-through . "(RESERVED: %s)")
          (underline . "<var>%s</var>")
          (verbatim . "(RESERVED: %s)")))
        (org-html-viewport nil)
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
;; * Miscellaneous hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'org-mode-hook 'daylight-set-org-table-number-regexp)
(defun daylight-set-org-table-number-regexp ()
  (setq org-table-number-regexp
    "^\\([$<>]?[-−+^.0-9]*[0-9][-−+^.,0-9eEdDx()%:]*\\|[<>]?[-−+]?0[xX][0-9a-fA-F.]+\\|[<>]?[-−+]?[0-9]+#[0-9a-zA-Z.]+\\|nan\\|[-−+u]?inf\\)$"))
      ; This is the same as the default except that leading
      ; dollar signs and thousands separators are also allowed.

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
  (when (bound-and-true-p exporting-daylight)
    (let ((label (org-element-property :label footnote-reference)))
      (setq ad-return-value (concat
        "<footenotelabel " label ">" ad-return-value)))))

(defvar daylight-css-href "https://arfer.net/daylight.css")
(defvar daylight-apa-css-href "https://arfer.net/daylight-apa.css")
(defvar daylight-slideshow-css-href "https://arfer.net/daylight-slideshow.css")

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
    (daylight-proc-on-string "python3" contents
      "-m" "daylight.html_cleanup"
      (json-encode info2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Links
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

(defadvice org-export-get-reference (around use-pretty-names activate)
  (cond
    ((not (bound-and-true-p exporting-daylight))
      ad-do-it)
    ((eq (org-element-type datum) 'target)
      (setq ad-return-value (org-element-property :value datum)))
    ((and (memq (org-element-type datum) '(table paragraph))
        (org-element-property :name datum))
      (setq ad-return-value (org-element-property :name datum)))
    (t
      ad-do-it)))

(org-add-link-type "cls"
  'ignore
  (lambda (path desc format)
    (cond
      ((eq format 'html)
        (format "<span class='%s'>%s</span>"
          (daylight-escape-html path)
          desc))
      ((eq format 'latex)
        (debug path desc))
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
      ((eq format 'latex)
        (cond
          ((string= path "i")
            (format "\\emph{%s}" desc))
          ((string= path "code")
            (format "\\texttt{%s}" desc))
      (t
        ""))))))

(org-add-link-type "var"
  'ignore
  (lambda (path desc format)
    (cond
      ((eq format 'html)
        (format "<var>%s</var>" (daylight-escape-html path)))
      ((eq format 'odt)
        (org-odt-format-fontify (daylight-escape-html path) "Emphasis"))
      ((eq 'format "latex")
        (format "\\emph{%s}" desc))
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
    ; Unescape square brackets that were escaped in daylight.html_cleanup.
    (setq path (replace-regexp-in-string "\ue006" "["
      (replace-regexp-in-string "\ue007" "]" path)))
    (cond
      ((eq format 'html)
        (format "<a class='doi' href='http://dx.doi.org/%s'>doi:%s</a>"
          (daylight-escape-url path)
          (daylight-escape-html path)))
      ((eq format 'odt)
        (daylight-escape-html (concat "doi:" path)))
      ((eq format 'latex)
        (format "\\texttt{doi:%s}" path))
      (t
        ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Math
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'org-export-before-parsing-hook 'daylight-simplify-math)
(defun daylight-simplify-math (backend)
  (when (and
      (org-export-derived-backend-p backend 'daylight)
      (save-excursion
        (goto-char (point-min))
        (search-forward-regexp "\\$\\|\\\\(" nil t)))
          ; This regex matches '$' or '\('.
    (message "Simplifying math...")
    (daylight-proc-buffer "python3" "-m" "daylight.simplify_math")))

(defun daylight-trans-latex-fragment (latex-fragment _ info)
  ; Just format it so daylight-html-cleanup can find it easily.
  (format "<latexfrag>%s</latexfrag>"
    (org-element-property :value latex-fragment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Hy integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This section implements some support for Hy code blocks in
; Babel but also adds make some adjustments to Hy mode and Python
; mode (e.g., adding "C-c l" to load Hy and Python files in their
; own namespace).

(setq daylight-hy-load-command-hy "(import daylight-hy.load) (daylight-hy.load.repl-import \"%s\" None (globals) __name__)\n")
(setq daylight-hy-load-command-py "(import daylight-hy.load) (daylight-hy.load.repl-import \"%s\" None (globals) __name__ \"python\")\n")

(defvar daylight-hy-init-file nil
  "File to load after starting an inferior Hy buffer.")

(defun hy ()
  (interactive)
  (inferior-lisp "hy --repl-output-fn=hy.contrib.hy-repr.hy-repr")
  (rename-buffer "*Hy*")
  (setq inferior-lisp-buffer "*Hy*")
  (when daylight-hy-init-file
    (comint-send-string (get-buffer-process (current-buffer))
      (format daylight-hy-load-command-hy daylight-hy-init-file)))
  (daylight-inferior-hy-setup))

(defun daylight-inferior-hy-setup ()
  (add-to-list 'lisp-source-modes 'hy-mode)
  (setq comint-get-old-input 'comint-get-old-input-default)
    ; This is supposed to keep the prompt "=>" from showing
    ; up as input when cycling through the input history.
  ; By default, inferior-lisp-mode doesn't recognize braces and
  ; square brackets as bracketing characters.
  (modify-syntax-entry ?\{ "(}")
  (modify-syntax-entry ?\} "){")
  (modify-syntax-entry ?\[ "(]")
  (modify-syntax-entry ?\] ")["))

(add-hook 'hy-mode-hook 'daylight-hy-mode-setup)
(defun daylight-hy-mode-setup ()
  (dolist (st (list (syntax-table) font-lock-syntax-table))
    (modify-syntax-entry ?| "_" st))
    ; However font-lock-syntax-table is normally set for Lisp,
    ; it seems to demand that ?| remain a quoting character.
  (setq inferior-lisp-load-command daylight-hy-load-command-hy)
  (define-key hy-mode-map "\C-cl" 'daylight-hy-load-buffer))

(defun daylight-hy-load-buffer ()
  (interactive)
  (save-current-buffer
    (unless (comint-check-proc "*Hy*")
      (hy)))
  (lisp-load-file (buffer-file-name)))

(add-hook 'python-mode-hook 'daylight-py-to-hy-setup)
(defun daylight-py-to-hy-setup ()
  (setq inferior-lisp-load-command daylight-hy-load-command-py)
  (define-key python-mode-map "\C-cl" 'daylight-hy-load-buffer))

(add-to-list 'org-babel-tangle-lang-exts '("hy" . "hy"))

(defvar org-babel-default-header-args:hy '(
  (:width . 800)
  (:height . 500)
  (:exports . "both")))

(defvar daylight-hy-buffers '((:default . "*Hy*")))

(defvar daylight-hy-image-file-output-fmt "
  (import [matplotlib.pyplot :as plt] daylight-hy.babel)
  (plt.ioff)
  (plt.figure \"daylight-file-output\"
    :figsize (, (/ %s 100) (/ %s 100)))
  %s
  (plt.savefig %S :bbox-inches \"tight\" :pad_inches .05)
  (plt.close \"daylight-file-output\")
  (plt.ion)")

(defun org-babel-execute:hy (body params)
"Execute a block of Hy code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* (
      (processed-params (org-babel-process-params params))
      (session (daylight-hy-initiate-session (cdr (assq :session processed-params))))
      ;(vars ...)   ; Not implemented.
      (result-params (cdr (assq :result-params processed-params)))
      (result-type (cdr (assq :result-type processed-params)))
      (full-body (org-babel-expand-body:hy
        body params processed-params))
      (session-buffer (get-buffer (cdr
        (assq session daylight-hy-buffers))))
      tmp-file)
    (when (and (assq :file processed-params) (cdr (assq :file processed-params)))
      (setq result-type 'matplotlib))
    (unless (or (eq result-type 'value) (eq result-type 'matplotlib))
      (error "Unimplemented :results type."))
    (process-send-string (get-buffer-process session-buffer) "[\"\"]\n")
      ; This should ensure that if the code block fails, '_' will
      ; be left as [""], so the RESULTS will be blank, alerting
      ; the user that something's gone wrong.
    (when (eq result-type 'matplotlib)
      (setq full-body (format daylight-hy-image-file-output-fmt
        (cdr (assq :width processed-params))
        (cdr (assq :height processed-params))
        full-body
        (cdr (assq :file processed-params)))))
    (setq full-body (format "(import daylight-hy.load) [(daylight-hy.load.repl-import %S '(do %s\n) (globals) __name__)]\n"
      ; The square brackets serve to keep the Hy REPL from hanging
      ; if the user forgot a close-parenthesis.
      (buffer-file-name)
      full-body))
    (process-send-string (get-buffer-process session-buffer)
      full-body)
    (setq tmp-file (org-babel-temp-file "hy-"))
    (org-babel-comint-eval-invisibly-and-wait-for-file
      session-buffer
      tmp-file
      (format
        (if (eq result-type 'matplotlib)
          "(with [o (open %S \"w\")] (.write o \"done\\n\"))"
          "(do (import daylight-hy.babel codecs) (with [o (codecs.open %S \"w\" \"UTF-8\")] (.write o (daylight-hy.babel.to-el (get _ 0)))))")
        (org-babel-process-file-name tmp-file t)))
    (if (eq result-type 'matplotlib)
      nil
       ; If we return something other than nil,
       ; org-babel-execute-src-block will write it to the file,
       ; clobbering the image we just made.
      (read (org-babel-eval-read-file tmp-file)))))
        ; Debugging tip: use M-: (org-table-to-lisp) to
        ; investigate table structure.

(defun org-babel-expand-body:hy (body params &optional processed-params)
  "Expand BODY according to PARAMS. Return the expanded body.

Currently, no expansion is implemented, so this is a no-op."
  body)

(defun daylight-hy-initiate-session (&optional session)
  "If there isn't a current inferior-process-buffer in SESSION, then create one.
Return the initialized session.

Currently, only a single session is supported."
  (unless (get-buffer "*Hy*")
    (save-current-buffer
      (hy)))
  :default)

(defadvice orgtbl-to-orgtbl (around FOO activate)
  ; This adds :raw t to the arguments of `orgtbl-to-generic'
  ; because otherwise, links inside cells will get thrown out.
  (if (daylight-buffer-is-daylit)
    (progn
      (require 'ox-org)
      (setq ad-return-value (orgtbl-to-generic table (org-combine-plists params (list :backend 'org :raw t)))))
    ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * R code blocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar daylight-ess nil)

(add-hook 'org-mode-hook 'daylight-set-babel-default)
(defun daylight-set-babel-default ()
  (when (daylight-buffer-is-daylit)
    (make-local-variable 'org-babel-default-header-args:R)
    (daylight-update-alist 'org-babel-default-header-args:R '(
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

(defadvice org-babel-execute:R (before infer-graphics-results activate)
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

(defadvice org-babel-R-evaluate-session (around use-kodi-eval activate)
"Do two things:
- Keep Babel from messing up numbers that were formatted at the
  R level. Otherwise, Babel may, e.g., throw away some leading 0s.
- For output, use org.write.table (from Kodi.R) instead of
  write.table."
  (if daylight-ess
    (let ((org-babel-R-write-object-command
        (replace-regexp-in-string
          "\\bwrite\\.table("
          "get('Daylight.Kodi', 'ESSR')$org.write.table("
          org-babel-R-write-object-command
          t t)))
      (daylight-aliasing 'org-babel--string-to-number (lambda (string) nil)
        '(daylight-aliasing 'org-babel-string-read (lambda (cell) (org-babel-read cell t))
          'ad-do-it)))
    ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Citematic integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
          ((eq format 'latex)
            (format "\\cite{%s}"
              (replace-regexp-in-string "[&, ]" "" path)))
          (t
            (daylight-escape-html text)))))))

(add-hook 'org-export-before-parsing-hook 'daylight-add-bib)
(defun daylight-add-bib (backend)
  (when (and
      (org-export-derived-backend-p backend 'daylight)
      (save-excursion
        (search-forward-regexp "\\[\\[bibp?:" nil t)))
    (message "Generating bibliography...")
    (daylight-proc-buffer "python3" "-m" "daylight.bibgen"
      (format "%s" (plist-get ext-plist :daylight-apa)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; * Utilities
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
