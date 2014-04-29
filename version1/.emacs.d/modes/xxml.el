;;; xxml.el --- Supplementary tools for handling SGML and HTML.

;; Copyright © 1999, 2000, 2001, 2002 Progiciels Bourbeau-Pinard inc.

;; Author: François Pinard <pinard@iro.umontreal.ca>.
;; Created: 1999-05-25.

;; This is GPL'ed code.  Big copyright block omitted! :-)

(defconst xxml-version "2002-11-08 10:36")

;; See `http://www.iro.umontreal.ca/~pinard/xxml/' for documentation on
;; installation and usage of this package.  The `README' file within the
;; distribution, at `http://www.iro.umontreal.ca/~pinard/xxml/xxml.tar.gz',
;; also holds the same information, but as plain text.  I intend to keep
;; only internal documentation here, and move the rest into the given
;; references.  The idea of removing duplication is to ease maintenance.

;;; Avoid byte compilation warnings.

;; ... font-lock

(defvar font-lock-mode)
(defvar lazy-lock-mode)

(eval-when-compile
  (autoload 'font-lock-append-text-property "font-lock")
  (autoload 'font-lock-fontify-region "font-lock"))

;; ... PSGML

(defvar sgml-indent-step)
(defvar sgml-mode-map)
(defvar sgml-set-face)

(eval-when-compile
  (autoload 'sgml-bpos-p "psgml")
  (autoload 'sgml-change-start-tag "psgml")
  (autoload 'sgml-element-attribute-specification-list "psgml")
  (autoload 'sgml-element-end "psgml")
  (autoload 'sgml-element-etag-start "psgml")
  (autoload 'sgml-element-gi "psgml")
  (autoload 'sgml-element-stag-end "psgml")
  (autoload 'sgml-element-stag-epos "psgml")
  (autoload 'sgml-element-start "psgml")
  (autoload 'sgml-find-attribute-element "psgml")
  (autoload 'sgml-find-element-of "psgml")
  (autoload 'sgml-indent-line "psgml")
  (autoload 'sgml-parse-prolog "psgml")
  (autoload 'sgml-save-options "psgml")
  (autoload 'sgml-top-element "psgml"))

;; ... xxml

(defvar xxml-rug-overlay)
(defvar xxml-sparkle-overlay)

;;; Overall options.

(defvar xxml-indent-step nil
  "If not nil, override `sgml-indent-step' while indenting.
Best is to rather set `sgml-indent-step' and leave this one to nil.")

(defvar xxml-transform-tag-id nil
  "Normalize case for tag names.  Values are nil, upcase or downcase.")

(defvar xxml-transform-attribute-id nil
  "Normalize case for attribute names.  Values are nil, upcase or downcase.")

(defvar xxml-reorder-attributes nil
  "Normalize tag attributes into some canonical order.")

(defvar xxml-normalize-quotes nil
  "Eliminate or add quotes around attribute values in a consistent manner.")

(defvar xxml-default-prolog nil
  "In SGML buffers lacking a declaration, insert this one.")

(defvar xxml-html-mode-hook nil
  "Hooks to be run once HTML mode has been established.")

;; Properties of specific tags, and convenience macros to access them.

(defvar xxml-highlight-tag-alist nil
  "Association list relating tag strings to face for the tag itself.")

(defvar xxml-highlight-initial-alist nil
  "Association list relating tag strings to face for initial text.")

(defvar xxml-highlight-recursive-alist nil
  "Association list relating tag strings to face for recursive contents.")

(defvar xxml-all-tags-are-indentable-inside nil
  "If whitespace may be used freely between tags and embedded text.")
(defvar xxml-indentable-inside-tags nil
  "List of tag symbols allowing whitespace around embedded text.")
(defsubst is-indentable-inside (tag)
  "If whitespace may be freely changed after start TAG or before end TAG."
  (or xxml-all-tags-are-indentable-inside
      (memq tag xxml-indentable-inside-tags)))

(defvar xxml-all-tags-are-indentable-outside nil
  "If whitespace may be used freely between tags and surrounding tags.")
(defvar xxml-indentable-outside-tags nil
  "List of tag symbols allowing whitespace around the element they delimit.")
(defsubst is-indentable-outside (tag)
  "If whitespace may be freely changed before start TAG or after end TAG."
  (or xxml-all-tags-are-indentable-outside
      (memq tag xxml-indentable-outside-tags)))

(defvar xxml-all-tags-are-splittable-after nil
  "If an end of line may be added or removed after any tag.")
(defvar xxml-splittable-after-tags nil
  "List of tag symbols ignoring end of line immediately after tag.")
(defsubst is-splittable-after (tag)
  "If new line following start or end of TAG may be removed."
  (or xxml-all-tags-are-splittable-after
      (memq tag xxml-splittable-after-tags)))

(defvar xxml-all-tags-are-splittable-before nil
  "If an end of line may be added or removed before any tag.")
(defvar xxml-splittable-before-tags nil
  "List of tag symbols ignoring end of line immediately before tag.")
(defsubst is-splittable-before (tag)
  "If new line following start or end TAG may be removed."
  (or xxml-all-tags-are-splittable-before
      (memq tag xxml-splittable-before-tags)))

(defvar xxml-all-tags-are-shrink-wrappable nil
  "Prefix and suffix whitespace is better squeezed and moved out of element.")
(defvar xxml-shrink-wrappable-tags nil
  "List of tags symbols for which prefix and suffix should be squeezed out.")
(defsubst is-shrink-wrappable (tag)
  "If prefix and suffix whitespace should be squeezed and moved out of element."
  (or xxml-all-tags-are-shrink-wrappable
      (memq tag xxml-shrink-wrappable-tags)))

(defvar xxml-all-contents-are-literal nil
  "Prevent any refilling of character data between tags.")
(defvar xxml-literal-contents-tags nil
  "List of tags symbols for which embedded text may not be reformatted.")
(defsubst is-literal-contents (tag)
  "If the tag preserves character data from being refilled."
  (memq tag xxml-literal-contents-tags))

(defvar xxml-break-tags nil
  "Tags after which a line break must occur.")
(defsubst is-break (tag)
  "When tag should be kept last on its line."
  (memq tag xxml-break-tags))

(defvar xxml-forbidden-end-tags nil
  "List of elements for which the end tag shall be omitted.")
(defsubst is-forbidden-end (tag)
  "When end tags should be removed."
  (memq tag xxml-forbidden-end-tags))

;; HTML highlighting.

(defvar xxml-html-highlight-tag-alist
  '(;; Text elements - form-fields
    ("form" . xxml-interaction-face)
    ("input" . xxml-interaction-face)
    ("option" . xxml-interaction-face)
    ("select" . xxml-interaction-face)
    ("textarea" . xxml-interaction-face))
  "Association list between HTML tag strings and faces to highlight tags.")

(defvar xxml-html-highlight-initial-alist nil
  "Association list between HTML tag strings and faces to highlight titles.")

(defvar xxml-html-highlight-recursive-alist
  '(;; Block elements.
    ("title" . xxml-header-1-face)
    ("h1" . xxml-header-1-face)
    ("h2" . xxml-header-2-face)
    ("h3" . xxml-header-3-face)
    ("h4" . xxml-header-4-face)
    ("h5" . xxml-header-4-face)
    ("h6" . xxml-header-4-face)
    ;; Text elements - font style
    ("b" . bold)
    ("big" . bold)
    ("i" . italic)
    ("small" . default)
    ("tt" . default)
    ("u" . xxml-emph-2-face)
    ;; Text elements - phrase
    ("cite" . italic)
    ("code" . bold)
    ("em" . xxml-emph-1-face)
    ("var" . bold-italic)
    ("strong" . bold)
    ;; Text elements - special
    ("a" . underline)
    ;; Text elements - unsorted
    ("blink" . highlight)
    ("s" . font-lock-warning-face)
    ("rev" . modeline))
  "Association list between HTML tag strings and faces to highlight contents.")

;;; Entertaining and debugging.

(defvar xxml-rug 15
  "If not nil, highlight the element which was selected around point.
If a number, then how much milliseconds to pause after highlight.")

(defvar xxml-sparkle nil
  "If not nil, highlight detailed spots in areas while working on them.
If a number, then how much milliseconds to pause after highlight.")

(defvar xxml-rug-face nil
  "Face used to show the whole element region being operated upon.")

(defvar xxml-sparkle-face nil
  "Face used to debug the running hot spots within the element region.")

(when window-system
  (copy-face 'default 'xxml-rug-face)
  (set-face-background 'xxml-rug-face "cyan")
  (copy-face 'default 'xxml-sparkle-face)
  (set-face-background 'xxml-sparkle-face "yellow"))

(defun xxml-show-rug (start end)
  (when xxml-rug
    (move-overlay xxml-rug-overlay start end)
    (if (numberp xxml-rug)
	(sit-for 0 xxml-rug)
      (sit-for 0))))

(defun xxml-unshow-rug ()
  (when xxml-rug
    (delete-overlay xxml-rug-overlay)))

(defun xxml-show-sparkle (start end)
  (when xxml-sparkle
    (move-overlay xxml-sparkle-overlay start end)
    (if (numberp xxml-sparkle)
	(sit-for 0 xxml-sparkle)
      (sit-for 0))))

(defun xxml-unshow-sparkle ()
  (when xxml-sparkle
    (delete-overlay xxml-sparkle-overlay)))

(defun xxml-show-sparkle-match ()
  (xxml-show-sparkle (match-beginning 0) (match-end 0)))

;; Shamelessly adapted from `progmodes/cc-cmds.el'.

(defvar xxml-progress-action "processing region"
  "Description of action to insert into progress reports.")

(defvar xxml-progress-interval 1
  "How many seconds between progress reports while indentation goes.
Use nil to silence all progress.")

;; For progress reporting, a vector of [START END LAST]
(defvar xxml-progress-info nil)

(defun xxml-progress-init (start end)
  "Issue first diagnostic of progress information."
  (when xxml-progress-interval
    (if (not (fboundp 'current-time))
	(message "%s... (this may take a while)" xxml-progress-action)
      (setq xxml-progress-info
	    (vector start
		    (save-excursion (goto-char end) (point-marker))
		    (nth 1 (current-time))))
      (message "%s..." xxml-progress-action))))

(defun xxml-progress-update ()
  "Possibly issue progress information."
  (when (and xxml-progress-interval xxml-progress-info)
    (let ((now (nth 1 (current-time)))
	  (start (aref xxml-progress-info 0))
	  (end (aref xxml-progress-info 1))
	  (last (aref xxml-progress-info 2)))
      (when (< xxml-progress-interval (- now last))
	(message "%s... (%d%% complete)" xxml-progress-action
		 (/ (* 100 (- (point) start)) (- end start)))
	(aset xxml-progress-info 2 now)))))

(defun xxml-progress-complete ()
  "Issue last diagnostic of progress information."
  (when xxml-progress-interval
    (set-marker (aref xxml-progress-info 1) nil)
    (setq xxml-progress-info nil)
    (message "%s...done" xxml-progress-action)))

;;; Setup code and SGML mode interface (assumes `PSGML').

(defun xxml-version ()
  "Identifies the current version of this module."
  (interactive)
  (if (interactive-p)
      (message xxml-version)
    xxml-version))

;; This is a convenience function.  `sgml-mode' is defined within PSGML, and
;; it will activate `xxml' through its `sgml-mode-hook'.  However, `html-mode'
;; is not defined within PSGML, so we do the appropriate thing here instead.
(defun html-mode ()
  "Edit this buffer with PSGML, with some XXML specialities for HTML."
  (interactive)
  (sgml-mode)
  (xxml-setup-for-html)
  (run-hooks 'html-mode-hook)
  ;; If there is a `mode: sgml' file local variable, SGML mode is going to be
  ;; re-initialized a second time, so loosing anything we just did.  Rather,
  ;; edit the value into `mode: html', so ensure things are done right, even
  ;; if done once too much.
  (save-excursion
    (goto-char (point-max))
    (when (search-backward "\nmode: sgml\n" (- (point-max) 1000) t)
      (replace-match "\nmode: html\n"))))

(defmacro xxml-setq-local (&rest arguments)
  "Ensure VARIABLE is buffer local, then set it to VALUE if still unset.
Arguments are VARIABLE-1 VALUE-1 VARIABLE-2 VALUE-2..."
  ;; FIXME: No test is made about if it is unset or not.
  (let ((counter (length arguments))
	fragments)
    (when (>= counter 2)
      (setq fragments (cons `(set (make-local-variable ',(car arguments))
				  ,(cadr arguments))
			    fragments)
	    arguments (cddr arguments)
	    counter (- counter 2)))
    (cons 'progn (nreverse fragments))))

(defun xxml-mode-routine ()
  "To be run as a hook at mode entry."
  ;; Install reformatting facilities.
  (define-key sgml-mode-map "\M-_" 'xxml-unbreakable-space)
  (define-key sgml-mode-map "\M-q" 'xxml-fill-element)
  (define-key sgml-mode-map "\M-\C-q" 'xxml-indent-element)
  ;; For debugging.
  (xxml-setq-local xxml-rug-overlay (make-overlay (point) (point)))
  (overlay-put xxml-rug-overlay 'face 'xxml-rug-face)
  (delete-overlay xxml-rug-overlay)
  (xxml-setq-local xxml-sparkle-overlay (make-overlay (point) (point)))
  (overlay-put xxml-sparkle-overlay 'face 'xxml-sparkle-face)
  (delete-overlay xxml-sparkle-overlay)
  ;; Let us highlight our own way.
  (xxml-setq-local font-lock-defaults '(xxml-font-lock-keywords t))
  (setq sgml-set-face nil)
  ;; Allow TAB to work everywhere.  I'm not fully sure this is a good idea.
  (xxml-setq-local sgml-indent-data t)
  ;; Preset formatting options for no specific DTD.
  (xxml-setq-local xxml-all-contents-are-literal t
		   xxml-all-tags-are-indentable-inside nil
		   xxml-all-tags-are-indentable-outside nil
		   xxml-all-tags-are-shrink-wrappable nil
		   xxml-all-tags-are-splittable-after nil
		   xxml-all-tags-are-splittable-before nil
		   xxml-break-tags nil
		   xxml-default-prolog nil
		   xxml-forbidden-end-tags nil
		   xxml-highlight-initial-alist nil
		   xxml-highlight-recursive-alist nil
		   xxml-highlight-tag-alist nil
		   xxml-indent-step 1
		   xxml-indentable-inside-tags nil
		   xxml-indentable-outside-tags nil
		   xxml-literal-contents-tags nil
		   xxml-normalize-quotes nil
		   xxml-reorder-attributes nil
		   xxml-shrink-wrappable-tags nil
		   xxml-splittable-after-tags nil
		   xxml-splittable-before-tags nil
		   xxml-transform-attribute-id nil
		   xxml-transform-tag-id nil))

(defun xxml-setup-for-html ()
  "Preset options for HTML.  Tuned for 3.2."
  (setq xxml-default-prolog
	"<!doctype HTML public \"-//W3C//DTD HTML 3.2//EN\">\n")
  (setq xxml-all-contents-are-literal nil
	xxml-all-tags-are-indentable-inside nil
	xxml-all-tags-are-indentable-outside nil
	xxml-all-tags-are-shrink-wrappable t
	xxml-all-tags-are-splittable-after nil
	xxml-all-tags-are-splittable-before nil
	xxml-highlight-initial-alist xxml-html-highlight-initial-alist
	xxml-highlight-recursive-alist xxml-html-highlight-recursive-alist
	xxml-highlight-tag-alist xxml-html-highlight-tag-alist
	xxml-indent-step 1
	xxml-normalize-quotes t
	xxml-reorder-attributes nil
	xxml-transform-attribute-id 'downcase
	xxml-transform-tag-id 'downcase)
  (let ((structure '(html head body))
	(no-end '(base br hr img input isindex link meta))
	;; The following is for head.
	(head '(title isindex base script style meta link))
	;; The following are for body.
	(heading '(h1 h2 h3 h4 h5 h6))
	(address '(address))
	(block-level '(blockquote center dir div dl form hr isindex menu ol
				  p pre table ul))
	(text-level (let ((font-style '(b big i small strike sub sup tt u))
			  (phrase '(em strong dfn code samp kbd var cite))
			  (form-fields '(input option select textarea))
			  (special '(a applet basefont br font img map)))
		      (append font-style phrase form-fields special))))
    ;; Given the above, preset tag lists.
    (setq xxml-indentable-inside-tags
	  (append structure head heading address no-end
		  '(dl ol ul dt dd li)))
    (setq xxml-indentable-outside-tags
	  (append structure head heading address block-level
		  '(applet caption img option li td th tr)))
    (setq xxml-break-tags '(br hr)
	  xxml-forbidden-end-tags no-end
	  xxml-literal-contents-tags '(pre)
	  xxml-splittable-after-tags nil
	  xxml-splittable-before-tags nil
	  xxml-shrink-wrappable-tags text-level))
  (xxml-refontify-region (point-min) (point-max)))

(defun xxml-unbreakable-space ()
  (interactive)
  (insert "\240"))

(defun xxml-find-element-around-cursor ()
  "Return which SGML element is near or around position of point.
If before a start tag, not considering whitespace, then select the element
starting from that tag.  Otherwise, the smallest enclosing element containing
the cursor is selected.  Near beginning or end of buffer, select top element."
  (let ((top (sgml-top-element)))
    (if (or (< (point) (sgml-element-start top))
	    (> (point) (sgml-element-end top)))
	top
      (save-excursion
	(skip-chars-forward " \t\n")
	(sgml-find-element-of (point))))))

(defun xxml-refontify-region (start end)
  "Ensure that fontification gets recomputed between START and END."
  (when (and (boundp 'font-lock-mode) font-lock-mode)
    (if (and (boundp 'lazy-lock-mode) lazy-lock-mode)
	(add-text-properties start end '(lazy-lock nil))
      (font-lock-fontify-region start end))))

;;; Font lock style highlighting.

(defvar xxml-sgml-delimiter-face 'font-lock-function-name-face
  "Face to use for SGML angle brackets delimiters, and compound delimiters.")

(defvar xxml-sgml-comment-face 'font-lock-comment-face
  "Face to use for SGML comments, excluding comment delimiters.")

(defvar xxml-sgml-opening-tag-face 'font-lock-function-name-face
  "Face to use for the tag name within an opening tag.")

(defvar xxml-sgml-attribute-name-face 'font-lock-variable-name-face
  "Face to use for an attribute name within an opening tag.")

(defvar xxml-sgml-attribute-value-face 'font-lock-constant-face
  "Face to use for an attribute value within an opening tag.")

(defvar xxml-sgml-closing-tag-face 'font-lock-builtin-face
  "Face to use for the slash and tag name within a closing tag.")

(defvar xxml-sgml-character-entity-face 'font-lock-string-face
  "Face to use for explicit SGML character entities within text.")

(defconst xxml-font-lock-keywords
  '(("\\(<!--\\)\\([^>]*\\)\\(-->\\)"
     (1 xxml-sgml-delimiter-face)
     (2 xxml-sgml-comment-face)
     (3 xxml-sgml-delimiter-face))
    ("\\(<[!?]\\)\\([^-]?[^?>]*\\)\\(\\??>\\)"
     (1 xxml-sgml-delimiter-face)
     (2 font-lock-keyword-face)
     (3 xxml-sgml-delimiter-face))
    ("\\(<\\)\\([a-zA-Z0-9-_]+\\)"
     (1 xxml-sgml-delimiter-face)
     (2 xxml-sgml-opening-tag-face))
    (">" 0 xxml-sgml-delimiter-face)
    ("\\(<\\)\\(/[a-zA-Z0-9-_]+\\)\\(>\\)"
     (1 xxml-sgml-delimiter-face)
     (2 xxml-sgml-closing-tag-face)
     (3 xxml-sgml-delimiter-face))
    ("&\\([a-zA-Z][a-zA-Z0-9-_.]*\\|#\\([xX][0-9a-fA-F]+\\|[0-9]+\\)\\);?"
     0 xxml-sgml-character-entity-face)
    ("\240" 0 xxml-unbreakable-space-face)
    (xxml-attribute-for-highlight (1 xxml-sgml-attribute-name-face append)
				  (2 xxml-sgml-attribute-value-face append)))
  "Data to drive fontification in SGML editing mode.")

(let ((header-background "seashell1")
      (emphasis-background "lightyellow")
      (interaction-background "lightcyan"))
  ;; Block elements.
  (copy-face 'bold 'xxml-header-1-face)
  (copy-face 'bold-italic 'xxml-header-2-face)
  (copy-face 'italic 'xxml-header-3-face)
  (copy-face 'default 'xxml-header-4-face)
  (set-face-background 'xxml-header-1-face header-background)
  (set-face-background 'xxml-header-2-face header-background)
  (set-face-background 'xxml-header-3-face header-background)
  (set-face-background 'xxml-header-4-face header-background)
  ;; Text elements.
  (copy-face 'italic 'xxml-emph-1-face)
  (copy-face 'default 'xxml-emph-2-face)
  (set-face-background 'xxml-emph-1-face emphasis-background)
  (set-face-background 'xxml-emph-2-face emphasis-background)
  (copy-face 'default 'xxml-interaction-face)
  (set-face-background 'xxml-interaction-face interaction-background))

(when window-system
  (defvar xxml-comment-face)
  (defvar xxml-unbreakable-space-face)
  (setq xxml-comment-face 'xxml-comment-face)
  (copy-face 'underline 'xxml-unbreakable-space-face)
  (set-face-foreground 'xxml-unbreakable-space-face "grey")
  (setq xxml-unbreakable-space-face 'xxml-unbreakable-space-face))

(defvar xxml-value-regexp
  "=\\([-.a-zA-Z0-9]+\\|\"\\(\\\\.\\|[^\"]\\)*\"\\)"
  "Regular expression for matching an equal sign and attribute value.")

(defun xxml-append-face-to-trimmed-lines (start end face)
  "To trimmed lines between START and END, append FACE to text properties.
That is, do not append face over starting or ending region, or lines in the
regions.  Point is left at END."
  (goto-char start)
  (skip-chars-forward " \t\n" end)
  (while (< (point) end)
    (setq start (point))
    (unless (search-forward "\n" end t)
      (goto-char end))
    (skip-chars-backward " \t\n")
    (font-lock-append-text-property start (point) 'face face)
    (skip-chars-forward " \t\n" end)))

(defun xxml-highlight-on-the-fly (tag tag-end limit)
  "Do any specially decided highlighting for tags or their whole contents.
Such TAG should not be recursively used, and must be explicitely ended.
Highlighting usually starts at TAG-END but should not extend beyond LIMIT."
  (setq tag (downcase tag))
  (let ((face-for-tag
	 (let ((pair (assoc tag xxml-highlight-tag-alist)))
	   (and pair (cdr pair))))
	(face-for-initial
	 (let ((pair (assoc tag xxml-highlight-initial-alist)))
	   (and pair (cdr pair))))
	(face-for-recursive
	 (let ((pair (assoc tag xxml-highlight-recursive-alist)))
	   (and pair (cdr pair)))))
    ;; Point always happens to be after the opening bracket of the start tag.
    (let ((here (point)))
      (when (or face-for-tag face-for-recursive)
	;; Find the end tag.  (FIXME: we might not find the correct one!)
	(when (let ((case-fold-search t))
		(re-search-forward (concat "</" tag ">") limit t))
	  (setq limit (match-beginning 0))
	  (when face-for-tag
	    ;; Highlight the end tag.
	    (font-lock-append-text-property (match-beginning 0) (match-end 0)
					    'face face-for-tag))))
      (when face-for-tag
	;; Highlight the start tag.
	(xxml-append-face-to-trimmed-lines (1- here) tag-end face-for-tag))
      (when face-for-initial
	;; Highlight the text before next tag.
	(goto-char tag-end)
	(when (> (skip-chars-forward "^<" limit) 0)
	  (xxml-append-face-to-trimmed-lines tag-end (point) face-for-initial))
	(setq tag-end (point)))
      (when face-for-recursive
	;; Highlight the whole remainder of recursive contents.
	(xxml-append-face-to-trimmed-lines tag-end limit face-for-recursive))
      ;; Restore position.
      (goto-char here))))

(defun xxml-attribute-for-highlight (limit)
  "Find next tag attribute to highlight, then set \1 to name and \2 to value.
Return t if found.  This routine handles tags spanning multiple lines, which
anchored matches would hardly do.  It might do the job a bit more speedily,
too.  Also highlight, on the fly, some special tags or embedded contents."
  (let ((pattern (concat "[ \t\n]+\\([-a-zA-Z0-9]+\\)\\(\\("
			 xxml-value-regexp
			 "\\)?\\)"))
	tag tag-end)
    ;; Find the end of that tag starting before current position.
    (save-excursion
      (when (search-backward "<" nil t)
	(setq tag (and (looking-at "<\\([-a-zA-Z0-9]+\\)")
		       (match-string-no-properties 1))
	      tag-end (or (search-forward ">" limit t) limit))))
    (or (and tag
	     (> tag-end (point))
	     ;; We were already within a simple start tag.
	     (progn
	       (xxml-highlight-on-the-fly tag tag-end limit)
	       (re-search-forward pattern tag-end t)))
	(let (found)
	  ;; Skip over text between tags.
	  (while (and (not found) (search-forward "<" limit t))
	    (setq tag-end (or (save-excursion (search-forward ">" limit t))
			      limit))
	    (if (and (setq tag (and (looking-at "[-a-zA-Z0-9]+")
				    (match-string-no-properties 0)))
		     ;; Now again within a simple start tag.
		     (progn
		       (xxml-highlight-on-the-fly tag tag-end limit)
		       (re-search-forward pattern tag-end t)))
		(setq found t)
	      (goto-char tag-end)))
	  found))))

;;; Clean up of SGML files and miscellaneous.

(defun xxml-cleanup (full)
  "Clean up various little things, here and there, in SGML code.
With prefix argument, insert prolog and epilog as needed."
  (interactive "P")
  (when full
    (xxml-guarantee-prolog)
    (xxml-guarantee-epilog))
  (xxml-elementary-cleanup (point-min) (point-max))
  (xxml-anti-clarisworks (point-min) (point-max)))

(defun xxml-guarantee-prolog (&optional replace)
  "If we know a default prolog, ensure we have one, as PSGML likes DTDs."
  (when xxml-default-prolog
    (let ((any-prolog-regexp "<!DOCTYPE[ \t\r\n][^>]*>\r?\n?")
	  (case-fold-search t))
      (save-excursion
	(goto-char (point-min))
	(if replace
	    (unless (re-search-forward (regexp-quote xxml-default-prolog)
				       nil t)
	      (if (re-search-forward any-prolog-regexp nil t)
		  (replace-match xxml-default-prolog t t)
		(insert xxml-default-prolog))
	      (sgml-parse-prolog))
	  (unless (re-search-forward any-prolog-regexp nil t)
	    (insert xxml-default-prolog)
	    (sgml-parse-prolog)))))))

(defun xxml-guarantee-epilog ()
  "Save PSGML settings with the file."
  (save-excursion
    (sgml-save-options)))

(defun xxml-elementary-cleanup (start end)
  "Do some elementary cleanups.  Use `whitespace.el' for complementing this."
  (let ((case-fold-search t))
    (save-excursion
      ;; Get rid of DOSish end of lines.
      (goto-char start)
      (while (search-forward "\r" end t)
	(replace-match "" t t))
      ;; Get rid of spurious whitespace, terminate last line if needed.
      (goto-char end)
      (delete-char (- (skip-chars-backward " \t\n")))
      (insert "\n")
      (goto-char start)
      (delete-char (- (skip-chars-forward " \t\n")))
      (while (re-search-forward "[ \t]+$" end t)
	(replace-match "" t t))
      ;; Use real unbreakable spaces, not the entity.  This is Emacs, guys!
      (goto-char start)
      (while (re-search-forward "&\\(nbsp\\|#160\\);?" end t)
	(replace-match "\240" t t)))))

(defun xxml-anti-clarisworks (start end)
  "Get rid of all this ClarisWorks generated s**t."
  (interactive)
  (let ((case-fold-search t))
    (save-excursion
      (goto-char start)
      (while (re-search-forward
	      "<x-claris-\\(window\\|tagview\\)[^>]*>\n?"
	      end t)
	(replace-match "" t t))
      (goto-char start)
      (while (re-search-forward
	      " ?x-claris-image\\(width\\|height\\) ?"
	      end t)
	(replace-match " " t t)))))

;;; Indentation of whole SGML elements.

(defun xxml-indent-element (step)
  "Indent tags of current element, or indent text if within character data.
Tags are often isolated one per line, which includes all tag attributes.  A
numeric prefix could be used to override current indentation step.  When given
a negative numeric prefix, the indentation step is overridden according to its
absolute value, and then, white lines around tags also get removed."
  (interactive "P")
  (let ((xxml-indent-step (or (when step (abs step)) xxml-indent-step))
	(element (xxml-find-element-around-cursor)))
    (let ((start (sgml-element-start element))
	  (end (make-marker)))
      (set-marker end (sgml-element-end element))
      (xxml-show-rug start end)
      (xxml-indent-region start end (when step (< step 0)))
      (xxml-unshow-rug)
      (xxml-refontify-region start end)
      (set-marker end nil))))

(defun xxml-indent-region (start end &optional no-white-lines-around-tags)
  (let ((xxml-progress-action "indenting region")
	(handle-text (not xxml-all-contents-are-literal))
	(sgml-indent-step (or xxml-indent-step sgml-indent-step))
	(last-implies-indent (bobp)))
    (xxml-progress-init start end)
    (save-excursion
      (goto-char start)
      (when (and no-white-lines-around-tags
		 (looking-at "[ \t]*\n\\([ \t]*\n\\)*"))
	(replace-match "" t t))
      (skip-chars-forward " \t\n")
      (while (< (point) end)
	(xxml-show-sparkle (point) end)
	(xxml-progress-update)
	(cond ((looking-at "<!--")
	       ;; Process an SGML comment.
	       (let ((here (point)))
		 (search-forward "-->")
		 (when last-implies-indent
		   (xxml-indent-rigidly here (point))
		   (setq last-implies-indent nil))))
	      ((looking-at "<!")
	       ;; Process an SGML directive.
	       (let ((here (point)))
		  (search-forward ">")
		  (when last-implies-indent
		    (xxml-indent-rigidly here (point))
		    (setq last-implies-indent nil))))
	      ((looking-at "<\\(/?\\)\\([a-zA-Z0-9]+\\)")
	       ;; Simple start or end tag.
	       (let ((begin (string-equal (match-string-no-properties 1) ""))
		     (tag (intern-soft
			   (downcase (match-string-no-properties 2)))))
		 (when (and begin xxml-reorder-attributes)
		   (xxml-reorder-attributes))
		 (if (and (not begin)
			  (is-forbidden-end tag)
			  ;; Delete this end tag.
			  (looking-at
			   "</[a-zA-Z0-9]+[ \t\n]*>\\([ \t]*\n\\)?"))
		     (delete-region (match-beginning 0) (match-end 0))
		   (let* ((outside (is-indentable-outside tag))
			  (inside (is-indentable-inside tag))
			  (indentable (or last-implies-indent
					  (and outside begin)
					  (and inside (not begin)))))
		     ;; Should we add an end of line before the tag?
		     (when (and (or indentable (is-splittable-before tag))
				(save-excursion
				  (skip-chars-backward " \t")
				  (not (bolp))))
		       (insert "\n"))
		     ;; Should we indent the tag?
		     (when indentable
		       (sgml-indent-line))
		     (setq last-implies-indent (or (and inside begin)
						   (and outside (not begin)))))
		   ;; Should text later be handled or preserved?
		   (when (is-literal-contents tag)
		     (setq handle-text
			   (not (or begin xxml-all-contents-are-literal))))
		   ;; Skip over the tag.
		   (skip-chars-forward "^ \t\n>")
		   (delete-char (- (skip-chars-forward " \t\n")))
		   (while (not (eq (following-char) ?>))
		     ;; Process an attribute within a tag.
		     (insert " ")
		     (skip-chars-forward "-a-zA-Z0-9")
		     (cond ((looking-at xxml-value-regexp)
			    (goto-char (match-end 0)))
			   ((looking-at "[ \t\n>]"))
			   (t (error "Unrecognized attribute (%d)" (point))))
		     (delete-char (- (skip-chars-forward " \t\n"))))
		   (forward-char 1))))
	      ((looking-at "\n<")
	       ;; New line before a tag.
	       (forward-char 1))
	      ((and no-white-lines-around-tags handle-text
		    (looking-at "[ \t]*\n\\([ \t]*\n\\)+\\([ \t]*\\)"))
	       ;; White lines at beginning of text or between tags.
	       (replace-match "\n\\2" t))
	      ((and no-white-lines-around-tags
		    (looking-at "[ \t]*\n\\([ \t]*\n\\)+\\([ \t]*\\)<"))
	       ;; White lines between tags.
	       (replace-match "\n\\2<" t)
	       (forward-char -1))
	      (t
	       ;; Character data.
	       (cond ((eolp) (forward-char 1))
		     ((bolp))
		     ((or last-implies-indent
			  (and handle-text (memq (following-char) '(?  ?\t))))
		      ;; Let's add an end of line before the text.
		      (insert "\n")))
	       (setq last-implies-indent nil)
	       (cond (handle-text
		      ;; Let's indent the text.
		      (let ((here (point)))
			(if (search-forward "<" end t)
			    (forward-char -1)
			  (goto-char end))
			(xxml-indent-softly here (point)))
		      (when no-white-lines-around-tags
			(save-excursion
			  (skip-chars-backward " \t\n")
			  (when (looking-at
				 "[ \t]*\n\\([ \t]*\n\\)+\\([ \t]*\\)")
			    ;; White lines at end of text.
			    (replace-match "\n\\2" t))))
		      (save-excursion
			(when (bolp)
			  (forward-char -1))
			(when (memq (preceding-char) '(?  ?\t ?\n))
			  (setq last-implies-indent t))))
		     ;; Skip over the text without indenting it.
		     ((search-forward "<" end t)
		      (forward-char -1))
		     (t (goto-char end))))))
      (xxml-unshow-sparkle))
    (xxml-progress-complete)))

(defun xxml-indent-rigidly (begin end)
  "Indent line at BEGIN, and all following lines until END, rigidly with it."
  (save-excursion
    (goto-char begin)
    (let ((limit (make-marker))
	  (before (current-indentation)))
      (set-marker limit end)
      (sgml-indent-line)
      (when (> (point) limit)
	(set-marker limit (point)))
      (let ((after (current-indentation)))
	(end-of-line)
	(when (< (point) limit)
	  (forward-char 1)
	  (indent-rigidly (point) limit (- after before))))
      (set-marker limit nil))))

(defun xxml-indent-softly (begin end)
  "Indent all lines individually from BEGIN to END, while cutting long lines.
Indentation is inhibited for first line if the initial position of the cursor
is not preceded by only whitespace."
  (save-excursion
    (goto-char begin)
    (let ((limit (make-marker)))
      (set-marker limit end)
      (skip-chars-backward " \t")
      (let ((skip (not (bolp))))
	(while (< (point) limit)
	  (if skip
	      (setq skip nil)
	    (if (looking-at "[ \t]*$")
		(delete-char (- (skip-chars-forward " \t")))
	      (sgml-indent-line)
	      (when (> (point) limit)
		(set-marker limit (point)))
	      (let ((margin (point)))
		(end-of-line)
		(when (> (current-column) fill-column)
		  (beginning-of-line)
		  (let ((fill-prefix (buffer-substring-no-properties
				      (point) margin)))
		    (fill-region (point)
				 (save-excursion (forward-line 1) (point))))
		  (goto-char margin)))))
	  (end-of-line)
	  (unless (eobp)
	    (forward-char 1))))
      (set-marker limit nil))))

;;; Refilling of whole SGML elements.

(defun xxml-fill-element (step)
  "Refill tags of current element, and possibly its embedded character data.
A numeric prefix could be used to override current indentation step.  When
given a negative numeric prefix, the indentation step is overridden according
to its absolute value, and then, white lines around tags also get removed."
  (interactive "P")
  (let ((xxml-indent-step (or (when step (abs step)) xxml-indent-step))
	(element (xxml-find-element-around-cursor)))
    (let ((start (sgml-element-start element))
	  (end (make-marker)))
      (set-marker end (sgml-element-end element))
      (xxml-show-rug start end)
      (xxml-indent-region start end (when step (< step 0)))
      (xxml-fill-region start end)
      (xxml-unshow-rug)
      (xxml-refontify-region start end)
      (set-marker end nil))))

(defun xxml-fill-region (start end)
  "Combine lines as far as possible, avoiding the production of long lines.
This works best after indentation has been done, when tags occupy exactly one
line, and the indentation gives a clue on the hierarchy.  Entities embedded in
text lines by the process, as well as non-text lines, are handled atomically."
  (let ((xxml-progress-action "filling region")
	(handle-text (not xxml-all-contents-are-literal)))
    (xxml-progress-init start end)
    (save-excursion
      (unwind-protect
	  (let ((inhibit-point-motion-hooks t))
	    (xxml-fill-region-sub nil nil start end))
	(xxml-unshow-sparkle)
	(remove-text-properties start end '(intangible t))))
    (xxml-progress-complete)
    (xxml-split-long-tags start end)))

(defun xxml-fill-region-sub (outside-tag terminated start end
					 &optional handle-text)
  "Given OUTSIDE-TAG and TERMINATED, fill SGML code running from START to END.
OUTSIDE-TAG is a symbol for the tag.  TERMINATED says that the end tag is not
omitted.  If not within a tag, both OUTSIDE-TAG and TERMINATED are nil.
Return t, to inhibit merging at the outer level, for example, when textual
refilling did occur at this level and produced a result spanning many lines."
  (save-excursion
    (let (;; Merge gets allowed theoretically from tag properties.  Being
	  ;; allowed is also needed for shrink-wrappability.
	  (allow-merge (or (not outside-tag)
			   (is-indentable-inside outside-tag)))
	  ;; Merge gets inhibited from various practical considerations.
	  (inhibit-merge (is-break outside-tag))
	  ;; Text refilling is only meaningful when there is intermixed text.
	  (should-refill nil))
      ;; Recursively process sub-elements, making them intangible.
      (let ((handle-text (and handle-text
			      (not (is-literal-contents outside-tag))))
	    (seen-one nil))
	(goto-char start)
	(xxml-progress-update)
	(while (< (point) end)
	  (xxml-show-sparkle (point) end)
	  (cond ((looking-at "<!--")
		 ;; Skip an SGML comment.
		 (let ((begin (point)))
		   (search-forward "-->")
		   (add-text-properties begin (point) '(intangible t)))
		 (setq inhibit-merge t))
		((looking-at "<!")
		 ;; Skip an SGML declaration.
		 (let ((begin (point)))
		   (search-forward ">")
		   (add-text-properties begin (point) '(intangible t)))
		 (setq inhibit-merge t))
		((looking-at "<[a-zA-Z0-9]")
		 ;; Process an SGML element.
		 (let* ((stag-start (point))
			(element (sgml-find-element-of stag-start))
			(string (sgml-element-gi element))
			(tag (intern-soft (downcase string))))
		   (let ((stag-end (sgml-element-stag-end element))
			 (etag-start (make-marker))
			 (etag-end (make-marker)))
		     (set-marker etag-start (sgml-element-etag-start element))
		     (set-marker etag-end (sgml-element-end element))
		     (when (xxml-fill-region-sub tag (/= etag-start etag-end)
						 stag-end etag-start
						 handle-text)
		       (setq inhibit-merge t))
		     (goto-char etag-end)
		     (when (and handle-text (is-break tag))
		       (setq inhibit-merge t)
		       (when (eolp)
			 (forward-char 1)))
		     (xxml-show-sparkle stag-start (point))
		     (add-text-properties stag-start (point) '(intangible t))
		     (set-marker etag-start nil)
		     (set-marker etag-end nil))
		   (if seen-one
		       (setq inhibit-merge t)
		     (setq seen-one t)
		     (when (is-indentable-outside tag)
		       (setq allow-merge t)))))
		((looking-at "[ \t\n]+<")
		 ;; Skip whitespace around tags.
		 (goto-char (1- (match-end 0))))
		(t
		 ;; Process character data.
		 (if (search-forward "<" end t)
		     (forward-char -1)
		   (goto-char end))
		 (when handle-text
		   (setq should-refill t))))))
      ;;(when (is-shrink-wrappable outside-tag)
      ;;  (xxml-shrink-wrap start end allow-merge terminated))
      ;; We now have all intangibility information.  Let's handle this level.
      (when (and allow-merge (not inhibit-merge))
	(xxml-attempt-merge start end terminated))
      (when should-refill
	(xxml-refill-text
	 (save-excursion
	   (goto-char start) (skip-chars-forward " \t\n") (point))
	 (save-excursion
	   (goto-char end) (skip-chars-backward " \t\n") (point)))))))

(defun xxml-refill-text (start end)
  "Fill lines of text from START to until END.
More text after END, may be moved as well if no intervening whitespace."
  (save-excursion
    (goto-char end)
    (while (cond ((memq (following-char) '(0 ?  ?\t ?\n)) nil)
		 ((eq (following-char) ?<) (search-forward ">"))
		 (t (skip-chars-forward "^ \t\n<")))
      nil)
    (let ((limit (make-marker)))
      (set-marker limit (point))
      (let ((inhibit-point-motion-hooks nil))
	(goto-char start)
	(end-of-line)
	(while (< (point) limit)
	  (if (not (eolp))
	      ;; If not at end of line, we ought to have skipped it as
	      ;; intagible text.  In that case, just do it again.
	      (end-of-line)
	    (xxml-show-sparkle (point) limit)
	    (skip-chars-backward " \t")
	    (if (bolp)
		(end-of-line 2)
	      (let ((here (point))
		    (room (- fill-column (current-column)))
		    (glue (if (memq (preceding-char) '(?. ?? ?!)) "  " " ")))
		(if (<= (- room (length glue)) 0)
		    (end-of-line 2)
		  (forward-line 1)
		  (skip-chars-forward " \t" limit)
		  (let ((left (point))
			(right (min limit
				    (+ (point) (- room (length glue)))
				    (save-excursion
				      (let ((inhibit-point-motion-hooks t))
					(end-of-line)
					(point))))))
		    (when (and (> right left)
			       (get-text-property right 'intangible))
		      (setq right (or (previous-single-property-change
				       right 'intangible nil left)
				      left)))
		    (if (<= right left)
			(end-of-line)
		      (goto-char right)
		      (skip-chars-backward " \t" left)
		      (unless (looking-at "[ \t\n]")
			(skip-chars-backward "^ \t" left)
			(skip-chars-backward " \t" left))
		      (if (<= (point) left)
			  (end-of-line)
			(let ((cut (point-marker)))
			  (goto-char here)
			  (skip-chars-forward " \t")
			  (let ((newline-and-margin (buffer-substring
						     (point) left)))
			    (delete-region here left)
			    (insert glue)
			    (goto-char cut)
			    (delete-char (- (skip-chars-forward " \t" limit)))
			    (unless (eolp)
			      (insert newline-and-margin)
			      (goto-char cut)))
			  (set-marker cut nil))))))))))
	;; Recognize the special case of a text split on two lines, while it
	;; would all fit on the second line.
	(goto-char start)
	(when (> (skip-chars-forward " \t") 0)
	  (let ((count (current-column)))
	    (when (and (search-forward "\n" limit t)
		       (not (search-forward "\n" limit t)))
	      (forward-char -1)
	      (let ((here (point))
		    (room (- fill-column
			     (save-excursion
			       (goto-char limit)
			       (current-column))))
		    (glue (if (memq (preceding-char) '(?. ?? ?!)) "  " " ")))
		(when (>= (- room (length glue)) (- (current-column) count))
		  (skip-chars-forward " \t\n" limit)
		  (let ((newline-and-margin (buffer-substring here (point))))
		    (delete-region here (point))
		    (insert glue)
		    (goto-char start)
		    (delete-char (- (skip-chars-forward " \t")))
		    (insert newline-and-margin))))))))
      ;; Clean up.
      (set-marker limit nil))))

(defun xxml-attempt-merge (start end terminated)
  "Try to simultaneously remove whitespace before START and after END.
By doing so, tags become batched with the surrounding tags."
  (let ((proceed t))
    ;; Blank lines inhibit merge and retain a more vertical alignment of tags.
    (goto-char start)
    (skip-chars-forward " \t")
    (when (eolp)
      (forward-char 1)
      (skip-chars-forward " \t")
      (when (eolp)
	(setq proceed nil)))
    (when (and proceed terminated)
      (goto-char end)
      (skip-chars-backward " \t")
      (when (bolp)
	(forward-char -1)
	(skip-chars-backward " \t")
	(when (bolp)
	  (setq proceed nil))))
    (when proceed
      ;; Enough line space must exist at both ends.
      (let (count)
	(goto-char start)
	(setq count (current-column))
	(skip-chars-forward " \t\n")
	(setq count (- count (current-column)))
	(end-of-line)
	(setq count (+ count (current-column)))
	(when terminated
	  (skip-chars-forward " \t\n" end)
	  (if (/= (point) end)
	      ;; End tag would be on a separate line than start tag.
	      (when (<= count fill-column)
		(goto-char end)
		(setq count (current-column))
		(end-of-line)
		(setq count (- (current-column) count))
		(skip-chars-backward " \t\n")
		(setq count (+ count (current-column))))
	    ;; Everything would fit on a single line.
	    (setq count (- count (current-column)))
	    (end-of-line)
	    (setq count (+ count (current-column)))))
	(when (<= count fill-column)
	  ;; Nothing prevents the merge.  Do it.
	  (goto-char start)
	  (delete-char (- (skip-chars-forward " \t\n")))
	  (when terminated
	    (goto-char end)
	    (delete-char (- (skip-chars-backward " \t\n")))))))))

(defun xxml-shrink-wrap (start end indentable terminated)
  "Squeeze out whitespace after START and before END.
INDENTABLE says if boundaries may be indented."
  (goto-char start)
  (when (looking-at "[ \t\n]+")
    (xxml-show-sparkle-match)
    (let ((string (match-string 0)))
      (delete-region (match-beginning 0) (match-end 0))
      (search-backward "<")
      (unless indentable
	(insert string))))
  (when terminated
    (goto-char end)
    (skip-chars-backward " \t\n")
    (when (looking-at "[ \t\n]+")
      (xxml-show-sparkle-match)
      (let ((string (match-string 0)))
	(delete-region (match-beginning 0) (match-end 0))
	(search-forward ">")
	(unless indentable
	  (insert string))))))

(defun xxml-reorder-attributes ()
  "Manage to get all attributes presented in some canonical order."
  (interactive)
  (save-excursion
    (let ((element (sgml-find-attribute-element)))
      (unless (sgml-bpos-p (sgml-element-stag-epos element))
	(error "Element's start-tag is not in the buffer"))
      (sgml-change-start-tag
       element
       (sgml-element-attribute-specification-list element)))))

(defun xxml-split-long-tags (start end)
  "Split overlong tags and long text lines on many lines.
Switch tags and attribute to proper case.
Remove spurious quotes in `normalize-quotes' is not nil."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (cond ((looking-at "<!--")
	     ;; Skip an SGML comment.
	     (search-forward "-->"))
	    ((looking-at "<!")
	     ;; Skip an SGML directive.
	     (search-forward ">"))
	    ((eq (following-char) ?<)
	     ;; Process a simple tag.  Refill by reducing whitespace between
	     ;; attributes to a single space or newlines, as appropriate.
	     ;; Also, as we go, switch tag or attribute names to lower case.
	     (forward-char 1)
	     (when (eq (following-char) ?/)
	       (forward-char 1))
	     (unless (looking-at "[a-zA-Z0-9]+")
	       (error "Missing tag id (%d)" (point)))
	     (if xxml-transform-tag-id
		 (replace-match (apply xxml-transform-tag-id
				       (list (match-string 0)))
				t t)
	       (goto-char (match-end 0)))
	     (let ((margin (1+ (current-column)))
		   (margin-point (point))
		   cut-point)
	       (delete-char (- (skip-chars-forward " \t\n")))
	       (while (not (eq (following-char) ?>))
		 (when (and cut-point (>= (current-column) fill-column))
		   (save-excursion
		     (goto-char cut-point)
		     (delete-char 1)
		     (insert "\n")
		     (indent-to margin)
		     (setq margin-point (point))))
		 (setq cut-point (and (not (eq (point) margin-point))
				      (point)))
		 (insert " ")
		 (unless (looking-at "[-a-zA-Z0-9]+")
		   (error "Missing attribute id (%d)" (point)))
		 (if xxml-transform-attribute-id
		     (replace-match (apply xxml-transform-attribute-id
					   (list (match-string 0)))
				    t t)
		   (goto-char (match-end 0)))
		 (cond ((looking-at xxml-value-regexp)
			(if xxml-normalize-quotes
			    (let ((after (match-end 0)))
			      (forward-char 1)
			      (cond ((looking-at
				      "\"\\([-a-zA-Z0-9]+\\|[.0-9]+\\)\"")
				     (delete-char 1)
				     (goto-char (1- after))
				     (delete-char -1))
				    ((looking-at "[-a-zA-Z0-9]+\\|[.0-9]+")
				     (goto-char after))
				    ((eq (following-char) ?\")
				     (goto-char after))
				    (t (insert "\"")
				       (goto-char (1+ after))
				       (insert "\""))))
			  (goto-char (match-end 0))))
		       ((looking-at "[ \t\n>]"))
		       (t (error "Unrecognized attribute (%d)" (point))))
		 (delete-char (- (skip-chars-forward " \t\n"))))
	       (when (and cut-point (>= (current-column) fill-column))
		 (save-excursion
		   (goto-char cut-point)
		   (delete-char 1)
		   (insert "\n")
		   (indent-to margin)))
	       (forward-char 1)))
	    (t
	     ;; Skip character data.
	     (if (search-forward "<" end t)
		 (forward-char -1)
	       (goto-char end)))))
    (xxml-unshow-sparkle)))

(defun xxml-write-file-hooks-routine ()
  (when
      (and buffer-file-name
	   (string-equal (file-name-nondirectory buffer-file-name) "xxml.el")
	   (buffer-modified-p))
    (save-excursion
      (goto-char (point-min))
      (when (and (re-search-forward "^(defconst xxml-version \".*\")" nil t)
		 (y-or-n-p "Replace xxml-version? "))
	(replace-match (concat "(defconst xxml-version \""
			       (format-time-string "%Y-%m-%d %H:%M")
			       "\")")))))
  nil)

(provide 'xxml)

;; Local Variables:
;; eval: (add-hook 'write-file-hooks 'xxml-write-file-hooks-routine)
;; End:

;; xxml.el ends here.
