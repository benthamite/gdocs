;;; gdocs-convert.el --- Bidirectional org <-> Google Docs format conversion -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: gdocs contributors
;; Keywords: docs, org
;; Package-Requires: ((emacs "29.1") (dash "2.19") (s "1.13") (org "9.6"))

;;; Commentary:

;; Bidirectional conversion between org-mode and Google Docs JSON via an
;; intermediate representation (IR).  All conversion passes through a flat
;; list of IR element plists, which serves as the canonical interchange
;; format shared with the diff engine.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'dash)
(require 's)
(require 'org)
(require 'org-element)

;; ---------------------------------------------------------------------------
;;; ID generation

(defvar gdocs-convert--id-counter 0
  "Running counter for element ID generation.")

(defun gdocs-convert--next-id ()
  "Return the next unique element ID string."
  (cl-incf gdocs-convert--id-counter)
  (format "elem-%03d" gdocs-convert--id-counter))

(defun gdocs-convert--reset-ids ()
  "Reset the element ID counter to zero."
  (setq gdocs-convert--id-counter 0))

;; ---------------------------------------------------------------------------
;;; Cross-document link resolution

(defcustom gdocs-link-directories nil
  "Additional directories to scan for cross-document link resolution.
`gdocs-directory' is always included."
  :type '(repeat directory)
  :group 'gdocs)

(defvar gdocs-convert--link-context nil
  "Plist with context for cross-document link resolution.
When non-nil, file: and id: links to org files with Google Doc
counterparts are resolved to Google Docs URLs during org-to-IR
conversion, and Google Docs URLs are reverse-resolved to org
links during IR-to-org conversion.
Contains :buffer-file and :docid-map.")

(defvar gdocs-convert--document-id nil
  "Document ID of the Google Doc being converted.
Used during JSON-to-IR conversion to resolve internal heading
links, which the Google Docs API returns as headingId rather
than full URLs.")

(defvar gdocs-convert--heading-cache (make-hash-table :test 'equal)
  "Session cache: doc-id -> alist of ((heading-text . heading-id) ...).
Populated during push/pull when we fetch a document's JSON.
Used during org-to-IR link resolution to append heading anchors.")

(defun gdocs-convert--read-file-local-gdocs-id (file-path)
  "Read `gdocs-document-id' from FILE-PATH's Local Variables block.
Reads only the tail of the file (~3KB) and uses a regexp.
Returns the document ID string or nil."
  (when (and file-path (file-readable-p file-path))
    (let ((tail-bytes 3072)
          (content nil))
      (with-temp-buffer
        (insert-file-contents file-path nil
                              (max 0 (- (file-attribute-size
                                         (file-attributes file-path))
                                        tail-bytes)))
        (setq content (buffer-string)))
      (when (string-match
             "gdocs-document-id:[[:space:]]+\"\\([^\"]+\\)\""
             content)
        (match-string 1 content)))))

(defun gdocs-convert--build-docid-to-file-map (directories)
  "Scan org files in DIRECTORIES and return a doc-id-to-file hash table.
Each key is a Google Docs document ID, each value is an absolute file path."
  (let ((map (make-hash-table :test 'equal)))
    (dolist (dir directories)
      (when (file-directory-p dir)
        (dolist (file (directory-files dir t "\\.org\\'"))
          (when-let* ((doc-id (gdocs-convert--read-file-local-gdocs-id file)))
            (puthash doc-id file map)))))
    map))

(defun gdocs-convert--cache-heading-ids (doc-id json)
  "Extract heading IDs from document JSON and cache them under DOC-ID.
Stores an alist of (heading-text . heading-id) pairs, plus a
reverse alist of (heading-id . heading-text) pairs under
DOC-ID-reverse."
  (let ((body (alist-get 'body json))
        (forward nil)
        (reverse nil))
    (when body
      (seq-doseq (structural-element (alist-get 'content body))
        (let ((paragraph (alist-get 'paragraph structural-element)))
          (when paragraph
            (let* ((style-obj (alist-get 'paragraphStyle paragraph))
                   (named-style (alist-get 'namedStyleType style-obj))
                   (heading-id (alist-get 'headingId style-obj)))
              (when (and heading-id
                         (string-match-p "^HEADING_[1-6]$" named-style))
                (let* ((elements (alist-get 'elements paragraph))
                       (text (gdocs-convert--docs-paragraph-plain-text
                              elements)))
                  (when (and text (not (string-empty-p text)))
                    (push (cons text heading-id) forward)
                    (push (cons heading-id text) reverse)))))))))
    (puthash doc-id (nreverse forward) gdocs-convert--heading-cache)
    (puthash (concat doc-id "-reverse") (nreverse reverse)
             gdocs-convert--heading-cache)))

(defun gdocs-convert--docs-paragraph-plain-text (elements)
  "Extract plain text from Google Docs paragraph ELEMENTS.
Returns the concatenated text content with trailing whitespace trimmed."
  (let ((parts nil))
    (seq-doseq (el elements)
      (let ((text-run (alist-get 'textRun el)))
        (when text-run
          (push (alist-get 'content text-run) parts))))
    (s-trim-right (apply #'concat (nreverse parts)))))

(defconst gdocs-convert--docs-url-regexp
  "https://docs\\.google\\.com/document/d/\\([^/#]+\\)\\(?:/[^#]*\\)?\\(?:#heading=h\\.\\([^&]+\\)\\)?"
  "Regexp matching a Google Docs document URL.
Group 1 captures the document ID.
Group 2 captures the heading ID (if present).")

;; ---------------------------------------------------------------------------
;;; Public API: org -> IR

(defun gdocs-convert-org-buffer-to-ir ()
  "Parse the current org buffer and return an IR element list.
Uses `org-element-parse-buffer' to obtain the AST, then walks it
to produce the intermediate representation."
  (gdocs-convert--reset-ids)
  (let* ((ast (org-element-parse-buffer))
         (title (gdocs-convert--extract-title ast))
         (body-elements (gdocs-convert--walk-org-data ast)))
    (if title
        (cons title body-elements)
      body-elements)))

(defun gdocs-convert-org-string-to-ir (string)
  "Convert an org-mode STRING to IR.
Creates a temporary buffer, inserts STRING, and calls
`gdocs-convert-org-buffer-to-ir'."
  (with-temp-buffer
    (insert string)
    (org-mode)
    (gdocs-convert-org-buffer-to-ir)))

(defun gdocs-convert--extract-title (ast)
  "Extract the #+TITLE keyword from AST and return an IR element, or nil."
  (let ((title-kw (org-element-map ast 'keyword
                    (lambda (kw)
                      (when (string= (org-element-property :key kw) "TITLE")
                        (org-element-property :value kw)))
                    nil t)))
    (when title-kw
      (list :type 'paragraph
            :style 'title
            :contents (list (gdocs-convert--make-plain-run title-kw))
            :source 'metadata
            :id (gdocs-convert--next-id)))))

(defun gdocs-convert--walk-org-data (ast)
  "Walk an org-element AST and return a flat list of IR elements."
  (let ((result nil))
    (org-element-map ast '(headline paragraph plain-list table
                           horizontal-rule keyword src-block
                           example-block quote-block)
      (lambda (el)
        (let ((parent-type (org-element-type (org-element-parent el))))
          (pcase (org-element-type el)
            ('headline
             (push (gdocs-convert--headline-to-ir el) result))
            ('paragraph
             (when (gdocs-convert--top-level-paragraph-p el)
               (dolist (ir (gdocs-convert--paragraph-to-ir el))
                 (push ir result))))
            ('plain-list
             (when (gdocs-convert--top-level-list-p el)
               ;; Prepend reversed sub-list; final nreverse restores order
               (setq result (nconc (nreverse
                                    (gdocs-convert--plain-list-to-ir el 0))
                                   result))))
            ('table
             (when (eq (org-element-property :type el) 'org)
               (push (gdocs-convert--table-to-ir el) result)))
            ('horizontal-rule
             (push (list :type 'horizontal-rule
                         :id (gdocs-convert--next-id))
                   result))
            ('quote-block
             (when (not (eq parent-type 'item))
               (push (gdocs-convert--quote-block-to-ir el) result)))
            ('src-block
             (push (gdocs-convert--src-block-to-ir el) result))
            ('example-block
             (setq result (nconc (nreverse
                                  (gdocs-convert--example-block-to-ir el))
                                 result)))
            ('keyword
             (when (gdocs-convert--preservable-keyword-p el)
               (push (gdocs-convert--keyword-to-ir el) result)))
            (_ nil)))))
    (nreverse result)))

;; ---------------------------------------------------------------------------
;;; Segmented org -> IR (for three-way merge)

(defun gdocs-convert-org-buffer-to-segments ()
  "Parse the current org buffer into segments for three-way merge.
Each segment pairs an IR element with its org text region,
including non-IR text like property drawers that follows the
element.  Returns a plist with:
  :ir        -- the full IR element list (including title)
  :segments  -- list of segment plists, each with :ir-element and :org-text
  :preamble  -- buffer text before the first body IR element
  :postamble -- buffer text after the last IR element"
  (gdocs-convert--reset-ids)
  (let* ((ast (org-element-parse-buffer))
         (title (gdocs-convert--extract-title ast))
         (entries (gdocs-convert--walk-org-data-positioned ast))
         (ir (mapcar (lambda (e) (plist-get e :ir)) entries))
         (full-ir (if title (cons title ir) ir))
         (postamble-start (gdocs-convert--find-postamble-start))
         (segments nil)
         (len (length entries)))
    ;; Preamble: text before the first body element
    (let ((first-pos (if entries
                         (plist-get (car entries) :begin)
                       postamble-start)))
      ;; Build segments: each segment's text runs from its :begin
      ;; to the next entry's :begin (or postamble-start for the last)
      (dotimes (i len)
        (let* ((entry (nth i entries))
               (begin (plist-get entry :begin))
               (end (if (< (1+ i) len)
                        (plist-get (nth (1+ i) entries) :begin)
                      postamble-start))
               (org-text (buffer-substring-no-properties begin end)))
          (push (list :ir-element (plist-get entry :ir)
                      :org-text org-text)
                segments)))
      (list :ir full-ir
            :segments (nreverse segments)
            :preamble (buffer-substring-no-properties 1 first-pos)
            :postamble (buffer-substring-no-properties
                        postamble-start (1+ (buffer-size)))))))

(defun gdocs-convert--find-postamble-start ()
  "Find the buffer position where the postamble begins.
The postamble is the file-local variables block at the end of
the buffer.  Returns `point-max' if no postamble is found."
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward "^# Local Variables:" nil t)
        ;; Include any blank lines before the block
        (progn
          (forward-line -1)
          (while (and (> (point) (point-min))
                      (looking-at-p "^[[:space:]]*$"))
            (forward-line -1))
          (forward-line 1)
          (point))
      (point-max))))

(defun gdocs-convert--walk-org-data-positioned (ast)
  "Walk AST and return IR elements with buffer positions.
Like `gdocs-convert--walk-org-data' but each entry is a plist
with :ir (the IR element) and :begin (buffer position).
The result is sorted by buffer position."
  (let ((result nil))
    (org-element-map ast '(headline paragraph plain-list table
                           horizontal-rule keyword src-block
                           example-block quote-block)
      (lambda (el)
        (let ((parent-type (org-element-type (org-element-parent el))))
          (pcase (org-element-type el)
            ('headline
             (push (list :ir (gdocs-convert--headline-to-ir el)
                         :begin (org-element-property :begin el))
                   result))
            ('paragraph
             (when (gdocs-convert--top-level-paragraph-p el)
               (let ((begin (org-element-property :begin el)))
                 (dolist (ir (gdocs-convert--paragraph-to-ir el))
                   (push (list :ir ir :begin begin) result)))))
            ('plain-list
             (when (gdocs-convert--top-level-list-p el)
               (dolist (entry (gdocs-convert--plain-list-positioned el 0))
                 (push entry result))))
            ('table
             (when (eq (org-element-property :type el) 'org)
               (push (list :ir (gdocs-convert--table-to-ir el)
                           :begin (org-element-property :begin el))
                     result)))
            ('horizontal-rule
             (push (list :ir (list :type 'horizontal-rule
                                   :id (gdocs-convert--next-id))
                         :begin (org-element-property :begin el))
                   result))
            ('quote-block
             (when (not (eq parent-type 'item))
               (push (list :ir (gdocs-convert--quote-block-to-ir el)
                           :begin (org-element-property :begin el))
                     result)))
            ('src-block
             (push (list :ir (gdocs-convert--src-block-to-ir el)
                         :begin (org-element-property :begin el))
                   result))
            ('example-block
             (let ((begin (org-element-property :begin el)))
               (dolist (ir-elem (gdocs-convert--example-block-to-ir el))
                 (push (list :ir ir-elem :begin begin) result))))
            ('keyword
             (when (gdocs-convert--preservable-keyword-p el)
               (push (list :ir (gdocs-convert--keyword-to-ir el)
                           :begin (org-element-property :begin el))
                     result)))
            (_ nil)))))
    (sort (nreverse result)
          (lambda (a b) (< (plist-get a :begin) (plist-get b :begin))))))

(defun gdocs-convert--plain-list-positioned (plain-list depth)
  "Convert PLAIN-LIST to positioned IR entries at nesting DEPTH.
Returns a list of (:ir ELEMENT :begin POS) plists."
  (let ((list-type (gdocs-convert--org-list-type plain-list))
        (result nil))
    (dolist (item (org-element-contents plain-list))
      (when (eq (org-element-type item) 'item)
        (setq result
              (nconc result
                     (gdocs-convert--item-positioned
                      item list-type depth)))))
    result))

(defun gdocs-convert--item-positioned (item list-type depth)
  "Convert list ITEM to positioned IR entries.
LIST-TYPE and DEPTH are as in `gdocs-convert--item-to-ir'.
Returns a list of (:ir ELEMENT :begin POS) plists."
  (let* ((checkbox (org-element-property :checkbox item))
         (effective-type (if checkbox 'check list-type))
         (checked (eq checkbox 'on))
         (paragraph (org-element-map item 'paragraph #'identity nil t))
         (runs (when paragraph
                 (gdocs-convert--strip-continuation-indent
                  (gdocs-convert--objects-to-runs
                   (org-element-contents paragraph)))))
         (trimmed (gdocs-convert--trim-runs runs))
         (list-plist (append (list :type effective-type :level depth)
                             (when (eq effective-type 'check)
                               (list :checked checked))))
         (ir-element (list :type 'paragraph
                           :style 'normal
                           :list list-plist
                           :contents (or trimmed
                                         (list (gdocs-convert--make-plain-run "")))
                           :id (gdocs-convert--next-id)))
         (result (list (list :ir ir-element
                             :begin (org-element-property :begin item)))))
    (dolist (child (org-element-contents item))
      (when (eq (org-element-type child) 'plain-list)
        (setq result
              (nconc result
                     (gdocs-convert--plain-list-positioned
                      child (1+ depth))))))
    result))

(defun gdocs-convert--top-level-paragraph-p (paragraph)
  "Return non-nil if PARAGRAPH is a direct section child.
A paragraph inside a list item is handled by the list walker, and
a paragraph inside a quote block is handled by the quote walker."
  (let ((parent-type (org-element-type (org-element-parent paragraph))))
    (or (eq parent-type 'section)
        (eq parent-type 'org-data))))

(defun gdocs-convert--top-level-list-p (plain-list)
  "Return non-nil if PLAIN-LIST is not nested inside another list item."
  (let ((parent (org-element-parent plain-list)))
    (not (eq (org-element-type parent) 'item))))

(defun gdocs-convert--preservable-keyword-p (keyword)
  "Return non-nil if KEYWORD is an org-only construct to preserve.
Excludes TITLE which is handled separately."
  (let ((key (org-element-property :key keyword)))
    (and key
         (not (string= key "TITLE"))
         (member key '("AUTHOR" "DATE" "EMAIL" "LANGUAGE")))))

;; ---------------------------------------------------------------------------
;;; Headline -> IR

(defun gdocs-convert--headline-to-ir (headline)
  "Convert an org HEADLINE element to an IR element."
  (let* ((level (org-element-property :level headline))
         (style (gdocs-convert--level-to-style level))
         (title-objects (org-element-property :title headline))
         (contents (gdocs-convert--objects-to-runs title-objects))
         (marker (gdocs-convert--headline-marker headline)))
    (append (list :type 'paragraph
                  :style style
                  :contents contents
                  :id (gdocs-convert--next-id))
            (when marker (list :gdocs-marker marker)))))

(defun gdocs-convert--level-to-style (level)
  "Convert an org heading LEVEL (integer) to an IR style symbol.
Levels above 6 are clamped to heading-6."
  (intern (format "heading-%d" (min level 6))))

(defun gdocs-convert--headline-marker (headline)
  "Extract org-only marker data from HEADLINE, or return nil.
Preserves TODO keywords, tags, priority, and scheduling info.
Returns a single marker plist when there is exactly one marker
part, or a list of marker plists when there are multiple.
Returns nil if the headline has no preservable metadata."
  (let* ((todo (org-element-property :todo-keyword headline))
         (tags (org-element-property :tags headline))
         (priority (org-element-property :priority headline))
         (scheduled (org-element-property :scheduled headline))
         (deadline (org-element-property :deadline headline))
         (parts nil))
    (when deadline
      (push (list :type 'deadline
                  :data (org-element-interpret-data deadline))
            parts))
    (when scheduled
      (push (list :type 'scheduled
                  :data (org-element-interpret-data scheduled))
            parts))
    (when priority
      (push (list :type 'priority
                  :data (char-to-string priority))
            parts))
    (when tags
      (push (list :type 'tags
                  :data (s-join ":" tags))
            parts))
    (when todo
      (push (list :type 'todo :data todo) parts))
    (when (= (length parts) 1)
      (setq parts (car parts)))
    (when parts
      parts)))

;; ---------------------------------------------------------------------------
;;; Paragraph -> IR

(defun gdocs-convert--paragraph-to-ir (paragraph)
  "Convert an org PARAGRAPH element to a list of IR elements.
Multi-line inline code paragraphs (consecutive ~...~ lines) are
split into individual monospace paragraphs.  Returns nil if the
paragraph contains only whitespace."
  (let* ((contents (org-element-contents paragraph))
         (runs (gdocs-convert--objects-to-runs contents))
         (trimmed (gdocs-convert--trim-runs runs)))
    (cond
     ((null trimmed) nil)
     ((gdocs-convert--multiline-code-paragraph-p trimmed)
      (gdocs-convert--split-code-paragraph-runs trimmed))
     (t (list (list :type 'paragraph
                    :style (gdocs-convert--paragraph-style paragraph)
                    :contents trimmed
                    :id (gdocs-convert--next-id)))))))

(defun gdocs-convert--paragraph-style (paragraph)
  "Determine the IR style for PARAGRAPH based on its context."
  (let ((parent (org-element-parent paragraph)))
    (if (eq (org-element-type parent) 'quote-block)
        'quote
      'normal)))

(defun gdocs-convert--multiline-code-paragraph-p (runs)
  "Return non-nil if RUNS represent consecutive inline code lines.
Detected when all non-code runs are whitespace-only and at least
one contains a newline, indicating multiple ~...~ lines parsed as
one paragraph by org-element."
  (and runs
       (cl-some (lambda (r)
                  (and (not (plist-get r :code))
                       (string-match-p "\n" (plist-get r :text))))
                runs)
       (cl-every (lambda (r)
                   (or (plist-get r :code)
                       (string-blank-p (plist-get r :text))))
                 runs)))

(defun gdocs-convert--split-code-paragraph-runs (runs)
  "Split multi-line inline code RUNS into individual code paragraphs.
Concatenates all run text and splits by newlines.  Each line
becomes a separate paragraph with a single code run, matching the
Google Docs representation of monospace blocks."
  (let* ((full-text (mapconcat (lambda (r) (plist-get r :text)) runs ""))
         (lines (split-string full-text "\n"))
         (lines (if (and lines (string-empty-p (car (last lines))))
                    (butlast lines)
                  lines)))
    (mapcar (lambda (line)
              (list :type 'paragraph
                    :style 'normal
                    :contents (list (gdocs-convert--make-run
                                    line (list :code t)))
                    :id (gdocs-convert--next-id)))
            lines)))

(defun gdocs-convert--trim-runs (runs)
  "Trim leading and trailing whitespace from RUNS.
Drops whitespace-only runs at the start and end, and trims
leading whitespace from the first substantive run and trailing
whitespace from the last.  Returns nil if no text remains."
  (let ((trimmed (gdocs-convert--drop-edge-whitespace-runs runs)))
    (when trimmed
      (gdocs-convert--trim-first-and-last-runs trimmed))))

(defun gdocs-convert--drop-edge-whitespace-runs (runs)
  "Drop whitespace-only runs from the start and end of RUNS."
  (let ((result (cl-loop for r in runs
                         with started = nil
                         when (or started
                                  (not (string-blank-p
                                        (plist-get r :text))))
                         collect r
                         and do (setq started t))))
    (nreverse (cl-loop for r in (nreverse result)
                       with started = nil
                       when (or started
                                (not (string-blank-p
                                      (plist-get r :text))))
                       collect r
                       and do (setq started t)))))

(defun gdocs-convert--trim-first-and-last-runs (runs)
  "Trim leading whitespace from first run and trailing from last in RUNS."
  (when runs
    (let* ((first (car runs))
           (first-text (s-trim-left (plist-get first :text)))
           (last-elt (car (last runs)))
           (last-text (s-trim-right (plist-get last-elt :text))))
      (when (string-empty-p first-text)
        (setq runs (cdr runs))
        (when runs
          (setq first (car runs))
          (setq first-text (s-trim-left (plist-get first :text)))))
      (when runs
        (setcar runs (plist-put (copy-sequence first) :text first-text))
        (when (string-empty-p last-text)
          (setq runs (butlast runs))
          (when runs
            (setq last-elt (car (last runs)))
            (setq last-text (s-trim-right (plist-get last-elt :text)))))
        (when runs
          (setcar (last runs) (plist-put (copy-sequence last-elt)
                                         :text last-text))))
      runs)))

;; ---------------------------------------------------------------------------
;;; Objects -> text runs

(defun gdocs-convert--objects-to-runs (objects)
  "Convert a list of org-element OBJECTS to a flat list of text run plists.
Adjacent runs with identical formatting are merged to match the
coarser boundaries from Google Docs JSON conversion."
  (let ((runs nil))
    (dolist (obj (if (listp objects) objects (list objects)))
      (setq runs (nconc runs (gdocs-convert--object-to-runs obj nil))))
    (gdocs-convert--merge-adjacent-runs runs)))

(defun gdocs-convert--object-to-runs (object inherited-props)
  "Convert a single org-element OBJECT to text runs.
INHERITED-PROPS is a plist of formatting inherited from parent
objects (e.g., bold wrapping italic)."
  (if (stringp object)
      (gdocs-convert--string-to-run object inherited-props)
    (let ((runs (gdocs-convert--typed-object-to-runs
                 object inherited-props))
          (post-blank (or (org-element-property :post-blank object) 0)))
      (when (> post-blank 0)
        (setq runs (append runs
                           (list (gdocs-convert--make-run
                                  (make-string post-blank ?\s)
                                  inherited-props)))))
      runs)))

(defun gdocs-convert--typed-object-to-runs (object inherited-props)
  "Dispatch OBJECT by type to produce text runs.
INHERITED-PROPS is a plist of formatting inherited from parent
objects."
  (pcase (org-element-type object)
    ('bold (gdocs-convert--markup-to-runs object inherited-props :bold))
    ('italic (gdocs-convert--markup-to-runs object inherited-props :italic))
    ('underline (gdocs-convert--markup-to-runs object inherited-props :underline))
    ('strike-through (gdocs-convert--markup-to-runs object inherited-props :strikethrough))
    ('code (gdocs-convert--code-object-to-run object inherited-props))
    ('verbatim (gdocs-convert--code-object-to-run object inherited-props))
    ('link (gdocs-convert--link-to-runs object inherited-props))
    ('timestamp (gdocs-convert--timestamp-to-runs object inherited-props))
    ('entity (gdocs-convert--entity-to-run object inherited-props))
    ('line-break (list (gdocs-convert--make-run "\n" inherited-props)))
    (_ (gdocs-convert--fallback-object-to-runs object inherited-props))))

(defun gdocs-convert--string-to-run (text inherited-props)
  "Create a text run list from plain TEXT string with INHERITED-PROPS."
  (if (string-empty-p text)
      nil
    (list (gdocs-convert--make-run
           (substring-no-properties text) inherited-props))))

(defun gdocs-convert--markup-to-runs (object inherited-props formatting-key)
  "Convert a markup OBJECT (bold, italic, etc.) to text runs.
INHERITED-PROPS carries parent formatting.  FORMATTING-KEY is the
keyword to set (e.g. :bold)."
  (let ((new-props (plist-put (copy-sequence inherited-props)
                              formatting-key t)))
    (let ((runs nil))
      (dolist (child (org-element-contents object))
        (setq runs (nconc runs
                          (gdocs-convert--object-to-runs child new-props))))
      runs)))

(defun gdocs-convert--code-object-to-run (object _inherited-props)
  "Convert a code or verbatim OBJECT to a text run with :code t.
Post-blank whitespace is handled by `gdocs-convert--object-to-runs'."
  (let* ((value (org-element-property :value object))
         (new-props (list :code t)))
    (list (gdocs-convert--make-run value new-props))))

(defun gdocs-convert--link-to-runs (link inherited-props)
  "Convert a LINK object to text runs with :link set.
INHERITED-PROPS carries parent formatting.  Preserves any
formatting (e.g. italic, bold) within the link description.
When the URL is nil (e.g. an org file with no Google Doc
counterpart), emits plain text with no hyperlink."
  (let* ((url (gdocs-convert--link-url link))
         (children (org-element-contents link))
         (props (if url
                    (plist-put (copy-sequence inherited-props) :link url)
                  inherited-props)))
    (if children
        (let ((runs nil))
          (dolist (child children)
            (setq runs (nconc runs
                              (gdocs-convert--object-to-runs child props))))
          (or runs (list (gdocs-convert--make-run
                          (or url (org-element-property :raw-link link))
                          props))))
      (list (gdocs-convert--make-run
             (or url (org-element-property :raw-link link))
             props)))))

(defun gdocs-convert--link-url (link)
  "Extract the URL string from a LINK element.
When `gdocs-convert--link-context' is bound, resolves file: and
id: links to Google Docs URLs for cross-document linking."
  (let ((type (org-element-property :type link))
        (path (org-element-property :path link))
        (search (org-element-property :search-option link)))
    (cond
     ((member type '("http" "https" "ftp" "mailto"))
      (concat type ":" path))
     ((and gdocs-convert--link-context (string= type "file"))
      (gdocs-convert--resolve-file-link path search))
     ((and gdocs-convert--link-context (string= type "id"))
      (gdocs-convert--resolve-id-link path))
     ((string= type "fuzzy")
      ;; Same-document heading link: [[*Heading]] or [[#custom-id]]
      (if (and gdocs-convert--link-context
               (string-prefix-p "*" path))
          (gdocs-convert--resolve-same-doc-heading-link
           (substring path 1))
        path))
     ;; Preserve file: prefix for non-org file links
     ((string= type "file")
      (concat "file:" path))
     ;; Preserve id: prefix for org-id links
     ((string= type "id")
      (concat "id:" path))
     (t path))))

(defun gdocs-convert--resolve-file-link (path search)
  "Resolve a file: link PATH to a Google Docs URL.
SEARCH is the search option from org-element (e.g. \"*Heading\").
If the target is an org file with no Google Doc ID, returns nil so
the link is rendered as plain text in Google Docs.  Non-org file
links are preserved with a file: prefix."
  (let* ((buffer-file (plist-get gdocs-convert--link-context :buffer-file))
         (abs-path (if (file-name-absolute-p path)
                       path
                     (expand-file-name path
                                       (file-name-directory buffer-file))))
         (target-doc-id (gdocs-convert--read-file-local-gdocs-id abs-path))
         ;; Strip * prefix from heading search options
         (heading-text (when (and search (string-prefix-p "*" search))
                         (substring search 1))))
    (cond
     (target-doc-id
      (gdocs-convert--make-docs-url target-doc-id heading-text))
     ;; Org file without a Google Doc: no link (plain text)
     ((string-suffix-p ".org" path t)
      nil)
     (t
      (concat "file:" path)))))

(defun gdocs-convert--resolve-id-link (id)
  "Resolve an id: link ID to a Google Docs URL.
Uses `org-id-find' to locate the target file, then reads its
`gdocs-document-id'.  Returns nil when the target has no Google
Doc, so the link is rendered as plain text."
  (when-let* ((location (org-id-find id)))
    (let* ((file (if (consp location) (car location) location))
           (target-doc-id (gdocs-convert--read-file-local-gdocs-id file)))
      (when target-doc-id
        (let* ((pos (when (consp location) (cdr location)))
               (heading-text (when pos
                               (gdocs-convert--heading-at-pos file pos))))
          (gdocs-convert--make-docs-url target-doc-id heading-text))))))

(defun gdocs-convert--resolve-same-doc-heading-link (heading-text)
  "Resolve a same-document heading link to HEADING-TEXT.
When the current document's heading cache is populated, returns a
Google Docs URL with heading anchor.  Otherwise returns the path."
  (let* ((buffer-file (plist-get gdocs-convert--link-context :buffer-file))
         (current-doc-id (when buffer-file
                           (gdocs-convert--read-file-local-gdocs-id
                            buffer-file))))
    (if current-doc-id
        (gdocs-convert--make-docs-url current-doc-id heading-text)
      (concat "*" heading-text))))

(defun gdocs-convert--heading-at-pos (file pos)
  "Return the heading text at position POS in FILE, or nil."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (goto-char pos)
    (when (org-at-heading-p)
      (org-element-property :raw-value (org-element-at-point)))))

(defun gdocs-convert--make-docs-url (doc-id &optional heading-search)
  "Construct a Google Docs URL for DOC-ID.
If HEADING-SEARCH is non-nil, look up the heading ID in the
heading cache and append a #heading=h.ID fragment."
  (let ((base (format "https://docs.google.com/document/d/%s/edit" doc-id)))
    (if heading-search
        (let* ((cache-entry (gethash doc-id gdocs-convert--heading-cache))
               (normalized (s-trim (downcase heading-search)))
               (heading-id
                (cl-loop for (text . id) in cache-entry
                         when (string= (s-trim (downcase text)) normalized)
                         return id)))
          (if heading-id
              (concat base "#heading=h." heading-id)
            base))
      base)))

(defun gdocs-convert--timestamp-to-runs (timestamp inherited-props)
  "Convert a TIMESTAMP object to a text run with a marker.
INHERITED-PROPS carries parent formatting."
  (let ((raw (org-element-interpret-data timestamp)))
    (list (gdocs-convert--make-run raw inherited-props))))

(defun gdocs-convert--entity-to-run (entity inherited-props)
  "Convert an ENTITY object to a text run.
INHERITED-PROPS carries parent formatting."
  (let ((utf8 (org-element-property :utf-8 entity)))
    (list (gdocs-convert--make-run (or utf8 "") inherited-props))))

(defun gdocs-convert--fallback-object-to-runs (object inherited-props)
  "Handle an unknown OBJECT type by interpreting it as text.
INHERITED-PROPS carries parent formatting."
  (let ((interpreted (org-element-interpret-data object)))
    (when (and interpreted (not (string-empty-p interpreted)))
      (list (gdocs-convert--make-run interpreted inherited-props)))))

(defun gdocs-convert--make-run (text props)
  "Create a text run plist for TEXT with formatting from PROPS."
  (list :text text
        :bold (plist-get props :bold)
        :italic (plist-get props :italic)
        :underline (plist-get props :underline)
        :strikethrough (plist-get props :strikethrough)
        :code (plist-get props :code)
        :link (plist-get props :link)))

(defun gdocs-convert--make-plain-run (text)
  "Create a plain (unformatted) text run plist for TEXT."
  (gdocs-convert--make-run text nil))

;; ---------------------------------------------------------------------------
;;; Lists -> IR

(defun gdocs-convert--plain-list-to-ir (plain-list depth)
  "Convert a PLAIN-LIST element to a list of IR elements at nesting DEPTH."
  (let ((list-type (gdocs-convert--org-list-type plain-list))
        (result nil))
    (dolist (item (org-element-contents plain-list))
      (when (eq (org-element-type item) 'item)
        (setq result
              (nconc result
                     (gdocs-convert--item-to-ir item list-type depth)))))
    result))

(defun gdocs-convert--org-list-type (plain-list)
  "Determine the IR list type symbol for PLAIN-LIST.
Returns `bullet', `number', or `check'."
  (let ((type (org-element-property :type plain-list)))
    (pcase type
      ('ordered 'number)
      ('descriptive 'bullet)
      (_ (if (gdocs-convert--list-has-checkboxes-p plain-list)
             'check
           'bullet)))))

(defun gdocs-convert--list-has-checkboxes-p (plain-list)
  "Return non-nil if any item in PLAIN-LIST has a checkbox."
  (org-element-map plain-list 'item
    (lambda (item)
      (org-element-property :checkbox item))
    nil t))

(defun gdocs-convert--strip-continuation-indent (runs)
  "Strip continuation line indentation from list item RUNS.
When `org-element' parses a multi-line list item, it preserves
the structural indentation in the text.  This function removes
it so the IR text is clean for round-tripping."
  (mapcar (lambda (run)
            (let ((text (plist-get run :text)))
              (if (string-match-p "\n" text)
                  (plist-put (copy-sequence run) :text
                             (replace-regexp-in-string
                              "\n[ \t]+" "\n" text))
                run)))
          runs))

(defun gdocs-convert--item-to-ir (item list-type depth)
  "Convert a list ITEM to IR elements.
LIST-TYPE is `bullet', `number', or `check'.  DEPTH is the
0-based nesting level."
  (let* ((checkbox (org-element-property :checkbox item))
         (effective-type (if checkbox 'check list-type))
         (checked (eq checkbox 'on))
         (paragraph (org-element-map item 'paragraph #'identity nil t))
         (runs (when paragraph
                 (gdocs-convert--strip-continuation-indent
                  (gdocs-convert--objects-to-runs
                   (org-element-contents paragraph)))))
         (trimmed (gdocs-convert--trim-runs runs))
         (list-plist (append (list :type effective-type :level depth)
                             (when (eq effective-type 'check)
                               (list :checked checked))))
         (ir-element (list :type 'paragraph
                           :style 'normal
                           :list list-plist
                           :contents (or trimmed
                                         (list (gdocs-convert--make-plain-run "")))
                           :id (gdocs-convert--next-id)))
         (result (list ir-element)))
    (dolist (child (org-element-contents item))
      (when (eq (org-element-type child) 'plain-list)
        (setq result
              (nconc result
                     (gdocs-convert--plain-list-to-ir child (1+ depth))))))
    result))

;; ---------------------------------------------------------------------------
;;; Tables -> IR

(defun gdocs-convert--table-to-ir (table)
  "Convert an org TABLE element to an IR table element."
  (let ((rows (gdocs-convert--table-rows table)))
    (list :type 'table
          :rows rows
          :id (gdocs-convert--next-id))))

(defun gdocs-convert--table-rows (table)
  "Extract rows from an org TABLE as a list of lists of text run lists.
Skips horizontal rule rows."
  (let ((result nil))
    (dolist (row (org-element-contents table))
      (when (and (eq (org-element-type row) 'table-row)
                 (eq (org-element-property :type row) 'standard))
        (push (gdocs-convert--table-row-cells row) result)))
    (nreverse result)))

(defun gdocs-convert--table-row-cells (row)
  "Extract cells from a table ROW as a list of text run lists."
  (let ((cells nil))
    (dolist (cell (org-element-contents row))
      (when (eq (org-element-type cell) 'table-cell)
        (push (gdocs-convert--objects-to-runs
               (org-element-contents cell))
              cells)))
    (nreverse cells)))

;; ---------------------------------------------------------------------------
;;; Quote blocks -> IR

(defun gdocs-convert--quote-block-to-ir (quote-block)
  "Convert a QUOTE-BLOCK element to an IR element."
  (let* ((paragraphs (org-element-map quote-block 'paragraph
                       #'identity))
         (all-runs nil))
    (dolist (para paragraphs)
      (when all-runs
        (setq all-runs (append all-runs
                               (list (gdocs-convert--make-plain-run "\n")))))
      (let ((runs (gdocs-convert--objects-to-runs
                   (org-element-contents para))))
        (setq all-runs (append all-runs runs))))
    (list :type 'paragraph
          :style 'quote
          :contents (gdocs-convert--trim-runs all-runs)
          :id (gdocs-convert--next-id))))

;; ---------------------------------------------------------------------------
;;; Src blocks / keywords -> IR (org-only markers)

(defun gdocs-convert--src-block-to-ir (src-block)
  "Convert a SRC-BLOCK to an IR element with a marker for round-trip."
  (let* ((language (or (org-element-property :language src-block) ""))
         (value (org-element-property :value src-block))
         (display-text (s-trim-right (or value ""))))
    (list :type 'paragraph
          :style 'normal
          :contents (list (gdocs-convert--make-run display-text
                                                   (list :code t)))
          :gdocs-marker (list :type 'src-block
                              :data (list :language language
                                          :value value))
          :id (gdocs-convert--next-id))))

(defun gdocs-convert--example-block-to-ir (example-block)
  "Convert an EXAMPLE-BLOCK to a list of monospace paragraph IR elements.
Each line becomes a separate paragraph with a code run, matching
the Google Docs representation."
  (let* ((value (or (org-element-property :value example-block) ""))
         (lines (split-string (s-trim-right value) "\n")))
    (mapcar (lambda (line)
              (list :type 'paragraph
                    :style 'normal
                    :contents (list (gdocs-convert--make-run
                                    line (list :code t)))
                    :id (gdocs-convert--next-id)))
            lines)))

(defun gdocs-convert--keyword-to-ir (keyword)
  "Convert a preservable org KEYWORD to an IR element with a marker."
  (let ((key (org-element-property :key keyword))
        (value (org-element-property :value keyword)))
    (list :type 'paragraph
          :style 'normal
          :contents (list (gdocs-convert--make-plain-run
                           (format "#+%s: %s" key value)))
          :gdocs-marker (list :type 'keyword
                              :data (list :key key :value value))
          :id (gdocs-convert--next-id))))

;; ---------------------------------------------------------------------------
;;; Public API: IR -> org

(defun gdocs-convert-ir-to-org (ir)
  "Convert an IR element list to an org-mode string.
Returns a well-formatted org string.  Consecutive all-code
paragraphs (monospace lines without a src-block marker) are
grouped into a single =#+BEGIN_EXAMPLE= block."
  (let ((parts nil)
        (prev-type nil)
        (elements (seq-into ir 'vector))
        (i 0)
        (len (length ir)))
    (while (< i len)
      (let ((element (aref elements i)))
        (cond
         ;; Empty paragraph (nil contents): skip in org output but
         ;; preserve in IR for correct index computation.
         ((and (eq (plist-get element :type) 'paragraph)
               (null (plist-get element :contents)))
          (setq i (1+ i)))
         ;; Collect consecutive all-code paragraphs into an example block
         ((gdocs-convert--all-code-paragraph-p element)
          (let ((code-lines nil))
            (while (and (< i len)
                        (gdocs-convert--all-code-paragraph-p
                         (aref elements i)))
              (push (gdocs-convert--plain-text (aref elements i))
                    code-lines)
              (setq i (1+ i)))
            (when (and parts prev-type)
              (push "" parts))
            (push (format "#+BEGIN_EXAMPLE\n%s\n#+END_EXAMPLE"
                          (s-join "\n" (nreverse code-lines)))
                  parts)
            (setq prev-type 'paragraph)))
         ;; Normal element
         (t
          (let* ((type (plist-get element :type))
                 (needs-blank (gdocs-convert--needs-blank-line-p
                               prev-type element))
                 (org-text (gdocs-convert--ir-element-to-org element)))
            (when (and needs-blank parts)
              (push "" parts))
            (push org-text parts)
            (setq prev-type type))
          (setq i (1+ i))))))
    (concat (s-join "\n" (nreverse parts)) "\n")))

(defun gdocs-convert--all-code-paragraph-p (element)
  "Return non-nil if ELEMENT is a normal paragraph with all-code runs.
Paragraphs that carry a src-block marker are excluded, since they
already round-trip via named ranges."
  (and (eq (plist-get element :type) 'paragraph)
       (eq (plist-get element :style) 'normal)
       (not (plist-get element :list))
       (not (plist-get element :gdocs-marker))
       (let ((runs (plist-get element :contents)))
         (and runs (seq-every-p (lambda (r) (plist-get r :code)) runs)))))

(defun gdocs-convert--plain-text (element)
  "Return the concatenated plain text of ELEMENT's runs."
  (mapconcat (lambda (r) (plist-get r :text))
             (plist-get element :contents) ""))

(defun gdocs-convert--needs-blank-line-p (prev-type element)
  "Return non-nil if a blank line is needed before ELEMENT.
PREV-TYPE is the :type of the previous element."
  (when prev-type
    (let ((type (plist-get element :type))
          (list-info (plist-get element :list)))
      (or (eq type 'table)
          (eq type 'horizontal-rule)
          (and (eq type 'paragraph)
               (not list-info)
               (not (eq prev-type 'horizontal-rule)))))))

(defun gdocs-convert--ir-element-to-org (element)
  "Convert a single IR ELEMENT to an org string."
  (pcase (plist-get element :type)
    ('paragraph (gdocs-convert--ir-paragraph-to-org element))
    ('table (gdocs-convert--ir-table-to-org element))
    ('horizontal-rule "-----")))

(defun gdocs-convert--ir-paragraph-to-org (element)
  "Convert an IR paragraph ELEMENT to an org string."
  (let ((style (plist-get element :style))
        (list-info (plist-get element :list))
        (marker (plist-get element :gdocs-marker))
        (text (gdocs-convert--runs-to-org (plist-get element :contents))))
    (cond
     (list-info
      (gdocs-convert--format-list-item list-info text))
     ((eq style 'title)
      (format "#+TITLE: %s" text))
     ((gdocs-convert--heading-style-p style)
      (gdocs-convert--format-heading style text marker))
     ((eq style 'quote)
      (format "#+BEGIN_QUOTE\n%s\n#+END_QUOTE" text))
     ((and marker (eq (plist-get marker :type) 'src-block))
      (gdocs-convert--format-src-block marker))
     ((and marker (eq (plist-get marker :type) 'keyword))
      (let ((data (plist-get marker :data)))
        (format "#+%s: %s" (plist-get data :key) (plist-get data :value))))
     (t text))))

(defun gdocs-convert--heading-style-p (style)
  "Return non-nil if STYLE is a heading style."
  (and style
       (string-prefix-p "heading-" (symbol-name style))))

(defun gdocs-convert--format-heading (style text marker)
  "Format a heading with STYLE, TEXT, and optional MARKER for org-only data."
  (let* ((level (string-to-number
                 (substring (symbol-name style)
                            (length "heading-"))))
         (stars (make-string level ?*))
         (todo (gdocs-convert--extract-marker-field marker 'todo))
         (tags (gdocs-convert--extract-marker-field marker 'tags))
         (priority (gdocs-convert--extract-marker-field marker 'priority))
         (head (concat stars " "
                       (when todo (concat todo " "))
                       (when priority (format "[#%s] " priority))
                       text))
         (scheduled (gdocs-convert--extract-marker-field marker 'scheduled))
         (deadline (gdocs-convert--extract-marker-field marker 'deadline))
         (planning-parts nil))
    (when deadline
      (push (format "DEADLINE: %s" deadline) planning-parts))
    (when scheduled
      (push (format "SCHEDULED: %s" scheduled) planning-parts))
    (let ((heading (if tags
                       (gdocs-convert--heading-with-tags head tags)
                     head)))
      (if planning-parts
          (concat heading "\n" (s-join " " planning-parts))
        heading))))

(defun gdocs-convert--heading-with-tags (head tags-string)
  "Format HEAD with TAGS-STRING aligned with org-mode convention."
  (format "%s :%s:" head tags-string))

(defun gdocs-convert--extract-marker-field (marker type-sym)
  "Extract the :data value for TYPE-SYM from MARKER.
MARKER can be a single marker plist or a list of marker plists."
  (cond
   ((null marker) nil)
   ((and (plist-get marker :type)
         (eq (plist-get marker :type) type-sym))
    (plist-get marker :data))
   ;; List-of-plists shape: first element is itself a list
   ((listp (car marker))
    (let ((found nil))
      (dolist (m marker)
        (when (eq (plist-get m :type) type-sym)
          (setq found (plist-get m :data))))
      found))
   (t nil)))

(defun gdocs-convert--format-list-item (list-info text)
  "Format a list item with LIST-INFO plist and TEXT content.
If TEXT contains newlines (from vertical tabs in Google Docs),
indent continuation lines to keep them inside the list item."
  (let* ((type (plist-get list-info :type))
         (level (plist-get list-info :level))
         (indent (make-string (* level 2) ?\s))
         (marker (gdocs-convert--list-marker type list-info))
         (cont-indent (make-string (+ (* level 2) (length marker)) ?\s))
         (indented-text (replace-regexp-in-string
                         "\n" (concat "\n" cont-indent) text)))
    (concat indent marker indented-text)))

(defun gdocs-convert--list-marker (type list-info)
  "Return the org list marker string for TYPE and LIST-INFO."
  (pcase type
    ('bullet "- ")
    ('number "1. ")
    ('check (if (plist-get list-info :checked)
                "- [X] "
              "- [ ] "))))

(defun gdocs-convert--format-src-block (marker)
  "Format a src block from its MARKER data."
  (let* ((data (plist-get marker :data))
         (language (plist-get data :language))
         (value (plist-get data :value)))
    (concat "#+BEGIN_SRC" (if (s-present? language)
                              (concat " " language)
                            "")
            "\n" value "#+END_SRC")))

;; ---------------------------------------------------------------------------
;;; Text runs -> org text

(defun gdocs-convert--runs-to-org (runs)
  "Convert a list of text RUNS to an org-formatted string."
  (mapconcat #'gdocs-convert--run-to-org runs ""))

(defun gdocs-convert--run-to-org (run)
  "Convert a single text RUN plist to an org-formatted string.
When `gdocs-convert--link-context' is bound, reverse-resolves
Google Docs URLs back to org file: links."
  (let ((text (plist-get run :text))
        (bold (plist-get run :bold))
        (italic (plist-get run :italic))
        (underline (plist-get run :underline))
        (strikethrough (plist-get run :strikethrough))
        (code (plist-get run :code))
        (link (plist-get run :link)))
    (when code
      (setq text (format "~%s~" text)))
    (when bold
      (setq text (gdocs-convert--wrap-emphasis "*" text)))
    (when italic
      (setq text (gdocs-convert--wrap-emphasis "/" text)))
    (when underline
      (setq text (gdocs-convert--wrap-emphasis "_" text)))
    (when strikethrough
      (setq text (gdocs-convert--wrap-emphasis "+" text)))
    (when link
      (let ((resolved-link (if gdocs-convert--link-context
                               (gdocs-convert--reverse-resolve-link link)
                             link)))
        (setq text (format "[[%s][%s]]" resolved-link text))))
    text))

(defun gdocs-convert--reverse-resolve-link (url)
  "Reverse-resolve a Google Docs URL to an org link.
If URL matches a known Google Doc, returns an org link.  For
same-document heading links, returns *Heading.  For
cross-document links, returns file:relative-path::*Heading.
Falls back to URL unchanged for unknown documents."
  (if (string-match gdocs-convert--docs-url-regexp url)
      (let* ((doc-id (match-string 1 url))
             (heading-id (match-string 2 url))
             (docid-map (plist-get gdocs-convert--link-context :docid-map))
             (buffer-file (plist-get gdocs-convert--link-context :buffer-file))
             (target-file (when docid-map (gethash doc-id docid-map))))
        (if target-file
            (let ((heading-text (gdocs-convert--lookup-heading-text
                                 doc-id heading-id))
                  (same-doc-p (gdocs-convert--same-file-p
                               target-file buffer-file)))
              (gdocs-convert--format-org-link
               target-file buffer-file heading-text same-doc-p))
          url))
    url))

(defun gdocs-convert--lookup-heading-text (doc-id heading-id)
  "Look up heading text for HEADING-ID in DOC-ID's cache.
Returns the heading text string or nil."
  (when heading-id
    (let* ((reverse-key (concat doc-id "-reverse"))
           (reverse-alist (gethash reverse-key
                                   gdocs-convert--heading-cache)))
      (cdr (assoc heading-id reverse-alist)))))

(defun gdocs-convert--same-file-p (file-a file-b)
  "Return non-nil if FILE-A and FILE-B refer to the same file."
  (and file-a file-b
       (string= (expand-file-name file-a)
                (expand-file-name file-b))))

(defun gdocs-convert--format-org-link (target-file buffer-file
                                                    heading-text same-doc-p)
  "Format an org link to TARGET-FILE from BUFFER-FILE.
HEADING-TEXT is the heading anchor, or nil.  SAME-DOC-P
indicates a link within the same document."
  (cond
   ((and same-doc-p heading-text)
    (gdocs-convert--same-doc-heading-link buffer-file heading-text))
   (heading-text
    (format "file:%s::*%s"
            (file-relative-name target-file
                                (file-name-directory buffer-file))
            heading-text))
   (t
    (concat "file:"
            (file-relative-name target-file
                                (file-name-directory buffer-file))))))

(defun gdocs-convert--same-doc-heading-link (file heading-text)
  "Format a same-document link to HEADING-TEXT in FILE.
Produces id:UUID when the heading has an org ID, preserving
robust links that survive heading renames.  Falls back to
*Heading when no org ID is found."
  (if-let* ((org-id (gdocs-convert--find-heading-org-id
                      file heading-text)))
      (concat "id:" org-id)
    (concat "*" heading-text)))

(defun gdocs-convert--find-heading-org-id (file heading-text)
  "Find the org ID for the heading matching HEADING-TEXT in FILE.
Returns the UUID string or nil."
  (when (and file heading-text (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (delay-mode-hooks (org-mode))
      (goto-char (point-min))
      (let ((normalized (s-trim (downcase heading-text))))
        (catch 'found
          (while (re-search-forward org-heading-regexp nil t)
            (when (string= (s-trim (downcase (org-get-heading t t t t)))
                           normalized)
              (throw 'found (org-entry-get nil "ID")))))))))

(defun gdocs-convert--wrap-emphasis (marker text)
  "Wrap TEXT with emphasis MARKER, moving edge whitespace outside.
Org-mode emphasis markers require a non-whitespace character
immediately inside the opening and closing markers.  This
function moves any leading or trailing whitespace outside the
markers to produce valid emphasis markup."
  (if (string-blank-p text)
      text
    (let ((leading "")
          (trailing "")
          (inner text))
      (when (string-match "\\`\\([ \t\n]+\\)" inner)
        (setq leading (match-string 1 inner))
        (setq inner (substring inner (match-end 1))))
      (when (and (not (string-empty-p inner))
                 (string-match "\\([ \t\n]+\\)\\'" inner))
        (setq trailing (match-string 1 inner))
        (setq inner (substring inner 0 (match-beginning 1))))
      (if (string-empty-p inner)
          text
        (concat leading marker inner marker trailing)))))

;; ---------------------------------------------------------------------------
;;; IR table -> org

(defun gdocs-convert--ir-table-to-org (element)
  "Convert an IR table ELEMENT to an org table string."
  (let* ((rows (plist-get element :rows))
         (formatted-rows (mapcar #'gdocs-convert--format-table-row rows))
         (parts nil))
    (when formatted-rows
      (push (car formatted-rows) parts)
      (when (cdr formatted-rows)
        (push (gdocs-convert--table-separator (car rows)) parts)
        (dolist (row (cdr formatted-rows))
          (push row parts))))
    (s-join "\n" (nreverse parts))))

(defun gdocs-convert--format-table-row (row)
  "Format a table ROW (list of cell run lists) as an org table row string."
  (let ((cells (mapcar (lambda (cell-runs)
                         (gdocs-convert--runs-to-org cell-runs))
                       row)))
    (concat "| " (s-join " | " cells) " |")))

(defun gdocs-convert--table-separator (row)
  "Generate a table separator string matching the column count of ROW."
  (let ((cols (length row)))
    (concat "|" (s-join "+" (make-list cols "---")) "|")))

;; ---------------------------------------------------------------------------
;;; Public API: Google Docs JSON -> IR

(defun gdocs-convert-docs-json-to-ir (json)
  "Convert Google Docs JSON (alist) to an IR element list.
JSON is an alist as returned by the Google Docs API, with keys
like `body', `title', `lists', and `namedRanges'."
  (gdocs-convert--reset-ids)
  (let* ((title (alist-get 'title json))
         (body (alist-get 'body json))
         (content (alist-get 'content body))
         (lists-map (alist-get 'lists json))
         (named-ranges (alist-get 'namedRanges json))
         (markers (gdocs-convert--parse-named-range-markers named-ranges))
         (result nil))
    (when title
      (push (list :type 'paragraph
                  :style 'title
                  :contents (list (gdocs-convert--make-plain-run title))
                  :source 'metadata
                  :id (gdocs-convert--next-id))
            result))
    (seq-doseq (structural-element content)
      (let ((ir (gdocs-convert--structural-element-to-ir
                 structural-element lists-map markers)))
        (when ir
          (if (and (listp ir) (listp (car ir)) (plist-get (car ir) :type))
              (setq result (nconc (nreverse ir) result))
            (push ir result)))))
    (nreverse result)))

(defun gdocs-convert--structural-element-to-ir (element lists-map markers)
  "Convert a Google Docs structural ELEMENT to IR.
LISTS-MAP is the document's lists property for bullet type
lookup.  MARKERS is an alist of element markers from named ranges."
  (cond
   ((alist-get 'paragraph element)
    (gdocs-convert--docs-paragraph-to-ir
     (alist-get 'paragraph element) lists-map markers))
   ((alist-get 'table element)
    (gdocs-convert--docs-table-to-ir (alist-get 'table element)))
   ((alist-get 'sectionBreak element)
    nil)
   (t nil)))

;; ---------------------------------------------------------------------------
;;; Google Docs paragraph -> IR

(defun gdocs-convert--docs-paragraph-to-ir (paragraph lists-map _markers)
  "Convert a Google Docs PARAGRAPH to an IR element.
LISTS-MAP is the document lists property.  MARKERS is parsed
named-range marker data."
  (let* ((elements (alist-get 'elements paragraph))
         (style-obj (alist-get 'paragraphStyle paragraph))
         (named-style (alist-get 'namedStyleType style-obj))
         (bullet (alist-get 'bullet paragraph))
         (ir-style (gdocs-convert--docs-style-to-ir named-style))
         (runs (gdocs-convert--docs-elements-to-runs elements))
         (trimmed (gdocs-convert--trim-trailing-newline-runs runs))
         (list-info (when bullet
                      (gdocs-convert--docs-bullet-to-list
                       bullet lists-map))))
    (append (list :type 'paragraph
                  :style (if list-info 'normal ir-style)
                  :contents (or trimmed nil)
                  :id (gdocs-convert--next-id))
            (when list-info (list :list list-info)))))

(defun gdocs-convert--docs-style-to-ir (named-style)
  "Convert a Google Docs NAMED-STYLE string to an IR style symbol."
  (pcase named-style
    ("TITLE" 'title)
    ("HEADING_1" 'heading-1)
    ("HEADING_2" 'heading-2)
    ("HEADING_3" 'heading-3)
    ("HEADING_4" 'heading-4)
    ("HEADING_5" 'heading-5)
    ("HEADING_6" 'heading-6)
    (_ 'normal)))

(defun gdocs-convert--docs-elements-to-runs (elements)
  "Convert Google Docs paragraph ELEMENTS to IR text runs.
ELEMENTS may be a list or a vector.  Adjacent runs with
identical formatting are merged, and edge whitespace on
formatted runs is normalized."
  (let ((runs nil))
    (seq-doseq (el elements)
      (let ((text-run (alist-get 'textRun el)))
        (when text-run
          (push (gdocs-convert--docs-text-run-to-ir text-run) runs))))
    (gdocs-convert--normalize-run-whitespace
     (gdocs-convert--merge-adjacent-runs (nreverse runs)))))

(defun gdocs-convert--merge-adjacent-runs (runs)
  "Merge adjacent RUNS that have identical formatting.
Google Docs often fragments text into many runs with the same
style due to editing history.  This merges them to match the
coarser run boundaries that org-element produces."
  (when runs
    (let ((result (list (car runs))))
      (dolist (run (cdr runs))
        (let ((prev (car result)))
          (if (gdocs-convert--runs-same-format-p prev run)
              (setcar result
                      (plist-put (copy-sequence prev) :text
                                 (concat (plist-get prev :text)
                                         (plist-get run :text))))
            (push run result))))
      (nreverse result))))

(defun gdocs-convert--runs-same-format-p (a b)
  "Return non-nil if runs A and B have identical formatting."
  (and (eq (plist-get a :bold) (plist-get b :bold))
       (eq (plist-get a :italic) (plist-get b :italic))
       (eq (plist-get a :underline) (plist-get b :underline))
       (eq (plist-get a :strikethrough) (plist-get b :strikethrough))
       (eq (plist-get a :code) (plist-get b :code))
       (equal (plist-get a :link) (plist-get b :link))))

(defun gdocs-convert--normalize-run-whitespace (runs)
  "Normalize whitespace at boundaries of formatted RUNS.
Move leading and trailing whitespace from formatted runs into
adjacent plain runs, so that run boundaries match the org-mode
emphasis model.  This prevents spurious diffs on round-trip."
  (let ((result nil))
    (dolist (run runs)
      (if (not (gdocs-convert--run-has-formatting-p run))
          (push run result)
        (let ((text (plist-get run :text))
              (leading "")
              (trailing ""))
          (when (string-match "\\`\\([ \t]+\\)" text)
            (setq leading (match-string 1 text))
            (setq text (substring text (match-end 1))))
          (when (and (not (string-empty-p text))
                     (string-match "\\([ \t]+\\)\\'" text))
            (setq trailing (match-string 1 text))
            (setq text (substring text 0 (match-beginning 1))))
          (when (not (string-empty-p leading))
            (push (gdocs-convert--make-plain-run leading) result))
          (unless (string-empty-p text)
            (push (plist-put (copy-sequence run) :text text) result))
          (when (not (string-empty-p trailing))
            (push (gdocs-convert--make-plain-run trailing) result)))))
    (nreverse result)))

(defun gdocs-convert--run-has-formatting-p (run)
  "Return non-nil if RUN has any formatting beyond plain text."
  (or (plist-get run :bold)
      (plist-get run :italic)
      (plist-get run :underline)
      (plist-get run :strikethrough)
      (plist-get run :code)
      (plist-get run :link)))

(defun gdocs-convert--strip-vertical-tabs (text)
  "Replace vertical tab characters in TEXT with newlines.
Google Docs uses vertical tabs (^K, U+000B) for soft line breaks
within paragraphs."
  (replace-regexp-in-string "\v" "\n" text))

(defun gdocs-convert--docs-text-run-to-ir (text-run)
  "Convert a Google Docs TEXT-RUN to an IR text run plist."
  (let* ((content (gdocs-convert--strip-vertical-tabs
                   (alist-get 'content text-run)))
         (style (alist-get 'textStyle text-run))
         (link-obj (alist-get 'link style))
         (url (when link-obj
                (gdocs-convert--extract-link-url link-obj))))
    (list :text content
          :bold (eq (alist-get 'bold style) t)
          :italic (eq (alist-get 'italic style) t)
          ;; Suppress underline when a URL is present: Google Docs
          ;; automatically underlines hyperlinks, so the underline is
          ;; presentational rather than semantic org emphasis.
          :underline (and (eq (alist-get 'underline style) t)
                          (not url))
          :strikethrough (eq (alist-get 'strikethrough style) t)
          :code (gdocs-convert--docs-is-monospace-p style)
          :link url)))

(defun gdocs-convert--extract-link-url (link-obj)
  "Extract a URL from a Google Docs LINK-OBJ.
Returns the explicit URL when present.  For internal heading
links (where the API returns a headingId instead of a URL),
constructs a full Google Docs URL using
`gdocs-convert--document-id'."
  (or (alist-get 'url link-obj)
      (gdocs-convert--heading-id-to-url
       (alist-get 'headingId link-obj))))

(defun gdocs-convert--heading-id-to-url (heading-id)
  "Convert internal HEADING-ID to a full Google Docs URL.
Requires `gdocs-convert--document-id' to be bound."
  (when (and heading-id gdocs-convert--document-id)
    (format "https://docs.google.com/document/d/%s/edit#heading=h.%s"
            gdocs-convert--document-id heading-id)))

(defvar gdocs-convert-monospace-font-families
  '("Courier New" "Consolas" "Source Code Pro" "Roboto Mono" "Courier")
  "Font families recognized as monospace for code detection.
Google Docs does not have a native code style, so we use font
heuristics.  When pushing, code is set to `Courier New'; when
pulling, any of these families is treated as code.")

(defun gdocs-convert--docs-is-monospace-p (style)
  "Return non-nil if STYLE indicates a monospace/code font."
  (let* ((font (alist-get 'weightedFontFamily style))
         (family (when font (alist-get 'fontFamily font))))
    (and family
         (member family gdocs-convert-monospace-font-families))))

(defun gdocs-convert--trim-trailing-newline-runs (runs)
  "Remove trailing whitespace from the last run(s) in RUNS.
Google Docs paragraphs always end with a newline character, and
may have trailing whitespace-only runs before it."
  (when runs
    (let ((result (copy-sequence runs))
          (done nil))
      (while (and result (not done))
        (let* ((last-run (car (last result)))
               (text (plist-get last-run :text))
               (trimmed (s-trim-right text)))
          (if (string-empty-p trimmed)
              (setq result (butlast result))
            (setcar (last result)
                    (plist-put (copy-sequence last-run) :text trimmed))
            (setq done t))))
      result)))

(defun gdocs-convert--docs-bullet-to-list (bullet lists-map)
  "Convert a Google Docs BULLET object to an IR :list plist.
LISTS-MAP is the document's lists property for type lookup."
  (let* ((list-id (alist-get 'listId bullet))
         (nesting-level (or (alist-get 'nestingLevel bullet) 0))
         (list-props (when list-id
                       (alist-get (intern list-id) lists-map)))
         (list-type (gdocs-convert--docs-list-type list-props nesting-level)))
    (list :type list-type :level nesting-level)))

(defun gdocs-convert--docs-list-type (list-props nesting-level)
  "Determine the IR list type from Google Docs LIST-PROPS at NESTING-LEVEL."
  (if (null list-props)
      'bullet
    (let* ((nesting (alist-get 'listProperties list-props))
           (levels (alist-get 'nestingLevels nesting))
           (level-info (when (and levels (> (length levels) nesting-level))
                         (aref levels nesting-level)))
           (glyph-type (when level-info
                         (alist-get 'glyphType level-info))))
      (if (and glyph-type
               (member glyph-type '("DECIMAL" "ALPHA" "ROMAN"
                                    "ZERO_DECIMAL")))
          'number
        'bullet))))

;; ---------------------------------------------------------------------------
;;; Named range markers

(defun gdocs-convert--parse-named-range-markers (named-ranges)
  "Parse NAMED-RANGES for gdocs-org-marker entries.
Named ranges encode org-only metadata in the format
\"gdocs-org-marker:TYPE:ID\" or \"gdocs-org-marker:TYPE:ID:DATA\"
where TYPE is the marker kind (e.g. todo, tags) and ID links to
the IR element.  Returns an alist mapping element IDs to marker plists."
  (let ((result nil))
    (when named-ranges
      (dolist (entry named-ranges)
        (let* ((name (if (consp entry) (symbol-name (car entry)) ""))
               (parts (s-split ":" name)))
          (when (and (>= (length parts) 3)
                     (string= (car parts) "gdocs-org-marker"))
            (push (cons (nth 2 parts)
                        (list :type (intern (nth 1 parts))
                              :data (when (> (length parts) 3)
                                      (s-join ":" (nthcdr 3 parts)))))
                  result)))))
    result))

;; ---------------------------------------------------------------------------
;;; Google Docs tables -> IR

(defun gdocs-convert--docs-table-to-ir (table)
  "Convert a Google Docs TABLE to an IR table element."
  (let* ((table-rows (alist-get 'tableRows table))
         (rows (seq-map #'gdocs-convert--docs-table-row-to-ir table-rows)))
    (list :type 'table
          :rows (seq-into rows 'list)
          :id (gdocs-convert--next-id))))

(defun gdocs-convert--docs-table-row-to-ir (row)
  "Convert a Google Docs table ROW to a list of cell run lists."
  (let ((cells (alist-get 'tableCells row)))
    (seq-into (seq-map #'gdocs-convert--docs-table-cell-to-runs cells)
              'list)))

(defun gdocs-convert--docs-table-cell-to-runs (cell)
  "Convert a Google Docs table CELL to a list of text runs."
  (let* ((content (alist-get 'content cell))
         (runs nil))
    (seq-doseq (element content)
      (let ((paragraph (alist-get 'paragraph element)))
        (when paragraph
          (let* ((elements (alist-get 'elements paragraph))
                 (cell-runs (gdocs-convert--docs-elements-to-runs elements))
                 (trimmed (gdocs-convert--trim-trailing-newline-runs
                           cell-runs)))
            (setq runs (nconc runs trimmed))))))
    runs))

;; ---------------------------------------------------------------------------
;;; Public API: IR -> Google Docs batchUpdate requests

(defun gdocs-convert-ir-to-docs-requests (ir &optional start-index)
  "Convert an IR element list to Google Docs batchUpdate requests.
Returns a list of request alists suitable for
`gdocs-api-batch-update'.  START-INDEX is the UTF-16 index where
the first element begins (default 1)."
  ;; Default 1: Google Docs index 0 is the document root
  (let ((index (or start-index 1))
        (requests nil)
        (element-ranges nil))
    (dolist (element ir)
      (let ((result (gdocs-convert--ir-element-to-requests element index)))
        (push (list :element element :start index
                    :end (plist-get result :index))
              element-ranges)
        (setq requests (nconc requests (plist-get result :requests)))
        (setq index (plist-get result :index))))
    ;; Post-process: merge per-paragraph bullet requests into
    ;; single-range requests per contiguous list group, so Google Docs
    ;; correctly assigns nesting levels from pre-set indentation.
    (gdocs-convert--fixup-list-nesting
     requests (nreverse element-ranges))))

(defun gdocs-convert--ir-element-to-requests (element index)
  "Convert an IR ELEMENT to batchUpdate requests starting at INDEX.
Returns a plist (:requests LIST :index NEW-INDEX)."
  (pcase (plist-get element :type)
    ('paragraph (gdocs-convert--paragraph-to-requests element index))
    ('table (gdocs-convert--table-to-requests element index))
    ('horizontal-rule (gdocs-convert--horizontal-rule-to-requests index))))

;; ---------------------------------------------------------------------------
;;; Paragraph -> requests

(defun gdocs-convert--paragraph-to-requests (element index)
  "Convert an IR paragraph ELEMENT to requests starting at INDEX.
Returns a plist (:requests LIST :index NEW-INDEX)."
  (let* ((text (gdocs-convert--runs-to-plain-text
                (plist-get element :contents)))
         (full-text (concat text "\n"))
         (text-len (gdocs-convert--string-to-utf16-length full-text))
         (insert-req (gdocs-convert--make-insert-text-request
                      full-text index))
         (style-reqs (gdocs-convert--make-paragraph-style-requests
                      element index (+ index text-len)))
         (run-reqs (gdocs-convert--make-text-style-requests
                    (plist-get element :contents) index))
         (list-reqs (gdocs-convert--make-list-requests element index
                                                       (+ index text-len)))
         (marker-reqs (gdocs-convert--make-marker-requests
                       element index (+ index text-len)))
         (all-requests (append (list insert-req)
                               style-reqs
                               run-reqs
                               list-reqs
                               marker-reqs)))
    (list :requests all-requests
          :index (+ index text-len))))

(defun gdocs-convert--runs-to-plain-text (runs)
  "Concatenate the :text values of all RUNS."
  (mapconcat (lambda (run) (plist-get run :text)) runs ""))

(defun gdocs-convert--make-insert-text-request (text index)
  "Create an insertText request for TEXT at INDEX."
  `((insertText . ((text . ,text)
                   (location . ((index . ,index)))))))

(defun gdocs-convert--make-paragraph-style-requests (element start end)
  "Create updateParagraphStyle requests for ELEMENT from START to END."
  (let* ((style (plist-get element :style))
         (named-style (gdocs-convert--ir-style-to-docs style)))
    (when named-style
      (list `((updateParagraphStyle
               . ((paragraphStyle . ((namedStyleType . ,named-style)))
                  (range . ((startIndex . ,start)
                            (endIndex . ,(1- end))))
                  (fields . "namedStyleType"))))))))

(defun gdocs-convert--ir-style-to-docs (style)
  "Convert an IR STYLE symbol to a Google Docs namedStyleType string."
  (pcase style
    ('title "TITLE")
    ('heading-1 "HEADING_1")
    ('heading-2 "HEADING_2")
    ('heading-3 "HEADING_3")
    ('heading-4 "HEADING_4")
    ('heading-5 "HEADING_5")
    ('heading-6 "HEADING_6")
    ('normal "NORMAL_TEXT")
    (_ "NORMAL_TEXT")))

(defun gdocs-convert--make-text-style-requests (runs start-index)
  "Create updateTextStyle requests for formatted RUNS starting at START-INDEX.
Each run that has any formatting generates a request."
  (let ((requests nil)
        (index start-index))
    (dolist (run runs)
      (let* ((text (plist-get run :text))
             (len (gdocs-convert--string-to-utf16-length text))
             (end (+ index len))
             (style-fields (gdocs-convert--run-to-docs-style run)))
        (when style-fields
          (push `((updateTextStyle
                   . ((textStyle . ,style-fields)
                      (range . ((startIndex . ,index)
                                (endIndex . ,end)))
                      (fields . ,(gdocs-convert--style-fields-mask
                                  style-fields)))))
                requests))
        (setq index end)))
    (nreverse requests)))

(defun gdocs-convert--run-to-docs-style (run)
  "Convert formatting of a text RUN to a Google Docs textStyle alist.
Returns nil if no formatting is applied."
  (let ((style nil))
    (when (plist-get run :link)
      (push `(link . ((url . ,(plist-get run :link)))) style))
    (when (plist-get run :code)
      (push `(weightedFontFamily . ((fontFamily . ,(car gdocs-convert-monospace-font-families)))) style))
    (when (plist-get run :strikethrough)
      (push `(strikethrough . t) style))
    (when (plist-get run :underline)
      (push `(underline . t) style))
    (when (plist-get run :italic)
      (push `(italic . t) style))
    (when (plist-get run :bold)
      (push `(bold . t) style))
    style))

(defun gdocs-convert--style-fields-mask (style-fields)
  "Compute the fields mask string for STYLE-FIELDS alist."
  (let ((fields nil))
    (dolist (pair style-fields)
      (push (symbol-name (car pair)) fields))
    (s-join "," (nreverse fields))))

(defun gdocs-convert--make-list-requests (element start end)
  "Create bullet/list requests for ELEMENT from START to END."
  (let ((list-info (plist-get element :list)))
    (when list-info
      (let* ((type (plist-get list-info :type))
             (level (plist-get list-info :level))
             (preset (gdocs-convert--list-type-to-preset type))
             (bullet-req `((createParagraphBullets
                            . ((range . ((startIndex . ,start)
                                         (endIndex . ,(1- end))))
                               (bulletPreset . ,preset)))))
             (indent-req (gdocs-convert--make-list-indent-request
                          level start end)))
        (if indent-req
            (list indent-req bullet-req)
          (list bullet-req))))))

(defun gdocs-convert--make-list-indent-request (level start end)
  "Create an updateParagraphStyle request for list nesting LEVEL.
START and END define the paragraph range.  Returns nil for level 0,
since `createParagraphBullets' already sets the correct indentation.
Each nesting level corresponds to 36pt of indent in Google Docs.

IMPORTANT: This request must appear BEFORE the corresponding
`createParagraphBullets' request in the batchUpdate sequence.
Google Docs reads pre-existing paragraph indentation when creating
bullets and uses it to determine the nesting level."
  (when (> level 0)
    (let ((indent-start (* 36 (1+ level)))
          (indent-first (+ 18 (* 36 level))))
      `((updateParagraphStyle
         . ((paragraphStyle
             . ((indentStart . ((magnitude . ,indent-start)
                                (unit . "PT")))
                (indentFirstLine . ((magnitude . ,indent-first)
                                    (unit . "PT")))))
            (range . ((startIndex . ,start)
                      (endIndex . ,(1- end))))
            (fields . "indentStart,indentFirstLine")))))))

(defun gdocs-convert--list-type-to-preset (type)
  "Convert an IR list TYPE to a Google Docs bullet preset string."
  (pcase type
    ('bullet "BULLET_DISC_CIRCLE_SQUARE")
    ('number "NUMBERED_DECIMAL_ALPHA_ROMAN")
    ('check "BULLET_CHECKBOX")))

(defun gdocs-convert--fixup-list-nesting (requests element-ranges)
  "Fix list nesting in REQUESTS by merging per-paragraph bullet requests.
ELEMENT-RANGES is a list of plists (:element EL :start S :end E) tracking
where each IR element was placed.

The Google Docs API only assigns correct nesting levels when
`createParagraphBullets' covers an entire contiguous list group in
a single request, with indentation pre-set on nested items.
Per-paragraph bullet requests produce flat lists.

This function removes per-paragraph `createParagraphBullets' and
appends a single merged request per contiguous list group."
  (let ((groups (gdocs-convert--find-list-groups element-ranges))
        (filtered (cl-remove-if
                   (lambda (req) (alist-get 'createParagraphBullets req))
                   requests)))
    (append filtered
            (mapcar #'gdocs-convert--list-group-bullet-request groups))))

(defun gdocs-convert--find-list-groups (element-ranges)
  "Find contiguous groups of list elements in ELEMENT-RANGES.
Returns a list of groups, where each group is a plist with
:start (index of first element), :end (index after last element),
and :preset (bullet preset string)."
  (let ((groups nil)
        (current-start nil)
        (current-end nil)
        (current-preset nil))
    (dolist (range element-ranges)
      (let* ((elem (plist-get range :element))
             (list-info (plist-get elem :list)))
        (if list-info
            (let ((preset (gdocs-convert--list-type-to-preset
                           (plist-get list-info :type))))
              (if (and current-start
                       (string= preset current-preset))
                  ;; Extend current group
                  (setq current-end (plist-get range :end))
                ;; Flush previous group and start new one
                (when current-start
                  (push (list :start current-start
                              :end current-end
                              :preset current-preset)
                        groups))
                (setq current-start (plist-get range :start)
                      current-end (plist-get range :end)
                      current-preset preset)))
          ;; Non-list element: flush current group
          (when current-start
            (push (list :start current-start
                        :end current-end
                        :preset current-preset)
                  groups)
            (setq current-start nil
                  current-end nil
                  current-preset nil)))))
    ;; Flush final group
    (when current-start
      (push (list :start current-start
                  :end current-end
                  :preset current-preset)
            groups))
    (nreverse groups)))

(defun gdocs-convert--list-group-bullet-request (group)
  "Create a single createParagraphBullets request for GROUP.
GROUP is a plist with :start, :end, and :preset."
  (let ((start (plist-get group :start))
        (end (plist-get group :end))
        (preset (plist-get group :preset)))
    `((createParagraphBullets
       . ((range . ((startIndex . ,start)
                    (endIndex . ,(1- end))))
          (bulletPreset . ,preset))))))

(defun gdocs-convert--make-marker-requests (element start end)
  "Create named range requests for org-only markers in ELEMENT.
START and END define the text range in the document.
Handles both single-plist and list-of-plists marker shapes."
  (let ((marker (plist-get element :gdocs-marker))
        (id (plist-get element :id)))
    (when (and marker id)
      (let ((markers (if (listp (car marker))
                         marker
                       (list marker))))
        (mapcar (lambda (m)
                  (let ((marker-type (plist-get m :type)))
                    `((createNamedRange
                       . ((name . ,(format "gdocs-org-marker:%s:%s"
                                           marker-type id))
                          (range . ((startIndex . ,start)
                                    (endIndex . ,(1- end)))))))))
                markers)))))

;; ---------------------------------------------------------------------------
;;; Table -> requests

(defun gdocs-convert--table-to-requests (element index)
  "Convert an IR table ELEMENT to requests starting at INDEX.
Returns a plist (:requests LIST :index NEW-INDEX).
After the insertTable request, generates insertText requests for
each non-empty cell, processed last-to-first so indices stay stable."
  (let* ((rows (plist-get element :rows))
         (nrows (length rows))
         (ncols (length (car rows)))
         (insert-req `((insertTable
                        . ((rows . ,nrows)
                           (columns . ,ncols)
                           (location . ((index . ,index)))))))
         (cell-text-len (gdocs-convert--table-total-cell-text-length rows))
         ;; Table UTF-16 overhead: 1 for the preserved paragraph (insertTable
         ;; keeps the paragraph at the insertion point), 1 for table-start,
         ;; 1 for table-end, plus per-row and per-cell structural markers.
         ;; See `gdocs-diff--table-utf16-length' for the corresponding
         ;; read-side formula (which uses 2 instead of 3 because the
         ;; preserved paragraph is not part of the table itself).
         (table-overhead (+ 3 (* nrows (+ 1 (* ncols 2))) cell-text-len))
         (new-index (+ index table-overhead))
         (cell-reqs (gdocs-convert--table-cell-requests rows index ncols))
         (requests (cons insert-req cell-reqs)))
    (list :requests requests
          :index new-index)))

(defun gdocs-convert--table-cell-requests (rows table-index ncols)
  "Generate insertText requests for non-empty cells in ROWS.
TABLE-INDEX is the insertTable location index.  NCOLS is the
column count.  The table body starts at TABLE-INDEX + 1 because
insertTable preserves the existing paragraph at the location.
Iterates forward and pushes, so the result is in reverse order
\(last cell first) for stable index processing."
  (let ((reqs nil)
        (r 0))
    (dolist (row rows)
      (let ((c 0))
        (dolist (cell row)
          (let ((text (gdocs-convert--runs-to-plain-text cell)))
            (when (> (length text) 0)
              ;; Cell content index: table-index + 4 accounts for the preserved
              ;; paragraph (+1), table-start (+1), first row-start (+1), and
              ;; first cell-start (+1).  Each subsequent row adds 1 + ncols*2
              ;; structural markers; each subsequent cell adds 2.
              (let ((cell-start (+ table-index 4
                                   (* r (+ 1 (* ncols 2)))
                                   (* c 2))))
                (push (gdocs-convert--make-insert-text-request
                       text cell-start)
                      reqs))))
          (setq c (1+ c))))
      (setq r (1+ r)))
    reqs))

(defun gdocs-convert--table-total-cell-text-length (rows)
  "Return total UTF-16 length of all cell text in ROWS."
  (let ((total 0))
    (dolist (row rows)
      (dolist (cell row)
        (setq total (+ total (gdocs-convert--string-to-utf16-length
                              (gdocs-convert--runs-to-plain-text cell))))))
    total))

;; ---------------------------------------------------------------------------
;;; Horizontal rule -> requests

(defun gdocs-convert--horizontal-rule-to-requests (index)
  "Create requests for a horizontal rule at INDEX.
Google Docs has no native horizontal rule element, so we represent
it as a bare newline paragraph.
Returns a plist (:requests LIST :index NEW-INDEX)."
  (let* ((text "\n")
         (len (gdocs-convert--string-to-utf16-length text))
         (insert-req (gdocs-convert--make-insert-text-request text index)))
    (list :requests (list insert-req)
          :index (+ index len))))

;; ---------------------------------------------------------------------------
;;; UTF-16 helper

(defun gdocs-convert--string-to-utf16-length (string)
  "Return the number of UTF-16 code units needed to encode STRING.
Google Docs uses UTF-16 for indexing.  Characters above U+FFFF
require 2 code units (surrogate pairs), all others require 1."
  (let ((len 0))
    (seq-doseq (char string)
      (setq len (+ len (if (> char #xFFFF) 2 1))))
    len))

(provide 'gdocs-convert)
;;; gdocs-convert.el ends here
