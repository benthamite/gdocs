;;; gdocs-convert-test.el --- Tests for gdocs-convert.el -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the bidirectional org <-> IR <-> Google Docs conversion.

;;; Code:

(require 'ert)
(require 'gdocs-convert)

;; ---------------------------------------------------------------------------
;;; Helpers

(defun gdocs-convert-test--first-content-run (ir)
  "Return the first text run from the first element of IR."
  (car (plist-get (car ir) :contents)))

(defun gdocs-convert-test--find-element (ir type &optional style)
  "Find the first IR element with TYPE and optional STYLE in IR."
  (--first (and (eq (plist-get it :type) type)
                (or (null style) (eq (plist-get it :style) style)))
           ir))

;; ---------------------------------------------------------------------------
;;; Basic paragraph

(ert-deftest gdocs-convert-test-basic-paragraph ()
  "A simple paragraph produces an IR element with :style normal."
  (let ((ir (gdocs-convert-org-string-to-ir "Hello world")))
    (should (= (length ir) 1))
    (should (eq (plist-get (car ir) :type) 'paragraph))
    (should (eq (plist-get (car ir) :style) 'normal))
    (should (string= (plist-get (gdocs-convert-test--first-content-run ir)
                                :text)
                      "Hello world"))))

;; ---------------------------------------------------------------------------
;;; Heading levels

(ert-deftest gdocs-convert-test-heading-levels ()
  "Heading levels 1-6 map to heading-1 through heading-6."
  (dolist (level '(1 2 3 4 5 6))
    (let* ((stars (make-string level ?*))
           (org-str (format "%s Heading %d" stars level))
           (ir (gdocs-convert-org-string-to-ir org-str))
           (elem (car ir)))
      (should (eq (plist-get elem :style)
                  (intern (format "heading-%d" level)))))))

(ert-deftest gdocs-convert-test-heading-level-7-clamped ()
  "Heading level 7+ is clamped to heading-6."
  (let* ((ir (gdocs-convert-org-string-to-ir "******* Deep heading"))
         (elem (car ir)))
    (should (eq (plist-get elem :style) 'heading-6))))

;; ---------------------------------------------------------------------------
;;; Text formatting

(ert-deftest gdocs-convert-test-bold ()
  "Bold markup produces a run with :bold t."
  (let* ((ir (gdocs-convert-org-string-to-ir "This is *bold* text"))
         (runs (plist-get (car ir) :contents))
         (bold-run (nth 1 runs)))
    (should (plist-get bold-run :bold))
    (should (string= (plist-get bold-run :text) "bold"))))

(ert-deftest gdocs-convert-test-italic ()
  "Italic markup produces a run with :italic t."
  (let* ((ir (gdocs-convert-org-string-to-ir "This is /italic/ text"))
         (runs (plist-get (car ir) :contents))
         (italic-run (nth 1 runs)))
    (should (plist-get italic-run :italic))
    (should (string= (plist-get italic-run :text) "italic"))))

(ert-deftest gdocs-convert-test-underline ()
  "Underline markup produces a run with :underline t."
  (let* ((ir (gdocs-convert-org-string-to-ir "This is _underlined_ text"))
         (runs (plist-get (car ir) :contents))
         (underline-run (nth 1 runs)))
    (should (plist-get underline-run :underline))
    (should (string= (plist-get underline-run :text) "underlined"))))

(ert-deftest gdocs-convert-test-strikethrough ()
  "Strikethrough markup produces a run with :strikethrough t."
  (let* ((ir (gdocs-convert-org-string-to-ir "This is +struck+ text"))
         (runs (plist-get (car ir) :contents))
         (strike-run (nth 1 runs)))
    (should (plist-get strike-run :strikethrough))
    (should (string= (plist-get strike-run :text) "struck"))))

(ert-deftest gdocs-convert-test-code ()
  "Code markup produces a run with :code t."
  (let* ((ir (gdocs-convert-org-string-to-ir "This is ~code~ text"))
         (runs (plist-get (car ir) :contents))
         (code-run (nth 1 runs)))
    (should (plist-get code-run :code))
    (should (string= (plist-get code-run :text) "code"))))

(ert-deftest gdocs-convert-test-verbatim ()
  "Verbatim markup produces a run with :code t."
  (let* ((ir (gdocs-convert-org-string-to-ir "This is =verbatim= text"))
         (runs (plist-get (car ir) :contents))
         (code-run (nth 1 runs)))
    (should (plist-get code-run :code))
    (should (string= (plist-get code-run :text) "verbatim"))))

(ert-deftest gdocs-convert-test-link ()
  "Link markup produces a run with :link set."
  (let* ((ir (gdocs-convert-org-string-to-ir
              "Visit [[https://example.com][Example]]"))
         (runs (plist-get (car ir) :contents))
         (link-run (nth 1 runs)))
    (should (string= (plist-get link-run :link) "https://example.com"))
    (should (string= (plist-get link-run :text) "Example"))))

;; ---------------------------------------------------------------------------
;;; Nested formatting

(ert-deftest gdocs-convert-test-bold-italic ()
  "Nested bold+italic produces a run with both :bold t and :italic t."
  (let* ((ir (gdocs-convert-org-string-to-ir "This is */bold-italic/* text"))
         (runs (plist-get (car ir) :contents))
         (bi-run (nth 1 runs)))
    (should (plist-get bi-run :bold))
    (should (plist-get bi-run :italic))
    (should (string= (plist-get bi-run :text) "bold-italic"))))

;; ---------------------------------------------------------------------------
;;; Unordered lists

(ert-deftest gdocs-convert-test-unordered-list ()
  "Unordered list items produce IR elements with :list info."
  (let* ((ir (gdocs-convert-org-string-to-ir "- First\n- Second\n- Third"))
         (first-elem (car ir)))
    (should (>= (length ir) 3))
    (should (eq (plist-get (plist-get first-elem :list) :type) 'bullet))
    (should (= (plist-get (plist-get first-elem :list) :level) 0))))

(ert-deftest gdocs-convert-test-nested-list ()
  "Nested list items have increasing :level."
  (let* ((ir (gdocs-convert-org-string-to-ir
              "- Parent\n  - Child\n    - Grandchild")))
    (should (>= (length ir) 3))
    (should (= (plist-get (plist-get (nth 0 ir) :list) :level) 0))
    (should (= (plist-get (plist-get (nth 1 ir) :list) :level) 1))
    (should (= (plist-get (plist-get (nth 2 ir) :list) :level) 2))))

;; ---------------------------------------------------------------------------
;;; Ordered lists

(ert-deftest gdocs-convert-test-ordered-list ()
  "Ordered list items produce IR elements with :type number."
  (let* ((ir (gdocs-convert-org-string-to-ir "1. First\n2. Second"))
         (first-elem (car ir)))
    (should (>= (length ir) 2))
    (should (eq (plist-get (plist-get first-elem :list) :type) 'number))))

;; ---------------------------------------------------------------------------
;;; Checkbox lists

(ert-deftest gdocs-convert-test-checkbox-unchecked ()
  "Unchecked checkbox items produce :type check, :checked nil."
  (let* ((ir (gdocs-convert-org-string-to-ir "- [ ] Todo item"))
         (elem (car ir))
         (list-info (plist-get elem :list)))
    (should (eq (plist-get list-info :type) 'check))
    (should-not (plist-get list-info :checked))))

(ert-deftest gdocs-convert-test-checkbox-checked ()
  "Checked checkbox items produce :type check, :checked t."
  (let* ((ir (gdocs-convert-org-string-to-ir "- [X] Done item"))
         (elem (car ir))
         (list-info (plist-get elem :list)))
    (should (eq (plist-get list-info :type) 'check))
    (should (plist-get list-info :checked))))

;; ---------------------------------------------------------------------------
;;; Tables

(ert-deftest gdocs-convert-test-table ()
  "A 2x2 org table produces an IR table element with :rows."
  (let* ((ir (gdocs-convert-org-string-to-ir
              "| A | B |\n|---+---|\n| C | D |"))
         (table-elem (gdocs-convert-test--find-element ir 'table)))
    (should table-elem)
    (let ((rows (plist-get table-elem :rows)))
      (should (= (length rows) 2))
      (should (= (length (car rows)) 2))
      (should (string= (plist-get (car (car (car rows))) :text) "A")))))

;; ---------------------------------------------------------------------------
;;; Horizontal rules

(ert-deftest gdocs-convert-test-horizontal-rule ()
  "A horizontal rule produces an IR element with :type horizontal-rule."
  (let* ((ir (gdocs-convert-org-string-to-ir "Before\n-----\nAfter"))
         (hr (gdocs-convert-test--find-element ir 'horizontal-rule)))
    (should hr)
    (should (eq (plist-get hr :type) 'horizontal-rule))))

;; ---------------------------------------------------------------------------
;;; Block quotes

(ert-deftest gdocs-convert-test-quote-block ()
  "A quote block produces an IR element with :style quote."
  (let* ((ir (gdocs-convert-org-string-to-ir
              "#+BEGIN_QUOTE\nTo be or not to be.\n#+END_QUOTE"))
         (quote-elem (gdocs-convert-test--find-element ir 'paragraph 'quote)))
    (should quote-elem)
    (should (string= (plist-get (car (plist-get quote-elem :contents)) :text)
                      "To be or not to be."))))

;; ---------------------------------------------------------------------------
;;; Round-trip fidelity

(ert-deftest gdocs-convert-test-round-trip-simple ()
  "A simple document survives org -> IR -> org round-trip."
  (let* ((original "#+TITLE: My document\n\n* Introduction\n\nHello world.\n")
         (ir (gdocs-convert-org-string-to-ir original))
         (result (gdocs-convert-ir-to-org ir)))
    (should (string= (s-trim result) (s-trim original)))))

(ert-deftest gdocs-convert-test-round-trip-formatting ()
  "Formatted text survives round-trip."
  (let* ((original "This has *bold*, /italic/, and ~code~ text.\n")
         (ir (gdocs-convert-org-string-to-ir original))
         (result (gdocs-convert-ir-to-org ir)))
    (should (string= (s-trim result) (s-trim original)))))

(ert-deftest gdocs-convert-test-round-trip-lists ()
  "List items survive round-trip."
  (let* ((original "- First\n- Second\n- Third\n")
         (ir (gdocs-convert-org-string-to-ir original))
         (result (gdocs-convert-ir-to-org ir)))
    (should (string= (s-trim result) (s-trim original)))))

(ert-deftest gdocs-convert-test-round-trip-table ()
  "A table survives round-trip."
  (let* ((original "| A | B |\n|---+---|\n| C | D |\n")
         (ir (gdocs-convert-org-string-to-ir original))
         (result (gdocs-convert-ir-to-org ir)))
    (should (string= (s-trim result) (s-trim original)))))

;; ---------------------------------------------------------------------------
;;; Google Docs JSON -> IR

(ert-deftest gdocs-convert-test-docs-json-paragraph ()
  "A Google Docs paragraph converts to a normal IR paragraph."
  (let* ((json `((title . "Test Doc")
                 (body . ((content
                           . [((paragraph
                                . ((elements
                                    . [((textRun
                                         . ((content . "Hello\n")
                                            (textStyle . ()))))])
                                   (paragraphStyle
                                    . ((namedStyleType . "NORMAL_TEXT"))))))])))))
         (ir (gdocs-convert-docs-json-to-ir json)))
    (should (>= (length ir) 2))
    (let ((para (nth 1 ir)))
      (should (eq (plist-get para :type) 'paragraph))
      (should (eq (plist-get para :style) 'normal))
      (should (string= (plist-get (car (plist-get para :contents)) :text)
                        "Hello")))))

(ert-deftest gdocs-convert-test-docs-json-heading ()
  "A Google Docs HEADING_1 converts to :style heading-1."
  (let* ((json `((body . ((content
                           . [((paragraph
                                . ((elements
                                    . [((textRun
                                         . ((content . "Title\n")
                                            (textStyle . ()))))])
                                   (paragraphStyle
                                    . ((namedStyleType . "HEADING_1"))))))])))))
         (ir (gdocs-convert-docs-json-to-ir json))
         (heading (car ir)))
    (should (eq (plist-get heading :style) 'heading-1))))

(ert-deftest gdocs-convert-test-docs-json-bold ()
  "Bold text in Google Docs JSON converts to a run with :bold t."
  (let* ((json `((body . ((content
                           . [((paragraph
                                . ((elements
                                    . [((textRun
                                         . ((content . "bold text\n")
                                            (textStyle
                                             . ((bold . t))))))])
                                   (paragraphStyle
                                    . ((namedStyleType
                                        . "NORMAL_TEXT"))))))])))))
         (ir (gdocs-convert-docs-json-to-ir json))
         (run (car (plist-get (car ir) :contents))))
    (should (plist-get run :bold))))

(ert-deftest gdocs-convert-test-docs-json-table ()
  "A Google Docs table converts to an IR table element."
  (let* ((json `((body
                  . ((content
                      . [((table
                           . ((rows . 1)
                              (columns . 2)
                              (tableRows
                               . [((tableCells
                                    . [((content
                                         . [((paragraph
                                              . ((elements
                                                  . [((textRun
                                                       . ((content . "A\n")
                                                          (textStyle))))]))))]))
                                       ((content
                                         . [((paragraph
                                              . ((elements
                                                  . [((textRun
                                                       . ((content . "B\n")
                                                          (textStyle))))]))))]))]))]))))])))))
         (ir (gdocs-convert-docs-json-to-ir json))
         (table-elem (gdocs-convert-test--find-element ir 'table)))
    (should table-elem)
    (should (= (length (plist-get table-elem :rows)) 1))
    (should (= (length (car (plist-get table-elem :rows))) 2))))

;; ---------------------------------------------------------------------------
;;; IR -> batchUpdate requests

(ert-deftest gdocs-convert-test-ir-to-requests-basic ()
  "A simple paragraph produces insertText and updateParagraphStyle requests."
  (let* ((ir (list (list :type 'paragraph
                         :style 'heading-1
                         :contents (list (gdocs-convert--make-plain-run
                                         "Hello"))
                         :id "elem-001")))
         (requests (gdocs-convert-ir-to-docs-requests ir)))
    (should (>= (length requests) 1))
    (should (alist-get 'insertText (car requests)))))

(ert-deftest gdocs-convert-test-ir-to-requests-indices ()
  "Multiple paragraphs produce requests with advancing indices."
  (let* ((ir (list (list :type 'paragraph
                         :style 'normal
                         :contents (list (gdocs-convert--make-plain-run "AB"))
                         :id "elem-001")
                   (list :type 'paragraph
                         :style 'normal
                         :contents (list (gdocs-convert--make-plain-run "CD"))
                         :id "elem-002")))
         (requests (gdocs-convert-ir-to-docs-requests ir))
         (first-insert (alist-get 'insertText (car requests)))
         (second-insert-req (--first (alist-get 'insertText it)
                                     (cdr requests)))
         (second-insert (alist-get 'insertText second-insert-req)))
    (should (= (alist-get 'index (alist-get 'location first-insert)) 1))
    (should (= (alist-get 'index (alist-get 'location second-insert)) 4))))

;; ---------------------------------------------------------------------------
;;; UTF-16 length

(ert-deftest gdocs-convert-test-utf16-ascii ()
  "ASCII characters are 1 UTF-16 code unit each."
  (should (= (gdocs-convert--string-to-utf16-length "hello") 5)))

(ert-deftest gdocs-convert-test-utf16-bmp ()
  "BMP characters (e.g., accented Latin) are 1 UTF-16 code unit each."
  (should (= (gdocs-convert--string-to-utf16-length "caf\u00e9") 4)))

(ert-deftest gdocs-convert-test-utf16-supplementary ()
  "Supplementary characters (emoji) require 2 UTF-16 code units."
  (should (= (gdocs-convert--string-to-utf16-length "\U0001F600") 2)))

(ert-deftest gdocs-convert-test-utf16-mixed ()
  "Mixed ASCII and supplementary characters compute correctly."
  (should (= (gdocs-convert--string-to-utf16-length "a\U0001F600b") 4)))

;; ---------------------------------------------------------------------------
;;; Org-only marker preservation

(ert-deftest gdocs-convert-test-todo-marker ()
  "TODO keywords are stored in :gdocs-marker."
  (let* ((ir (gdocs-convert-org-string-to-ir "* TODO Task"))
         (elem (car ir))
         (marker (plist-get elem :gdocs-marker)))
    (should marker)
    (should (eq (plist-get marker :type) 'todo))
    (should (string= (plist-get marker :data) "TODO"))))

(ert-deftest gdocs-convert-test-tags-marker ()
  "Tags are stored in :gdocs-marker."
  (let* ((ir (gdocs-convert-org-string-to-ir "* Heading :tag1:tag2:"))
         (elem (car ir))
         (marker (plist-get elem :gdocs-marker)))
    (should marker)
    (should (eq (plist-get marker :type) 'tags))
    (should (string= (plist-get marker :data) "tag1:tag2"))))

(ert-deftest gdocs-convert-test-todo-round-trip ()
  "TODO keywords survive round-trip via markers."
  (let* ((original "* TODO Important task\n")
         (ir (gdocs-convert-org-string-to-ir original))
         (result (gdocs-convert-ir-to-org ir)))
    (should (string-match-p "\\* TODO Important task" result))))

(ert-deftest gdocs-convert-test-tags-round-trip ()
  "Tags survive round-trip via markers."
  (let* ((original "* Heading :work:urgent:\n")
         (ir (gdocs-convert-org-string-to-ir original))
         (result (gdocs-convert-ir-to-org ir)))
    (should (string-match-p ":work:urgent:" result))))

(provide 'gdocs-convert-test)
;;; gdocs-convert-test.el ends here
