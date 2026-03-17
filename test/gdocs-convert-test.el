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

(ert-deftest gdocs-convert-test-nested-ordered-list ()
  "Nested ordered list items have increasing :level with type number."
  (let* ((ir (gdocs-convert-org-string-to-ir
              "1. Parent\n   1. Child\n      1. Grandchild")))
    (should (>= (length ir) 3))
    (should (eq (plist-get (plist-get (nth 0 ir) :list) :type) 'number))
    (should (= (plist-get (plist-get (nth 0 ir) :list) :level) 0))
    (should (= (plist-get (plist-get (nth 1 ir) :list) :level) 1))
    (should (= (plist-get (plist-get (nth 2 ir) :list) :level) 2))))

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

(ert-deftest gdocs-convert-test-round-trip-nested-lists ()
  "Nested list items survive round-trip."
  (let* ((original "- Parent\n  - Child\n    - Grandchild\n")
         (ir (gdocs-convert-org-string-to-ir original))
         (result (gdocs-convert-ir-to-org ir)))
    (should (string= (s-trim result) (s-trim original)))))

(ert-deftest gdocs-convert-test-round-trip-nested-ordered-lists ()
  "Nested ordered list items survive round-trip with correct nesting."
  (let* ((original "1. Parent\n   1. Child\n      1. Grandchild\n")
         (ir (gdocs-convert-org-string-to-ir original))
         (result (gdocs-convert-ir-to-org ir))
         ;; Re-parse the output to verify nesting survives
         (re-ir (gdocs-convert-org-string-to-ir result)))
    (should (= (length re-ir) 3))
    (should (= (plist-get (plist-get (nth 0 re-ir) :list) :level) 0))
    (should (= (plist-get (plist-get (nth 1 re-ir) :list) :level) 1))
    (should (= (plist-get (plist-get (nth 2 re-ir) :list) :level) 2))))

(ert-deftest gdocs-convert-test-round-trip-table ()
  "A table survives round-trip."
  (let* ((original "| A | B |\n|---+---|\n| C | D |\n")
         (ir (gdocs-convert-org-string-to-ir original))
         (result (gdocs-convert-ir-to-org ir)))
    (should (string= (s-trim result) (s-trim original)))))

;; ---------------------------------------------------------------------------
;;; List nesting requests

(ert-deftest gdocs-convert-test-nested-list-indent-requests ()
  "Nested list items generate updateParagraphStyle indent requests."
  (let* ((ir (gdocs-convert-org-string-to-ir
              "- Parent\n  - Child\n    - Grandchild"))
         (requests (gdocs-convert-ir-to-docs-requests ir)))
    ;; Level 0: only createParagraphBullets, no indent request
    (should (cl-some (lambda (r) (alist-get 'createParagraphBullets r))
                     requests))
    ;; Level 1: indent request with indentStart=72, indentFirstLine=54
    (let ((indent-reqs
           (cl-remove-if-not
            (lambda (r)
              (let ((ups (alist-get 'updateParagraphStyle r)))
                (when ups
                  (let ((style (alist-get 'paragraphStyle ups)))
                    (alist-get 'indentStart style)))))
            requests)))
      (should (>= (length indent-reqs) 2))  ;; level 1 and level 2
      ;; Check level 1 indentation (72pt start, 54pt first)
      (let* ((req1 (car indent-reqs))
             (style (alist-get 'paragraphStyle
                               (alist-get 'updateParagraphStyle req1)))
             (start-mag (alist-get 'magnitude (alist-get 'indentStart style)))
             (first-mag (alist-get 'magnitude
                                   (alist-get 'indentFirstLine style))))
        (should (= start-mag 72))
        (should (= first-mag 54)))
      ;; Check level 2 indentation (108pt start, 90pt first)
      (let* ((req2 (cadr indent-reqs))
             (style (alist-get 'paragraphStyle
                               (alist-get 'updateParagraphStyle req2)))
             (start-mag (alist-get 'magnitude (alist-get 'indentStart style)))
             (first-mag (alist-get 'magnitude
                                   (alist-get 'indentFirstLine style))))
        (should (= start-mag 108))
        (should (= first-mag 90))))))

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

;; ---------------------------------------------------------------------------
;;; Source block markers

(ert-deftest gdocs-convert-test-src-block-marker ()
  "A src block produces an IR element with :gdocs-marker of :type src-block."
  (let* ((ir (gdocs-convert-org-string-to-ir
              "#+BEGIN_SRC python\nprint(\"hi\")\n#+END_SRC"))
         (elem (car ir))
         (marker (plist-get elem :gdocs-marker)))
    (should marker)
    (should (eq (plist-get marker :type) 'src-block))
    (should (string= (plist-get (plist-get marker :data) :language) "python"))))

;; ---------------------------------------------------------------------------
;;; Priority markers

(ert-deftest gdocs-convert-test-priority-marker ()
  "A heading with priority produces a marker with :type priority."
  (let* ((ir (gdocs-convert-org-string-to-ir "* [#A] Urgent task"))
         (elem (car ir))
         (marker (plist-get elem :gdocs-marker)))
    (should marker)
    (should (eq (plist-get marker :type) 'priority))
    (should (string= (plist-get marker :data) "A"))))

;; ---------------------------------------------------------------------------
;;; Scheduled/Deadline markers

(ert-deftest gdocs-convert-test-scheduled-marker ()
  "SCHEDULED produces a marker with :type scheduled."
  (let* ((ir (gdocs-convert-org-string-to-ir
              "* Task\nSCHEDULED: <2026-03-15>"))
         (elem (car ir))
         (marker (plist-get elem :gdocs-marker)))
    (should marker)
    (should (eq (plist-get marker :type) 'scheduled))
    (should (string-match-p "2026-03-15" (plist-get marker :data)))))

(ert-deftest gdocs-convert-test-deadline-marker ()
  "DEADLINE produces a marker with :type deadline."
  (let* ((ir (gdocs-convert-org-string-to-ir
              "* Task\nDEADLINE: <2026-03-20>"))
         (elem (car ir))
         (marker (plist-get elem :gdocs-marker)))
    (should marker)
    (should (eq (plist-get marker :type) 'deadline))
    (should (string-match-p "2026-03-20" (plist-get marker :data)))))

;; ---------------------------------------------------------------------------
;;; Empty paragraph handling

(ert-deftest gdocs-convert-test-empty-paragraph-between-paragraphs ()
  "A blank line between paragraphs does not create a whitespace-only element."
  (let* ((ir (gdocs-convert-org-string-to-ir "First paragraph\n\nSecond paragraph")))
    (should (= (length ir) 2))
    (should (string= (plist-get (car (plist-get (nth 0 ir) :contents)) :text)
                      "First paragraph"))
    (should (string= (plist-get (car (plist-get (nth 1 ir) :contents)) :text)
                      "Second paragraph"))))

;; ---------------------------------------------------------------------------
;;; Segmented parsing

(ert-deftest gdocs-convert-test-segmented-parsing ()
  "Segmented parsing returns :ir, :segments, :preamble, and :postamble."
  (with-temp-buffer
    (insert "* Heading\n\nParagraph text\n")
    (org-mode)
    (let ((result (gdocs-convert-org-buffer-to-segments)))
      (should (plist-get result :ir))
      (should (plist-get result :segments))
      (should (stringp (plist-get result :preamble)))
      (should (stringp (plist-get result :postamble)))
      ;; Segments should correspond to IR elements
      (should (= (length (plist-get result :segments))
                 (length (plist-get result :ir)))))))

;; ---------------------------------------------------------------------------
;;; Multiple formatting runs

(ert-deftest gdocs-convert-test-multiple-formatting-runs ()
  "*bold* and /italic/ mixed produces correct run sequence."
  (let* ((ir (gdocs-convert-org-string-to-ir "*bold* and /italic/ mixed"))
         (runs (plist-get (car ir) :contents)))
    ;; Should have at least 3 runs: bold, plain, italic
    (should (>= (length runs) 3))
    ;; First run should be bold
    (let ((bold-run (car runs)))
      (should (plist-get bold-run :bold))
      (should (string= (plist-get bold-run :text) "bold")))
    ;; There should be an italic run
    (let ((italic-run (--first (plist-get it :italic) runs)))
      (should italic-run)
      (should (string= (plist-get italic-run :text) "italic")))))

;; ---------------------------------------------------------------------------
;;; Keyword markers

(ert-deftest gdocs-convert-test-keyword-marker ()
  "#+AUTHOR: Pablo produces an IR element with :gdocs-marker of :type keyword."
  (let* ((ir (gdocs-convert-org-string-to-ir "#+AUTHOR: Pablo"))
         (elem (--first (let ((m (plist-get it :gdocs-marker)))
                          (and m (eq (plist-get m :type) 'keyword)))
                        ir))
         (marker (plist-get elem :gdocs-marker)))
    (should marker)
    (should (eq (plist-get marker :type) 'keyword))
    (should (string= (plist-get (plist-get marker :data) :key) "AUTHOR"))
    (should (string= (plist-get (plist-get marker :data) :value) "Pablo"))))

;; ---------------------------------------------------------------------------
;;; gdocs-convert--runs-to-plain-text

(ert-deftest gdocs-convert-test-runs-to-plain-text ()
  "Concatenates :text values from multiple runs."
  (let ((runs (list (list :text "Hello" :bold nil)
                    (list :text " " :bold nil)
                    (list :text "world" :bold t))))
    (should (string= (gdocs-convert--runs-to-plain-text runs) "Hello world"))))

(ert-deftest gdocs-convert-test-runs-to-plain-text-empty ()
  "Empty run list produces empty string."
  (should (string= (gdocs-convert--runs-to-plain-text nil) "")))

;; ---------------------------------------------------------------------------
;;; gdocs-convert--make-plain-run

(ert-deftest gdocs-convert-test-make-plain-run ()
  "Creates a plist with :text set and all formatting nil."
  (let ((run (gdocs-convert--make-plain-run "Hello")))
    (should (string= (plist-get run :text) "Hello"))
    (should-not (plist-get run :bold))
    (should-not (plist-get run :italic))
    (should-not (plist-get run :underline))
    (should-not (plist-get run :strikethrough))
    (should-not (plist-get run :code))
    (should-not (plist-get run :link))))

;; ---------------------------------------------------------------------------
;;; Round-trip with headings and nested lists

(ert-deftest gdocs-convert-test-round-trip-headings-and-nested-lists ()
  "Headings and nested lists survive round-trip."
  (let* ((original "* Top heading\n\n- Parent item\n  - Child item\n    - Grandchild item\n\n** Sub heading\n\nParagraph.\n")
         (ir (gdocs-convert-org-string-to-ir original))
         (result (gdocs-convert-ir-to-org ir)))
    (should (string-match-p "\\* Top heading" result))
    (should (string-match-p "\\*\\* Sub heading" result))
    (should (string-match-p "- Parent item" result))
    (should (string-match-p "  - Child item" result))
    (should (string-match-p "    - Grandchild item" result))
    (should (string-match-p "Paragraph\\." result))))

;; ---------------------------------------------------------------------------
;;; File-local variable reader

(ert-deftest gdocs-convert-test-read-file-local-gdocs-id ()
  "Reads gdocs-document-id from a file's Local Variables block."
  (let ((temp-file (make-temp-file "gdocs-test" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "* Heading\n\nSome text.\n\n"
                    ";; Local Variables:\n"
                    ";; gdocs-document-id: \"abc123def\"\n"
                    ";; End:\n"))
          (should (string= (gdocs-convert--read-file-local-gdocs-id temp-file)
                            "abc123def")))
      (delete-file temp-file))))

(ert-deftest gdocs-convert-test-read-file-local-gdocs-id-missing ()
  "Returns nil when file has no gdocs-document-id."
  (let ((temp-file (make-temp-file "gdocs-test" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "* Just a heading\n"))
          (should-not (gdocs-convert--read-file-local-gdocs-id temp-file)))
      (delete-file temp-file))))

(ert-deftest gdocs-convert-test-read-file-local-gdocs-id-nonexistent ()
  "Returns nil for nonexistent files."
  (should-not (gdocs-convert--read-file-local-gdocs-id
               "/nonexistent/path/to/file.org")))

;; ---------------------------------------------------------------------------
;;; Cross-document link resolution

(ert-deftest gdocs-convert-test-link-url-web-unchanged ()
  "Web links are unchanged regardless of link context."
  (let ((gdocs-convert--link-context
         (list :buffer-file "/tmp/test.org"
               :docid-map (make-hash-table :test 'equal))))
    (let* ((ir (gdocs-convert-org-string-to-ir
                "Visit [[https://example.com][Example]]"))
           (runs (plist-get (car ir) :contents))
           (link-run (nth 1 runs)))
      (should (string= (plist-get link-run :link) "https://example.com")))))

(ert-deftest gdocs-convert-test-link-url-file-resolved ()
  "file: links to org files with doc IDs resolve to Google Docs URLs."
  (let ((temp-dir (make-temp-file "gdocs-test" t))
        (source-file nil)
        (target-file nil))
    (unwind-protect
        (progn
          (setq source-file (expand-file-name "source.org" temp-dir))
          (setq target-file (expand-file-name "target.org" temp-dir))
          (with-temp-file target-file
            (insert "* Target heading\n\n"
                    ";; Local Variables:\n"
                    ";; gdocs-document-id: \"TARGET_DOC_ID\"\n"
                    ";; End:\n"))
          (with-temp-file source-file
            (insert (format "Link to [[file:%s][Target]]" target-file)))
          (let ((gdocs-convert--link-context
                 (list :buffer-file source-file
                       :docid-map (make-hash-table :test 'equal))))
            (with-temp-buffer
              (insert-file-contents source-file)
              (org-mode)
              (let* ((ir (gdocs-convert-org-buffer-to-ir))
                     (runs (plist-get (car ir) :contents))
                     (link-run (nth 1 runs)))
                (should (string=
                         (plist-get link-run :link)
                         "https://docs.google.com/document/d/TARGET_DOC_ID/edit"))))))
      (delete-file source-file)
      (delete-file target-file)
      (delete-directory temp-dir))))

(ert-deftest gdocs-convert-test-link-url-file-no-doc-id ()
  "file: links to org files without doc IDs preserve file: prefix."
  (let ((temp-dir (make-temp-file "gdocs-test" t))
        (source-file nil)
        (target-file nil))
    (unwind-protect
        (progn
          (setq source-file (expand-file-name "source.org" temp-dir))
          (setq target-file (expand-file-name "target.org" temp-dir))
          (with-temp-file target-file
            (insert "* Target heading\n"))
          (with-temp-file source-file
            (insert "Link to [[file:target.org][Target]]"))
          (let ((gdocs-convert--link-context
                 (list :buffer-file source-file
                       :docid-map (make-hash-table :test 'equal))))
            (with-temp-buffer
              (insert-file-contents source-file)
              (org-mode)
              (let* ((ir (gdocs-convert-org-buffer-to-ir))
                     (runs (plist-get (car ir) :contents))
                     (link-run (nth 1 runs)))
                (should (string= (plist-get link-run :link)
                                  "file:target.org"))))))
      (delete-file source-file)
      (delete-file target-file)
      (delete-directory temp-dir))))

(ert-deftest gdocs-convert-test-link-url-no-context ()
  "Without link context, file: links return raw path (backward compat)."
  (let ((gdocs-convert--link-context nil))
    (let* ((ir (gdocs-convert-org-string-to-ir
                "Link to [[file:other.org][Other]]"))
           (runs (plist-get (car ir) :contents))
           (link-run (nth 1 runs)))
      (should (string= (plist-get link-run :link) "file:other.org")))))

(ert-deftest gdocs-convert-test-link-url-id-no-doc-id ()
  "id: links to files without doc IDs preserve id: prefix."
  (let ((temp-dir (make-temp-file "gdocs-test" t))
        (source-file nil)
        (target-file nil))
    (unwind-protect
        (progn
          (setq source-file (expand-file-name "source.org" temp-dir))
          (setq target-file (expand-file-name "target.org" temp-dir))
          (with-temp-file target-file
            (insert "* Target heading\n"
                    ":PROPERTIES:\n"
                    ":ID:       TEST-UUID-1234\n"
                    ":END:\n"))
          ;; Register the ID in org-id so org-id-find can locate it
          (let ((org-id-locations (make-hash-table :test 'equal)))
            (puthash "TEST-UUID-1234" target-file org-id-locations)
            (with-temp-file source-file
              (insert "Link to [[id:TEST-UUID-1234][Target]]"))
            (let ((gdocs-convert--link-context
                   (list :buffer-file source-file
                         :docid-map (make-hash-table :test 'equal))))
              (with-temp-buffer
                (insert-file-contents source-file)
                (org-mode)
                (let* ((ir (gdocs-convert-org-buffer-to-ir))
                       (runs (plist-get (car ir) :contents))
                       (link-run (nth 1 runs)))
                  (should (string= (plist-get link-run :link)
                                    "id:TEST-UUID-1234")))))))
      (delete-file source-file)
      (delete-file target-file)
      (delete-directory temp-dir))))

(ert-deftest gdocs-convert-test-link-url-id-no-context ()
  "Without link context, id: links preserve id: prefix."
  (let ((gdocs-convert--link-context nil))
    (let* ((ir (gdocs-convert-org-string-to-ir
                "Link to [[id:SOME-UUID][Other]]"))
           (runs (plist-get (car ir) :contents))
           (link-run (nth 1 runs)))
      (should (string= (plist-get link-run :link) "id:SOME-UUID")))))

;; ---------------------------------------------------------------------------
;;; Heading cache

(ert-deftest gdocs-convert-test-cache-heading-ids ()
  "Heading IDs are extracted and cached from docs JSON."
  (let ((gdocs-convert--heading-cache (make-hash-table :test 'equal))
        (json `((body . ((content
                          . [((paragraph
                               . ((elements
                                   . [((textRun
                                        . ((content . "Introduction\n")
                                           (textStyle . ()))))])
                                  (paragraphStyle
                                   . ((namedStyleType . "HEADING_1")
                                      (headingId . "h123abc"))))))])))
                (title . "Test"))))
    (gdocs-convert--cache-heading-ids "doc-001" json)
    (let ((forward (gethash "doc-001" gdocs-convert--heading-cache))
          (reverse (gethash "doc-001-reverse" gdocs-convert--heading-cache)))
      (should (equal forward '(("Introduction" . "h123abc"))))
      (should (equal reverse '(("h123abc" . "Introduction")))))))

(ert-deftest gdocs-convert-test-heading-anchor-appended ()
  "When heading cache is populated, links include heading anchor."
  (let ((temp-dir (make-temp-file "gdocs-test" t))
        (source-file nil)
        (target-file nil)
        (gdocs-convert--heading-cache (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (setq source-file (expand-file-name "source.org" temp-dir))
          (setq target-file (expand-file-name "target.org" temp-dir))
          (with-temp-file target-file
            (insert "* Introduction\n\n"
                    ";; Local Variables:\n"
                    ";; gdocs-document-id: \"TARGET_DOC\"\n"
                    ";; End:\n"))
          ;; Populate heading cache
          (puthash "TARGET_DOC"
                   '(("Introduction" . "h_abc123"))
                   gdocs-convert--heading-cache)
          (with-temp-file source-file
            (insert "See [[file:target.org::*Introduction][intro]]"))
          (let ((gdocs-convert--link-context
                 (list :buffer-file source-file
                       :docid-map (make-hash-table :test 'equal))))
            (with-temp-buffer
              (insert-file-contents source-file)
              (org-mode)
              (let* ((ir (gdocs-convert-org-buffer-to-ir))
                     (runs (plist-get (car ir) :contents))
                     (link-run (nth 1 runs)))
                (should (string=
                         (plist-get link-run :link)
                         "https://docs.google.com/document/d/TARGET_DOC/edit#heading=h.h_abc123"))))))
      (delete-file source-file)
      (delete-file target-file)
      (delete-directory temp-dir))))

(ert-deftest gdocs-convert-test-no-heading-anchor-on-cache-miss ()
  "Without heading cache, link resolves to document-level URL."
  (let ((temp-dir (make-temp-file "gdocs-test" t))
        (source-file nil)
        (target-file nil)
        (gdocs-convert--heading-cache (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (setq source-file (expand-file-name "source.org" temp-dir))
          (setq target-file (expand-file-name "target.org" temp-dir))
          (with-temp-file target-file
            (insert "* Introduction\n\n"
                    ";; Local Variables:\n"
                    ";; gdocs-document-id: \"TARGET_DOC\"\n"
                    ";; End:\n"))
          ;; No heading cache populated
          (with-temp-file source-file
            (insert "See [[file:target.org::*Introduction][intro]]"))
          (let ((gdocs-convert--link-context
                 (list :buffer-file source-file
                       :docid-map (make-hash-table :test 'equal))))
            (with-temp-buffer
              (insert-file-contents source-file)
              (org-mode)
              (let* ((ir (gdocs-convert-org-buffer-to-ir))
                     (runs (plist-get (car ir) :contents))
                     (link-run (nth 1 runs)))
                (should (string=
                         (plist-get link-run :link)
                         "https://docs.google.com/document/d/TARGET_DOC/edit"))))))
      (delete-file source-file)
      (delete-file target-file)
      (delete-directory temp-dir))))

;; ---------------------------------------------------------------------------
;;; Reverse resolution

(ert-deftest gdocs-convert-test-reverse-resolve-google-docs-url ()
  "Google Docs URLs reverse-resolve to file: links on pull."
  (let* ((docid-map (make-hash-table :test 'equal))
         (gdocs-convert--heading-cache (make-hash-table :test 'equal))
         (gdocs-convert--link-context
          (list :buffer-file "/home/user/org/source.org"
                :docid-map docid-map)))
    (puthash "DOC_ABC" "/home/user/org/target.org" docid-map)
    (let* ((run (list :text "link text"
                      :bold nil :italic nil :underline nil
                      :strikethrough nil :code nil
                      :link "https://docs.google.com/document/d/DOC_ABC/edit"))
           (org-text (gdocs-convert--run-to-org run)))
      (should (string= org-text "[[file:target.org][link text]]")))))

(ert-deftest gdocs-convert-test-reverse-resolve-with-heading ()
  "Google Docs URLs with heading anchors reverse-resolve with ::*Heading."
  (let* ((docid-map (make-hash-table :test 'equal))
         (gdocs-convert--heading-cache (make-hash-table :test 'equal))
         (gdocs-convert--link-context
          (list :buffer-file "/home/user/org/source.org"
                :docid-map docid-map)))
    (puthash "DOC_ABC" "/home/user/org/target.org" docid-map)
    (puthash "DOC_ABC-reverse" '(("h_intro" . "Introduction"))
             gdocs-convert--heading-cache)
    (let* ((run (list :text "intro"
                      :bold nil :italic nil :underline nil
                      :strikethrough nil :code nil
                      :link "https://docs.google.com/document/d/DOC_ABC/edit#heading=h.h_intro"))
           (org-text (gdocs-convert--run-to-org run)))
      (should (string= org-text
                        "[[file:target.org::*Introduction][intro]]")))))

(ert-deftest gdocs-convert-test-reverse-resolve-unknown-doc ()
  "Unknown Google Docs URLs pass through unchanged."
  (let* ((docid-map (make-hash-table :test 'equal))
         (gdocs-convert--link-context
          (list :buffer-file "/home/user/org/source.org"
                :docid-map docid-map)))
    (let* ((run (list :text "unknown"
                      :bold nil :italic nil :underline nil
                      :strikethrough nil :code nil
                      :link "https://docs.google.com/document/d/UNKNOWN_DOC/edit"))
           (org-text (gdocs-convert--run-to-org run)))
      (should (string=
               org-text
               "[[https://docs.google.com/document/d/UNKNOWN_DOC/edit][unknown]]")))))

(ert-deftest gdocs-convert-test-reverse-resolve-non-docs-url ()
  "Non-Google Docs URLs pass through unchanged."
  (let* ((gdocs-convert--link-context
          (list :buffer-file "/home/user/org/source.org"
                :docid-map (make-hash-table :test 'equal))))
    (let* ((run (list :text "example"
                      :bold nil :italic nil :underline nil
                      :strikethrough nil :code nil
                      :link "https://example.com"))
           (org-text (gdocs-convert--run-to-org run)))
      (should (string= org-text "[[https://example.com][example]]")))))

;; ---------------------------------------------------------------------------
;;; Build docid-to-file map

(ert-deftest gdocs-convert-test-build-docid-map ()
  "Builds a hash table mapping doc IDs to file paths."
  (let ((temp-dir (make-temp-file "gdocs-test" t))
        (file-a nil)
        (file-b nil))
    (unwind-protect
        (progn
          (setq file-a (expand-file-name "a.org" temp-dir))
          (setq file-b (expand-file-name "b.org" temp-dir))
          (with-temp-file file-a
            (insert "* A\n\n"
                    ";; Local Variables:\n"
                    ";; gdocs-document-id: \"DOC_A\"\n"
                    ";; End:\n"))
          (with-temp-file file-b
            (insert "* B\n\n"
                    ";; Local Variables:\n"
                    ";; gdocs-document-id: \"DOC_B\"\n"
                    ";; End:\n"))
          (let ((map (gdocs-convert--build-docid-to-file-map
                      (list temp-dir))))
            (should (string= (gethash "DOC_A" map) file-a))
            (should (string= (gethash "DOC_B" map) file-b))))
      (delete-file file-a)
      (delete-file file-b)
      (delete-directory temp-dir))))

;; ---------------------------------------------------------------------------
;;; Integration: round-trip with cross-document links

(ert-deftest gdocs-convert-test-cross-doc-link-round-trip ()
  "File link -> IR (resolved URL) -> org (file: link) round-trip."
  (let* ((temp-dir (make-temp-file "gdocs-test" t))
         (source-file (expand-file-name "source.org" temp-dir))
         (target-file (expand-file-name "target.org" temp-dir))
         (docid-map (make-hash-table :test 'equal))
         (gdocs-convert--heading-cache (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (with-temp-file target-file
            (insert "* Target\n\n"
                    ";; Local Variables:\n"
                    ";; gdocs-document-id: \"DOC_TARGET\"\n"
                    ";; End:\n"))
          (with-temp-file source-file
            (insert "See [[file:target.org][Target doc]]"))
          ;; Build docid-map for reverse resolution
          (puthash "DOC_TARGET" target-file docid-map)
          (let ((gdocs-convert--link-context
                 (list :buffer-file source-file
                       :docid-map docid-map)))
            ;; org -> IR: file link becomes Google Docs URL
            (let* ((ir (with-temp-buffer
                         (insert-file-contents source-file)
                         (org-mode)
                         (gdocs-convert-org-buffer-to-ir)))
                   (link-run (nth 1 (plist-get (car ir) :contents))))
              (should (string=
                       (plist-get link-run :link)
                       "https://docs.google.com/document/d/DOC_TARGET/edit"))
              ;; IR -> org: Google Docs URL becomes file: link
              (let ((result (gdocs-convert-ir-to-org ir)))
                (should (string-match-p "\\[\\[file:target\\.org\\]" result))))))
      (delete-file source-file)
      (delete-file target-file)
      (delete-directory temp-dir))))

;; ---------------------------------------------------------------------------
;;; Internal heading links (headingId)

(ert-deftest gdocs-convert-test-heading-id-to-url ()
  "Internal headingId links are converted to full Google Docs URLs."
  (let* ((gdocs-convert--document-id "DOC_SELF")
         (json `((body . ((content
                           . [((paragraph
                                . ((elements
                                    . [((textRun
                                         . ((content . "click here\n")
                                            (textStyle
                                             . ((link
                                                 . ((headingId . "h_intro"))))))))])
                                   (paragraphStyle
                                    . ((namedStyleType . "NORMAL_TEXT"))))))])))))
         (ir (gdocs-convert-docs-json-to-ir json))
         (runs (plist-get (car ir) :contents))
         (link-run (car runs)))
    (should (string=
             (plist-get link-run :link)
             "https://docs.google.com/document/d/DOC_SELF/edit#heading=h.h_intro"))))

(ert-deftest gdocs-convert-test-heading-id-without-doc-id ()
  "Without `gdocs-convert--document-id', headingId links are nil."
  (let* ((gdocs-convert--document-id nil)
         (text-run `((content . "click\n")
                     (textStyle . ((link . ((headingId . "h_intro")))))))
         (ir-run (gdocs-convert--docs-text-run-to-ir text-run)))
    (should (null (plist-get ir-run :link)))))

;; ---------------------------------------------------------------------------
;;; Same-document reverse resolution

(ert-deftest gdocs-convert-test-reverse-resolve-same-doc-with-org-id ()
  "Same-document heading links resolve to id:UUID when heading has an ID."
  (let* ((temp-dir (make-temp-file "gdocs-test" t))
         (doc-file (expand-file-name "doc.org" temp-dir))
         (docid-map (make-hash-table :test 'equal))
         (gdocs-convert--heading-cache (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (with-temp-file doc-file
            (insert "* Building a pipeline\n"
                    ":PROPERTIES:\n"
                    ":ID:       DE2746D4-0EE2-4D37-9CCE-7264E953EB90\n"
                    ":END:\n"))
          (puthash "DOC_SELF" doc-file docid-map)
          (puthash "DOC_SELF-reverse"
                   '(("h_build" . "Building a pipeline"))
                   gdocs-convert--heading-cache)
          (let* ((gdocs-convert--link-context
                  (list :buffer-file doc-file
                        :docid-map docid-map))
                 (run (list :text "more"
                            :bold nil :italic nil :underline nil
                            :strikethrough nil :code nil
                            :link "https://docs.google.com/document/d/DOC_SELF/edit#heading=h.h_build"))
                 (org-text (gdocs-convert--run-to-org run)))
            (should (string= org-text
                              "[[id:DE2746D4-0EE2-4D37-9CCE-7264E953EB90][more]]"))))
      (delete-file doc-file)
      (delete-directory temp-dir))))

(ert-deftest gdocs-convert-test-reverse-resolve-same-doc-no-org-id ()
  "Same-document heading links fall back to *Heading without an ID."
  (let* ((temp-dir (make-temp-file "gdocs-test" t))
         (doc-file (expand-file-name "doc.org" temp-dir))
         (docid-map (make-hash-table :test 'equal))
         (gdocs-convert--heading-cache (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (with-temp-file doc-file
            (insert "* Building a pipeline\n"))
          (puthash "DOC_SELF" doc-file docid-map)
          (puthash "DOC_SELF-reverse"
                   '(("h_build" . "Building a pipeline"))
                   gdocs-convert--heading-cache)
          (let* ((gdocs-convert--link-context
                  (list :buffer-file doc-file
                        :docid-map docid-map))
                 (run (list :text "more"
                            :bold nil :italic nil :underline nil
                            :strikethrough nil :code nil
                            :link "https://docs.google.com/document/d/DOC_SELF/edit#heading=h.h_build"))
                 (org-text (gdocs-convert--run-to-org run)))
            (should (string= org-text
                              "[[*Building a pipeline][more]]"))))
      (delete-file doc-file)
      (delete-directory temp-dir))))

(ert-deftest gdocs-convert-test-reverse-resolve-same-doc-no-heading ()
  "Same-document links without heading anchor produce file: link."
  (let* ((docid-map (make-hash-table :test 'equal))
         (gdocs-convert--link-context
          (list :buffer-file "/home/user/org/doc.org"
                :docid-map docid-map)))
    (puthash "DOC_SELF" "/home/user/org/doc.org" docid-map)
    (let* ((url "https://docs.google.com/document/d/DOC_SELF/edit")
           (result (gdocs-convert--reverse-resolve-link url)))
      (should (string= result "file:doc.org")))))

(provide 'gdocs-convert-test)
;;; gdocs-convert-test.el ends here
