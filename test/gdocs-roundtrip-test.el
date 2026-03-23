;;; gdocs-roundtrip-test.el --- Kitchen-sink roundtrip tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;;; Commentary:

;; Comprehensive roundtrip tests using kitchen-sink fixtures that cover
;; every formatting type from the org-mode and Google Docs specifications.
;; Failures indicate features that need to be supported.
;;
;; Test fixtures:
;;   test/fixtures/kitchen-sink.org  — org file with every org element type
;;   test/fixtures/kitchen-sink.json — Google Docs JSON with every GDocs feature
;;
;; Three test suites:
;;   1. Org → IR → Org   (org-mode fidelity)
;;   2. GDocs JSON → IR   (Google Docs parsing)
;;   3. Org → IR → GDocs requests   (request generation)

;;; Code:

(require 'ert)
(require 'json)
(require 'gdocs-convert)
(require 's)

;; ---------------------------------------------------------------------------
;;; Fixture loading helpers

(defvar gdocs-roundtrip-test--fixtures-dir
  (expand-file-name "fixtures/"
                    (file-name-directory (or load-file-name
                                             buffer-file-name)))
  "Directory containing test fixtures.")

(defun gdocs-roundtrip-test--fixture-path (name)
  "Return the full path to fixture NAME."
  (expand-file-name name gdocs-roundtrip-test--fixtures-dir))

(defun gdocs-roundtrip-test--read-org-fixture ()
  "Read and return the kitchen-sink.org fixture as a string."
  (with-temp-buffer
    (insert-file-contents (gdocs-roundtrip-test--fixture-path "kitchen-sink.org"))
    (buffer-string)))

(defun gdocs-roundtrip-test--read-json-fixture ()
  "Read and return the kitchen-sink.json fixture as parsed JSON."
  (let ((json-object-type 'alist)
        (json-array-type 'vector)
        (json-key-type 'symbol))
    (with-temp-buffer
      (insert-file-contents
       (gdocs-roundtrip-test--fixture-path "kitchen-sink.json"))
      (goto-char (point-min))
      (json-read))))

(defun gdocs-roundtrip-test--org-to-ir (org-string)
  "Convert ORG-STRING to IR, returning the IR list."
  (gdocs-convert-org-string-to-ir org-string))

(defun gdocs-roundtrip-test--find-ir-element (ir type &optional style)
  "Find first IR element with TYPE and optional STYLE."
  (--first (and (eq (plist-get it :type) type)
                (or (null style) (eq (plist-get it :style) style)))
           ir))

(defun gdocs-roundtrip-test--find-ir-elements (ir type &optional style)
  "Find all IR elements with TYPE and optional STYLE."
  (--filter (and (eq (plist-get it :type) type)
                 (or (null style) (eq (plist-get it :style) style)))
            ir))

(defun gdocs-roundtrip-test--find-ir-list-elements (ir list-type)
  "Find all IR elements that are list items of LIST-TYPE."
  (--filter (let ((list-info (plist-get it :list)))
              (and list-info (eq (plist-get list-info :type) list-type)))
            ir))

(defun gdocs-roundtrip-test--find-run-with-prop (ir prop)
  "Find first text run in IR that has PROP set to non-nil."
  (catch 'found
    (dolist (elem ir)
      (dolist (run (plist-get elem :contents))
        (when (plist-get run prop)
          (throw 'found run))))))

(defun gdocs-roundtrip-test--find-run-with-text (ir text)
  "Find first text run in IR whose :text contains TEXT."
  (catch 'found
    (dolist (elem ir)
      (dolist (run (plist-get elem :contents))
        (when (and (plist-get run :text)
                   (string-match-p (regexp-quote text) (plist-get run :text)))
          (throw 'found run))))))

(defun gdocs-roundtrip-test--find-marker (ir marker-type)
  "Find first IR element with a :gdocs-marker of MARKER-TYPE."
  (catch 'found
    (dolist (elem ir)
      (let ((markers (plist-get elem :gdocs-marker)))
        (dolist (m (if (and markers (plist-get (car markers) :type))
                       markers
                     (list markers)))
          (when (eq (plist-get m :type) marker-type)
            (throw 'found elem)))))))

;; ===========================================================================
;;; Suite 1: Org → IR → Org roundtrip
;; ===========================================================================

;; ---------------------------------------------------------------------------
;;; 1a. Paragraph styles

(ert-deftest gdocs-roundtrip/org-title ()
  "#+TITLE keyword produces a title element that survives roundtrip."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "#+TITLE: Kitchen sink"))
         (title (gdocs-roundtrip-test--find-ir-element ir 'paragraph 'title)))
    (should title)
    (should (string-match-p "Kitchen sink"
                            (plist-get (car (plist-get title :contents)) :text)))))

(ert-deftest gdocs-roundtrip/org-headings-1-through-6 ()
  "Heading levels 1-6 each produce the correct IR style."
  (dolist (level '(1 2 3 4 5 6))
    (let* ((stars (make-string level ?*))
           (ir (gdocs-roundtrip-test--org-to-ir
                (format "%s Heading %d" stars level)))
           (elem (car ir)))
      (should (eq (plist-get elem :style)
                  (intern (format "heading-%d" level)))))))

(ert-deftest gdocs-roundtrip/org-heading-7-clamped ()
  "Heading level 7 is clamped to heading-6."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "******* Level 7"))
         (elem (car ir)))
    (should (eq (plist-get elem :style) 'heading-6))))

(ert-deftest gdocs-roundtrip/org-normal-paragraph ()
  "A plain paragraph produces :style normal."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "Just a paragraph."))
         (elem (car ir)))
    (should (eq (plist-get elem :style) 'normal))))

(ert-deftest gdocs-roundtrip/org-quote-block ()
  "A quote block produces :style quote."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir
              "#+BEGIN_QUOTE\nQuoted text.\n#+END_QUOTE"))
         (elem (gdocs-roundtrip-test--find-ir-element ir 'paragraph 'quote)))
    (should elem)))

(ert-deftest gdocs-roundtrip/org-center-block ()
  "A center block is parsed into IR."
  :expected-result :failed
  (let* ((ir (gdocs-roundtrip-test--org-to-ir
              "#+BEGIN_CENTER\nCentered.\n#+END_CENTER")))
    (should ir)
    (should (>= (length ir) 1))))

(ert-deftest gdocs-roundtrip/org-verse-block ()
  "A verse block is parsed into IR."
  :expected-result :failed
  (let* ((ir (gdocs-roundtrip-test--org-to-ir
              "#+BEGIN_VERSE\nLine one\nLine two\n#+END_VERSE")))
    (should ir)))

(ert-deftest gdocs-roundtrip/org-example-block ()
  "An example block produces code-formatted IR elements."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir
              "#+BEGIN_EXAMPLE\nExample text.\n#+END_EXAMPLE")))
    (should ir)
    ;; Example blocks should produce runs with :code t
    (let ((code-run (gdocs-roundtrip-test--find-run-with-prop ir :code)))
      (should code-run))))

(ert-deftest gdocs-roundtrip/org-src-block ()
  "A src block produces a :gdocs-marker of type src-block."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir
              "#+BEGIN_SRC python\nprint('hi')\n#+END_SRC"))
         (elem (gdocs-roundtrip-test--find-marker ir 'src-block)))
    (should elem)))

(ert-deftest gdocs-roundtrip/org-special-block ()
  "A special block (e.g., #+BEGIN_WARNING) is parsed into IR."
  :expected-result :failed
  (let* ((ir (gdocs-roundtrip-test--org-to-ir
              "#+BEGIN_WARNING\nWatch out!\n#+END_WARNING")))
    (should ir)))

(ert-deftest gdocs-roundtrip/org-fixed-width ()
  "Fixed-width lines (: prefix) produce code-formatted runs."
  :expected-result :failed
  (let* ((ir (gdocs-roundtrip-test--org-to-ir ": Fixed width text.")))
    (should ir)
    (let ((code-run (gdocs-roundtrip-test--find-run-with-prop ir :code)))
      (should code-run))))

;; ---------------------------------------------------------------------------
;;; 1b. Text styles

(ert-deftest gdocs-roundtrip/org-bold ()
  "Bold markup *text* produces :bold t."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "Some *bold* here"))
         (run (gdocs-roundtrip-test--find-run-with-prop ir :bold)))
    (should run)
    (should (string= (plist-get run :text) "bold"))))

(ert-deftest gdocs-roundtrip/org-italic ()
  "Italic markup /text/ produces :italic t."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "Some /italic/ here"))
         (run (gdocs-roundtrip-test--find-run-with-prop ir :italic)))
    (should run)
    (should (string= (plist-get run :text) "italic"))))

(ert-deftest gdocs-roundtrip/org-underline ()
  "Underline markup _text_ produces :underline t."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "Some _underlined_ here"))
         (run (gdocs-roundtrip-test--find-run-with-prop ir :underline)))
    (should run)
    (should (string= (plist-get run :text) "underlined"))))

(ert-deftest gdocs-roundtrip/org-strikethrough ()
  "Strikethrough markup +text+ produces :strikethrough t."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "Some +struck+ here"))
         (run (gdocs-roundtrip-test--find-run-with-prop ir :strikethrough)))
    (should run)
    (should (string= (plist-get run :text) "struck"))))

(ert-deftest gdocs-roundtrip/org-code ()
  "Code markup ~text~ produces :code t."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "Some ~code~ here"))
         (run (gdocs-roundtrip-test--find-run-with-prop ir :code)))
    (should run)
    (should (string= (plist-get run :text) "code"))))

(ert-deftest gdocs-roundtrip/org-verbatim ()
  "Verbatim markup =text= produces :code t."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "Some =verbatim= here"))
         (run (gdocs-roundtrip-test--find-run-with-prop ir :code)))
    (should run)
    (should (string= (plist-get run :text) "verbatim"))))

(ert-deftest gdocs-roundtrip/org-link ()
  "Bracket link produces :link with URL."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir
              "Visit [[https://example.com][Example]]"))
         (run (gdocs-roundtrip-test--find-run-with-prop ir :link)))
    (should run)
    (should (string= (plist-get run :link) "https://example.com"))
    (should (string= (plist-get run :text) "Example"))))

(ert-deftest gdocs-roundtrip/org-nested-bold-italic ()
  "*/bold italic/* produces a run with both :bold and :italic."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "Text */bold italic/* here"))
         (runs (plist-get (car ir) :contents))
         (bi-run (--first (and (plist-get it :bold) (plist-get it :italic))
                          runs)))
    (should bi-run)))

(ert-deftest gdocs-roundtrip/org-bold-link ()
  "[[url][*bold*]] produces a run with both :bold and :link."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir
              "A [[https://example.com][*bold link*]]"))
         (runs (plist-get (car ir) :contents))
         (bl-run (--first (and (plist-get it :bold) (plist-get it :link))
                          runs)))
    (should bl-run)))

;; ---------------------------------------------------------------------------
;;; 1c. Lists

(ert-deftest gdocs-roundtrip/org-unordered-list ()
  "Bullet list items produce :list with :type bullet."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "- A\n- B\n- C"))
         (items (gdocs-roundtrip-test--find-ir-list-elements ir 'bullet)))
    (should (= (length items) 3))))

(ert-deftest gdocs-roundtrip/org-ordered-list ()
  "Numbered list items produce :list with :type number."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "1. A\n2. B"))
         (items (gdocs-roundtrip-test--find-ir-list-elements ir 'number)))
    (should (= (length items) 2))))

(ert-deftest gdocs-roundtrip/org-checkbox-unchecked ()
  "Unchecked checkbox produces :type check, :checked nil."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "- [ ] Todo"))
         (items (gdocs-roundtrip-test--find-ir-list-elements ir 'check)))
    (should (= (length items) 1))
    (should-not (plist-get (plist-get (car items) :list) :checked))))

(ert-deftest gdocs-roundtrip/org-checkbox-checked ()
  "Checked checkbox produces :type check, :checked t."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "- [X] Done"))
         (items (gdocs-roundtrip-test--find-ir-list-elements ir 'check)))
    (should (= (length items) 1))
    (should (plist-get (plist-get (car items) :list) :checked))))

(ert-deftest gdocs-roundtrip/org-descriptive-list ()
  "Descriptive list items are parsed into IR."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "- Term :: Definition")))
    (should ir)
    (should (>= (length ir) 1))))

(ert-deftest gdocs-roundtrip/org-nested-list-levels ()
  "Nested lists produce items with increasing :level."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir
              "- L0\n  - L1\n    - L2"))
         (levels (mapcar (lambda (e) (plist-get (plist-get e :list) :level))
                         ir)))
    (should (equal levels '(0 1 2)))))

(ert-deftest gdocs-roundtrip/org-mixed-nested-lists ()
  "Mixed bullet/numbered nesting is parsed correctly."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir
              "- Bullet\n  1. Numbered child")))
    (should (>= (length ir) 2))
    (let ((first-type (plist-get (plist-get (nth 0 ir) :list) :type))
          (second-type (plist-get (plist-get (nth 1 ir) :list) :type)))
      (should (eq first-type 'bullet))
      (should (eq second-type 'number)))))

;; ---------------------------------------------------------------------------
;;; 1d. Tables

(ert-deftest gdocs-roundtrip/org-table ()
  "An org table produces an IR table element with :rows."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir
              "| A | B |\n|---+---|\n| C | D |"))
         (table (gdocs-roundtrip-test--find-ir-element ir 'table)))
    (should table)
    (should (= (length (plist-get table :rows)) 2))))

(ert-deftest gdocs-roundtrip/org-table-with-formatting ()
  "A table with formatted cells preserves formatting in IR."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir
              "| *Bold* | /Italic/ |\n|--------+----------|\n| ~code~ | normal   |"))
         (table (gdocs-roundtrip-test--find-ir-element ir 'table))
         (first-row (car (plist-get table :rows)))
         (first-cell-runs (car first-row)))
    (should table)
    (should (plist-get (car first-cell-runs) :bold))))

;; ---------------------------------------------------------------------------
;;; 1e. Horizontal rules

(ert-deftest gdocs-roundtrip/org-horizontal-rule ()
  "A ----- line produces :type horizontal-rule."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "Above\n-----\nBelow"))
         (hr (gdocs-roundtrip-test--find-ir-element ir 'horizontal-rule)))
    (should hr)))

;; ---------------------------------------------------------------------------
;;; 1f. Org-only markers

(ert-deftest gdocs-roundtrip/org-todo-keyword ()
  "TODO keyword produces a marker with :type todo."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "* TODO Task"))
         (elem (gdocs-roundtrip-test--find-marker ir 'todo)))
    (should elem)))

(ert-deftest gdocs-roundtrip/org-done-keyword ()
  "DONE keyword produces a marker with :type todo, :data DONE."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "* DONE Task"))
         (elem (gdocs-roundtrip-test--find-marker ir 'todo))
         (markers (plist-get elem :gdocs-marker))
         (todo-marker (--first (eq (plist-get it :type) 'todo) markers)))
    (should (string= (plist-get todo-marker :data) "DONE"))))

(ert-deftest gdocs-roundtrip/org-priority ()
  "Priority cookie produces a marker with :type priority."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "* [#A] Urgent"))
         (elem (gdocs-roundtrip-test--find-marker ir 'priority)))
    (should elem)))

(ert-deftest gdocs-roundtrip/org-tags ()
  "Tags produce a marker with :type tags."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "* Heading :work:urgent:"))
         (elem (gdocs-roundtrip-test--find-marker ir 'tags)))
    (should elem)))

(ert-deftest gdocs-roundtrip/org-scheduled ()
  "SCHEDULED produces a marker with :type scheduled."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir
              "* Task\nSCHEDULED: <2026-04-01 Wed>"))
         (elem (gdocs-roundtrip-test--find-marker ir 'scheduled)))
    (should elem)))

(ert-deftest gdocs-roundtrip/org-deadline ()
  "DEADLINE produces a marker with :type deadline."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir
              "* Task\nDEADLINE: <2026-04-15 Wed>"))
         (elem (gdocs-roundtrip-test--find-marker ir 'deadline)))
    (should elem)))

(ert-deftest gdocs-roundtrip/org-keyword-author ()
  "#+AUTHOR produces a marker with :type keyword."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "#+AUTHOR: Test"))
         (elem (gdocs-roundtrip-test--find-marker ir 'keyword)))
    (should elem)))

;; ---------------------------------------------------------------------------
;;; 1g. Inline elements

(ert-deftest gdocs-roundtrip/org-entity ()
  "Org entities (\\alpha etc.) produce UTF-8 text in IR."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "Greek: \\alpha"))
         (run (gdocs-roundtrip-test--find-run-with-text ir "α")))
    (should run)))

(ert-deftest gdocs-roundtrip/org-line-break ()
  "Line break (\\\\) produces a newline in the text run."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "First \\\\\nSecond"))
         (run (gdocs-roundtrip-test--find-run-with-text ir "\n")))
    (should run)))

(ert-deftest gdocs-roundtrip/org-timestamp ()
  "An active timestamp is rendered as text in IR."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "Date: <2026-03-22 Sun>"))
         (run (gdocs-roundtrip-test--find-run-with-text ir "2026-03-22")))
    (should run)))

;; ---------------------------------------------------------------------------
;;; 1h. Elements that org supports but gdocs may not yet handle
;;      These tests document the expected behavior; failures indicate
;;      features to implement.

(ert-deftest gdocs-roundtrip/org-footnote-definition ()
  "Footnote definitions produce a dedicated IR element (not just inline text)."
  :expected-result :failed
  (let* ((ir (gdocs-roundtrip-test--org-to-ir
              "Text[fn:1].\n\n[fn:1] The footnote.")))
    ;; Should have a dedicated footnote element, not just inline text
    (should (gdocs-roundtrip-test--find-ir-element ir 'footnote))))

(ert-deftest gdocs-roundtrip/org-footnote-reference ()
  "Footnote references produce a dedicated IR marker, not just text."
  :expected-result :failed
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "Text[fn:1]."))
         (elem (gdocs-roundtrip-test--find-marker ir 'footnote)))
    (should elem)))

(ert-deftest gdocs-roundtrip/org-latex-fragment ()
  "LaTeX fragments produce a code-formatted run preserving the LaTeX source."
  :expected-result :failed
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "Math: $E = mc^2$"))
         (run (gdocs-roundtrip-test--find-run-with-text ir "E = mc^2")))
    (should run)
    ;; Should be formatted as code/monospace to distinguish from plain text
    (should (plist-get run :code))))

(ert-deftest gdocs-roundtrip/org-latex-environment ()
  "LaTeX environments produce a dedicated IR element or code block."
  :expected-result :failed
  (let* ((ir (gdocs-roundtrip-test--org-to-ir
              "\\begin{equation}\nx = 1\n\\end{equation}")))
    (should (>= (length ir) 1))
    ;; Should have a marker indicating this is a LaTeX environment
    (should (gdocs-roundtrip-test--find-marker ir 'latex-environment))))

(ert-deftest gdocs-roundtrip/org-subscript ()
  "Subscripts (H_2O) produce a run with :subscript t."
  :expected-result :failed
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "H_2O")))
    (should ir)
    ;; Should have a run with subscript indication
    (should (catch 'found
              (dolist (elem ir)
                (dolist (run (plist-get elem :contents))
                  (when (plist-get run :subscript)
                    (throw 'found t))))))))

(ert-deftest gdocs-roundtrip/org-superscript ()
  "Superscripts (x^2) produce a run with :superscript t."
  :expected-result :failed
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "x^2")))
    (should ir)
    ;; Should have a run with superscript indication
    (should (catch 'found
              (dolist (elem ir)
                (dolist (run (plist-get elem :contents))
                  (when (plist-get run :superscript)
                    (throw 'found t))))))))

(ert-deftest gdocs-roundtrip/org-inline-src-block ()
  "Inline source blocks (src_lang{code}) produce a code run with language info."
  :expected-result :failed
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "Result: src_python{1+1}"))
         (run (gdocs-roundtrip-test--find-run-with-text ir "1+1")))
    (should run)
    (should (plist-get run :code))
    ;; Should also have language metadata
    (should (gdocs-roundtrip-test--find-marker ir 'inline-src-block))))

(ert-deftest gdocs-roundtrip/org-statistics-cookie ()
  "Statistics cookies [2/5] survive org→IR→org via fallback."
  (let* ((original "* Progress [2/5]\n")
         (ir (gdocs-roundtrip-test--org-to-ir original))
         (result (gdocs-convert-ir-to-org ir)))
    ;; The cookie should survive roundtrip in its original form
    (should (string-match-p "\\[2/5\\]" result))))

(ert-deftest gdocs-roundtrip/org-radio-target ()
  "Radio targets <<<text>>> survive org→IR→org via fallback."
  (let* ((original "Define <<<important>>> here.\n")
         (ir (gdocs-roundtrip-test--org-to-ir original))
         (result (gdocs-convert-ir-to-org ir)))
    ;; The radio target should roundtrip preserving <<<>>>
    (should (string-match-p "<<<important>>>" result))))

(ert-deftest gdocs-roundtrip/org-target ()
  "Dedicated targets <<target>> survive org→IR→org via fallback."
  (let* ((original "See <<my-target>>.\n")
         (ir (gdocs-roundtrip-test--org-to-ir original))
         (result (gdocs-convert-ir-to-org ir)))
    ;; The target should roundtrip preserving <<>>
    (should (string-match-p "<<my-target>>" result))))

(ert-deftest gdocs-roundtrip/org-citation ()
  "Citations [cite:@key] survive org→IR→org via fallback text rendering."
  (let* ((original "Ref: [cite:@knuth84]\n")
         (ir (gdocs-roundtrip-test--org-to-ir original))
         (result (gdocs-convert-ir-to-org ir)))
    ;; Citation syntax should be preserved in roundtrip
    (should (string-match-p "\\[cite:@knuth84\\]" result))))

(ert-deftest gdocs-roundtrip/org-macro ()
  "Macro references {{{name}}} survive org→IR→org via fallback."
  (let* ((original "#+MACRO: ver 1.0\nVersion: {{{ver}}}\n")
         (ir (gdocs-roundtrip-test--org-to-ir original))
         (result (gdocs-convert-ir-to-org ir)))
    ;; Macro reference should be preserved
    (should (string-match-p "{{{ver}}}" result))))

(ert-deftest gdocs-roundtrip/org-export-snippet ()
  "Export snippets @@backend:value@@ survive org→IR→org via fallback."
  (let* ((original "Text @@html:<br>@@ here\n")
         (ir (gdocs-roundtrip-test--org-to-ir original))
         (result (gdocs-convert-ir-to-org ir)))
    ;; Export snippet syntax should be preserved
    (should (string-match-p "@@html:<br>@@" result))))

(ert-deftest gdocs-roundtrip/org-comment ()
  "Comment lines are excluded from IR."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "# This is a comment")))
    ;; Comments should produce no output or be filtered
    (should (or (null ir)
                (= (length ir) 0)
                ;; Or if present, should not contain the comment text
                (not (gdocs-roundtrip-test--find-run-with-text
                      ir "This is a comment"))))))

(ert-deftest gdocs-roundtrip/org-drawer ()
  "Custom drawers survive roundtrip preserving :DRAWER: ... :END: syntax."
  :expected-result :failed
  (let* ((original "* Heading\n:MYDATA:\nContent\n:END:\n\nParagraph.\n")
         (ir (gdocs-roundtrip-test--org-to-ir original))
         (result (gdocs-convert-ir-to-org ir)))
    ;; Drawer should be preserved in roundtrip
    (should (string-match-p ":MYDATA:" result))
    (should (string-match-p ":END:" result))))

(ert-deftest gdocs-roundtrip/org-clock ()
  "CLOCK entries survive roundtrip preserving CLOCK: syntax."
  :expected-result :failed
  (let* ((original "* Task\n:LOGBOOK:\nCLOCK: [2026-03-22 Sun 09:00]--[2026-03-22 Sun 10:00] =>  1:00\n:END:\n")
         (ir (gdocs-roundtrip-test--org-to-ir original))
         (result (gdocs-convert-ir-to-org ir)))
    ;; Clock entry should be preserved
    (should (string-match-p "CLOCK:" result))))

;; ---------------------------------------------------------------------------
;;; 1i. Full fixture roundtrip

(ert-deftest gdocs-roundtrip/org-full-fixture-parses ()
  "The complete kitchen-sink.org fixture parses to IR without error."
  (let* ((org-str (gdocs-roundtrip-test--read-org-fixture))
         (ir (gdocs-roundtrip-test--org-to-ir org-str)))
    (should ir)
    (should (> (length ir) 50))))  ;; sanity: should have many elements

(ert-deftest gdocs-roundtrip/org-full-fixture-roundtrip ()
  "The kitchen-sink.org fixture survives org → IR → org."
  (let* ((org-str (gdocs-roundtrip-test--read-org-fixture))
         (ir (gdocs-roundtrip-test--org-to-ir org-str))
         (result (gdocs-convert-ir-to-org ir)))
    (should (stringp result))
    (should (> (length result) 100))
    ;; Check key elements are present in the output
    (should (string-match-p "Kitchen sink" result))
    (should (string-match-p "\\*bold text\\*" result))
    (should (string-match-p "/italic text/" result))
    (should (string-match-p "~inline code~" result))
    (should (string-match-p "- First item" result))
    (should (string-match-p "1\\. First item" result))
    (should (string-match-p "| Name" result))
    (should (string-match-p "TODO" result))
    (should (string-match-p ":work:urgent:" result))))

(ert-deftest gdocs-roundtrip/org-full-fixture-re-parses ()
  "The org → IR → org → IR roundtrip produces structurally similar IR."
  (let* ((org-str (gdocs-roundtrip-test--read-org-fixture))
         (ir1 (gdocs-roundtrip-test--org-to-ir org-str))
         (org2 (gdocs-convert-ir-to-org ir1))
         (ir2 (gdocs-roundtrip-test--org-to-ir org2)))
    ;; Second IR should have the same number of elements (approximately)
    ;; Allow some tolerance for elements that collapse differently
    (let ((diff (abs (- (length ir1) (length ir2)))))
      (should (< diff (/ (length ir1) 5))))))  ;; within 20%

;; ===========================================================================
;;; Suite 2: GDocs JSON → IR (Google Docs parsing)
;; ===========================================================================

(ert-deftest gdocs-roundtrip/json-fixture-parses ()
  "The kitchen-sink.json fixture parses to IR without error."
  (let* ((json (gdocs-roundtrip-test--read-json-fixture))
         (ir (gdocs-convert-docs-json-to-ir json)))
    (should ir)
    (should (> (length ir) 30))))

(ert-deftest gdocs-roundtrip/json-has-title ()
  "The JSON fixture contains a title element."
  :expected-result :failed ;; SUBTITLE is not a currently supported style
  (let* ((json (gdocs-roundtrip-test--read-json-fixture))
         (ir (gdocs-convert-docs-json-to-ir json))
         (subtitle (gdocs-roundtrip-test--find-ir-element
                    ir 'paragraph 'subtitle)))
    (should subtitle)))

(ert-deftest gdocs-roundtrip/json-has-headings ()
  "The JSON fixture produces heading elements at multiple levels."
  (let* ((json (gdocs-roundtrip-test--read-json-fixture))
         (ir (gdocs-convert-docs-json-to-ir json)))
    (dolist (level '(1 2 3 4 5 6))
      (let ((style (intern (format "heading-%d" level))))
        (should (gdocs-roundtrip-test--find-ir-element
                 ir 'paragraph style))))))

(ert-deftest gdocs-roundtrip/json-has-bold ()
  "The JSON fixture contains bold text runs."
  (let* ((json (gdocs-roundtrip-test--read-json-fixture))
         (ir (gdocs-convert-docs-json-to-ir json))
         (run (gdocs-roundtrip-test--find-run-with-prop ir :bold)))
    (should run)))

(ert-deftest gdocs-roundtrip/json-has-italic ()
  "The JSON fixture contains italic text runs."
  (let* ((json (gdocs-roundtrip-test--read-json-fixture))
         (ir (gdocs-convert-docs-json-to-ir json))
         (run (gdocs-roundtrip-test--find-run-with-prop ir :italic)))
    (should run)))

(ert-deftest gdocs-roundtrip/json-has-underline ()
  "The JSON fixture contains underlined text runs."
  (let* ((json (gdocs-roundtrip-test--read-json-fixture))
         (ir (gdocs-convert-docs-json-to-ir json))
         (run (gdocs-roundtrip-test--find-run-with-prop ir :underline)))
    (should run)))

(ert-deftest gdocs-roundtrip/json-has-strikethrough ()
  "The JSON fixture contains strikethrough text runs."
  (let* ((json (gdocs-roundtrip-test--read-json-fixture))
         (ir (gdocs-convert-docs-json-to-ir json))
         (run (gdocs-roundtrip-test--find-run-with-prop ir :strikethrough)))
    (should run)))

(ert-deftest gdocs-roundtrip/json-has-code ()
  "The JSON fixture contains monospace/code text runs."
  (let* ((json (gdocs-roundtrip-test--read-json-fixture))
         (ir (gdocs-convert-docs-json-to-ir json))
         (run (gdocs-roundtrip-test--find-run-with-prop ir :code)))
    (should run)))

(ert-deftest gdocs-roundtrip/json-has-links ()
  "The JSON fixture contains hyperlinked text runs."
  (let* ((json (gdocs-roundtrip-test--read-json-fixture))
         (ir (gdocs-convert-docs-json-to-ir json))
         (run (gdocs-roundtrip-test--find-run-with-prop ir :link)))
    (should run)))

(ert-deftest gdocs-roundtrip/json-has-bullet-lists ()
  "The JSON fixture contains bullet list items."
  (let* ((json (gdocs-roundtrip-test--read-json-fixture))
         (ir (gdocs-convert-docs-json-to-ir json))
         (items (gdocs-roundtrip-test--find-ir-list-elements ir 'bullet)))
    (should (> (length items) 0))))

(ert-deftest gdocs-roundtrip/json-has-numbered-lists ()
  "The JSON fixture contains numbered list items."
  (let* ((json (gdocs-roundtrip-test--read-json-fixture))
         (ir (gdocs-convert-docs-json-to-ir json))
         (items (gdocs-roundtrip-test--find-ir-list-elements ir 'number)))
    (should (> (length items) 0))))

(ert-deftest gdocs-roundtrip/json-has-checkbox-lists ()
  "The JSON fixture contains checkbox list items.
The MCP markdown renderer doesn't produce BULLET_CHECKBOX presets,
so this requires manual fixture update or a doc created via the GDocs UI."
  :expected-result :failed
  (let* ((json (gdocs-roundtrip-test--read-json-fixture))
         (ir (gdocs-convert-docs-json-to-ir json))
         (items (gdocs-roundtrip-test--find-ir-list-elements ir 'check)))
    (should (> (length items) 0))))

(ert-deftest gdocs-roundtrip/json-has-table ()
  "The JSON fixture contains a table element."
  (let* ((json (gdocs-roundtrip-test--read-json-fixture))
         (ir (gdocs-convert-docs-json-to-ir json))
         (table (gdocs-roundtrip-test--find-ir-element ir 'table)))
    (should table)
    (should (>= (length (plist-get table :rows)) 2))))

(ert-deftest gdocs-roundtrip/json-has-nested-lists ()
  "The JSON fixture has list items at multiple nesting levels."
  (let* ((json (gdocs-roundtrip-test--read-json-fixture))
         (ir (gdocs-convert-docs-json-to-ir json))
         (max-level 0))
    (dolist (elem ir)
      (let ((list-info (plist-get elem :list)))
        (when list-info
          (setq max-level (max max-level (plist-get list-info :level))))))
    (should (>= max-level 2))))

(ert-deftest gdocs-roundtrip/json-has-emoji ()
  "The JSON fixture preserves emoji (supplementary Unicode)."
  (let* ((json (gdocs-roundtrip-test--read-json-fixture))
         (ir (gdocs-convert-docs-json-to-ir json))
         (run (gdocs-roundtrip-test--find-run-with-text ir "🎉")))
    (should run)))

(ert-deftest gdocs-roundtrip/json-has-block-quote ()
  "The JSON fixture contains a quote-styled paragraph."
  :expected-result :failed  ;; GDocs doesn't have native block quotes
  (let* ((json (gdocs-roundtrip-test--read-json-fixture))
         (ir (gdocs-convert-docs-json-to-ir json))
         (quote-elem (gdocs-roundtrip-test--find-ir-element
                      ir 'paragraph 'quote)))
    (should quote-elem)))

;; ---------------------------------------------------------------------------
;;; Suite 2b: GDocs features not yet supported in IR
;;            These tests document expected failures.

(ert-deftest gdocs-roundtrip/json-has-inline-image ()
  "The JSON fixture's inline image is handled in IR."
  :expected-result :failed
  (let* ((json (gdocs-roundtrip-test--read-json-fixture))
         (ir (gdocs-convert-docs-json-to-ir json)))
    ;; Look for any element that represents an image
    (should (--first (eq (plist-get it :type) 'image) ir))))

(ert-deftest gdocs-roundtrip/json-has-page-break ()
  "The JSON fixture's page break is handled in IR."
  :expected-result :failed
  (let* ((json (gdocs-roundtrip-test--read-json-fixture))
         (ir (gdocs-convert-docs-json-to-ir json)))
    ;; Look for any element that represents a page break
    (should (--first (eq (plist-get it :type) 'page-break) ir))))

(ert-deftest gdocs-roundtrip/json-foreground-color ()
  "Foreground color information is preserved in IR."
  :expected-result :failed
  (let* ((json (gdocs-roundtrip-test--read-json-fixture))
         (ir (gdocs-convert-docs-json-to-ir json)))
    ;; Look for a run with color info
    (should (catch 'found
              (dolist (elem ir)
                (dolist (run (plist-get elem :contents))
                  (when (plist-get run :foreground-color)
                    (throw 'found t))))))))

(ert-deftest gdocs-roundtrip/json-background-color ()
  "Background color / highlight is preserved in IR."
  :expected-result :failed
  (let* ((json (gdocs-roundtrip-test--read-json-fixture))
         (ir (gdocs-convert-docs-json-to-ir json)))
    (should (catch 'found
              (dolist (elem ir)
                (dolist (run (plist-get elem :contents))
                  (when (plist-get run :background-color)
                    (throw 'found t))))))))

(ert-deftest gdocs-roundtrip/json-font-size ()
  "Font size changes are preserved in IR."
  :expected-result :failed
  (let* ((json (gdocs-roundtrip-test--read-json-fixture))
         (ir (gdocs-convert-docs-json-to-ir json)))
    (should (catch 'found
              (dolist (elem ir)
                (dolist (run (plist-get elem :contents))
                  (when (plist-get run :font-size)
                    (throw 'found t))))))))

(ert-deftest gdocs-roundtrip/json-paragraph-alignment ()
  "Paragraph alignment (center, right, justified) is preserved in IR."
  :expected-result :failed
  (let* ((json (gdocs-roundtrip-test--read-json-fixture))
         (ir (gdocs-convert-docs-json-to-ir json)))
    (should (catch 'found
              (dolist (elem ir)
                (when (plist-get elem :alignment)
                  (throw 'found t)))))))

;; ===========================================================================
;;; Suite 3: Org → IR → GDocs requests (request generation)
;; ===========================================================================

(ert-deftest gdocs-roundtrip/requests-from-fixture ()
  "The kitchen-sink.org fixture generates batchUpdate requests without error."
  (let* ((org-str (gdocs-roundtrip-test--read-org-fixture))
         (ir (gdocs-roundtrip-test--org-to-ir org-str))
         (requests (gdocs-convert-ir-to-docs-requests ir)))
    (should requests)
    (should (> (length requests) 50))))

(ert-deftest gdocs-roundtrip/requests-have-insert-text ()
  "Generated requests include insertText operations."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "Hello world"))
         (requests (gdocs-convert-ir-to-docs-requests ir))
         (inserts (--filter (alist-get 'insertText it) requests)))
    (should (> (length inserts) 0))))

(ert-deftest gdocs-roundtrip/requests-have-paragraph-style ()
  "Headings generate updateParagraphStyle requests."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "* Heading"))
         (requests (gdocs-convert-ir-to-docs-requests ir))
         (style-reqs (--filter (alist-get 'updateParagraphStyle it) requests)))
    (should (> (length style-reqs) 0))))

(ert-deftest gdocs-roundtrip/requests-have-text-style ()
  "Bold text generates updateTextStyle requests."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "Some *bold* here"))
         (requests (gdocs-convert-ir-to-docs-requests ir))
         (style-reqs (--filter (alist-get 'updateTextStyle it) requests)))
    (should (> (length style-reqs) 0))))

(ert-deftest gdocs-roundtrip/requests-have-bullets ()
  "List items generate createParagraphBullets requests."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir "- Item 1\n- Item 2"))
         (requests (gdocs-convert-ir-to-docs-requests ir))
         (bullet-reqs (--filter (alist-get 'createParagraphBullets it)
                                requests)))
    (should (> (length bullet-reqs) 0))))

(ert-deftest gdocs-roundtrip/requests-have-table ()
  "Tables generate insertTable requests."
  (let* ((ir (gdocs-roundtrip-test--org-to-ir
              "| A | B |\n|---+---|\n| C | D |"))
         (requests (gdocs-convert-ir-to-docs-requests ir))
         (table-reqs (--filter (alist-get 'insertTable it) requests)))
    (should (> (length table-reqs) 0))))

(ert-deftest gdocs-roundtrip/requests-full-fixture ()
  "All request types from the full fixture are accounted for."
  (let* ((org-str (gdocs-roundtrip-test--read-org-fixture))
         (ir (gdocs-roundtrip-test--org-to-ir org-str))
         (requests (gdocs-convert-ir-to-docs-requests ir))
         (types (delete-dups
                 (mapcar (lambda (r)
                           (car (car r)))  ;; first key of each request alist
                         requests))))
    ;; Should have at least these request types
    (should (memq 'insertText types))
    (should (memq 'updateParagraphStyle types))
    (should (memq 'updateTextStyle types))
    (should (memq 'createParagraphBullets types))
    (should (memq 'insertTable types))))

;; ---------------------------------------------------------------------------

(provide 'gdocs-roundtrip-test)
;;; gdocs-roundtrip-test.el ends here
