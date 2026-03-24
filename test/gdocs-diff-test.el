;;; gdocs-diff-test.el --- Tests for gdocs-diff -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;; Author: Pablo Stafforini

;; This file is not part of GNU Emacs.

;;; Commentary:

;; ERT tests for the incremental diff engine.  Covers LCS computation,
;; element key generation, request generation, request ordering,
;; fallback detection, and UTF-16 index computation.

;;; Code:

(require 'ert)
(require 'gdocs-diff)

;;;; Test helpers

(defun gdocs-diff-test--make-paragraph (id text &optional style)
  "Create a minimal IR paragraph element with ID, TEXT, and optional STYLE."
  (list :type 'paragraph
        :style (or style 'normal)
        :contents (list (list :text text
                              :bold nil :italic nil :underline nil
                              :strikethrough nil :code nil :link nil))
        :list nil
        :id id))

(defun gdocs-diff-test--make-bold-paragraph (id text)
  "Create an IR paragraph with bold TEXT and ID."
  (list :type 'paragraph
        :style 'normal
        :contents (list (list :text text
                              :bold t :italic nil :underline nil
                              :strikethrough nil :code nil :link nil))
        :list nil
        :id id))

(defun gdocs-diff-test--make-table (id rows)
  "Create a minimal IR table element with ID and ROWS.
ROWS is a list of lists of lists of run plists."
  (list :type 'table
        :rows rows
        :id id))

(defun gdocs-diff-test--make-hrule (id)
  "Create an IR horizontal rule element with ID."
  (list :type 'horizontal-rule :id id))

(defun gdocs-diff-test--plain-run (text)
  "Create a plain (unformatted) text run for TEXT."
  (list :text text
        :bold nil :italic nil :underline nil
        :strikethrough nil :code nil :link nil))

;;;; LCS tests

(ert-deftest gdocs-diff-test-lcs-identical ()
  "Identical sequences produce a full match."
  (let ((keys '("a" "b" "c")))
    (should (equal (gdocs-diff--lcs keys keys)
                   '((0 . 0) (1 . 1) (2 . 2))))))

(ert-deftest gdocs-diff-test-lcs-empty ()
  "Empty old or new sequence produces no matches."
  (should (null (gdocs-diff--lcs nil '("a" "b"))))
  (should (null (gdocs-diff--lcs '("a" "b") nil)))
  (should (null (gdocs-diff--lcs nil nil))))

(ert-deftest gdocs-diff-test-lcs-insertion ()
  "New has extra elements; LCS finds the common subsequence."
  (let ((result (gdocs-diff--lcs '("a" "c") '("a" "b" "c"))))
    (should (equal result '((0 . 0) (1 . 2))))))

(ert-deftest gdocs-diff-test-lcs-deletion ()
  "Old has extra elements; LCS finds the common subsequence."
  (let ((result (gdocs-diff--lcs '("a" "b" "c") '("a" "c"))))
    (should (equal result '((0 . 0) (2 . 1))))))

(ert-deftest gdocs-diff-test-lcs-mixed ()
  "Mixed insertions and deletions in both sequences."
  (let ((result (gdocs-diff--lcs '("a" "b" "c" "d") '("a" "c" "e"))))
    (should (equal result '((0 . 0) (2 . 1))))))

;;;; Element key tests

(ert-deftest gdocs-diff-test-element-key-paragraph ()
  "Paragraph key includes text and style."
  (let* ((elem (gdocs-diff-test--make-paragraph "e1" "hello" 'heading-1))
         (key (gdocs-diff--element-key elem)))
    (should (string-match-p "paragraph:" key))
    (should (string-match-p "heading-1" key))
    (should (string-match-p "hello" key))))

(ert-deftest gdocs-diff-test-element-key-table ()
  "Table key includes cell contents."
  (let* ((runs (list (gdocs-diff-test--plain-run "cell1")))
         (elem (gdocs-diff-test--make-table
                "t1" (list (list runs))))
         (key (gdocs-diff--element-key elem)))
    (should (string-match-p "table:" key))
    (should (string-match-p "cell1" key))))

(ert-deftest gdocs-diff-test-element-key-formatting ()
  "Formatting changes produce different keys."
  (let* ((plain (gdocs-diff-test--make-paragraph "e1" "text"))
         (bold (gdocs-diff-test--make-bold-paragraph "e1" "text")))
    (should-not (equal (gdocs-diff--element-key plain)
                       (gdocs-diff--element-key bold)))))

;;;; Diff generation tests

(ert-deftest gdocs-diff-test-no-changes ()
  "Identical IR produces an empty request list."
  (let* ((ir (list (gdocs-diff-test--make-paragraph "e1" "hello")))
         (result (gdocs-diff-generate ir ir)))
    (should (null result))))

(ert-deftest gdocs-diff-test-insert-paragraph ()
  "Inserting a new paragraph generates insertText requests."
  (let* ((old-ir (list (gdocs-diff-test--make-paragraph "e1" "first")))
         (new-ir (list (gdocs-diff-test--make-paragraph "e1" "first")
                       (gdocs-diff-test--make-paragraph "e2" "second")))
         (result (gdocs-diff-generate old-ir new-ir)))
    (should (cl-some (lambda (req)
                       (alist-get 'insertText req))
                     result))))

(ert-deftest gdocs-diff-test-delete-paragraph ()
  "Deleting a paragraph generates a deleteContentRange request."
  (let* ((old-ir (list (gdocs-diff-test--make-paragraph "e1" "first")
                       (gdocs-diff-test--make-paragraph "e2" "second")))
         (new-ir (list (gdocs-diff-test--make-paragraph "e1" "first")))
         (result (gdocs-diff-generate old-ir new-ir)))
    (should (cl-some (lambda (req)
                       (alist-get 'deleteContentRange req))
                     result))))

(ert-deftest gdocs-diff-test-modify-text ()
  "Text change generates delete + insert requests."
  (let* ((old-ir (list (gdocs-diff-test--make-paragraph "e1" "old text")))
         (new-ir (list (gdocs-diff-test--make-paragraph "e1" "new text")))
         (result (gdocs-diff-generate old-ir new-ir))
         (has-delete (cl-some (lambda (req)
                                (alist-get 'deleteContentRange req))
                              result))
         (has-insert (cl-some (lambda (req)
                                (alist-get 'insertText req))
                              result)))
    (should has-delete)
    (should has-insert)))

(ert-deftest gdocs-diff-test-modify-style ()
  "Heading level change generates only updateParagraphStyle."
  (let* ((old-ir (list (gdocs-diff-test--make-paragraph
                        "e1" "heading" 'heading-1)))
         (new-ir (list (gdocs-diff-test--make-paragraph
                        "e1" "heading" 'heading-2)))
         (result (gdocs-diff-generate old-ir new-ir))
         (has-style (cl-some (lambda (req)
                               (alist-get 'updateParagraphStyle req))
                             result))
         (has-delete (cl-some (lambda (req)
                                (alist-get 'deleteContentRange req))
                              result)))
    (should has-style)
    (should-not has-delete)))

(ert-deftest gdocs-diff-test-modify-formatting ()
  "Adding bold generates updateTextStyle without delete/insert."
  (let* ((old-ir (list (gdocs-diff-test--make-paragraph "e1" "text")))
         (new-ir (list (gdocs-diff-test--make-bold-paragraph "e1" "text")))
         (result (gdocs-diff-generate old-ir new-ir))
         (has-text-style (cl-some
                          (lambda (req)
                            (alist-get 'updateTextStyle req))
                          result))
         (has-delete (cl-some (lambda (req)
                                (alist-get 'deleteContentRange req))
                              result)))
    (should has-text-style)
    (should-not has-delete)))

(ert-deftest gdocs-diff-test-reorder-paragraphs ()
  "Reordered paragraphs produce correct delete and insert requests."
  (let* ((p1 (gdocs-diff-test--make-paragraph "e1" "alpha"))
         (p2 (gdocs-diff-test--make-paragraph "e2" "beta"))
         (p3 (gdocs-diff-test--make-paragraph "e3" "gamma"))
         (old-ir (list p1 p2 p3))
         (new-ir (list p3 p1 p2))
         (result (gdocs-diff-generate old-ir new-ir)))
    (should result)
    (should (cl-some (lambda (req)
                       (or (alist-get 'deleteContentRange req)
                           (alist-get 'insertText req)))
                     result))))

(ert-deftest gdocs-diff-test-multiple-operations ()
  "Mixed insert/delete/modify produces correct requests."
  (let* ((p1 (gdocs-diff-test--make-paragraph "e1" "keep"))
         (p2 (gdocs-diff-test--make-paragraph "e2" "delete-me"))
         (p3 (gdocs-diff-test--make-paragraph "e3" "new-para"))
         (old-ir (list p1 p2))
         (new-ir (list p1 p3))
         (result (gdocs-diff-generate old-ir new-ir)))
    (should (cl-some (lambda (req)
                       (alist-get 'deleteContentRange req))
                     result))
    (should (cl-some (lambda (req)
                       (alist-get 'insertText req))
                     result))))

;;;; Request ordering tests

(ert-deftest gdocs-diff-test-deletions-reverse-order ()
  "Deletions are ordered by descending startIndex."
  (let* ((p1 (gdocs-diff-test--make-paragraph "e1" "first"))
         (p2 (gdocs-diff-test--make-paragraph "e2" "second"))
         (p3 (gdocs-diff-test--make-paragraph "e3" "third"))
         (p4 (gdocs-diff-test--make-paragraph "e4" "fourth"))
         (p5 (gdocs-diff-test--make-paragraph "e5" "fifth"))
         (old-ir (list p1 p2 p3 p4 p5))
         (new-ir (list p1 p3 p5))
         (result (gdocs-diff-generate old-ir new-ir))
         (delete-indices
          (mapcar (lambda (req)
                    (when-let* ((dcr (alist-get 'deleteContentRange req)))
                      (alist-get 'startIndex (alist-get 'range dcr))))
                  result))
         (filtered (delq nil delete-indices)))
    (should (>= (length filtered) 2))
    (should (equal filtered (sort (copy-sequence filtered) #'>)))))

(ert-deftest gdocs-diff-test-insertions-after-deletions ()
  "All deletions appear before all insertions in the request list."
  (let* ((p1 (gdocs-diff-test--make-paragraph "e1" "keep"))
         (p2 (gdocs-diff-test--make-paragraph "e2" "remove"))
         (p3 (gdocs-diff-test--make-paragraph "e3" "add-me"))
         (old-ir (list p1 p2))
         (new-ir (list p1 p3))
         (result (gdocs-diff-generate old-ir new-ir))
         (last-delete-pos -1)
         (first-insert-pos most-positive-fixnum))
    (cl-loop for req in result
             for i from 0
             do (cond
                 ((alist-get 'deleteContentRange req)
                  (setq last-delete-pos (max last-delete-pos i)))
                 ((alist-get 'insertText req)
                  (setq first-insert-pos (min first-insert-pos i)))))
    (when (and (>= last-delete-pos 0)
               (< first-insert-pos most-positive-fixnum))
      (should (< last-delete-pos first-insert-pos)))))

;;;; Index computation tests

(ert-deftest gdocs-diff-test-element-indices-simple ()
  "Correct index computation for simple paragraphs."
  (let* ((ir (list (gdocs-diff-test--make-paragraph "e1" "hello")
                   (gdocs-diff-test--make-paragraph "e2" "world")))
         (indices (gdocs-diff--compute-element-indices ir))
         (first-range (cdr (assq 0 indices)))
         (second-range (cdr (assq 1 indices))))
    (should (= (car first-range) 1))
    (should (= (cdr first-range) 7))
    (should (= (car second-range) 7))
    (should (= (cdr second-range) 13))))

(ert-deftest gdocs-diff-test-element-indices-utf16 ()
  "Correct UTF-16 index computation for non-BMP characters."
  (let* ((ir (list (gdocs-diff-test--make-paragraph
                    "e1" (concat "hi" (string #x1F600)))))
         (indices (gdocs-diff--compute-element-indices ir))
         (range (cdr (assq 0 indices))))
    (should (= (car range) 1))
    (should (= (cdr range) 6))))

;;;; Start-index offset tests

(ert-deftest gdocs-diff-test-start-index-offset ()
  "Start-index shifts all generated indices."
  (let* ((old-ir (list (gdocs-diff-test--make-paragraph "e1" "old")))
         (new-ir (list (gdocs-diff-test--make-paragraph "e1" "new")))
         (result (gdocs-diff-generate old-ir new-ir 20))
         (delete-req (cl-find-if (lambda (req)
                                   (alist-get 'deleteContentRange req))
                                 result))
         (insert-req (cl-find-if (lambda (req)
                                   (alist-get 'insertText req))
                                 result))
         (del-range (alist-get 'range
                               (alist-get 'deleteContentRange delete-req)))
         (ins-loc (alist-get 'location
                             (alist-get 'insertText insert-req))))
    (should (= (alist-get 'startIndex del-range) 20))
    (should (= (alist-get 'index ins-loc) 20))))

(ert-deftest gdocs-diff-test-start-index-default ()
  "Default start-index is 1."
  (let* ((ir (list (gdocs-diff-test--make-paragraph "e1" "hello")))
         (indices (gdocs-diff--compute-element-indices ir)))
    (should (= (cadr (assq 0 indices)) 1))))

(ert-deftest gdocs-diff-test-start-index-element-indices ()
  "Custom start-index shifts element index computation."
  (let* ((ir (list (gdocs-diff-test--make-paragraph "e1" "hello")))
         (indices (gdocs-diff--compute-element-indices ir 10)))
    (should (= (cadr (assq 0 indices)) 10))
    (should (= (cddr (assq 0 indices)) 16))))

;;;; Empty paragraph deletion tests

(ert-deftest gdocs-diff-test-delete-empty-paragraph ()
  "Deleting an empty paragraph (nil contents) generates a delete request.
Empty paragraphs occupy exactly 1 UTF-16 unit (the trailing newline).
The delete must include the trailing newline to actually remove the
paragraph from the document."
  (let* ((empty (list :type 'paragraph :style 'normal
                      :contents nil :list nil :id "e0"))
         (kept (gdocs-diff-test--make-paragraph "e1" "hello"))
         (old-ir (list empty kept))
         (new-ir (list kept))
         (result (gdocs-diff-generate old-ir new-ir)))
    ;; Should produce a delete request
    (should (cl-some (lambda (req)
                       (alist-get 'deleteContentRange req))
                     result))
    ;; The delete range should be [1, 2) — the full empty paragraph
    (let* ((del-req (cl-find-if (lambda (req)
                                  (alist-get 'deleteContentRange req))
                                result))
           (range (alist-get 'range
                             (alist-get 'deleteContentRange del-req))))
      (should (= (alist-get 'startIndex range) 1))
      (should (= (alist-get 'endIndex range) 2)))))

(ert-deftest gdocs-diff-test-modify-table-to-paragraph ()
  "Modifying a table into a paragraph uses full delete, not partial."
  ;; When a table is replaced by a paragraph, the diff collapses the
  ;; adjacent delete+insert into a modify.  The modify must delete the
  ;; full table range [start, end), not [start, end-1) which would be
  ;; a partial structural deletion rejected by the API.
  (let* ((table (gdocs-diff-test--make-table
                 "t1"
                 (list (list (list (gdocs-diff-test--plain-run "A"))
                             (list (gdocs-diff-test--plain-run "B"))))))
         (para (gdocs-diff-test--make-paragraph "p1" "Replacement"))
         (old-ir (list table))
         (new-ir (list para))
         (result (gdocs-diff-generate old-ir new-ir)))
    ;; Should produce a deleteContentRange covering the full table
    (let* ((del-req (cl-find-if (lambda (req)
                                  (alist-get 'deleteContentRange req))
                                result))
           (range (alist-get 'range
                             (alist-get 'deleteContentRange del-req))))
      (should del-req)
      ;; Table: 2 (start+end) + 1 (row) + 2 cells × (1 marker + text + 1 nl)
      ;; = 2 + 1 + (1+1+1) + (1+1+1) = 9
      ;; Range should be [1, 10) — the full table, not [1, 9)
      (should (= (alist-get 'startIndex range) 1))
      (should (= (alist-get 'endIndex range) 10)))))

(ert-deftest gdocs-diff-test-delete-last-table ()
  "Deleting a table that is the last element uses its full range."
  ;; When the last element is a table, the deletion must cover the
  ;; full table range, not subtract 1 (which is only correct for
  ;; paragraphs that have a trailing newline to preserve).
  (let* ((para (gdocs-diff-test--make-paragraph "p1" "Keep me"))
         (table (gdocs-diff-test--make-table
                 "t1"
                 (list (list (list (gdocs-diff-test--plain-run "X"))))))
         (old-ir (list para table))
         (new-ir (list para))
         (result (gdocs-diff-generate old-ir new-ir)))
    ;; Should produce a deleteContentRange for the table
    (let* ((del-req (cl-find-if (lambda (req)
                                  (alist-get 'deleteContentRange req))
                                result))
           (range (alist-get 'range
                             (alist-get 'deleteContentRange del-req))))
      (should del-req)
      ;; Paragraph: "Keep me" (7) + newline = 8, so table starts at 9
      ;; Table: 2 + 1 + (1+1+1) = 6, so table range is [9, 15)
      ;; The delete should cover the FULL table [9, 15), not [9, 14)
      (should (= (alist-get 'startIndex range) 9))
      (should (= (alist-get 'endIndex range) 15)))))

;;;; Bullet removal tests

(ert-deftest gdocs-diff-test-modify-removes-bullet ()
  "Modifying a list item into a heading removes the bullet.
When the old element has :list but the new one does not, the diff
should emit a deleteParagraphBullets request to remove the
residual bullet from the preserved paragraph."
  (let* ((old-elem (list :type 'paragraph :style 'normal
                         :contents (list (list :text "Title"
                                               :bold t :italic nil
                                               :underline nil
                                               :strikethrough nil
                                               :code nil :link nil))
                         :list (list :type 'number :level 0)
                         :id "e1"))
         (new-elem (list :type 'paragraph :style 'heading-3
                         :contents (list (list :text "Title"
                                               :bold t :italic nil
                                               :underline nil
                                               :strikethrough nil
                                               :code nil :link nil))
                         :list nil
                         :id "e1"))
         (old-ir (list old-elem))
         (new-ir (list new-elem))
         (result (gdocs-diff-generate old-ir new-ir))
         (has-delete-bullets
          (cl-some (lambda (req)
                     (alist-get 'deleteParagraphBullets req))
                   result)))
    (should has-delete-bullets)))

(ert-deftest gdocs-diff-test-modify-no-bullet-removal-when-both-lists ()
  "No deleteParagraphBullets when both old and new are list items."
  (let* ((old-elem (list :type 'paragraph :style 'normal
                         :contents (list (list :text "Item"
                                               :bold nil :italic nil
                                               :underline nil
                                               :strikethrough nil
                                               :code nil :link nil))
                         :list (list :type 'number :level 0)
                         :id "e1"))
         (new-elem (list :type 'paragraph :style 'normal
                         :contents (list (list :text "Changed"
                                               :bold nil :italic nil
                                               :underline nil
                                               :strikethrough nil
                                               :code nil :link nil))
                         :list (list :type 'number :level 0)
                         :id "e1"))
         (old-ir (list old-elem))
         (new-ir (list new-elem))
         (result (gdocs-diff-generate old-ir new-ir))
         (has-delete-bullets
          (cl-some (lambda (req)
                     (alist-get 'deleteParagraphBullets req))
                   result)))
    (should-not has-delete-bullets)))

;;;; Empty IR edge cases

(ert-deftest gdocs-diff-test-both-empty ()
  "Both old and new IR empty produces no requests."
  (should (null (gdocs-diff-generate nil nil))))

(ert-deftest gdocs-diff-test-empty-old-to-non-empty ()
  "Empty old IR to non-empty new IR produces only insertions."
  (let* ((new-ir (list (gdocs-diff-test--make-paragraph "e1" "hello")
                       (gdocs-diff-test--make-paragraph "e2" "world")))
         (result (gdocs-diff-generate nil new-ir)))
    (should result)
    (should (cl-every (lambda (req)
                        (not (alist-get 'deleteContentRange req)))
                      result))
    (should (cl-some (lambda (req)
                       (alist-get 'insertText req))
                     result))))

(ert-deftest gdocs-diff-test-non-empty-old-to-empty ()
  "Non-empty old IR to empty new IR produces only deletions."
  (let* ((old-ir (list (gdocs-diff-test--make-paragraph "e1" "hello")
                       (gdocs-diff-test--make-paragraph "e2" "world")))
         (result (gdocs-diff-generate old-ir nil)))
    (should result)
    (should (cl-some (lambda (req)
                       (alist-get 'deleteContentRange req))
                     result))
    (should (cl-every (lambda (req)
                        (not (alist-get 'insertText req)))
                      result))))

;;;; Single element tests

(ert-deftest gdocs-diff-test-single-element-modify ()
  "Modifying the only paragraph generates delete + insert."
  (let* ((old-ir (list (gdocs-diff-test--make-paragraph "e1" "before")))
         (new-ir (list (gdocs-diff-test--make-paragraph "e1" "after")))
         (result (gdocs-diff-generate old-ir new-ir)))
    (should (cl-some (lambda (req)
                       (alist-get 'deleteContentRange req))
                     result))
    (should (cl-some (lambda (req)
                       (alist-get 'insertText req))
                     result))))

(ert-deftest gdocs-diff-test-single-element-unchanged ()
  "Single unchanged element produces no requests."
  (let* ((ir (list (gdocs-diff-test--make-paragraph "e1" "same")))
         (result (gdocs-diff-generate ir ir)))
    (should (null result))))

;;;; Threshold boundary tests

(ert-deftest gdocs-diff-test-many-changes-uses-incremental ()
  "Even with all elements changed, incremental diff is used.
This ensures comments on any remaining matching content survive."
  (let* ((old-ir (list (gdocs-diff-test--make-paragraph "e1" "a")
                       (gdocs-diff-test--make-paragraph "e2" "b")
                       (gdocs-diff-test--make-paragraph "e3" "c")))
         (new-ir (list (gdocs-diff-test--make-paragraph "e4" "x")
                       (gdocs-diff-test--make-paragraph "e5" "y")
                       (gdocs-diff-test--make-paragraph "e6" "z")))
         (result (gdocs-diff-generate old-ir new-ir)))
    ;; Should produce per-element modify requests, not a single
    ;; delete-all + insert-all that would destroy comments.
    (should result)
    ;; Each of the 3 old paragraphs gets modified to a new one, so
    ;; we should have individual delete+insert pairs, not one bulk delete.
    (let ((deletes (cl-count-if
                    (lambda (req) (alist-get 'deleteContentRange req))
                    result)))
      (should (>= deletes 3)))))

;;;; Horizontal rule operation tests

(ert-deftest gdocs-diff-test-insert-horizontal-rule ()
  "Inserting a horizontal rule generates requests."
  (let* ((p1 (gdocs-diff-test--make-paragraph "e1" "above"))
         (hr (gdocs-diff-test--make-hrule "hr1"))
         (old-ir (list p1))
         (new-ir (list p1 hr))
         (result (gdocs-diff-generate old-ir new-ir)))
    (should result)))

(ert-deftest gdocs-diff-test-delete-horizontal-rule ()
  "Deleting a horizontal rule generates a deleteContentRange request."
  (let* ((p1 (gdocs-diff-test--make-paragraph "e1" "above"))
         (hr (gdocs-diff-test--make-hrule "hr1"))
         (old-ir (list p1 hr))
         (new-ir (list p1))
         (result (gdocs-diff-generate old-ir new-ir)))
    (should (cl-some (lambda (req)
                       (alist-get 'deleteContentRange req))
                     result))))

;;;; List property change tests

(ert-deftest gdocs-diff-test-change-list-type ()
  "Changing list type from ordered to unordered modifies content."
  (let* ((old-elem (list :type 'paragraph :style 'normal
                         :contents (list (list :text "Item"
                                               :bold nil :italic nil
                                               :underline nil
                                               :strikethrough nil
                                               :code nil :link nil))
                         :list (list :type 'number :level 0)
                         :id "e1"))
         (new-elem (list :type 'paragraph :style 'normal
                         :contents (list (list :text "Item"
                                               :bold nil :italic nil
                                               :underline nil
                                               :strikethrough nil
                                               :code nil :link nil))
                         :list (list :type 'bullet :level 0)
                         :id "e1"))
         (old-ir (list old-elem))
         (new-ir (list new-elem))
         (result (gdocs-diff-generate old-ir new-ir)))
    ;; The list type change should produce requests
    (should result)))

(ert-deftest gdocs-diff-test-change-list-level ()
  "Changing list nesting level produces requests."
  (let* ((old-elem (list :type 'paragraph :style 'normal
                         :contents (list (list :text "Nested"
                                               :bold nil :italic nil
                                               :underline nil
                                               :strikethrough nil
                                               :code nil :link nil))
                         :list (list :type 'bullet :level 0)
                         :id "e1"))
         (new-elem (list :type 'paragraph :style 'normal
                         :contents (list (list :text "Nested"
                                               :bold nil :italic nil
                                               :underline nil
                                               :strikethrough nil
                                               :code nil :link nil))
                         :list (list :type 'bullet :level 2)
                         :id "e1"))
         (old-ir (list old-elem))
         (new-ir (list new-elem))
         (result (gdocs-diff-generate old-ir new-ir)))
    (should result)))

;;;; Insertion ordering tests

(ert-deftest gdocs-diff-test-insert-at-beginning ()
  "Insertion before all existing elements uses start-index."
  (let* ((p1 (gdocs-diff-test--make-paragraph "e1" "existing"))
         (p0 (gdocs-diff-test--make-paragraph "e0" "prepended"))
         (old-ir (list p1))
         (new-ir (list p0 p1))
         (result (gdocs-diff-generate old-ir new-ir))
         (insert-req (cl-find-if (lambda (req)
                                   (alist-get 'insertText req))
                                 result))
         (loc (alist-get 'location
                         (alist-get 'insertText insert-req))))
    ;; The insertion should target index 1 (the default start-index),
    ;; since there is no preceding kept element.
    (should insert-req)
    (should (= (alist-get 'index loc) 1))))

(ert-deftest gdocs-diff-test-insert-at-beginning-custom-offset ()
  "Insertion before all elements with custom start-index uses that offset."
  (let* ((p1 (gdocs-diff-test--make-paragraph "e1" "existing"))
         (p0 (gdocs-diff-test--make-paragraph "e0" "prepended"))
         (old-ir (list p1))
         (new-ir (list p0 p1))
         (result (gdocs-diff-generate old-ir new-ir 15))
         (insert-req (cl-find-if (lambda (req)
                                   (alist-get 'insertText req))
                                 result))
         (loc (alist-get 'location
                         (alist-get 'insertText insert-req))))
    (should insert-req)
    (should (= (alist-get 'index loc) 15))))

;;;; Tokenizer tests

(ert-deftest gdocs-diff-test-tokenize-simple ()
  "Tokenizes a sentence into words and spaces."
  (let ((tokens (gdocs-diff--tokenize-text "Hello world")))
    (should (= (length tokens) 3))
    (should (equal (plist-get (nth 0 tokens) :text) "Hello"))
    (should (equal (plist-get (nth 1 tokens) :text) " "))
    (should (equal (plist-get (nth 2 tokens) :text) "world"))
    ;; Check offsets
    (should (= (plist-get (nth 0 tokens) :offset) 0))
    (should (= (plist-get (nth 1 tokens) :offset) 5))
    (should (= (plist-get (nth 2 tokens) :offset) 6))))

(ert-deftest gdocs-diff-test-tokenize-multiple-spaces ()
  "Multiple spaces form a single whitespace token."
  (let ((tokens (gdocs-diff--tokenize-text "A  B")))
    (should (= (length tokens) 3))
    (should (equal (plist-get (nth 1 tokens) :text) "  "))
    (should (= (plist-get (nth 1 tokens) :length) 2))))

(ert-deftest gdocs-diff-test-tokenize-empty ()
  "Empty string produces no tokens."
  (should (null (gdocs-diff--tokenize-text ""))))

(ert-deftest gdocs-diff-test-tokenize-single-word ()
  "Single word produces one token."
  (let ((tokens (gdocs-diff--tokenize-text "word")))
    (should (= (length tokens) 1))
    (should (equal (plist-get (car tokens) :text) "word"))
    (should (= (plist-get (car tokens) :offset) 0))
    (should (= (plist-get (car tokens) :length) 4))))

(ert-deftest gdocs-diff-test-tokenize-utf16-surrogate ()
  "Non-BMP characters get correct UTF-16 lengths."
  ;; U+1F600 (grinning face) requires 2 UTF-16 code units
  (let* ((text (concat "hi" (string #x1F600) "bye"))
         (tokens (gdocs-diff--tokenize-text text)))
    ;; Single word token (no whitespace)
    (should (= (length tokens) 1))
    ;; "hi" = 2, emoji = 2, "bye" = 3 → 7 UTF-16 units
    (should (= (plist-get (car tokens) :length) 7))))

;;;; Word-level diff tests

(ert-deftest gdocs-diff-test-word-level-single-word-change ()
  "Changing one word in a paragraph produces targeted delete+insert."
  (let* ((old-ir (list (gdocs-diff-test--make-paragraph "e1" "Hello world")))
         (new-ir (list (gdocs-diff-test--make-paragraph "e1" "Hello earth")))
         (result (gdocs-diff-generate old-ir new-ir))
         (deletes (cl-remove-if-not
                   (lambda (req) (alist-get 'deleteContentRange req))
                   result))
         (inserts (cl-remove-if-not
                   (lambda (req) (alist-get 'insertText req))
                   result)))
    ;; Should delete "world" and insert "earth", not delete entire text
    (should (= (length deletes) 1))
    (should (>= (length inserts) 1))
    ;; The delete range should cover only "world" (5 UTF-16 units at offset 6)
    ;; Paragraph starts at 1, "world" starts at offset 6 → doc pos 7
    (let* ((del (car deletes))
           (range (alist-get 'range (alist-get 'deleteContentRange del))))
      (should (= (alist-get 'startIndex range) 7))
      (should (= (alist-get 'endIndex range) 12)))
    ;; The insert should place "earth" at position 7 (after "Hello ")
    (let* ((ins (cl-find-if (lambda (req)
                              (let ((it (alist-get 'insertText req)))
                                (and it (equal (alist-get 'text it)
                                               "earth"))))
                            inserts))
           (loc (alist-get 'location (alist-get 'insertText ins))))
      (should ins)
      (should (= (alist-get 'index loc) 7)))))

(ert-deftest gdocs-diff-test-word-level-multi-word-change ()
  "Changing multiple words generates correct delete+insert ops."
  (let* ((old-ir (list (gdocs-diff-test--make-paragraph
                        "e1" "A B C D E")))
         (new-ir (list (gdocs-diff-test--make-paragraph
                        "e1" "A X C Y E")))
         (result (gdocs-diff-generate old-ir new-ir))
         (deletes (cl-remove-if-not
                   (lambda (req) (alist-get 'deleteContentRange req))
                   result)))
    ;; Two word changes: B→X and D→Y
    (should (= (length deletes) 2))))

(ert-deftest gdocs-diff-test-word-level-whitespace-preservation ()
  "Double spaces between words are preserved as tokens."
  (let* ((old-ir (list (gdocs-diff-test--make-paragraph
                        "e1" "A  B")))
         (new-ir (list (gdocs-diff-test--make-paragraph
                        "e1" "A  C")))
         (result (gdocs-diff-generate old-ir new-ir))
         (deletes (cl-remove-if-not
                   (lambda (req) (alist-get 'deleteContentRange req))
                   result)))
    ;; Only "B" should be deleted, not the double-space
    (should (= (length deletes) 1))
    (let* ((range (alist-get 'range
                             (alist-get 'deleteContentRange
                                        (car deletes)))))
      ;; "A  B" starts at doc pos 1
      ;; "A"=offset 0, "  "=offset 1, "B"=offset 3
      ;; Delete "B" at doc [4, 5)
      (should (= (alist-get 'startIndex range) 4))
      (should (= (alist-get 'endIndex range) 5)))))

(ert-deftest gdocs-diff-test-word-level-full-rewrite ()
  "Full paragraph rewrite still produces valid operations."
  (let* ((old-ir (list (gdocs-diff-test--make-paragraph
                        "e1" "old text here")))
         (new-ir (list (gdocs-diff-test--make-paragraph
                        "e1" "completely different")))
         (result (gdocs-diff-generate old-ir new-ir)))
    ;; Should produce some deletes and inserts
    (should (cl-some (lambda (req)
                       (alist-get 'deleteContentRange req))
                     result))
    (should (cl-some (lambda (req)
                       (alist-get 'insertText req))
                     result))))

(ert-deftest gdocs-diff-test-word-level-utf16-offsets ()
  "Word-level diff computes correct UTF-16 offsets for non-BMP chars."
  (let* ((emoji (string #x1F600))
         (old-ir (list (gdocs-diff-test--make-paragraph
                        "e1" (concat "Hi " emoji " world"))))
         (new-ir (list (gdocs-diff-test--make-paragraph
                        "e1" (concat "Hi " emoji " earth"))))
         (result (gdocs-diff-generate old-ir new-ir))
         (deletes (cl-remove-if-not
                   (lambda (req) (alist-get 'deleteContentRange req))
                   result)))
    ;; "world" should be deleted.
    ;; Paragraph at doc pos 1.
    ;; "Hi"=2, " "=1, emoji=2(UTF-16), " "=1, "world"=5
    ;; "world" starts at UTF-16 offset 6, doc pos 7
    (should (= (length deletes) 1))
    (let ((range (alist-get 'range
                            (alist-get 'deleteContentRange (car deletes)))))
      (should (= (alist-get 'startIndex range) 7))
      (should (= (alist-get 'endIndex range) 12)))))

(ert-deftest gdocs-diff-test-word-level-insert-at-beginning ()
  "Adding a word at the beginning of a paragraph."
  (let* ((old-ir (list (gdocs-diff-test--make-paragraph "e1" "world")))
         (new-ir (list (gdocs-diff-test--make-paragraph
                        "e1" "Hello world")))
         (result (gdocs-diff-generate old-ir new-ir))
         (inserts (cl-remove-if-not
                   (lambda (req)
                     (let ((it (alist-get 'insertText req)))
                       (and it (equal (alist-get 'text it) "Hello "))))
                   result)))
    ;; Should insert "Hello " at the paragraph start (doc pos 1)
    (should (= (length inserts) 1))
    (let ((loc (alist-get 'location
                          (alist-get 'insertText (car inserts)))))
      (should (= (alist-get 'index loc) 1)))))

(ert-deftest gdocs-diff-test-word-level-delete-at-end ()
  "Removing a word from the end of a paragraph."
  (let* ((old-ir (list (gdocs-diff-test--make-paragraph
                        "e1" "Hello world")))
         (new-ir (list (gdocs-diff-test--make-paragraph "e1" "Hello")))
         (result (gdocs-diff-generate old-ir new-ir))
         (deletes (cl-remove-if-not
                   (lambda (req) (alist-get 'deleteContentRange req))
                   result)))
    ;; Should delete " world" (space + word)
    (should (= (length deletes) 1))
    (let ((range (alist-get 'range
                            (alist-get 'deleteContentRange (car deletes)))))
      ;; " world" at UTF-16 offset 5 in paragraph, doc pos 6
      (should (= (alist-get 'startIndex range) 6))
      (should (= (alist-get 'endIndex range) 12)))))

;;;; Public API: compute-operations and generate-from-ops

(ert-deftest gdocs-diff-test-compute-operations ()
  "compute-operations returns correct op types."
  (let* ((p1 (gdocs-diff-test--make-paragraph "e1" "keep"))
         (p2 (gdocs-diff-test--make-paragraph "e2" "delete-me"))
         (p3 (gdocs-diff-test--make-paragraph "e3" "insert-me"))
         (old-ir (list p1 p2))
         (new-ir (list p1 p3))
         (ops (gdocs-diff-compute-operations old-ir new-ir)))
    (should (cl-some (lambda (op) (eq (plist-get op :op) 'keep)) ops))
    ;; Adjacent delete+insert collapses to modify
    (should (cl-some (lambda (op) (eq (plist-get op :op) 'modify)) ops))
    (should (= (length ops) 2))))

(ert-deftest gdocs-diff-test-generate-from-ops ()
  "generate-from-ops produces same result as generate."
  (let* ((p1 (gdocs-diff-test--make-paragraph "e1" "alpha"))
         (p2 (gdocs-diff-test--make-paragraph "e2" "beta"))
         (p3 (gdocs-diff-test--make-paragraph "e3" "gamma"))
         (old-ir (list p1 p2))
         (new-ir (list p1 p3))
         (direct (gdocs-diff-generate old-ir new-ir 5))
         (ops (gdocs-diff-compute-operations old-ir new-ir))
         (from-ops (gdocs-diff-generate-from-ops old-ir new-ir ops 5)))
    (should (equal direct from-ops))))

;;;; Insertion anchoring tests

(ert-deftest gdocs-diff-test-insert-after-modify-anchors-correctly ()
  "Insertions after a modified element use the modify as anchor.
Regression: `preceding-kept-old-index' used to ignore :modify ops,
causing insertions to land before the modified element instead of
after it."
  (let* ((pA (gdocs-diff-test--make-paragraph "A" "alpha"))
         (pB (gdocs-diff-test--make-paragraph "B" "beta"))
         (pC (gdocs-diff-test--make-paragraph "C" "gamma"))
         ;; B is modified to B', D is inserted between B' and C.
         (pB2 (gdocs-diff-test--make-paragraph "B2" "beta-modified"))
         (pD (gdocs-diff-test--make-paragraph "D" "delta"))
         (old-ir (list pA pB pC))
         (new-ir (list pA pB2 pD pC))
         (ops (gdocs-diff-compute-operations old-ir new-ir)))
    ;; The modify op for B→B' should exist.
    (should (cl-some (lambda (op) (eq (plist-get op :op) 'modify)) ops))
    ;; The insert op for D should exist.
    (should (cl-some (lambda (op) (eq (plist-get op :op) 'insert)) ops))
    ;; Key check: the insertion of D must anchor to B's old range end,
    ;; not A's.  B is "beta" (4 chars + 1 newline = 5 UTF-16 units),
    ;; starting at index 1 + 6 (A is "alpha"=5 + 1 newline = 6), so
    ;; B's range is [7, 12).  The insert should be at index 12.
    (let* ((result (gdocs-diff-generate old-ir new-ir))
           ;; Find the insertText request for "delta"
           (insert-req (cl-find-if
                        (lambda (req)
                          (let ((it (alist-get 'insertText req)))
                            (and it
                                 (string-match-p "delta"
                                                 (alist-get 'text it)))))
                        result)))
      (should insert-req)
      ;; The insertion index must be at 12 (B's old range end),
      ;; not at 7 (A's old range end).
      (let ((loc (alist-get 'location (alist-get 'insertText insert-req))))
        (should (= (alist-get 'index loc) 12))))))

(ert-deftest gdocs-diff-test-multiple-inserts-after-modify ()
  "Multiple insertions after a modify are ordered correctly.
When two new elements are inserted after a modified element, they
should both anchor to the modify and stack in forward order."
  (let* ((pA (gdocs-diff-test--make-paragraph "A" "alpha"))
         (pB (gdocs-diff-test--make-paragraph "B" "beta"))
         (pC (gdocs-diff-test--make-paragraph "C" "gamma"))
         (pB2 (gdocs-diff-test--make-paragraph "B2" "beta-new"))
         (pD (gdocs-diff-test--make-paragraph "D" "delta"))
         (pE (gdocs-diff-test--make-paragraph "E" "epsilon"))
         (old-ir (list pA pB pC))
         (new-ir (list pA pB2 pD pE pC))
         (result (gdocs-diff-generate old-ir new-ir))
         ;; Both D and E should be inserted at B's old range end (12).
         (insert-reqs
          (cl-remove-if-not
           (lambda (req)
             (let ((it (alist-get 'insertText req)))
               (and it (let ((text (alist-get 'text it)))
                         (or (string-match-p "delta" text)
                             (string-match-p "epsilon" text))))))
           result)))
    (should (= (length insert-reqs) 2))
    ;; Both insert at B's old end (12), not A's end (7).
    (dolist (req insert-reqs)
      (let ((loc (alist-get 'location (alist-get 'insertText req))))
        (should (= (alist-get 'index loc) 12))))))

(ert-deftest gdocs-diff-test-insert-in-gap-with-delete-and-modify ()
  "Insert after a gap containing both deletes and a modify.
Old: [A, B, C, D, E], New: [A, X, C, Y, Z, E]
Gap 1 has delete(B)+insert(X)→modify.  Gap 2 has delete(D)+
insert(Y)→modify, plus insert(Z).  Z must anchor to the modify
at D's position, not to keep(C)."
  (let* ((pA (gdocs-diff-test--make-paragraph "A" "aa"))
         (pB (gdocs-diff-test--make-paragraph "B" "bb"))
         (pC (gdocs-diff-test--make-paragraph "C" "cc"))
         (pD (gdocs-diff-test--make-paragraph "D" "dd"))
         (pE (gdocs-diff-test--make-paragraph "E" "ee"))
         (pX (gdocs-diff-test--make-paragraph "X" "xx"))
         (pY (gdocs-diff-test--make-paragraph "Y" "yy"))
         (pZ (gdocs-diff-test--make-paragraph "Z" "zz"))
         (old-ir (list pA pB pC pD pE))
         (new-ir (list pA pX pC pY pZ pE))
         (result (gdocs-diff-generate old-ir new-ir))
         ;; Each element is 2 chars + 1 newline = 3 UTF-16 units.
         ;; Ranges: A=[1,4), B=[4,7), C=[7,10), D=[10,13), E=[13,16)
         ;; Z should anchor to modify at D's old-index, i.e., 13.
         (z-req (cl-find-if
                 (lambda (req)
                   (let ((it (alist-get 'insertText req)))
                     (and it (string-match-p "zz" (alist-get 'text it)))))
                 result)))
    (should z-req)
    (let ((loc (alist-get 'location (alist-get 'insertText z-req))))
      (should (= (alist-get 'index loc) 13)))))

(provide 'gdocs-diff-test)
;;; gdocs-diff-test.el ends here
