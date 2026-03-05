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

;;;; Fallback tests

(ert-deftest gdocs-diff-test-full-replace-threshold ()
  "Too many changes triggers full replace (delete-all + insert-all)."
  (let* ((old-ir (list (gdocs-diff-test--make-paragraph "e1" "a")
                       (gdocs-diff-test--make-paragraph "e2" "b")
                       (gdocs-diff-test--make-paragraph "e3" "c")))
         (new-ir (list (gdocs-diff-test--make-paragraph "e4" "x")
                       (gdocs-diff-test--make-paragraph "e5" "y")
                       (gdocs-diff-test--make-paragraph "e6" "z")))
         (old-keys (gdocs-diff--element-keys old-ir))
         (new-keys (gdocs-diff--element-keys new-ir))
         (lcs (gdocs-diff--lcs old-keys new-keys))
         (ops (gdocs-diff--classify-operations old-ir new-ir lcs)))
    (should (gdocs-diff--should-use-full-replace-p old-ir new-ir ops))))

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

(provide 'gdocs-diff-test)
;;; gdocs-diff-test.el ends here
