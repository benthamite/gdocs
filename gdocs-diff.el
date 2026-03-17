;;; gdocs-diff.el --- Incremental diff engine for gdocs -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Pablo Stafforini

;; Author: Pablo Stafforini
;; Package-Requires: ((emacs "29.1") (dash "2.19") (s "1.13"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Incremental diff engine that compares two IR (Intermediate
;; Representation) element lists and generates Google Docs API
;; batchUpdate requests representing the minimal changes.
;;
;; The diff operates at two levels: element-level LCS to classify
;; elements as kept/deleted/inserted/modified, then intra-element
;; character-level diff for modified paragraphs.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 's)
(require 'gdocs-convert)

;; ---------------------------------------------------------------------------
;;; Public API

(defun gdocs-diff-generate (old-ir new-ir &optional start-index)
  "Compare OLD-IR and NEW-IR and return batchUpdate request plists.
OLD-IR and NEW-IR are flat lists of IR element plists.  Returns a
list of request plists suitable for `gdocs-api-batch-update'.
Falls back to full replacement when the diff is too complex.
START-INDEX is the UTF-16 index where the first element begins in
the document (default 1).  This should account for title elements
or other content that precedes the IR."
  ;; Default 1: Google Docs index 0 is the document root
  (let* ((idx (or start-index 1))
         (old-keys (gdocs-diff--element-keys old-ir))
         (new-keys (gdocs-diff--element-keys new-ir))
         (lcs-pairs (gdocs-diff--lcs old-keys new-keys))
         (diff-ops (gdocs-diff--classify-operations
                    old-ir new-ir lcs-pairs)))
    (if (gdocs-diff--should-use-full-replace-p old-ir new-ir diff-ops)
        (gdocs-diff--full-replace-requests old-ir new-ir idx)
      (gdocs-diff--generate-requests old-ir new-ir diff-ops idx))))

;; ---------------------------------------------------------------------------
;;; Element keys

(defun gdocs-diff--element-keys (ir)
  "Compute content-based keys for each element in IR.
Returns a list of strings, one per element."
  (mapcar #'gdocs-diff--element-key ir))

(defun gdocs-diff--element-key (element)
  "Compute a content-based key string for ELEMENT.
The key captures type, style, text content, and formatting so
that elements with identical content and formatting produce
identical keys."
  (pcase (plist-get element :type)
    ('paragraph (gdocs-diff--paragraph-key element))
    ('table (gdocs-diff--table-key element))
    ('horizontal-rule "horizontal-rule")))

(defun gdocs-diff--paragraph-key (element)
  "Compute a content key for a paragraph ELEMENT.
Includes type, style, list info, and full run details."
  (let ((style (symbol-name (or (plist-get element :style) 'normal)))
        (text (gdocs-convert--runs-to-plain-text (plist-get element :contents)))
        (formatting (gdocs-diff--runs-formatting-key
                     (plist-get element :contents)))
        (list-key (gdocs-diff--list-key (plist-get element :list))))
    (format "paragraph:%s:%s:%s:%s" style list-key formatting text)))

(defun gdocs-diff--table-key (element)
  "Compute a content key for a table ELEMENT.
Concatenates all cell text across all rows."
  (let ((cell-texts nil))
    (dolist (row (plist-get element :rows))
      (dolist (cell row)
        (push (gdocs-convert--runs-to-plain-text cell) cell-texts)))
    (format "table:%s" (s-join "|" (nreverse cell-texts)))))

(defun gdocs-diff--runs-formatting-key (runs)
  "Produce a string capturing the formatting of each run in RUNS."
  (mapconcat #'gdocs-diff--run-formatting-key runs ";"))

(defun gdocs-diff--run-formatting-key (run)
  "Produce a formatting fingerprint string for RUN."
  (format "%s%s%s%s%s%s"
          (if (plist-get run :bold) "B" "")
          (if (plist-get run :italic) "I" "")
          (if (plist-get run :underline) "U" "")
          (if (plist-get run :strikethrough) "S" "")
          (if (plist-get run :code) "C" "")
          (if (plist-get run :link) (format "L(%s)" (plist-get run :link)) "")))

(defun gdocs-diff--list-key (list-info)
  "Produce a key fragment for LIST-INFO, or empty string if nil."
  (if list-info
      (format "%s:%d:%s"
              (plist-get list-info :type)
              (plist-get list-info :level)
              (plist-get list-info :checked))
    ""))

;; ---------------------------------------------------------------------------
;;; LCS (Longest Common Subsequence)

(defun gdocs-diff--lcs (old-keys new-keys)
  "Compute the LCS of OLD-KEYS and NEW-KEYS.
OLD-KEYS and NEW-KEYS are lists of strings.  Returns a list of
(OLD-INDEX . NEW-INDEX) pairs indicating matching elements."
  (let* ((m (length old-keys))
         (n (length new-keys))
         (old-vec (vconcat old-keys))
         (new-vec (vconcat new-keys))
         (table (gdocs-diff--lcs-table old-vec new-vec m n)))
    (gdocs-diff--lcs-backtrack table old-vec new-vec m n)))

(defun gdocs-diff--lcs-table (old-vec new-vec m n)
  "Build the LCS dynamic programming table.
OLD-VEC and NEW-VEC are vectors of key strings.  M and N are
their lengths.  Returns a 2D vector of dimensions (M+1) x (N+1)."
  (let ((table (make-vector (1+ m) nil)))
    (dotimes (i (1+ m))
      (aset table i (make-vector (1+ n) 0)))
    (dotimes (i m)
      (dotimes (j n)
        (let ((cell-above (aref (aref table i) (1+ j)))
              (cell-left (aref (aref table (1+ i)) j))
              (cell-diag (aref (aref table i) j)))
          (aset (aref table (1+ i)) (1+ j)
                (if (equal (aref old-vec i) (aref new-vec j))
                    (1+ cell-diag)
                  (max cell-above cell-left))))))
    table))

(defun gdocs-diff--lcs-backtrack (table old-vec new-vec m n)
  "Backtrack through the LCS TABLE to extract matching pairs.
OLD-VEC, NEW-VEC are the key vectors.  M, N are their lengths.
Returns a list of (OLD-INDEX . NEW-INDEX) pairs in order."
  (let ((result nil)
        (i m)
        (j n))
    (while (and (> i 0) (> j 0))
      (cond
       ((equal (aref old-vec (1- i)) (aref new-vec (1- j)))
        (push (cons (1- i) (1- j)) result)
        (setq i (1- i)
              j (1- j)))
       ((> (aref (aref table (1- i)) j)
           (aref (aref table i) (1- j)))
        (setq i (1- i)))
       (t
        (setq j (1- j)))))
    result))

;; ---------------------------------------------------------------------------
;;; Operation classification

(defun gdocs-diff--classify-operations (old-ir new-ir lcs-pairs)
  "Classify diff operations from OLD-IR, NEW-IR, and LCS-PAIRS.
Returns a list of operation plists, each with :op and relevant
indices.  Operations: :keep, :delete, :insert, :modify."
  (let ((ops (gdocs-diff--raw-operations old-ir new-ir lcs-pairs)))
    (gdocs-diff--collapse-adjacent-into-modify ops)))

(defun gdocs-diff--raw-operations (old-ir new-ir lcs-pairs)
  "Produce raw :keep, :delete, :insert operations.
OLD-IR and NEW-IR are the element lists.  LCS-PAIRS is the LCS
match list.  Returns operations in document order."
  (let ((ops nil)
        (old-idx 0)
        (new-idx 0)
        (old-len (length old-ir))
        (new-len (length new-ir)))
    (dolist (pair lcs-pairs)
      (let ((oi (car pair))
            (ni (cdr pair)))
        (while (< old-idx oi)
          (push (list :op 'delete :old-index old-idx) ops)
          (setq old-idx (1+ old-idx)))
        (while (< new-idx ni)
          (push (list :op 'insert :new-index new-idx) ops)
          (setq new-idx (1+ new-idx)))
        (push (list :op 'keep :old-index oi :new-index ni) ops)
        (setq old-idx (1+ oi)
              new-idx (1+ ni))))
    (while (< old-idx old-len)
      (push (list :op 'delete :old-index old-idx) ops)
      (setq old-idx (1+ old-idx)))
    (while (< new-idx new-len)
      (push (list :op 'insert :new-index new-idx) ops)
      (setq new-idx (1+ new-idx)))
    (nreverse ops)))

(defun gdocs-diff--collapse-adjacent-into-modify (ops)
  "Collapse adjacent delete+insert of the same type into :modify.
OPS is a list of operation plists."
  (let ((result nil)
        (remaining ops))
    (while remaining
      (let ((current (car remaining))
            (next (cadr remaining)))
        (if (gdocs-diff--collapsible-pair-p current next)
            (progn
              (push (list :op 'modify
                          :old-index (plist-get current :old-index)
                          :new-index (plist-get next :new-index))
                    result)
              (setq remaining (cddr remaining)))
          (push current result)
          (setq remaining (cdr remaining)))))
    (nreverse result)))

(defun gdocs-diff--collapsible-pair-p (current next)
  "Return non-nil if CURRENT delete and NEXT insert can be collapsed.
They must be adjacent delete+insert operations."
  (and current next
       (eq (plist-get current :op) 'delete)
       (eq (plist-get next :op) 'insert)))

;; ---------------------------------------------------------------------------
;;; Element index computation

(defun gdocs-diff--compute-element-indices (ir &optional start-index)
  "Compute start and end UTF-16 indices for each element in IR.
Returns an alist of (ELEMENT-INDEX . (START . END)).  START-INDEX
is the UTF-16 index where the first element begins (default 1)."
  (let ((index (or start-index 1))
        (result nil)
        (i 0))
    (dolist (element ir)
      (let ((len (gdocs-diff--element-utf16-length element)))
        (push (cons i (cons index (+ index len))) result)
        (setq index (+ index len))
        (setq i (1+ i))))
    (nreverse result)))

(defun gdocs-diff--element-utf16-length (element)
  "Compute the UTF-16 code unit length of ELEMENT in the document."
  (pcase (plist-get element :type)
    ('paragraph (gdocs-diff--paragraph-utf16-length element))
    ('table (gdocs-diff--table-utf16-length element))
    ;; A horizontal rule is a bare newline in the document (1 UTF-16 unit)
    ('horizontal-rule 1)))

(defun gdocs-diff--paragraph-utf16-length (element)
  "Compute UTF-16 length of a paragraph ELEMENT including trailing newline."
  (let ((text (gdocs-convert--runs-to-plain-text (plist-get element :contents))))
    (+ (gdocs-convert--string-to-utf16-length text) 1)))

(defun gdocs-diff--table-utf16-length (element)
  "Compute UTF-16 length of a table ELEMENT in the document.
Tables have structural overhead: 1 for table start, 1 for table
end, plus for each row 1 marker, plus for each cell 1 marker +
content length + 1 newline.
Note: `gdocs-convert--table-to-requests' uses 3 as the base
instead of 2 because insertTable preserves the existing paragraph
at the insertion point (an extra +1 not part of the table itself)."
  (let ((total 2)
        (rows (plist-get element :rows)))
    (dolist (row rows)
      (setq total (+ total 1))
      (dolist (cell row)
        (let ((text (gdocs-convert--runs-to-plain-text cell)))
          (setq total (+ total 1
                         (gdocs-convert--string-to-utf16-length text)
                         1)))))
    total))

;; ---------------------------------------------------------------------------
;;; Fallback detection

(defconst gdocs-diff--full-replace-min-elements 3
  "Minimum element count before the full-replacement fallback can trigger.
Documents with 2 or fewer elements always use incremental diff,
which is simpler and cheaper at that scale.")

(defconst gdocs-diff--full-replace-kept-ratio 0.5
  "Minimum fraction of kept elements to stay in incremental mode.
When the fraction of unchanged elements drops below this value,
the diff engine falls back to full document replacement to avoid
generating a large number of granular requests.")

(defun gdocs-diff--should-use-full-replace-p (old-ir new-ir diff-ops)
  "Return non-nil if the diff is too complex for incremental update.
OLD-IR and NEW-IR are the element lists.  DIFF-OPS is the
classified operation list."
  (let* ((total (max (length old-ir) (length new-ir) 1))
         (kept (cl-count-if
                (lambda (op) (eq (plist-get op :op) 'keep))
                diff-ops)))
    (and (>= total gdocs-diff--full-replace-min-elements)
         (< kept (* gdocs-diff--full-replace-kept-ratio total)))))

;; ---------------------------------------------------------------------------
;;; Full replacement requests

(defun gdocs-diff--full-replace-requests (old-ir new-ir start-index)
  "Generate delete-all + insert-all requests for OLD-IR to NEW-IR.
START-INDEX is the UTF-16 index where the first element begins."
  (let ((delete-reqs (gdocs-diff--delete-all-requests old-ir start-index))
        (insert-reqs (gdocs-diff--insert-all-requests new-ir start-index)))
    (append delete-reqs insert-reqs)))

(defun gdocs-diff--delete-all-requests (old-ir start-index)
  "Generate a single delete request covering all content in OLD-IR.
START-INDEX is the UTF-16 index where the first element begins.
Excludes the trailing newline of the last element (when it is a
paragraph) to avoid deleting the mandatory body newline that the
Google Docs API protects.  Non-paragraph last elements (tables)
are deleted in full to avoid partial structural deletions."
  (let ((indices (gdocs-diff--compute-element-indices old-ir start-index)))
    (when indices
      (let* ((first-entry (cdar indices))
             (last-entry (cdar (last indices)))
             (last-elem (car (last old-ir)))
             (start (car first-entry))
             (end (if (eq (plist-get last-elem :type) 'paragraph)
                      (1- (cdr last-entry))
                    (cdr last-entry))))
        (when (< start end)
          (list (gdocs-diff--make-delete-request start end)))))))

(defun gdocs-diff--insert-all-requests (new-ir start-index)
  "Generate insert requests for all elements in NEW-IR.
START-INDEX is the UTF-16 index where the first element begins.
Delegates to `gdocs-convert-ir-to-docs-requests' so that list
nesting fixups (e.g. alternating numbered presets) are applied."
  (gdocs-convert-ir-to-docs-requests new-ir start-index))

;; ---------------------------------------------------------------------------
;;; Request generation from diff operations

;; Sort phases for request ordering.  Deletions and modifications
;; must run before insertions at the same index so that content is
;; freed before new content is inserted at that location.
(defconst gdocs-diff--sort-phase-delete/modify 0
  "Sort phase for deletion and modification request groups.")
(defconst gdocs-diff--sort-phase-insert 1
  "Sort phase for insertion request groups.")

(defun gdocs-diff--generate-requests (old-ir new-ir diff-ops start-index)
  "Generate batchUpdate requests from OLD-IR, NEW-IR, and DIFF-OPS.
All operations are grouped by their document index and processed
from highest to lowest.  Each group's requests are kept together
so that a modification's delete+insert+style sequence runs
atomically before any lower-index operation can shift its indices.
START-INDEX is the UTF-16 index where the first element begins."
  (let* ((old-indices (gdocs-diff--compute-element-indices old-ir start-index))
         (last-old-index (1- (length old-ir)))
         (groups nil))
    ;; Pure deletions — delete the full element range including the
    ;; trailing newline so the element is actually removed.  The
    ;; last element in old-ir preserves its trailing newline (the
    ;; mandatory body newline that Google Docs API protects), but
    ;; only when it is a paragraph — non-paragraph elements (tables)
    ;; must be deleted in full to avoid partial structural deletions.
    (dolist (op diff-ops)
      (when (eq (plist-get op :op) 'delete)
        (let* ((oi (plist-get op :old-index))
               (elem (nth oi old-ir))
               (range (cdr (assq oi old-indices)))
               (start (car range))
               (end (if (and (= oi last-old-index)
                             (eq (plist-get elem :type) 'paragraph))
                        (1- (cdr range))
                      (cdr range))))
          (when (< start end)
            (push (list :index start
                        :sort-phase gdocs-diff--sort-phase-delete/modify
                        :reqs (list (gdocs-diff--make-delete-request start end)))
                  groups)))))
    ;; Modifications (keep delete+insert+style together)
    (dolist (op diff-ops)
      (when (eq (plist-get op :op) 'modify)
        (let* ((oi (plist-get op :old-index))
               (ni (plist-get op :new-index))
               (old-elem (nth oi old-ir))
               (new-elem (nth ni new-ir))
               (range (cdr (assq oi old-indices)))
               (result (gdocs-diff--modification-requests
                        old-elem new-elem range))
               (reqs (append (plist-get result :delete-reqs)
                             (plist-get result :insert-reqs)
                             (plist-get result :style-reqs))))
          (when reqs
            (push (list :index (car range)
                        :sort-phase gdocs-diff--sort-phase-delete/modify
                        :reqs reqs)
                  groups)))))
    ;; Insertions
    (dolist (op diff-ops)
      (when (eq (plist-get op :op) 'insert)
        (let* ((ni (plist-get op :new-index))
               (element (nth ni new-ir))
               (insert-index (gdocs-diff--insertion-point
                              op diff-ops old-indices start-index))
               (result (gdocs-convert--ir-element-to-requests
                        element insert-index)))
          (push (list :index insert-index
                      :sort-phase gdocs-diff--sort-phase-insert
                      :new-index ni
                      :reqs (plist-get result :requests))
                groups))))
    ;; Sort groups for correct index stability:
    ;; 1. Descending by index — process from end to start so earlier
    ;;    indices are not shifted by later operations.
    ;; 2. At the same index, deletions/modifications before insertions
    ;;    — delete first, then insert at the freed location.
    ;; 3. Among insertions at the same index, higher :new-index first
    ;;    so they stack in the correct forward order after reversal.
    (setq groups
          (sort groups
                (lambda (a b)
                  (let ((ia (plist-get a :index))
                        (ib (plist-get b :index)))
                    (cond
                     ((/= ia ib) (> ia ib))
                     ((/= (plist-get a :sort-phase) (plist-get b :sort-phase))
                      (< (plist-get a :sort-phase) (plist-get b :sort-phase)))
                     ((and (= (plist-get a :sort-phase)
                              gdocs-diff--sort-phase-insert)
                           (= (plist-get b :sort-phase)
                              gdocs-diff--sort-phase-insert))
                      (> (or (plist-get a :new-index) 0)
                         (or (plist-get b :new-index) 0)))
                     (t nil))))))
    (apply #'append (mapcar (lambda (g) (plist-get g :reqs)) groups))))

;; ---------------------------------------------------------------------------
;;; Deletion requests

(defun gdocs-diff--make-delete-request (start end)
  "Create a deleteContentRange request from START to END."
  `((deleteContentRange
     . ((range . ((startIndex . ,start)
                  (endIndex . ,end)
                  ;; Empty segmentId targets the document body
                  ;; (as opposed to headers, footers, or footnotes)
                  (segmentId . "")))))))

;; ---------------------------------------------------------------------------
;;; Insertion requests

(defun gdocs-diff--insertion-point (op diff-ops old-indices start-index)
  "Determine the document index at which to insert for OP.
DIFF-OPS is the full operation list.  OLD-INDICES maps old
element indices to document ranges.  START-INDEX is the fallback
index when inserting before all kept elements."
  (let ((preceding-old-index (gdocs-diff--preceding-kept-old-index
                              op diff-ops)))
    (if preceding-old-index
        (let ((range (cdr (assq preceding-old-index old-indices))))
          (cdr range))
      start-index)))

(defun gdocs-diff--preceding-kept-old-index (op diff-ops)
  "Find the old-index of the nearest preceding :keep operation before OP.
DIFF-OPS is the full operation list.  Returns nil if OP has no
preceding kept element."
  (let ((found nil)
        (target-reached nil))
    (dolist (other diff-ops)
      (unless target-reached
        (if (eq other op)
            (setq target-reached t)
          (when (eq (plist-get other :op) 'keep)
            (setq found (plist-get other :old-index))))))
    found))

;; ---------------------------------------------------------------------------
;;; Modification requests

(defun gdocs-diff--modification-requests (old-elem new-elem range)
  "Generate requests to modify OLD-ELEM into NEW-ELEM at RANGE.
RANGE is a (START . END) cons.  Returns a plist with
:delete-reqs, :insert-reqs, and :style-reqs."
  (cond
   ((gdocs-diff--only-style-changed-p old-elem new-elem)
    (gdocs-diff--style-only-modification old-elem new-elem range))
   ((gdocs-diff--only-formatting-changed-p old-elem new-elem)
    (gdocs-diff--formatting-only-modification new-elem range))
   (t
    (gdocs-diff--content-modification old-elem new-elem range))))

(defun gdocs-diff--same-text-paragraphs-p (old-elem new-elem)
  "Return non-nil if OLD-ELEM and NEW-ELEM are paragraphs with identical text."
  (and (eq (plist-get old-elem :type) 'paragraph)
       (eq (plist-get new-elem :type) 'paragraph)
       (equal (gdocs-convert--runs-to-plain-text (plist-get old-elem :contents))
              (gdocs-convert--runs-to-plain-text (plist-get new-elem :contents)))))

(defun gdocs-diff--only-style-changed-p (old-elem new-elem)
  "Return non-nil if only the paragraph style differs between elements."
  (and (gdocs-diff--same-text-paragraphs-p old-elem new-elem)
       (not (equal (plist-get old-elem :style)
                   (plist-get new-elem :style)))
       (equal (gdocs-diff--runs-formatting-key
               (plist-get old-elem :contents))
              (gdocs-diff--runs-formatting-key
               (plist-get new-elem :contents)))))

(defun gdocs-diff--only-formatting-changed-p (old-elem new-elem)
  "Return non-nil if only run formatting differs between elements."
  (and (gdocs-diff--same-text-paragraphs-p old-elem new-elem)
       (equal (plist-get old-elem :style)
              (plist-get new-elem :style))
       (not (equal (gdocs-diff--runs-formatting-key
                    (plist-get old-elem :contents))
                   (gdocs-diff--runs-formatting-key
                    (plist-get new-elem :contents))))))

(defun gdocs-diff--style-only-modification (_old-elem new-elem range)
  "Generate requests for a paragraph style change.
NEW-ELEM provides the new style.  RANGE is (START . END)."
  (let* ((start (car range))
         (end (cdr range))
         (style (plist-get new-elem :style))
         (named-style (gdocs-convert--ir-style-to-docs style)))
    (list :delete-reqs nil
          :insert-reqs nil
          :style-reqs
          (list `((updateParagraphStyle
                   . ((paragraphStyle
                       . ((namedStyleType . ,named-style)))
                      (range . ((startIndex . ,start)
                                (endIndex . ,(1- end))))
                      (fields . "namedStyleType"))))))))

(defun gdocs-diff--formatting-only-modification (new-elem range)
  "Generate updateTextStyle requests for formatting changes.
NEW-ELEM provides the new runs.  RANGE is (START . END)."
  (let* ((start (car range))
         (runs (plist-get new-elem :contents))
         (style-reqs (gdocs-convert--make-text-style-requests runs start)))
    (list :delete-reqs nil
          :insert-reqs nil
          :style-reqs style-reqs)))

(defun gdocs-diff--content-modification (old-elem new-elem range)
  "Generate delete+insert requests for a content change.
OLD-ELEM and NEW-ELEM are the old and new IR elements.  RANGE is
\(START . END) of OLD-ELEM in the document.  When both elements
are paragraphs, preserves the trailing newline to avoid breaking
document structure.  For any type mismatch (e.g. table to
paragraph) or non-paragraph old element, performs a full delete
and re-insert to avoid partial structural deletions that the API
rejects."
  (let* ((start (car range))
         (end (cdr range)))
    (if (and (eq (plist-get old-elem :type) 'paragraph)
             (eq (plist-get new-elem :type) 'paragraph))
        (gdocs-diff--paragraph-content-modification new-elem start end)
      (let* ((delete-req (gdocs-diff--make-delete-request start end))
             (insert-result (gdocs-convert--ir-element-to-requests
                             new-elem start)))
        (list :delete-reqs (list delete-req)
              :insert-reqs (plist-get insert-result :requests)
              :style-reqs nil)))))

(defun gdocs-diff--paragraph-content-modification (new-elem start end)
  "Generate all requests to replace a paragraph at START to END.
Handles text deletion and insertion, paragraph style, text
formatting, and list properties.  Deletes [START, END-1) to
preserve the trailing newline, then inserts new text at START."
  (let* ((text (gdocs-convert--runs-to-plain-text
                (plist-get new-elem :contents)))
         (text-len (gdocs-convert--string-to-utf16-length text))
         (para-end (+ start text-len 1))
         (delete-req (when (> (1- end) start)
                       (gdocs-diff--make-delete-request start (1- end))))
         (insert-req (when (> text-len 0)
                       (gdocs-convert--make-insert-text-request text start)))
         (style-reqs (gdocs-convert--make-paragraph-style-requests
                      new-elem start para-end))
         (run-reqs (gdocs-convert--make-text-style-requests
                    (plist-get new-elem :contents) start))
         (list-reqs (gdocs-convert--make-list-requests
                     new-elem start para-end)))
    ;; Pack style, run, and list requests into :insert-reqs so the
    ;; caller groups them with the text insertion at the same index,
    ;; keeping the whole modification atomic.
    (list :delete-reqs (when delete-req (list delete-req))
          :insert-reqs (append (when insert-req (list insert-req))
                               style-reqs run-reqs list-reqs)
          :style-reqs nil)))

(provide 'gdocs-diff)
;;; gdocs-diff.el ends here
