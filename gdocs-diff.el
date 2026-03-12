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
        (text (gdocs-diff--runs-text (plist-get element :contents)))
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
        (push (gdocs-diff--runs-text cell) cell-texts)))
    (format "table:%s" (s-join "|" (nreverse cell-texts)))))

(defun gdocs-diff--runs-text (runs)
  "Concatenate the :text values from RUNS."
  (mapconcat (lambda (run) (plist-get run :text)) runs ""))

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
    ('horizontal-rule 1)))

(defun gdocs-diff--paragraph-utf16-length (element)
  "Compute UTF-16 length of a paragraph ELEMENT including trailing newline."
  (let ((text (gdocs-diff--runs-text (plist-get element :contents))))
    (+ (gdocs-convert--string-to-utf16-length text) 1)))

(defun gdocs-diff--table-utf16-length (element)
  "Compute UTF-16 length of a table ELEMENT.
Tables have structural overhead: 1 for table start, 1 for table
end, plus for each row 1 start + 1 end, plus for each cell 2
(start + end) + content length + 1 newline."
  (let ((total 2)
        (rows (plist-get element :rows)))
    (dolist (row rows)
      (setq total (+ total 2))
      (dolist (cell row)
        (let ((text (gdocs-diff--runs-text cell)))
          (setq total (+ total 2
                         (gdocs-convert--string-to-utf16-length text)
                         1)))))
    total))

;; ---------------------------------------------------------------------------
;;; Fallback detection

(defun gdocs-diff--should-use-full-replace-p (old-ir new-ir diff-ops)
  "Return non-nil if the diff is too complex for incremental update.
OLD-IR and NEW-IR are the element lists.  DIFF-OPS is the
classified operation list.  Triggers when more than 50% of
elements changed and there are enough elements to justify it."
  (let* ((total (max (length old-ir) (length new-ir) 1))
         (kept (cl-count-if
                (lambda (op) (eq (plist-get op :op) 'keep))
                diff-ops)))
    (and (> total 2)
         (< (* 2 kept) total))))

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
Excludes the trailing newline of the last element to avoid
deleting structural characters that Google Docs API protects."
  (let ((indices (gdocs-diff--compute-element-indices old-ir start-index)))
    (when indices
      (let* ((first-entry (cdar indices))
             (last-entry (cdar (last indices)))
             (start (car first-entry))
             (end (1- (cdr last-entry))))
        (when (< start end)
          (list (gdocs-diff--make-delete-request start end)))))))

(defun gdocs-diff--insert-all-requests (new-ir start-index)
  "Generate insert requests for all elements in NEW-IR.
START-INDEX is the UTF-16 index where the first element begins."
  (let ((requests nil)
        (index start-index))
    (dolist (element new-ir)
      (let ((result (gdocs-convert--ir-element-to-requests element index)))
        (setq requests (append requests (plist-get result :requests)))
        (setq index (plist-get result :index))))
    requests))

;; ---------------------------------------------------------------------------
;;; Request generation from diff operations

(defun gdocs-diff-generate-requests (old-ir new-ir diff-ops &optional start-index)
  "Generate batchUpdate requests from OLD-IR, NEW-IR, and DIFF-OPS.
Returns a list of request plists with correct ordering: deletions
in reverse index order, then insertions in forward order.
START-INDEX is the UTF-16 index where the first element begins
\(default 1)."
  (gdocs-diff--generate-requests old-ir new-ir diff-ops (or start-index 1)))

(defun gdocs-diff--generate-requests (old-ir new-ir diff-ops start-index)
  "Generate batchUpdate requests from OLD-IR, NEW-IR, and DIFF-OPS.
Returns a list of request plists with correct ordering: deletions
in reverse index order, then insertions in forward order.
START-INDEX is the UTF-16 index where the first element begins."
  (let* ((old-indices (gdocs-diff--compute-element-indices old-ir start-index))
         (deletions (gdocs-diff--collect-deletions diff-ops old-indices))
         (modifications (gdocs-diff--collect-modifications
                         diff-ops old-ir new-ir old-indices))
         (insertions (gdocs-diff--collect-insertions
                      diff-ops new-ir old-indices start-index)))
    (append (gdocs-diff--sort-deletions-descending
             (append deletions (plist-get modifications :delete-reqs)))
            (plist-get modifications :style-reqs)
            (gdocs-diff--sort-insertions-ascending
             (append insertions (plist-get modifications :insert-reqs))))))

;; ---------------------------------------------------------------------------
;;; Deletion requests

(defun gdocs-diff--collect-deletions (diff-ops old-indices)
  "Collect delete requests for :delete operations in DIFF-OPS.
OLD-INDICES is the index alist.  Uses END-1 to preserve the
trailing newline, avoiding invalid deletion when followed by a
structural element."
  (let ((requests nil))
    (dolist (op diff-ops)
      (when (eq (plist-get op :op) 'delete)
        (let* ((oi (plist-get op :old-index))
               (range (cdr (assq oi old-indices)))
               (start (car range))
               (end (1- (cdr range))))
          (when (< start end)
            (push (gdocs-diff--make-delete-request start end) requests)))))
    requests))

(defun gdocs-diff--make-delete-request (start end)
  "Create a deleteContentRange request from START to END."
  `((deleteContentRange
     . ((range . ((startIndex . ,start)
                  (endIndex . ,end)
                  (segmentId . "")))))))

(defun gdocs-diff--sort-deletions-descending (requests)
  "Sort deletion REQUESTS by startIndex in descending order."
  (sort requests
        (lambda (a b)
          (> (gdocs-diff--request-start-index a)
             (gdocs-diff--request-start-index b)))))

(defun gdocs-diff--request-start-index (request)
  "Extract the startIndex from a REQUEST alist."
  (cond
   ((alist-get 'deleteContentRange request)
    (alist-get 'startIndex
               (alist-get 'range (alist-get 'deleteContentRange request))))
   ((alist-get 'insertText request)
    (alist-get 'index
               (alist-get 'location (alist-get 'insertText request))))
   (t 0)))

;; ---------------------------------------------------------------------------
;;; Insertion requests

(defun gdocs-diff--collect-insertions (diff-ops new-ir old-indices start-index)
  "Collect insert requests for :insert operations in DIFF-OPS.
NEW-IR is the new element list.  OLD-INDICES is the index alist.
START-INDEX is the fallback insertion index when inserting before
all kept elements.  Computes insertion points based on preceding
kept elements."
  (let ((requests nil))
    (dolist (op diff-ops)
      (when (eq (plist-get op :op) 'insert)
        (let* ((ni (plist-get op :new-index))
               (element (nth ni new-ir))
               (insert-index (gdocs-diff--insertion-point
                              op diff-ops old-indices start-index)))
          (setq requests
                (append requests
                        (plist-get
                         (gdocs-convert--ir-element-to-requests
                          element insert-index)
                         :requests))))))
    requests))

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

(defun gdocs-diff--sort-insertions-ascending (requests)
  "Sort insertion REQUESTS by index in ascending order."
  (sort requests
        (lambda (a b)
          (< (gdocs-diff--request-start-index a)
             (gdocs-diff--request-start-index b)))))

;; ---------------------------------------------------------------------------
;;; Modification requests

(defun gdocs-diff--collect-modifications (diff-ops old-ir new-ir old-indices)
  "Collect requests for :modify operations in DIFF-OPS.
OLD-IR and NEW-IR are the element lists.  OLD-INDICES is the
index alist.  Returns a plist with :delete-reqs, :insert-reqs,
and :style-reqs."
  (let ((delete-reqs nil)
        (insert-reqs nil)
        (style-reqs nil))
    (dolist (op diff-ops)
      (when (eq (plist-get op :op) 'modify)
        (let* ((oi (plist-get op :old-index))
               (ni (plist-get op :new-index))
               (old-elem (nth oi old-ir))
               (new-elem (nth ni new-ir))
               (range (cdr (assq oi old-indices)))
               (result (gdocs-diff--modification-requests
                        old-elem new-elem range)))
          (setq delete-reqs (append delete-reqs
                                    (plist-get result :delete-reqs)))
          (setq insert-reqs (append insert-reqs
                                    (plist-get result :insert-reqs)))
          (setq style-reqs (append style-reqs
                                   (plist-get result :style-reqs))))))
    (list :delete-reqs delete-reqs
          :insert-reqs insert-reqs
          :style-reqs style-reqs)))

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

(defun gdocs-diff--only-style-changed-p (old-elem new-elem)
  "Return non-nil if only the paragraph style differs between elements.
OLD-ELEM and NEW-ELEM must both be paragraphs with the same text
content and formatting but different :style."
  (and (eq (plist-get old-elem :type) 'paragraph)
       (eq (plist-get new-elem :type) 'paragraph)
       (not (equal (plist-get old-elem :style)
                   (plist-get new-elem :style)))
       (equal (gdocs-diff--runs-text (plist-get old-elem :contents))
              (gdocs-diff--runs-text (plist-get new-elem :contents)))
       (equal (gdocs-diff--runs-formatting-key
               (plist-get old-elem :contents))
              (gdocs-diff--runs-formatting-key
               (plist-get new-elem :contents)))))

(defun gdocs-diff--only-formatting-changed-p (old-elem new-elem)
  "Return non-nil if only run formatting differs between elements.
OLD-ELEM and NEW-ELEM must be paragraphs with the same text and
style but different run formatting."
  (and (eq (plist-get old-elem :type) 'paragraph)
       (eq (plist-get new-elem :type) 'paragraph)
       (equal (plist-get old-elem :style)
              (plist-get new-elem :style))
       (equal (gdocs-diff--runs-text (plist-get old-elem :contents))
              (gdocs-diff--runs-text (plist-get new-elem :contents)))
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

(defun gdocs-diff--content-modification (_old-elem new-elem range)
  "Generate delete+insert requests for a content change.
NEW-ELEM provides the new content.  RANGE is (START . END).
For paragraphs, preserves the trailing newline to avoid breaking
document structure (Google Docs API rejects deletion of a
paragraph's trailing newline when followed by a structural
element like a table or table of contents)."
  (let* ((start (car range))
         (end (cdr range)))
    (if (eq (plist-get new-elem :type) 'paragraph)
        (gdocs-diff--paragraph-content-modification new-elem start end)
      (let* ((delete-req (gdocs-diff--make-delete-request start end))
             (insert-result (gdocs-convert--ir-element-to-requests
                             new-elem start)))
        (list :delete-reqs (list delete-req)
              :insert-reqs (plist-get insert-result :requests)
              :style-reqs nil)))))

(defun gdocs-diff--paragraph-content-modification (new-elem start end)
  "Generate requests to replace paragraph content at START to END.
Deletes [START, END-1) to preserve the trailing newline, then
inserts the new text (without newline) at START."
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
    (list :delete-reqs (when delete-req (list delete-req))
          :insert-reqs (append (when insert-req (list insert-req))
                               style-reqs run-reqs list-reqs)
          :style-reqs nil)))

;; ---------------------------------------------------------------------------
;;; List change requests

(defun gdocs-diff--list-change-requests (old-elem new-elem range)
  "Generate requests for list property changes between OLD-ELEM and NEW-ELEM.
RANGE is (START . END)."
  (let ((old-list (plist-get old-elem :list))
        (new-list (plist-get new-elem :list))
        (start (car range))
        (end (cdr range)))
    (cond
     ((and (null old-list) new-list)
      (let ((preset (gdocs-convert--list-type-to-preset
                     (plist-get new-list :type))))
        (list `((createParagraphBullets
                 . ((range . ((startIndex . ,start)
                              (endIndex . ,(1- end))))
                    (bulletPreset . ,preset)))))))
     ((and old-list (null new-list))
      (list `((deleteParagraphBullets
               . ((range . ((startIndex . ,start)
                            (endIndex . ,(1- end)))))))))
     (t nil))))

(provide 'gdocs-diff)
;;; gdocs-diff.el ends here
