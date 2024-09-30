;;; org-readwise.el --- Sync Readwise highlights with Org-mode -*- lexical-binding: t; -*-
;; Author: CountGreven
;; URL: https://github.com/CountGreven/org-readwise
;; Version: 0.2
;; Package-Requires: ((emacs "24.3") (request "0.3.2") (org "9.1"))
;; Keywords: tools, convenience, outlines, hypermedia

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package integrates Readwise highlight syncing with Org-mode.
;; It provides commands to fetch highlights from Readwise and insert
;; them into an Org buffer or a specified file.

;;; Code:
(require 'org)
(require 'auth-source)
(require 'url)
(require 'request)
(require 'json)
(require 'readwise-lib)

(defvar org-readwise--sync-url "https://readwise.io/api/v2/export")
(defvar org-readwise--last-cursor nil
  "Tracks the last cursor received from the Readwise API during pagination.")

(defgroup org-readwise ()
  "Integrate the Readwise.io highlight syncing service with `org-mode`."
  :group 'files
  :prefix "org-readwise-"
  :link '(info-link "(org-readwise) Top"))

(defcustom org-readwise-last-sync-time-file "~/.emacs.d/org-readwise-last-sync"
  "File to store the last sync time for org-readwise."
  :group 'org-readwise
  :type 'string)

(defcustom org-readwise-output-location 'buffer
  "Specify where to output the Readwise highlights: 'buffer or a file path."
  :group 'org-readwise
  :type '(choice (const :tag "Buffer" buffer)
                 (file :tag "File")))

(defcustom org-readwise-debug-level 0
  "Debug level for the org-readwise package.
0 - No debug output.
1 - Basic debug output.
2 - Detailed debug output."
  :group 'org-readwise
  :type 'integer)

(defvar org-readwise-last-sync-time nil
  "The timestamp of the last successful sync.")

(defun org-readwise-debug (level message &rest args)
  "Log a debug message at LEVEL.
MESSAGE is the format string, and ARGS are the arguments for the format string."
  (when (>= org-readwise-debug-level level)
    (apply #'message (concat "[org-readwise] " message) args)))

(defun org-readwise--load-last-sync-time ()
  "Load the last sync time from `org-readwise-last-sync-time-file`."
  (when (file-exists-p org-readwise-last-sync-time-file)
    (with-temp-buffer
      (insert-file-contents org-readwise-last-sync-time-file)
      (setq org-readwise-last-sync-time (buffer-string)))))

(defun org-readwise--save-last-sync-time (timestamp)
  "Save the last sync time TIMESTAMP to `org-readwise-last-sync-time-file`."
  (with-temp-file org-readwise-last-sync-time-file
    (insert timestamp))
  (setq org-readwise-last-sync-time timestamp))

(defun org-readwise--get-access-token ()
  "Get the access token for Readwise.
This function expects the token to be present in one of the files defined in
`auth-sources`. If it is not present, it will prompt the user for their access
token. It returns a list containing the token and a save function."
  (let ((found (nth 0 (auth-source-search :host "readwise.io"))))
    (if found
        (list (let ((secret (plist-get found :secret)))
                (if (functionp secret)
                    (funcall secret)
                  secret))
              (plist-get found :save-function))
      nil)))

(defun org-readwise--get-highlights (&optional cursor updated-after)
  "Get highlights from the Readwise API, handling pagination with CURSOR.
Include the UPDATED-AFTER parameter only in the initial request."
  (let* ((token-header (list (cons "Authorization" (concat "Token " (nth 0 (org-readwise--get-access-token))))))
         (url (if cursor
                  (concat org-readwise--sync-url "?pageCursor=" (format "%s" cursor))
                (concat org-readwise--sync-url
                        (when updated-after (concat "?updatedAfter=" (url-hexify-string updated-after)))))))
    (org-readwise-debug 1 "Making request to: %s" url)
    (request url
      :headers token-header
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (org-readwise-debug 2 "Response Data: %S" data)
                  (let ((results (assoc-default 'results data))
                        (next-cursor (assoc-default 'nextPageCursor data)))
                    (when results
                      (org-readwise--process-highlights results))
                    (if (and next-cursor (not (string-empty-p next-cursor))
                             (not (string= next-cursor org-readwise--last-cursor)))
                        (progn
                          (setq org-readwise--last-cursor next-cursor)
                          (org-readwise--get-highlights next-cursor updated-after))
                      (org-readwise-debug 1 "No more pages to fetch or cursor repeated, stopping pagination.")))))
      :error (cl-function
              (lambda (&key response data &allow-other-keys)
                (let* ((status-code (request-response-status-code response))
                       (retry-after (assoc-default 'Retry-After (request-response-headers response)))
                       (detail (assoc-default 'detail data))
                       (retry-seconds (if retry-after
                                          (string-to-number retry-after)
                                        (if (string-match "in \\([0-9]+\\) seconds" detail)
                                            (string-to-number (match-string 1 detail))
                                          30))))  ;; Default to 30 seconds if no information is available
                  (if (eq status-code 429)
                      (progn
                        (org-readwise-debug 1 "Rate limit hit, retrying after %s seconds" retry-seconds)
                        (run-at-time retry-seconds nil #'org-readwise--get-highlights cursor updated-after))
                    (org-readwise-debug 1 "Error Response: %S" response)
                    (message "Error fetching highlights: %S" status-code))))))))


(defun org-readwise--insert-org-heading (level title id &optional author url body tags buffer)
  "Insert an org-mode heading.
LEVEL is the heading level.
TITLE is the heading title.
ID is the ID property.
AUTHOR is the optional author property.
URL is the optional URL property.
BODY is the optional body content.
TAGS are optional tags for the heading.
BUFFER is the buffer or file to insert the heading into."
  (with-current-buffer (or buffer (current-buffer))
    (insert (format "%s %s" (make-string level ?*) title))
    (when (and tags (not (string-empty-p tags)))
      (insert (format " :%s:" tags)))
    (insert "\n  :PROPERTIES:\n  :ID: " id "\n")
    (when author
      (setq author (replace-regexp-in-string "\n" " " author))  ; Replace newlines with spaces
      (insert (format "  :AUTHOR: %s\n" author)))
    (when url
      (insert (format "  :URL: %s\n" url)))
    (insert "  :END:\n")
    (when body
      (insert (format "  %s\n\n" body)))))


(defun org-readwise--process-highlight (highlight buffer)
  "Process a single HIGHLIGHT and insert it into BUFFER."
  (let ((highlight-id (number-to-string (assoc-default 'id highlight)))
        (text (assoc-default 'text highlight))
        (note (assoc-default 'note highlight)))
    (org-readwise--insert-org-heading 2 "Highlight" highlight-id nil nil text nil buffer)
    (when (and note (not (string-empty-p note)))
      (org-readwise--insert-org-heading 3 "Note" (concat highlight-id "-note") nil nil note nil buffer))))


(defun org-readwise--process-book (book buffer)
  "Process a single BOOK and insert its details into BUFFER."
  (let* ((book-id (number-to-string (assoc-default 'user_book_id book)))
         (title (or (assoc-default 'title book) "No Title"))
         (author (assoc-default 'author book))
         (tags (mapconcat (lambda (tag) (assoc-default 'name tag))
                          (assoc-default 'book_tags book) ":"))
         (summary (assoc-default 'summary book))
         (source-url (assoc-default 'source_url book)))
    (org-readwise--insert-org-heading 1 title book-id author source-url summary tags buffer)
    (let ((highlights (assoc-default 'highlights book)))
      (when (vectorp highlights)
        (setq highlights (append highlights nil)))
      (dolist (highlight highlights)
        (org-readwise--process-highlight highlight buffer)))))

(defun org-readwise--process-highlights (results)
  "Process highlights data and print it in a structured way."
  (org-readwise-debug 1 "Processing %d books" (length results))
  (org-readwise-debug 2 "Results type: %s" (type-of results))
  (when (or (listp results) (vectorp results))
    (setq results (append results nil))  ; Convert vector to list if necessary
    (let* ((output-buffer (when (eq org-readwise-output-location 'buffer)
                            (get-buffer-create "*Readwise Highlights*")))
           (output-file (when (and (stringp org-readwise-output-location)
                                   (not (eq org-readwise-output-location 'buffer)))
                          org-readwise-output-location)))
      (org-readwise-debug 2 "Output buffer: %s" output-buffer)
      (org-readwise-debug 2 "Output file: %s" output-file)
      (with-current-buffer (or output-buffer (find-file-noselect output-file))
        (goto-char (point-max))
        (dolist (book results)
          (org-readwise-debug 1 "Processing book: %s (ID: %s)" (or (assoc-default 'title book) "No Title") (assoc-default 'user_book_id book))
          (org-readwise--process-book book (current-buffer))
          (org-readwise-debug 2 "Finished processing book: %s (ID: %s)" (or (assoc-default 'title book) "No Title") (assoc-default 'user_book_id book))))
      (when output-buffer
        (with-current-buffer output-buffer
          (org-mode)
          (goto-char (point-min))
          (display-buffer (current-buffer)))))))

(defun org-readwise--get-documents (&optional cursor updated-after)
  "Get documents from the Readwise API v3, handling pagination with CURSOR.
Include the UPDATED-AFTER parameter only in the initial request."
  (let* ((token-header (list (cons "Authorization" (concat "Token " (nth 0 (org-readwise--get-access-token))))))
         (url (concat "https://readwise.io/api/v3/list/"
                      (when cursor (concat "?pageCursor=" cursor))
                      (when updated-after (concat (if cursor "&" "?") "updatedAfter=" (url-hexify-string updated-after))))))
    (org-readwise-debug 1 "Making request to: %s" url)
    (request url
      :headers token-header
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (org-readwise-debug 2 "Response Data: %S" data)
                  (let ((results (assoc-default 'results data))
                        (next-cursor (assoc-default 'nextPageCursor data)))
                    (when results
                      (org-readwise--process-documents results (get-buffer-create "*Readwise Highlights*")))
                    (if (and next-cursor (not (string-empty-p next-cursor))
                             (not (string= next-cursor org-readwise--last-cursor)))
                        (progn
                          (setq org-readwise--last-cursor next-cursor)
                          (org-readwise--get-documents next-cursor updated-after))
                      (org-readwise-debug 1 "No more pages to fetch or cursor repeated, stopping pagination.")))))
      :error (cl-function
              (lambda (&key response data &allow-other-keys)
                (let* ((status-code (request-response-status-code response))
                       (retry-after (assoc-default 'Retry-After (request-response-headers response)))
                       (detail (assoc-default 'detail data))
                       (retry-seconds (if retry-after
                                          (string-to-number retry-after)
                                        (if (string-match "in \\([0-9]+\\) seconds" detail)
                                            (string-to-number (match-string 1 detail))
                                          30))))  ;; Default to 30 seconds if no information is available
                  (if (eq status-code 429)
                      (progn
                        (org-readwise-debug 1 "Rate limit hit, retrying after %s seconds" retry-seconds)
                        (run-at-time retry-seconds nil #'org-readwise--get-documents cursor updated-after))
                    (org-readwise-debug 1 "Error Response: %S" response)
                    (message "Error fetching documents: %S" status-code))))))))


(defun org-readwise--process-document (document buffer &optional parent-id)
  "Process a single DOCUMENT and insert its details into BUFFER.
If PARENT-ID is provided, insert the document as a subheading under the parent."
  (let* ((doc-id (assoc-default 'id document))
         (title (or (assoc-default 'title document) "No Title"))
         (author (assoc-default 'author document))
         (summary (assoc-default 'summary document))
         (notes (assoc-default 'notes document))
         (source-url (assoc-default 'source_url document))
         (tags (assoc-default 'tags document)))
    (org-readwise-debug 2 "Inserting document with ID: %s, Title: %s" doc-id title)
    ;; Convert tags to a string if necessary and ensure it's a list of strings
    (when (and tags (listp tags))
      (org-readwise-debug 2 "Processing tags for document ID: %s" doc-id)
      (setq tags (mapconcat (lambda (tag)
                              (if (stringp tag)
                                  tag
                                (assoc-default 'name tag))) ;; Ensure we only get the string 'name'
                            tags ":")))
    (org-readwise--insert-org-heading (if parent-id 2 1) title doc-id author source-url summary tags buffer)
    ;; Only insert notes if they are not null and not empty
    (when (and notes (not (string-empty-p notes)))
      (org-readwise--insert-org-heading (if parent-id 3 2) "Notes" (concat doc-id "-notes") nil nil notes nil buffer))))


(defun org-readwise--process-documents (documents buffer)
  "Process documents data and insert them into Org mode, handling parent-child relationships."
  ;; Convert documents to a list if it's a vector
  (when (vectorp documents)
    (org-readwise-debug 2 "Converting documents vector to list")
    (setq documents (append documents nil)))  ;; Convert vector to list

  ;; Ensure it's a list before proceeding
  (when (listp documents)
    (org-readwise-debug 1 "Processing %d documents" (length documents))
    (let ((document-hash (make-hash-table :test 'equal))
          (processed-ids (make-hash-table :test 'equal)))  ;; Keep track of processed documents
      ;; First, populate the hash table with all documents by their IDs
      (dolist (document documents)
        (let ((doc-id (assoc-default 'id document)))
          (org-readwise-debug 2 "Adding document to hash with ID: %s" doc-id)
          (puthash doc-id document document-hash)))
      ;; Now process each document
      (dolist (document documents)
        (let ((doc-id (assoc-default 'id document))
              (parent-id (assoc-default 'parent_id document)))
          (unless (gethash doc-id processed-ids)  ;; Skip if document is already processed
            (org-readwise-debug 2 "Processing document with ID: %s" doc-id)
            (if parent-id
                (let ((parent-document (gethash parent-id document-hash)))
                  (if (and parent-document (not (gethash parent-id processed-ids)))
                      (progn
                        (org-readwise-debug 2 "Found parent document with ID: %s for document: %s" parent-id doc-id)
                        (org-readwise--process-document document buffer parent-id))
                    (org-readwise-debug 2 "Parent document with ID: %s not found for document: %s" parent-id doc-id)))
              (org-readwise--process-document document buffer))
            ;; Mark as processed and log the action
            (org-readwise-debug 2 "Document with ID: %s has been processed" doc-id)
            (puthash doc-id t processed-ids)))))))

(defun string-empty-p (str)
  "Check whether STR is empty."
  (string= str ""))

(defun org-readwise--get-next-page-cursors (&optional cursor updated-after)
  "Get nextPageCursor from the Readwise API v3, handling pagination with CURSOR."
  (let* ((token-header (list (cons "Authorization" (concat "Token " (nth 0 (org-readwise--get-access-token))))))
         (url (concat "https://readwise.io/api/v3/list/"
                      (when cursor (concat "?pageCursor=" cursor))
                      (when updated-after (concat (if cursor "&" "?") "updatedAfter=" (url-hexify-string updated-after))))))
    (request url
      :headers token-header
      :parser 'json-read
      :success
      (cl-function
       (lambda (&key data &allow-other-keys)
         (let ((next-cursor (assoc-default 'nextPageCursor data)))
           ;; Print nextPageCursor at debug level 1
           (when next-cursor
             (org-readwise-debug 1 "Next Page Cursor: %s" next-cursor))
           ;; The rest of the processing at debug level 2
           (org-readwise-debug 2 "Response Data: %S" data)
           (when (and next-cursor (not (string= next-cursor cursor))) ;; Avoid looping on the same cursor
             (org-readwise--get-next-page-cursors next-cursor updated-after)))))
      :error (cl-function
              (lambda (&key response &allow-other-keys)
                (message "Error fetching documents: %S" (request-response-status-code response)))))
    :status-code '((401 . (lambda (&rest _) (message "Unauthorized"))))))

(defun org-readwise-sync-cursors (&optional all)
  "Print only nextPageCursor from Readwise during pagination."
  (interactive "P")
  (setq org-readwise--last-cursor nil)  ;; Reset the cursor before sync
  (org-readwise--load-last-sync-time)
  (let ((updated-after (unless all org-readwise-last-sync-time)))
    (org-readwise--get-next-page-cursors nil updated-after)))

(defun org-readwise-sync (&optional all)
  "Synchronize highlights and documents from Readwise and insert them into an Org buffer or file.
If ALL is non-nil (when called with a universal argument), pull all highlights and documents."
  (interactive "P")
  (let* ((output-buffer (when (eq org-readwise-output-location 'buffer)
                          (get-buffer-create "*Readwise Highlights*")))
         (output-file (when (and (stringp org-readwise-output-location)
                                 (not (eq org-readwise-output-location 'buffer)))
                        org-readwise-output-location)))
    (when output-buffer
      (with-current-buffer output-buffer
        (erase-buffer)))
    (setq org-readwise--last-cursor nil)  ;; Reset the cursor before sync
    (org-readwise--load-last-sync-time)
    (let ((updated-after (unless all org-readwise-last-sync-time)))
      ;; Sync highlights from v2
      (org-readwise--get-highlights nil updated-after)
      ;; Sync documents from v3 with updated-after
      (org-readwise--get-documents nil updated-after)
      ;; Save the last sync time
      (org-readwise--save-last-sync-time (format-time-string "%Y-%m-%dT%H:%M:%S%z")))
    (when output-file
      (with-temp-file output-file
        (insert-buffer-substring (or output-buffer (get-buffer-create "*Readwise Highlights*")))))))

(provide 'org-readwise)
