;;; readwise-lib.el --- Library for interacting with Readwise API -*- lexical-binding: t; -*-
;; Author: CountGreven
;; URL: https://github.com/CountGreven/readwise-lib
;; Version: 0.1
;; Package-Requires: ((emacs "24.3") (request "0.3.2"))
;; Keywords: tools, convenience

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

;; This library provides functions to interact with the Readwise API.

;;; Code:
(require 'request)
(require 'json)
(require 'auth-source)

(defvar readwise-api-base-url "https://readwise.io/api/v2"
  "The base URL for the Readwise API.")

(defcustom readwise-debug-level 0
  "Debug level for the Readwise library.
0 - No debug output.
1 - Basic debug output.
2 - Detailed debug output."
  :group 'readwise
  :type 'integer)

(defun readwise-debug (level msg &rest args)
  "Print debug message MSG at debug LEVEL with ARGS."
  (when (>= readwise-debug-level level)
    (apply 'message (concat "[Readwise] " msg) args)))

(defun readwise-get-access-token ()
  "Get the access token for Readwise from auth-source."
  (let ((found (nth 0 (auth-source-search :host "readwise.io"))))
    (if found
        (let ((secret (plist-get found :secret)))
          (if (functionp secret)
              (funcall secret)
            secret))
      (error "Readwise access token not found in auth-source"))))

(defun readwise-request (endpoint &optional method data success-callback error-callback)
  "Make a request to the Readwise API.
ENDPOINT is the API endpoint (e.g., \"/books\").
METHOD is the HTTP method (e.g., \"GET\", \"POST\").
DATA is an optional data payload for POST/PUT requests.
SUCCESS-CALLBACK is a function to call on success.
ERROR-CALLBACK is a function to call on error."
  (let ((url (concat readwise-api-base-url endpoint))
        (token (readwise-get-access-token))
        (method (or method "GET")))
    (readwise-debug 1 "Making %s request to %s" method url)
    (request url
      :type method
      :headers `(("Authorization" . ,(concat "Token " token))
                 ("Content-Type" . "application/json"))
      :data (when data (json-encode data))
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (readwise-debug 2 "Success: %S" data)
                  (when success-callback
                    (funcall success-callback data))))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (readwise-debug 1 "Error: %S" error-thrown)
                (when error-callback
                  (funcall error-callback error-thrown)))))))

(defun readwise-create-book (title &optional author success-callback error-callback)
  "Create a new book in Readwise with TITLE and optional AUTHOR.
SUCCESS-CALLBACK is called on success.
ERROR-CALLBACK is called on error."
  (let ((data `(("title" . ,title)
                ("author" . ,author))))
    (readwise-debug 1 "Creating book with title: %s, author: %s" title author)
    (readwise-request "/books" "POST" data success-callback error-callback)))

(defun readwise-update-book (book-id title &optional author success-callback error-callback)
  "Update an existing book in Readwise with BOOK-ID, TITLE, and optional AUTHOR.
SUCCESS-CALLBACK is called on success.
ERROR-CALLBACK is called on error."
  (let ((data `(("title" . ,title)
                ("author" . ,author))))
    (readwise-debug 1 "Updating book ID: %s with title: %s, author: %s" book-id title author)
    (readwise-request (format "/books/%s" book-id) "PUT" data success-callback error-callback)))

(defun readwise-delete-book (book-id success-callback error-callback)
  "Delete a book in Readwise with BOOK-ID.
SUCCESS-CALLBACK is called on success.
ERROR-CALLBACK is called on error."
  (readwise-debug 1 "Deleting book ID: %s" book-id)
  (readwise-request (format "/books/%s" book-id) "DELETE" nil success-callback error-callback))

(defun readwise-create-highlight (book-id text &optional note tags success-callback error-callback)
  "Create a new highlight in Readwise with BOOK-ID, TEXT, and optional NOTE and TAGS.
SUCCESS-CALLBACK is called on success.
ERROR-CALLBACK is called on error."
  (let ((data `(("book_id" . ,book-id)
                ("text" . ,text)
                ("note" . ,note)
                ("tags" . ,tags))))
    (readwise-debug 1 "Creating highlight in book ID: %s with text: %s" book-id text)
    (readwise-request "/highlights" "POST" data success-callback error-callback)))

(defun readwise-update-highlight (highlight-id text &optional note tags success-callback error-callback)
  "Update an existing highlight in Readwise with HIGHLIGHT-ID, TEXT, and optional NOTE and TAGS.
SUCCESS-CALLBACK is called on success.
ERROR-CALLBACK is called on error."
  (let ((data `(("text" . ,text)
                ("note" . ,note)
                ("tags" . ,tags))))
    (readwise-debug 1 "Updating highlight ID: %s with text: %s" highlight-id text)
    (readwise-request (format "/highlights/%s" highlight-id) "PUT" data success-callback error-callback)))

(defun readwise-delete-highlight (highlight-id success-callback error-callback)
  "Delete a highlight in Readwise with HIGHLIGHT-ID.
SUCCESS-CALLBACK is called on success.
ERROR-CALLBACK is called on error."
  (readwise-debug 1 "Deleting highlight ID: %s" highlight-id)
  (readwise-request (format "/highlights/%s" highlight-id) "DELETE" nil success-callback error-callback))

(defun readwise-get-highlights (&optional cursor updated-after success-callback error-callback)
  "Get highlights from Readwise.
Optional CURSOR and UPDATED-AFTER parameters for pagination and filtering.
SUCCESS-CALLBACK is called on success.
ERROR-CALLBACK is called on error."
  (let ((endpoint "/export"))
    (when cursor
      (setq endpoint (concat endpoint "?pageCursor=" cursor)))
    (when updated-after
      (setq endpoint (concat endpoint (if cursor "&" "?") "updatedAfter=" (url-hexify-string updated-after))))
    (readwise-debug 1 "Fetching highlights with cursor: %s, updated-after: %s" cursor updated-after)
    (readwise-request endpoint "GET" nil success-callback error-callback)))

(provide 'readwise-lib)

;;; readwise-lib.el ends here
