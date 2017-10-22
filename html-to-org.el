;;; html-to-org.el --- Convert articles into org mode for archiving -*- lexical-binding: t -*-

;; Copyright (C) 2013 ailbe

;; Author: ailbe
;; Keywords: utils, data
;; Version: 1.0.0
;; Package-Requires: ((json "1.2") (request "0.2.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;; TODO: The ailbe_utils need to be removed
(require 'ailbe_utils)
(require 'request)
(require 'json)


(defcustom html-to-org/default-directory nil "Default path to download org files")
(defcustom html-to-org/org-file nil "Default org file to store headings in")
(defcustom html-to-org/pandoc-process nil "Set the location of the pandoc-process")
(defcustom html-to-org/mercury-api-key nil "Mercury API key")
(defvar html-to-org/mercury-api-parse-url nil)
(setq html-to-org/mercury-api-parse-url "https://mercury.postlight.com/parser?url=")

(defun html-to-org/build-readability-article-url (url)
  "Create readability request url for article at URL."
  (concat html-to-org/mercury-api-parse-url url))

(defun html-to-org/readability-request (url success-callback)
  "Make request to readability api to retrieve text of article at URL. 
Run SUCCESS-CALLBACK on parsed content."
  (let* (
         (readability-url (html-to-org/build-readability-article-url url))
         (fn success-callback)
         )
    (request readability-url
             :headers `(("x-api-key" . ,html-to-org/mercury-api-key))
             :parser 'json-read
             :timeout 20
             :success (cl-function
                       (lambda (&key data &allow-other-keys)
                         (setq html-to-org/json-response (assoc-default 'content data))
                         (funcall fn html-to-org/json-response)))
             :error (cl-function (lambda (&key error-thrown &allow-other-keys&rest _))))))

(defun html-to-org/write-string-to-file (string filename)
  "Write STRING to file FILENAME."
  (with-temp-buffer (insert string) (write-file filename)))

(defun html-to-org/html-to-file (url destfile)
  "Extract the text content from webpage, URL, and save it into new file, DESTFILE."
  (interactive "sEnter url: \nsEnter destination file path: ")
  (if (string= "" destfile) (progn
                              ;; build the default output file path and save org contents to that file
                              ;; TODO: move this into a function
                              (setq url-segments (url-generic-parse-url url))
                              (setq org-filename (format "%s" (file-name-nondirectory (url-filename url-segments))))
                              (setq destfile (concat (file-name-as-directory html-to-org/default-directory) org-filename))
                              (setq destfile (concat (file-name-sans-extension destfile) ".org")))
    (progn
      (setq destfile (expand-file-name destfile))))
  (html-to-org/readability-request url (apply-partially (lambda (destfile data)
                                                          (html-to-org/write-string-to-file data destfile)
                                                          (setq orgstring (html-to-org/convert-html-to-org destfile))
                                                          (html-to-org/write-string-to-file orgstring destfile)) destfile))

  ;; TODO.. wip
  (defun html-to-org/html-to-org-heading (url)
    "Extract the text content from the webpage URL and create a new org heading in the current buffer."
    (interactive "sEnter url: \n")
    (setq title-orgstring (ailbe/html-to-org url))
    (setq title (car title-orgstring))
    (setq orgstring (with-temp-buffer
                      (org-mode)
                      (insert (cdr title-orgstring))
                      (goto-char (point-min))
                      (insert "* " title
                              "\n** Article" "\n\n")
                      (buffer-string)))
    (setq html2orgbuffer (find-file-noselect html-to-org/org-file))
    (with-current-buffer html2orgbuffer
      (org-paste-subtree nil orgstring)
      (save-buffer))))

(defun html-to-org/convert-html-to-org (inputfile)
  "Convert html file, INPUTFILE,  to org string."
  (let ((convert-process)
        ;; there's a better way
        (pout "tempbuffer")
        (process-output-buffer))
    (setq convert-process (start-process "convert-html-to-org" pout html-to-org/pandoc-process "--from=html" "--to=org" inputfile))
    (setq process-output-buffer (process-buffer convert-process))
    (ailbe/buffer-string* process-output-buffer)))

(provide 'html-to-org)
;;; html-to-org.el ends here
