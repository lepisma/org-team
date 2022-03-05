;;; org-team.el --- Team management with Org -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;; Version: 0.1.1
;; Package-Requires: ((emacs "27") (f "0.20.0") (helm "3.7.0") (org "9.4"))
;; URL: https://github.com/lepisma/org-team

;;; Commentary:

;; Team management with Org
;; This file is not a part of GNU Emacs.

;;; License:

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

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'eieio)
(require 'f)
(require 'helm)
(require 'org)
(require 's)

(defcustom org-team-dir nil
  "Directory with team files."
  :type 'directory)

(defclass org-team-person ()
  ((directory :initarg :directory))
  "A person in org-team")

(cl-defmethod org-team-person-file ((p org-team-person))
  "Return index file for person P if present."
  (let ((file-path (f-join (oref p :directory) "index.org")))
    (when (f-exists-p file-path)
      file-path)))

(defun org--file-title (file)
  "Return title for an org file"
  (with-temp-buffer
    (insert-file-contents file nil nil 1000)
    (goto-char (point-min))
    (when (re-search-forward "^#\\+TITLE:\\(.*\\)$")
      (s-trim (match-string-no-properties 1)))))

(cl-defmethod org-team-person-name ((p org-team-person))
  "Return name of the person."
  (let ((index-file (org-team-person-file p)))
    (when index-file
      (org--file-title index-file))))

(cl-defmethod org-team-person-open-file ((p org-team-person))
  (find-file (org-team-person-file p)))

(defun org-team--parse-list-of-links ()
  "Parse list items of org mode links from current position."
  (let ((elem (org-element-context)))
    (when (eq 'plain-list (org-element-type elem))
      (-map (lambda (ls)
              (car (org-element-parse-secondary-string (s-trim (buffer-substring-no-properties (+ (nth 0 ls) (length (nth 2 ls))) (nth 6 ls))) '(link))))
            (org-element-property :structure elem)))))

(cl-defmethod org-team-person--links ((p org-team-person))
  (with-current-buffer (find-file-noselect (org-team-person-file p))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^* Links$" nil t)
        ;; TODO: Assumption is that the list starts from next line
        (forward-char)
        (org-team--parse-list-of-links)))))

(cl-defmethod org-team-person-open-links ((p org-team-person))
  (let ((links (org-team-person--links p)))
    (unless links
      (error "No links found"))
    (browse-url
     (org-element-property :raw-link (helm :sources (helm-build-sync-source "Links"
                                                     :candidates (-map (lambda (l) (cons (substring-no-properties (car (org-element-contents l))) l)) links))
                                          :buffer "*helm org team - links*"
                                          :prompt "Link: ")))))

(defun org-team-list-people ()
  "Return a list of people found in ORG-TEAM-DIR."
  (unless org-team-dir
    (error "`org-team-dir' not set."))
  (->> (f-directories org-team-dir)
     (-map (lambda (d) (make-instance 'org-team-person :directory d)))
     (-remove (lambda (p) (null (org-team-person-file p))))))

(defun org-team--pick-person ()
  "Helm based picker for registered team members."
  (let ((people (org-team-list-people)))
    (helm :sources (helm-build-sync-source "Team member"
                     :candidates (-map (lambda (p) (cons (org-team-person-name p) p)) people))
          :buffer "*helm org team*"
          :prompt "Name: ")))

(defun org-team-visit-person-log ()
  "Ask to pick the person and position cursor to add log entry."
  (let ((person (org-team--pick-person)))
    (org-team-person-open-file person)
    (goto-char (point-min))
    (re-search-forward "^* Log$")))

;;;###autoload
(defun org-team-add-person (name)
  "Initialize a new person in the system."
  (interactive "sName: ")
  (let ((dir-path (f-join org-team-dir (s-dashed-words name))))
    (mkdir dir-path t)
    (with-temp-file (f-join dir-path "index.org")
      (insert "#+TITLE: " (s-titleized-words name))
      (org-insert-heading nil nil t)
      (insert "Log"))))

(defun org-team ()
  "Main function to interact with people in team."
  (interactive)
  (let ((people (org-team-list-people)))
    (helm :sources (helm-build-sync-source "Team member"
                     :candidates (-map (lambda (p) (cons (org-team-person-name p) p)) people)
                     :action '(("Open File" . org-team-person-open-file)
                               ("Open Links" . org-team-person-open-links)))
          :buffer "*helm org team*"
          :prompt "Name: ")))

(provide 'org-team)

;;; org-team.el ends here

