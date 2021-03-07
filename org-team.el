;;; org-team.el --- Team management with Org -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;; Version: 0.0.2
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

(defun org-team ()
  "Main function to interact with people in team."
  (interactive)
  (org-team-person-open-file (org-team--pick-person)))

(provide 'org-team)

;;; org-team.el ends here

