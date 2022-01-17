;;; counsel-winsearch.el --- Windows Search for Counsel  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Usage:
;; M-x counsel-winsearch

;;; Code:

(require 'counsel)
(require 'winsearch)

(defvar counsel-winsearch-history nil
  "History for `counsel-winsearch'.")

(defun counsel-winsearch-function (str)
  (or
   (ivy-more-chars)

   (winsearch-avoid-multibyte-issue
    (let ((counsel-async-command-delay 0));;Avoid multibyte issue
      (counsel--async-command
       (winsearch-make-command-string str))
      '("" "working...")))))

(defun counsel-winsearch-action (x)
  "Go to candidate X."
  (when (and x
             (string-match "\\`file:\\([^\n]+\\)\\'" x))
    (let ((file-path (match-string-no-properties 1 x)))
      (with-ivy-window
        (find-file file-path)))))

;;;###autoload
(defun counsel-winsearch (&optional initial-input)
  "Windows Search command.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
  (ivy-read "WinSearch: " #'counsel-winsearch-function
            :initial-input initial-input
            :dynamic-collection t
            :history 'counsel-winsearch-history
            :action #'counsel-winsearch-action
            :unwind #'counsel-delete-process
            :caller 'counsel-winsearch))

(provide 'counsel-winsearch)
;;; counsel-winsearch.el ends here
