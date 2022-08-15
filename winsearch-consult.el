;;; winsearch-consult.el --- Windows Search for Consult -*- lexical-binding: t; -*-

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
;; M-x consult-winsearch

;;; Code:

(require 'consult)
(require 'winsearch)

(defun consult--winsearch-builder (input)
  "Build command line given CONFIG and INPUT."
  (unless (string-blank-p input)
    (list :command (winsearch-make-command-program-args input)
          :highlight (cdr (consult--default-regexp-compiler input 'basic t)))))

;;;###autoload
(defun consult-winsearch (&optional initial)
  "Search with `winsearch' for files which match input given INITIAL input."
  (interactive)
  (let ((url (consult--find "WinSearch: " #'consult--winsearch-builder initial)))
    (when (string-match "\\`file:\\([^\n]+\\)\\'" url)
      (find-file (match-string-no-properties 1 url)))))


(provide 'winsearch-consult)
;;; winsearch-consult.el ends here
