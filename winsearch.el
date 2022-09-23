;;; winsearch.el --- Windows Search for Emacs       -*- lexical-binding: t; -*-

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
;; (require 'winsearch)
;; M-x winsearch

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'compile)
(require 'grep)

(defgroup winsearch nil
  "Windows Search."
  :group 'winsearch)

(defcustom winsearch-adoquery-path nil
  "Path to adoquery.exe (See https://github.com/misohena/adoquery )"
  :type 'string :group 'winsearch)

;;;; Initialize Adoquery

(defconst winsearch-adoquery-filename "adoquery.exe")

(defun winsearch-find-adoquery-path ()
  "Find adoquery.exe."
  (if-let ((el-path (or load-file-name
                        (locate-library "winsearch")))
           (el-dir (file-name-directory el-path))
           (exe-path (expand-file-name
                      winsearch-adoquery-filename el-dir))
           (exists-exe-path (if (file-exists-p exe-path) exe-path)))
      exists-exe-path
    winsearch-adoquery-filename))

(defun winsearch-update-adoquery-path (&optional forced-p)
  "Setup `winsearch-adoquery-path' if not already defined."
  (when (or forced-p (null winsearch-adoquery-path))
    (setq winsearch-adoquery-path
          (winsearch-find-adoquery-path))))

(winsearch-update-adoquery-path)

;;;; Query String

;;;;; List of Patterns

(defun winsearch-split-pattern (pattern)
  ;; Tokenize pattern string
  ;;
  ;; " aaa  bbb  ccc " => ("aaa" "bbb" "ccc")
  ;; " \"aaa bbb\" " => ("\"aaa bbb\"")
  ;; " \"aaa bbb"    => ("\"aaa bbb\"")  ;;EndOfString => "
  ;;
  ;; Escape white space:
  ;; "aaa\\ bbb ccc" => ("aaa bbb" "ccc") ;;escape space in word
  ;; "\"aaa\\ bbb\" ccc" => ("\"aaa bbb\"" "ccc") ;;escape space in phrase
  ;; "\"aaa bbb\" ccc"   => ("\"aaa bbb\"" "ccc")
  ;;
  ;; Escape double quotation mark:
  ;; " aaa \\\"bbb ccc" => ("aaa" "\"bbb" "ccc")
  ;; " \"aaa \\\"bbb\" ccc" => ("\"aaa \"bbb\"" "ccc") ;;@todo
  ;;
  ;; WHITESPACE ::=   | \f | \t | \n | \r | \v
  ;; ESCAPESEQ ::= \ ( " | WHITESPACE )
  ;; quoted ::= " ( ESCAPESEQ | \ | [^\"] )* ( " | EOS )
  ;; word ::= ( ESCAPESEQ | \ | [^\ WHITESPACE] )*
  ;; spaces ::= WHITESPACE*
  ;; pattern ::= spaces ((quoted | word) spaces)*
  (let ((pos 0)
        (end (length pattern))
        (state 'spaces)
        (char-list nil)
        (args nil))
    (cl-flet ((start (next-state)
                     (setq state next-state))
              (add (char)
                   (push char char-list))
              (end-token ()
                         (push (apply #'string (nreverse char-list)) args)
                         (setq char-list nil)
                         (setq state 'spaces)))
      (while (< pos end)
        (let ((curr-char (elt pattern pos))
              (next-char (if (< (1+ pos) end) (elt pattern (1+ pos)))))
          (pcase state
            ;; Skip white spaces
            ('spaces
             (pcase curr-char
               ((or ?  ?\f ?\t ?\n ?\r ?\v) (cl-incf pos 1))
               (?\" (start 'quoted) (add curr-char) (cl-incf pos 1))
               (_ (start 'word))))

            ;; Quoted phrase
            ('quoted
             (pcase curr-char
               (?\" (add curr-char) (cl-incf pos 1) (end-token))
               (?\\
                (pcase next-char
                  ((or ?\" ?  ?\f ?\t ?\n ?\r ?\v) (add next-char) (cl-incf pos 2))
                  (_ (add curr-char) (cl-incf pos 1))))
               (_ (add curr-char) (cl-incf pos 1))))

            ;; Word
            ('word
             (pcase curr-char
               ((or ?  ?\f ?\t ?\n ?\r ?\v) (end-token))
               (?\\
                (pcase next-char
                  ((or ?\" ?  ?\f ?\t ?\n ?\r ?\v) (add next-char) (cl-incf pos 2))
                  (_ (add curr-char) (cl-incf pos 1))))
               (_ (add curr-char) (cl-incf pos 1)))))))

      ;; End Of String
      (pcase state
        ('quoted (add ?\") (end-token))
        ('word (end-token)))

      (nreverse args))))
;;TEST: (winsearch-split-pattern " aaa  bbb  ccc ") => ("aaa" "bbb" "ccc")
;;TEST: (winsearch-split-pattern " \"aaa bbb\" ") => ("\"aaa bbb\"")
;;TEST: (winsearch-split-pattern " \"aaa bbb")    => ("\"aaa bbb\"")  ;;EndOfString => "
;;
;; Escape white space:
;;TEST: (winsearch-split-pattern "aaa\\ bbb ccc") => ("aaa bbb" "ccc") ;;escape space in word
;;TEST: (winsearch-split-pattern "\"aaa\\ bbb\" ccc") => ("\"aaa bbb\"" "ccc") ;;escape space in phrase
;;TEST: (winsearch-split-pattern "\"aaa bbb\" ccc")   => ("\"aaa bbb\"" "ccc")
;;
;; Escape double quotation mark:
;;TEST: (winsearch-split-pattern " aaa \\\"bbb ccc") => ("aaa" "\"bbb" "ccc")
;;TEST: (winsearch-split-pattern " \"aaa \\\"bbb\" ccc") => ("\"aaa \"bbb\"" "ccc") ;;@todo

;;;;; Date Query

(defconst winsearch-date-query-regexp
  (let* ((date-regexp "\\(?:\\(today\\)\\|\\(?:\\([0-9][0-9][0-9][0-9]\\)\\(?:[/-]\\([0-9][0-9]?\\)\\(?:[/-]\\([0-9][0-9]?\\)\\)?\\)?\\)\\)")
         (unary-regexp (format "\\(\\(<\\|<=\\|=\\|>\\|>=\\)?%s\\)" date-regexp))
         (binary-regexp (format "\\(%s\\.\\.%s\\)" date-regexp date-regexp))
         (expr-regexp (format "\\(?:%s\\|%s\\) *$" unary-regexp binary-regexp)))
    (concat "^date:" expr-regexp)))

(defun winsearch-parse-date (word &optional use-current-match)
  "Parse date query string.

e.g.
 date:today
 date:2018-12-19    => ((\">=\" (enc 2018 12 19)) (\"<\" (enc 2018 12 20)))
 date:2018-12       => ((\">=\" (enc 2018 12 1)) (\"<\" (enc 2019 1 1)))
 date:2018          => ((\">=\" (enc 2018 1 1)) (\"<\" (enc 2019 1 1)))
 date:>=2018-12-19  => ((\">=\" (enc 2018 12 19)))
 date:>2018-12-19   => ((\">=\" (enc 2018 12 20)))
 date:<today
 date:2018-11-3..2019-12-23  => ((\">=\" (enc 2018 11 3)) (\"<\" (enc 2019 12 24)))
 date:2018-11..2019          => ((\">=\" (enc 2018 11 1)) (\"<\" (enc 2020 1 1)))
 date:2018-11..today

 NOTE: (enc y m d) = (encode-time 0 0 0 d m y)"

  (when (or use-current-match
            (string-match winsearch-date-query-regexp word))
    (let ((unary-expr (match-string 1 word))
          (op (match-string 2 word)) ;; < <= = > >= or nil
          (op-date (winsearch-date-query-match-date 3 word))
          (binary-expr (match-string 7 word))
          (start-date (winsearch-date-query-match-date 8 word))
          (end-date (winsearch-date-query-match-date 12 word)))
      (cond
       (unary-expr
        (let ((date-range (apply #'winsearch-make-date-range op-date)))
          (cond
           ((or (null op) (string= op "="))
            (list (cons ">=" (car date-range)) (cons "<" (cdr date-range))))
           ((member op '("<" ">="))
            (list (cons op (car date-range))))
           ((member op '(">" "<="))
            (list (cons op (cdr date-range)))))))
       (binary-expr
        (let ((start-date-range (apply #'winsearch-make-date-range start-date))
              (end-date-range (apply #'winsearch-make-date-range end-date)))
          (list (cons ">=" (car start-date-range)) (cons "<" (cdr end-date-range)))))))))
;; (winsearch-parse-date "date:today")
;;TEST: (winsearch-parse-date "date:2018-12-19") => ((">=" 23577 2928) ("<" 23578 23792))
;;TEST: (winsearch-parse-date "date:2018-12") => ((">=" 23553 20592) ("<" 23594 12016))
;;TEST: (winsearch-parse-date "date:2018") => ((">=" 23112 64368) ("<" 23594 12016))
;;TEST: (winsearch-parse-date "date:>=2018-12-19") => ((">=" 23577 2928))
;;TEST: (winsearch-parse-date "date:>2018-12-19") => ((">" 23578 23792))
;; (winsearch-parse-date "date:<today") => (("<" 25060 13040))
;;TEST: (winsearch-parse-date "date:2018-11-3..2019-12-23") => ((">=" 23516 26224) ("<" 24064 54896))
;;TEST: (winsearch-parse-date "date:2018-11..2019") => ((">=" 23513 50032) ("<" 24075 25200))
;;TEST: (winsearch-parse-date "date:2018-11..today") => ((">=" 23513 50032) ("<" 25061 33904))

(defun winsearch-date-query-match-date (n str)
  (list
   (match-string n str)
   (match-string (+ n 1) str)
   (match-string (+ n 2) str)
   (match-string (+ n 3) str)))

(defun winsearch-make-date-range (dayname year month day)
"Calculate lower & upper bound of date.

e.g.
 (nil 2018 10 19) => ((enc 2018 10 19) . (enc 2018 10 20))
 (nil 2018 10 nil) => ((enc 2018 10 1) . (enc 2018 11 1))
 (nil 2018 nil nil) => ((enc 2018 1 1) . (enc 2019 1 1))

 NOTE: (enc y m d) = (encode-time 0 0 0 d m y)"
  (when (stringp year) (setq year (string-to-number year)))
  (when (stringp month) (setq month (string-to-number month)))
  (when (stringp day) (setq day (string-to-number day)))

  (when (and (stringp dayname) (string= dayname "today"))
    (let ((now (decode-time (current-time))))
      (setq year (nth 5 now))
      (setq month (nth 4 now))
      (setq day (nth 3 now))))

  ;;@todo support to year ommit

  (let ((range
         (cond
          (day (list year month day  year month (1+ day)))
          (month (list year month 1  year (1+ month) 1))
          (year (list year 1 1  (1+ year) 1 1)))))
    (when range
      ;; convert to time & normalize date (2018/13/32 => (23635 3440))
      (cons
       (encode-time 0 0 0 (nth 2 range) (nth 1 range) (nth 0 range))
       (encode-time 0 0 0 (nth 5 range) (nth 4 range) (nth 3 range))))))

;;;;; Escape

(defun winsearch-escape-single-quote (value)
  "Replace ' with ''."
  ;; https://docs.microsoft.com/en-us/windows/desktop/search/-search-sql-literals
  (replace-regexp-in-string "'" "''" value nil t))

(defun winsearch-escape-like (value)
  "Replace % _ [ with [%] [_] [[]"
  ;; https://docs.microsoft.com/en-us/windows/desktop/search/-search-sql-like
  (replace-regexp-in-string "[%_[]" "[\\&]" value))

;;;;; SQL

(defconst winsearch-query-sql-map
  `(
    ;; https://docs.microsoft.com/ja-jp/windows/desktop/lwef/-search-2x-wds-aqsreference#properties-by-file-kind
    ;; ( regexp sql-generator )
    ("^\\(?:filename\\|file\\):\\(.+\\)" (lambda (word) (format "(System.FileName Like '%%%s%%')" (winsearch-escape-single-quote (winsearch-escape-like (match-string 1 word))))))
    ("^\\(?:fileext\\|ext\\):\\.?\\(.+\\)" (lambda (word) (format "(System.FileExtension = '.%s')" (winsearch-escape-single-quote (match-string 1 word)))))
    ("^kind:\\(.+\\)" (lambda (word) (format "(System.Kind = '%s')" (winsearch-escape-single-quote (match-string 1 word))))) ;;https://docs.microsoft.com/ja-jp/windows/desktop/properties/props-system-kind
    ("^author:\\(.+\\)" (lambda (word) (format "(System.Author = '%s')" (winsearch-escape-single-quote (match-string 1 word)))))
    ("^title:\\(.+\\)" (lambda (word) (format "(System.Title Like '%%%s%%')" (winsearch-escape-single-quote (winsearch-escape-like (match-string 1 word))))))
    ("^contents:\\(.+\\)" (lambda (word) (format "FREETEXT('%s')" (winsearch-escape-single-quote (match-string 1 word)))))
    ("^size:\\(<\\|<=\\|=\\|>\\|>=\\|!=\\)?\\([0-9]+\\)" (lambda (word) (format "(System.Size %s %s)" (or (match-string 1 word) "=") (match-string 2 word))))
    (,winsearch-date-query-regexp
     (lambda (word)
       (let ((date-conds (winsearch-parse-date word t)))
         (if date-conds
             (mapconcat (lambda (op-date) (format "(System.DateModified %s '%s')"
                                                  (car op-date)
                                                  (format-time-string "%Y-%m-%d %T" (cdr op-date) "UTC0")))
                        date-conds
                        " AND ")))))
    (t (lambda (word)
         (concat
          "("
          "(System.FileName Like '%" (winsearch-escape-single-quote (winsearch-escape-like word)) "%')"
          " OR "
          "(System.ItemFolderPathDisplay Like '%" (winsearch-escape-single-quote (winsearch-escape-like word)) "%')"
          " OR "
          "FREETEXT('" (winsearch-escape-single-quote word) "')"
          ")")))))

(defun winsearch-make-sql (pattern &optional scope)
  ;; https://docs.microsoft.com/en-us/windows/desktop/search/-search-sql-windowssearch-entry
  (let* ((words (winsearch-split-pattern pattern))
         (where
          (concat
           (if scope (format "SCOPE='file:%s' AND " scope)) ;;https://docs.microsoft.com/en-us/windows/desktop/search/-search-sql-folderdepth
           (mapconcat
            (lambda (word)
              (cl-loop for query-type in winsearch-query-sql-map
                       do (let ((regexp (nth 0 query-type))
                                (generator (nth 1 query-type)))
                            (when (or (eq regexp t)
                                      (string-match regexp word))
                              (cl-return (funcall generator word))))))
            words " AND ")
           )))

    (concat
     "SELECT"
     " TOP 100"
     ;;" System.ItemFolderPathDisplay, System.FileName" ;;%1% %2% (`Display' means localized folder name!)
     " System.ItemUrl" ;;%1%
     " FROM SystemIndex"
     " WHERE " where)))

;;;; Execute

(defun winsearch-make-command-args (pattern &optional scope)
  (list
   "/conn" "Provider=Search.CollatorDSO;Extended Properties='Application=Windows';" ;;Connection String
   "/format" "%1%" ;; file\directory="%1%\\\\%2%"
   "/header" "" ;; no header
   "/query" (winsearch-make-sql pattern scope)))

(defun winsearch-make-command-program-args (pattern &optional scope)
  (cons
   ;; Program name
   (or winsearch-adoquery-path
       (winsearch-find-adoquery-path))
   ;; Arguments
   (winsearch-make-command-args pattern scope)))

(defun winsearch-make-command-string (pattern &optional scope)
  (mapconcat
   #'shell-quote-argument
   (winsearch-make-command-program-args pattern scope)
   " "))

(defmacro winsearch-avoid-multibyte-issue (&rest body)
  `(let ((shell-file-name (or (executable-find "cmdproxy.exe")
                             shell-file-name)))
     ,@body))

(defconst winsearch-help-pattern-string
  "  word  \"multiple word\"
  file:<name>  ext:<str>  author:<name>  title:<name>  contents:<text>
  kind:picture|video|music|playlist|program|folder|link|...
  size:<op><number>    (<op>= > >= = < <= !=)
  date:<op>YYYY-MM-DD
  date:YYYY-MM-DD..YYYY-MM-DD
  date:today
")

;;;###autoload
(defun winsearch (pattern &optional scope)
  (interactive
   (list (read-string (concat "Search Patterns\n" winsearch-help-pattern-string ": "))))

  (winsearch-avoid-multibyte-issue
   (compilation-start
    (winsearch-make-command-string pattern scope)
    'winsearch-result-mode)))

(defconst winsearch-result-mode-regexp-alist
  `(("^file:\\(?1:\\(?:[a-zA-Z]:\\)?[^\n:]+\\)$" 1)))

(define-compilation-mode winsearch-result-mode "WinSearch"
  ""
  (setq-local compilation-error-face grep-hit-face)
  (setq-local compilation-error-regexp-alist
              winsearch-result-mode-regexp-alist)
  (setq-local compilation-mode-line-errors grep-mode-line-matches)
  ;; compilation-directory-matcher can't be nil, so we set it to a regexp that
  ;; can never match.
  (setq-local compilation-directory-matcher (list regexp-unmatchable))
  (setq-local compilation-process-setup-function 'grep-process-setup)
  (setq-local compilation-disable-input t)
  (setq-local compilation-error-screen-columns nil)

  (add-hook 'compilation-filter-hook 'grep-filter nil t))


(provide 'winsearch)
;;; winsearch.el ends here
