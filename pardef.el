;;; pardef.el --- A general definition parser        -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Lifoz

;; Author: Lifoz <lizgfn@outlook.com>
;; Keywords: convenience, tools

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

;; 

;;; Code:

(require 'cl-lib)

(defconst pardef--single-parameter-regexp
  (string-join '("^\\s-*\\(\\w+\\)"     ; parameter name
                 "\\(:\\s-*[^=]+\\|\\)" ; parameter type
                 "\\(=\\s-*.+\\|\\)$")  ; parameter default value
               "\\s-*")
  "Regexp which used to parse a single python parameter
specifier.  See `pardef--parse-python-parameter'")

(defun pardef--split-python-defun (definition)
  "Splitting python's function `DEFINITION' into list.
Returned list has three elements, which is representing function
name, parameter list and return specification respectively."
  (let ((before-parlist-regex "^def\\s-+\\(\\w+\\)\\s-*(")
        (after-parlist-regex ")\\s-*\\(->[^:]+:\\|:\\)$"))
    (when (string-match before-parlist-regex definition)
      (let ((fun-name (match-string-no-properties 1 definition))
            (parlist-begin (match-end 0)))
          (when (string-match after-parlist-regex definition)
            (let* ((return-spec (match-string-no-properties 1 definition))
                   (parlist-end (match-beginning 0))
                   (parlist (substring-no-properties definition parlist-begin
                                                     parlist-end)))
              (list fun-name parlist return-spec)))))))

(defun pardef--adjust-par-stack (stack cc openers closers)
  (cond ((and stack (char-equal cc (aref closers (cdar stack))))
         (cdr stack))
        ((string-match (regexp-opt-charset (list cc)) openers)
         (let ((rezidx (match-beginning 0)))
           (cons (cons (aref closers rezidx) rezidx)
                 stack)))
        (t stack)))

(defun pardef--find-next-outside-par
    (source char start &optional openers closers)
  " Find next `CHAR' in `SOURCE' outside any parentheses.  
The definition of 'parentheses' is in alist `PARALIST', which is
a ALIST consists of opening and closing."
  (setq openers (or openers (string ?\( ?\[ ?\{ ?\" ?\'))
        closers (or closers (string ?\) ?\] ?\} ?\" ?\')))
  (cl-do* ((stack nil)
           (idx start (+ 1 idx)))
      ((>= idx (length source)) nil)
    (if (and (null stack) (char-equal char (aref source idx)))
        (cl-return idx)
      (setq stack (pardef--adjust-par-stack stack (aref source idx)
                                            openers closers)))))

(defun pardef--parse-python-parameter (parelt)
  "Parsing a single python-style parameter specifier.
If success to parsing, function returns a list has three
elements: parameter's name, parameter's type and parameter's
default value. Otherwise a string which is indicating a error
will be throwed with tag `pardef--parsing-par-err'."
  (if (not (string-match pardef--single-parameter-regexp parelt))
      (throw 'pardef--parsing-par-err
             (format "Unable to parse parameter %s" parelt))
    (let ((name (match-string 1 parelt))
          (type (match-string 2 parelt))
          (value (match-string 3 parelt)))
      (when (string-empty-p name)
        (throw 'pardef--parsing-par-err
               (format "Unable to parse parameter's name in '%s'" parelt)))
      (cond ((string-match "^\\s-*:\\s-*\\([[:graph:]].*?\\)\\s-*$" type)
             (setq type (match-string 1 type)))
            ((string-match "^\\s-*$" type) (setq type ""))
            (t (throw 'pardef--parsing-par-err
                      (format "Unable to parse type specifier '%s'" type))))
      (cond ((string-match "^\\s-*=\\s-*\\([[:graph:]].*?\\)\\s-*$" value)
             (setq value (match-string 1 value)))
            ((string-match "^\\s-*$" value) (setq value ""))
            (t (throw 'pardef--parsing-par-err
                      (format "Unable to parse default value '%s'" value))))
      (list name type value))))

(defun pardef--parse-python-parlist (parlist)
  "Parsing python-style function's parameter list(`PARLIST').
Returns the result as a list, whose each element is a list has 3
elements, each component corresponds name, type and default
value.  First element(i.e. name) will be a non-empty string, but
both type and value can be empty.  If `PARLIST' is empty or only
contains white-space, empty list(i.e. nil) will be returned.  As
a result, we use STRING to indicate a parse error: if invocation
returns a string, it's mean there is something wrong, and the
STRING contains a short message about the reason."
  (catch 'pardef--parsing-par-err       ; See `pardef--parse-python-parameter'
    (let ((rezseq nil)
          (commap 0)
          (commac (pardef--find-next-outside-par parlist ?\, 0)))
      (let ((forward!
             (lambda ()
               (let* ((par (substring-no-properties parlist commap commac))
                      (parserez (pardef--parse-python-parameter par)))
                 (push parserez rezseq)
                 (when commac
                   (setq commap (+ 1 commac)
                         commac (pardef--find-next-outside-par
                                 parlist ?\, commap)))))))
        (while commac (funcall forward!))
        (funcall forward!))
      (nreverse rezseq))))

(provide 'pardef)
;;; pardef.el ends here
