;;; pardef.el --- A general definition parser        -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Lifoz

;; Author: Lifoz <li-fn@outlook.com>
;; Keywords: convenience, generator, Python, docstring

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
  (string-join '("^\\s-*\\(\\*\\{0,2\\}\\w+\\)" ; parameter name
                 "\\(:\\s-*[^=]+\\|\\)"        ; parameter type
                 "\\(=\\s-*.+\\|\\)$")  ; parameter default value
               "\\s-*")
  "Regexp which used to parse a single python parameter
specifier.  See `pardef--parse-python-parameter'")

(defun pardef--split-python-defun (definition)
  "Splitting python's function `DEFINITION' into list.
Returned a list has three elements if succeed in parsing,
elements are bound to function's name, parameter list and
return-specification respectively. Otherwise, namely fail to
parse, function will raise an exception with tag
`pardef--unable-to-split'."
  (let ((before-parlist-regex "^def\\s-+\\(\\w+\\)\\s-*(")
        (after-parlist-regex ")\\s-*\\(->[^:]+:\\|:\\)$")
        (exception-msg (format "Unable to parse python-def %s" definition)))
    (unless (string-match before-parlist-regex definition)
      (throw 'pardef--unable-to-split exception-msg))
    (let ((fun-name (match-string-no-properties 1 definition))
          (parlist-begin (match-end 0)))
      (unless (string-match after-parlist-regex definition)
        (throw 'pardef--unable-to-split exception-msg))
      (let* ((return-spec (match-string-no-properties 1 definition))
             (parlist-end (match-beginning 0))
             (parlist (substring-no-properties definition parlist-begin
                                               parlist-end)))
        (cl-values fun-name parlist return-spec)))))

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
  "Find next `CHAR' in `SOURCE' outside any parentheses.  
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

(defun pardef--parse-python-parameter-split-regex (parelt)
  (if (not (string-match pardef--single-parameter-regexp parelt))
      (throw 'pardef--parsing-par-err
             (format "Unable to parse parameter %s" parelt))
    (cl-values (match-string-no-properties 1 parelt)
               (match-string-no-properties 2 parelt)
               (match-string-no-properties 3 parelt))))
  

(defun pardef--parse-python-parameter (parelt)
  "Parsing a single python-style parameter specifier.
If success to parsing, function returns a list has three
elements: parameter's name, parameter's type and parameter's
default value. Otherwise a string which is indicating a error
will be throwed with tag `pardef--parsing-par-err'."
  (cl-multiple-value-bind (name type value)
      (pardef--parse-python-parameter-split-regex parelt)
    (when (string-empty-p name)
      (throw 'pardef--parsing-par-err
             (format "Unable to parse parameter's name in '%s'" parelt)))
    (cond ((string-match "^\\s-*:\\s-*\\([[:graph:]].*?\\)\\s-*$" type)
           (setq type (match-string-no-properties 1 type)))
          ((string-match "^\\s-*$" type) (setq type ""))
          (t (throw 'pardef--parsing-par-err
                    (format "Unable to parse type specifier '%s'" type))))
    (cond ((string-match "^\\s-*=\\s-*\\([[:graph:]].*?\\)\\s-*$" value)
           (setq value (match-string-no-properties 1 value)))
          ((string-match "^\\s-*$" value) (setq value ""))
          (t (throw 'pardef--parsing-par-err
                    (format "Unable to parse default value '%s'" value))))
    (cl-values name type value)))

(defun pardef--parse-python-parlist (parlist)
  "Parsing python-style function's parameter list(`PARLIST').
Returns the result as a list, whose each element is a list has 3
elements, each component corresponds name, type and default
value.  First element(i.e. name) will be a non-empty string, but
both type and value can be empty.  If `PARLIST' is empty or only
contains white-space, empty list(i.e. nil) will be returned.  If
`PARLIST' has illegal form, or contains some features are
unsupporting currently, function will raise a exception contains
a short message which indicates the reason of failure with tag
`pardef--parsing-par-err'"
  (let ((rezseq nil)
        (commap 0)
        (commac (pardef--find-next-outside-par parlist ?\, 0)))
    (let ((forward!
           (lambda ()
             (let ((par (substring-no-properties parlist commap commac)))
               (unless (string-match "^\\s-*\\(?:\\*\\|/\\)\\s-*$" par)
                 (let ((parserez (pardef--parse-python-parameter par)))
                   (push parserez rezseq)))
               (when commac
                 (setq commap (+ 1 commac)
                       commac (pardef--find-next-outside-par
                               parlist ?\, commap)))))))
      (while commac (funcall forward!))
      (funcall forward!))
    (nreverse rezseq)))

(defun pardef--trim-python-defun-retype (ret)
  "Trim `RET'-'s prefix (->)."
  (if (string-match "^\\s-*\\(?:->\\s-*\\(.+\\)\\|\\(\\)\\)\\s-*:\\s-*$" ret)
      (or (match-string-no-properties 1 ret) "")
    (user-error "[PARDEF] Internal error raised when parsing %s" ret)))

(defun pardef-parse-python-defun (definition)
  "Parsing python-style function `DEFINITION'.
Spliting and extracting `DEFINITION' to a `alist', which has field

  - `name'   A non-empty string represent function's name.
  - `params' List represent Function's parameters.
  - `return' A string may be empty, represent function's return
    type (i.e. -> ..)

Field `params' is a list of list, each element is string and has
form:

  (`param-name' `param-type' `param-defaule-value')

And in these three components, only `param-name' always has
positive length, both `param-type' and `param-default-value' may
be empty if doesn't provide in `DEFINITION'.  
If `DEFINITION' are illegal formed or contains any features which
are unsupporting now, a string contains a short message about the
reason of failure will be returned."
  (catch 'pardef--unable-to-split
    (catch 'pardef--parsing-par-err
      (cl-multiple-value-bind (name parlist ret)
          (pardef--split-python-defun definition)
        (list (cons 'name name)
              (cons 'params (pardef--parse-python-parlist parlist))
              (cons 'return (pardef--trim-python-defun-retype ret)))))))



(defun pardef--line-at-point ()
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

(defun pardef-python-current-line ()
  "Get line at point in current buffer as a string.
This function accepts using backslash at end of line to continue
this line to next. It's return three value: text of current
line(backslashs are trimmed), line number of the first line and
the last line. For example, if current line number is x and
doesn't use backslash, then return (list <current-line> x (+ x 1))"
  (save-excursion
    (cl-do ((lines nil)
            (first-lino (line-number-at-pos))
            (last-lino (line-number-at-pos) (line-number-at-pos))
            (prev-char ?\\ last-char)
            (last-char (char-before (line-end-position))
                       (char-before (line-end-position))))
        ((not (char-equal ?\\ prev-char))
         (cl-values (string-join (nreverse lines)) first-lino last-lino))
      (let* ((line (pardef--line-at-point))
             (line* (if (string-match "\\\\$" line)
                        (substring-no-properties line 0 (match-beginning 0))
                      line)))
        (push line* lines)
        (forward-line)))))


(provide 'pardef)
;;; pardef.el ends here
