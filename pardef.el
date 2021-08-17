;;; pardef.el --- A general definition parser        -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Lifoz

;; Author: Lifoz <li-fn@outlook.com>
;; Keywords: convenience, generator, Python, docstring
;; Package-Requires: ((dash "2.19.0"))

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
(require 'dash)

(defgroup pardef nil
  "Python defun's docstring generator."
  :group 'programming
  :group 'python)

(defcustom pardef-docstring-style "'''"
  "Python docstring's style, use single quotes or double quotes."
  :group 'pardef
  :type '(radio (const :tag "Single quotes" "'''")
                (const :tag "Double quotes" "\"\"\"")))

(defconst pardef--single-parameter-regexp
  (string-join '("^\\s-*\\(\\*\\{0,2\\}[a-zA-Z_]+\\)" ; parameter name
                 "\\(:\\s-*[^=]+\\|\\)"         ; parameter type
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
  (let ((before-parlist-regex "^def\\s-+\\([a-zA-Z_]+\\)\\s-*(")
        (after-parlist-regex ")\\s-*\\(->[^:]+:\\|:\\)$")
        (exception-msg (format "Unable to parse python-defun %s" definition)))
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

(defun pardef--load-docstring ()
  "Try to load docstring after current point.
This function assume that there is a correct function definition,
and current point is after the terminate notation (:). In
particular, when this function is called, the buffer looks like:

--- BUFFER ---
def __init__(self):
                   ^
---   END  ---
(Character (^) means current point)

Both of single-quotes-style and double-quotes-style are
accept. If docstring is found, multiple values formed (<content>
<begin-point> <end-point>) will be returned, content is a list of
string, each element is a line of docstring and prefix indent has
been trimmed. Otherwise, namely no docstring is found, function
returns nil."
  (save-excursion
    (skip-chars-forward "\n[:space:]")
    (when (looking-at "'''\\|\"\"\"")
      (let ((begin (point))
            (indent (current-indentation))
            (quotes (match-string-no-properties 0)))
        (forward-char (length quotes))
        (re-search-forward quotes)
        (let* ((docstring (buffer-substring-no-properties begin (point)))
               (indent-regexp (format "\n[ ]\\{,%d\\}" indent))
               (lines (split-string docstring indent-regexp)))
          (cl-values lines begin (point)))))))

(defmacro pardef--user-error (format &rest args)
  (let ((tagged-format (concat "[PARDEF] " format)))
    `(user-error ,tagged-format ,@args)))

(defun pardef-gen (renderer)
  "Generate docstring for python-style defun.

To generate docstring for a function, place cursor on the line
contains keyword `def', then call this function with a particular
`RENDERER'. Note that this function is not `interactive', so you
should wrap it in `lambda' or use `pardef-make-gen' in key
binding.

`RENDERER' is a callback who can produce or update a docstring by
a parsed function definition. It should be a function accepts a
`ALIST' as parameter, and returns a specifier which `pardef-gen'
can use to generate docstring. In particular, `RENDERER' will
receive a `ALIST' has following fields:

  `name'   Function's name, as a non-empty string.
  `return' Function's return type, is a string and may be empty.
  `params' Parameter list, a list of list in lisp data.

Parameter list is represented by list which consists of fix form:

  (list <param-name> <param-type> <param-default-value>)

Both param-type and param-default-value may be empty. You can use
`cl-multiple-value-bind' to destructuralize them.

`RENDERER' should return a list has three elements, first one is
a list of string, whose each element specifies a single line of
docstring. You don't need consider the absolute indentation of
them, and local indent is acceptable. For example, part of the
list may like

'(\":param length:\"
  \"  The length of the rectangle.\")

Both of the rest return values are non-negative integer represent
row index and column index, used to specify location of cursor
after docstring is inserted into buffer. Both of them are based
on zero, and similarly, don't need to consider the absolute
indent. Consider that we want to generate following docstring:

--- DOCSTRING ---
'''This is my favorite function.
You should call it carefully.
                ^
'''
---    END    ---
(Character '^' means cursor)

Then we should return

(list '(\"'''This is my favorite function.\" ; 0
        \"You should call it carefully.\"    ; 1
        ; 0               16
        \"'''\")
      1
      16)

Finally, you can use custom variable `pardef-docstring-style' as
docstring's quotes. If docstring already exists, `pardef-gen'
will update it automatically."
  (cl-multiple-value-bind (line first-lino last-lino)
      (pardef-python-current-line)
    (if (not (string-match "^\\s-*\\(def\\)" line)) ; `def' must in current line
        (pardef--user-error "Unable to parse Current line as a python defun")
      (let* ((defi-begin (match-beginning 1))
             (defi-end (pardef--find-next-outside-par line ?\: defi-begin)))
        (when (null defi-end)
          (pardef--user-error "Can't find this def's end(i.e. character ':')"))
        (cl-incf defi-end)
        (let* ((definition (substring-no-properties line defi-begin defi-end))
               (parez (pardef-parse-python-defun definition)))
          (when (stringp parez) (pardef--user-error "%s" parez))
          (beginning-of-line)
          (forward-char (+ defi-end (* 2 (- last-lino first-lino 1))))
          ;; Now cursor is following colon
          (let ((curdoc (pardef--load-docstring)))
            (cl-multiple-value-bind (lines row col)
                (funcall renderer parez (and curdoc (cl-first curdoc)))
              (unless (and (< row (length lines))
                           (< col (length (nth row lines))))
                (pardef--user-error
                 "Renderer returned invalid location (%d, %d)" row col))
              (when curdoc (delete-region (point) (cl-third curdoc)))
              (newline-and-indent)
              (let* ((indent-level (current-indentation))
                     (indent (make-string indent-level ?\ ))
                     (docstring (string-join lines (concat "\n" indent))))
                (save-excursion
                  (insert docstring))
                (beginning-of-line)
                (forward-line row)
                (forward-char (+ indent-level col))))))))))

(defun pardef-make-gen (renderer)
  "Create a generator function with `RENDERER'.
Creating a non-parameter function which will call
(pardef-gen `RENDERER') `interactive'-ly. It can be used to
define key bindings in your init.el:

  (define-key xx-mode-map (kbd ..) (pardef-make-gen `RENDERER'))

See `pardef-gen' for more details."
  (lambda () (interactive)
    (pardef-gen renderer)))


(defun pardef--render-brief (alist)
  (list (format "%s[TODO] Document %s"
                pardef-docstring-style
                (assoc-default 'name alist))))

(defun pardef--render-param-before (alist) nil)
(defun pardef--render-param-after (alist)
  (list (format "  :returns: %s" (assoc-default 'return alist))))

(defun pardef--render-param-list (alist)
  (let ((cvt-param (lambda (param) (format "  :param %s:" (car param)))))
    (mapcar cvt-param (assoc-default 'params alist))))

(defun pardef--render-rest (alist) (list "" pardef-docstring-style))

(defun pardef-util-split-docstring-blocks (docstring)
  "Split `DOCSTRING' by blank line.
`DOCSTRING' should be a list of string which represent lines of
text, and it will be split into sublists bounded by blank lines."
  (--split-when (string-blank-p it) docstring))

(defun pardef--sru-info-cons (line)
  (when (string-match "^  :param \\([A-Za-z0-9_]+\\):\\(.*\\)" line)
    (cons (match-string-no-properties 1 line)
          (match-string-no-properties 2 line))))

(defun pardef--sru-make-paramline (param-spec doc-alist)
  (let* ((name (car param-spec))
         (doc (or (assoc-default name doc-alist 'string-equal)
                  (string))))
    (format "  :param %s:%s" name doc)))

(defun pardef--simple-renderer-update-param (alist blocks)
  (let* ((param-block (cl-second blocks))
         (doc-alist (mapcar #'pardef--sru-info-cons param-block))
         (doc-alist (-remove #'null doc-alist))
         (paralines (--map (pardef--sru-make-paramline it doc-alist)
                           (assoc-default 'params alist)))
         (returns (pardef--render-param-after alist)))
    (->> (-replace-at 1 (append paralines returns) blocks)
         (-interpose (string))
         (-flatten))))

(defun pardef--simple-renderer-update (alist docstring)
  (let ((blocks (pardef-util-split-docstring-blocks docstring)))
    (cl-values (if (< (length blocks) 2)
                   (append (or blocks (string))
                           (string)
                           (pardef--render-param-before alist)
                           (pardef--render-param-list alist)
                           (pardef--render-param-after alist)
                           (string)
                           (pardef--render-rest alist))
                 (pardef--simple-renderer-update-param alist blocks))
               0 (length pardef-docstring-style))))

(defun pardef-simple-renderer (alist docstring)
  "Rendering `ALIST' into python-style docstring.
Returns a list of strings, each string is a unindented line."
  (if docstring
      (pardef--simple-renderer-update alist docstring)
    (let ((new-line (list "")))
      (cl-values (append (pardef--render-brief alist)
                         new-line
                         (pardef--render-param-before alist)
                         (pardef--render-param-list alist)
                         (pardef--render-param-after alist)            
                         new-line
                         (pardef--render-rest alist))
                 0 (length pardef-docstring-style)))))

(provide 'pardef)
;;; pardef.el ends here
