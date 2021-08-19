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

(defcustom pardef-renderer-sphinx-list-indent 0
  "Indent of list items in Sphinx-formated documentation.
It's used to specify indentations of parameter, return-type, etc.
For example, it defaults zero, which means no indentations:

'''[Summary]

:param x: ...
^
Prefixed empty string.
'''

If you want two spaces to indent param-specifier, `setq' it to 2:

'''[Summary]

  :param x: ...
^
Two spaces prefixed
'''

Prefixes are only added before parameter (and type) and
return (and type) specifiers, and it become operative when
`pardef-gen' is using `pardef-render-sphinx' renderer."
  :group 'pardef
  :type 'integer)

(defcustom pardef-renderer-sphinx-add-defaults t
  " Whether or not add a ', defaults to xx' for parameters who
has default values."
  :group 'pardef
  :type 'boolean)

(defconst pardef--ignored-param-field-regexp
  "^\\s-*\\(?:\\*\\|/\\|self\\)\\s-*$"
  "Regexp used to filter ignorable parameter fields.")

(defconst pardef--python-parameter-name
  "\\*\\{0,2\\}[a-zA-Z0-9_]+"
  "Parameter's identifier, includes *args and **args")

(defconst pardef--single-parameter-regexp
  (string-join `(,(format "^\\s-*\\(%s\\)" pardef--python-parameter-name)
                 "\\(:\\s-*[^=]+\\|\\)" ; parameter type
                 "\\(=\\s-*.+\\|\\)$")  ; parameter default value
               "\\s-*")
  "Regexp which used to parse a single python parameter
specifier.  See `pardef--parse-python-parameter'")

(defun pardef--split-python-defun (definition)
  "Splitting python's function `DEFINITION' into list.
Returned a list has three elements if succeed in parsing,
elements are bound to function's name, parameter list and
return-specification respectively.  Otherwise, namely fail to
parse, function will raise an exception with tag
`pardef--unable-to-split'."
  (let ((before-parlist-regex "^def\\s-+\\([a-zA-Z0-9_]+\\)\\s-*(")
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
  "Adjust stack to maintains balance of `OPENERS' and `CLOSERS'.

Stack is a list, but you may not modify it manually.  `OPENERS'
and `CLOSERS' are matched parentheses, such as () [] '' etc. and
both of them are string, whose index a corresponds in match to
another.  You can scan a string and pass a originally nil list to
this function, and reset stack to its return value. You can be
sure that any `OPENERS' in your source string are matched to
`CLOSERS' if stack is still nil any time."
  (cond ((and stack (char-equal cc (aref closers (car stack))))
         (cdr stack))
        ((string-match (regexp-opt-charset (list cc)) openers)
         (let ((rezidx (match-beginning 0)))
           (cons rezidx stack)))
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
  "Parsing a single python-style parameter specifier `PARELT'.
If success to parsing, function returns a list has three
elements: parameter's name, parameter's type and parameter's
default value.  Otherwise a string which is indicating a error
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
unsupported currently, function will raise a exception contains
a short message which indicates the reason of failure with tag
`pardef--parsing-par-err'"
  (unless (string-match "^\\s-*$" parlist)
    (let ((rezseq nil)
          (commap 0)
          (commac (pardef--find-next-outside-par parlist ?\, 0)))
      (let ((forward!
             (lambda ()
               (let ((par (substring-no-properties parlist commap commac)))
                 (unless (string-match pardef--ignored-param-field-regexp par)
                   (let ((parserez (pardef--parse-python-parameter par)))
                     (push parserez rezseq)))
                 (when commac
                   (setq commap (+ 1 commac)
                         commac (pardef--find-next-outside-par
                                 parlist ?\, commap)))))))
        (while commac (funcall forward!))
        (funcall forward!))
      (nreverse rezseq))))


(defun pardef--trim-python-defun-retype (ret)
  "Trim `RET'-'s prefix (->)."
  (if (string-match "^\\s-*\\(?:->\\s-*\\(.+\\)\\|\\(\\)\\)\\s-*:\\s-*$" ret)
      (or (match-string-no-properties 1 ret) "")
    (user-error "[PARDEF] Internal error raised when parsing %s" ret)))


(defun pardef-load-python-defun (definition)
  "Parsing python-style function `DEFINITION'.
Splitting and extracting `DEFINITION' to a `alist', which has field

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
are unsupported now, a string contains a short message about the
reason of failure will be returned."
  (catch 'pardef--unable-to-split
    (catch 'pardef--parsing-par-err
      (cl-multiple-value-bind (name parlist ret)
          (pardef--split-python-defun definition)
        (list (cons 'name name)
              (cons 'params (pardef--parse-python-parlist parlist))
              (cons 'return (pardef--trim-python-defun-retype ret)))))))


(defun pardef-load-python-line ()
  "Get line at point in current buffer as a string.
This function read a python-style line at point.  In detail, all following code blocks it's a single python line:

def f(x):

def g(x: int) \\
  -> Tuple[int, int]:

def h(x     # www!
,     y):

It's return four value. 

  (line-content begin-line-number end-line-number ignored-count)

LINE-CONTENT is text of current python line.  Although a python
line may be number of normal line, this function returns a single
string, which is trimmed backslash and newline at end of line,
and inline comment lead by hash.

BEGIN-LINE-NUMBER and END-LINE-NUMBER are line numbers of the
first line and the last line.  For example, if current line
number is x and doesn't use backslash, then returns

  (x (+ x 1))

Finally, IGNORED-COUNT means the number of chars that ignored
when concat lines into single."
  (save-excursion
    (let* ((lines nil)
           (stack nil)
           (continue t)
           (ignored-count 0)
           (first-lino (line-number-at-pos))
           (last-lino (line-number-at-pos))
           (openers (string ?\( ?\[ ?\{))
           (closers (string ?\) ?\] ?\}))
           (is-balance (lambda (string)
                         (dotimes (i (length string))
                           (setq stack (pardef--adjust-par-stack
                                        stack (aref string i)
                                        openers closers)))
                         (null stack))))
      (while continue
        (let ((curl (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position))))
          (if (string-match "^\\([^#]*\\)\\(#.*\\)" curl)
              (let ((content (match-string-no-properties 1 curl))
                    (comment (match-string-no-properties 2 curl)))
                (push content lines)
                (if (funcall is-balance content)
                    (setq continue nil)
                  (cl-incf ignored-count (+ 1 (length comment)))))
            (let ((len (length curl)))
              (cond ((char-equal ?\\ (aref curl (- len 1)))
                     (funcall is-balance curl)
                     (cl-incf ignored-count 2)
                     (push (substring-no-properties curl 0 (- len 1)) lines))
                    (t (push curl lines)
                       (if (funcall is-balance curl)
                           (setq continue nil)
                         (cl-incf ignored-count)))))))
        (forward-line)
        (setq last-lino (line-number-at-pos))
        (when (eobp) (setq continue nil)))
      (-> (nreverse lines)
          string-join
          (cl-values first-lino last-lino ignored-count)))))


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


;; FIXME
;; This function assume that there is no more continued line after the
;; end of defun. So in this case:
;;
;;   def fix_u(who): return \
;;                   who
;;
;; `pardef-gen' will fail.
(defun pardef-gen (renderer)
  "Generate docstring for python-style defun.

To generate docstring for a function, place cursor on the line
contains keyword `def', then call this function with a particular
`RENDERER'.  Note that this function is not `interactive', so you
should wrap it in `lambda' or use `pardef-make-gen' in key
binding.

`RENDERER' is a callback who can produce or update a docstring by
a parsed function definition.  It should be a function accepts a
`ALIST' as parameter, and returns a specifier which `pardef-gen'
can use to generate docstring.  In particular, `RENDERER' will
receive a `ALIST' has following fields:

  `name'   Function's name, as a non-empty string.
  `return' Function's return type, is a string and may be empty.
  `params' Parameter list, a list of list in Lisp data.

Parameter list is represented by list which consists of fix form:

  (list <param-name> <param-type> <param-default-value>)

Both param-type and param-default-value may be empty.  You can use
`cl-multiple-value-bind' to destructuralize them.

`RENDERER' should return a list has three elements, first one is
a list of string, whose each element specifies a single line of
docstring.  You don't need consider the absolute indentation of
them, and local indent is acceptable.  For example, part of the
list may like

'(\":param length:\"
  \"  The length of the rectangle.\")

Both of the rest return values are non-negative integer represent
row index and column index, used to specify location of cursor
after docstring is inserted into buffer.  Both of them are based
on zero, and similarly, don't need to consider the absolute
indent.  Consider that we want to generate following docstring:

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
  (cl-multiple-value-bind (line first-lino last-lino ignored-count)
      (pardef-load-python-line)
    (if (not (string-match "^\\s-*\\(def\\)" line)) ; `def' must in current line
        (pardef--user-error "Unable to parse Current line as a python defun")
      (let* ((defi-begin (match-beginning 1))
             (defi-end (pardef--find-next-outside-par line ?\: defi-begin)))
        (when (null defi-end)
          (pardef--user-error "Can't find this def's end(i.e. character ':')"))
        (cl-incf defi-end)
        (let* ((definition (substring-no-properties line defi-begin defi-end))
               (parez (pardef-load-python-defun definition)))
          (when (stringp parez) (pardef--user-error "%s" parez))
          (beginning-of-line)
          (forward-char (+ defi-end ignored-count))
          (let ((curdoc (pardef--load-docstring)))
            (cl-multiple-value-bind (lines row col)
                (funcall renderer parez (and curdoc (cl-first curdoc)))
              (unless (and (< row (length lines))
                           (<= col (length (nth row lines))))
                (pardef--user-error
                 "Renderer returned invalid location (%d, %d)" row col))
              (when curdoc (delete-region (point) (cl-third curdoc)))
              (newline-and-indent)
              (let* ((indent-level (current-indentation))
                     (indent (make-string indent-level ?\ ))
                     (docstring (string-join lines (concat "\n" indent))))
                (save-excursion
                  (insert docstring)
                  (unless (looking-at "\\s-*$")
                    (newline-and-indent)))
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


(defun pardef-util-split-docstring-blocks (docstring)
  "Split `DOCSTRING' by blank line.
`DOCSTRING' should be a list of string which represent lines of
text, and it will be split into sublists bounded by blank lines."
  (--split-when (string-blank-p it) docstring))


(defun pardef-util-indent-of (string)
  "Returns number of space prefixed in `STRING'."
  (if (not (string-match "^\\s-*" string)) 0
    (match-end 0)))


;; A simple renderer, uses Sphinx docstring format.
;; See https://sphinx-rtd-tutorial.readthedocs.io/en/latest/docstrings.html
;; for more information about the standard.

(defconst pardef--rsph-destructing-regexp
  (string-join `("^\\s-*"
                 ,(format "\\([a-zA-Z0-9_]+\\)\\(\\|\\s-+%s\\)"
                          pardef--python-parameter-name)
                 "\\(.*\\)$")
               (string ?\:))
  "Parameter specifier line's regexp.
See `pardef--rsph-destruct-line'.")

(defconst pardef--rsph-keywords
  (regexp-opt '("param" "type" "return" "rtype"))
  "Sphinx document's keywords.
See https://sphinx-rtd-tutorial.readthedocs.io/en/latest/docstrings.html
for more details.")


(defun pardef--rsph-format (string &rest objects)
  (concat (make-string pardef-renderer-sphinx-list-indent ?\ )
          (apply 'format string objects)))


(defun pardef--rsph-create-params (param-alist doc-alist type-alist)
  "Creating parameter specifier lines from `PARAM-ALIST'.
`PARAM-ALIST' is the primitive alist returned by
`pardef-load-python-defun', both of `DOC-ALIST' and `TYPE-ALIST'
is alist and has form:

  ((\"function_name\": . (\"user document content\"))
  ...)

Lines of parameter specifier will be returned, however, note that
THE FORM OF return value may be nested list, it may be flattened
by `-flatten' before used."
  (let ((build-param-line
         (lambda (param-spec)
           (let ((result nil))
             (cl-multiple-value-bind (name type value) param-spec
               (let* ((nodefault (not pardef-renderer-sphinx-add-defaults))
                      (doc (or (assoc-default name doc-alist 'string-equal)
                               (format " [ParamDescription]%s"
                                       (if (or nodefault (string-blank-p value))
                                           (string)
                                         (format ", defaults to %s" value)))))
                      (doc (if (listp doc) doc (list doc)))
                      (rest (cl-rest doc))
                      (first (pardef--rsph-format
                              ":param %s:%s" name (cl-first doc))))
                 (push (cons first rest) result))
               (unless (string-blank-p type)
                 (let* ((default (format " %s" type))
                        (doc (or (assoc-default name type-alist 'string-equal)
                                 (list (string))))
                        (rest (cl-rest doc))
                        (first (pardef--rsph-format
                                ":type %s: %s%s" name type (cl-first doc))))
                   (push (cons first rest) result)))
               (nreverse result))))))
    (mapcar build-param-line (assoc-default 'params param-alist))))


(defun pardef--rsph-create-params-and-return
    (alist &optional doc-alist type-alist raises-list)
  "Combine parameter generate and return generate.
See `pardef--rsph-create-params' for more details about
arguments. Special key for only `DOC-ALIST' is \"return\", which
used to specify documentation of return value. `RAISES-LIST' is a
list of lines which are between param-specifiers and
return-specifiers. It will be insert between that two directly."
  (-flatten (list (pardef--rsph-create-params alist doc-alist type-alist)
                  raises-list
                  (let* ((doc (or (assoc-default "return" doc-alist 'string=)
                                  '(" [ReturnDescription]")))
                         (rest (cl-rest doc))
                         (first (cl-first doc)))
                    (cons (pardef--rsph-format ":return:%s" first) rest))
                  (let* ((rtype (assoc-default 'return alist))
                         (doc (or (assoc-default "return" type-alist 'string=)
                                  '("")))
                         (rest (cl-rest doc))
                         (first (cl-first doc)))
                    (unless (string-blank-p rtype)
                      (cons (pardef--rsph-format ":rtype: %s%s" rtype first)
                            rest))))))


(defun pardef--rsph-create (alist)
  (let ((blank-line (string)))
    (-flatten (list (format "%s[Summary]" pardef-docstring-style)
                    blank-line
                    (pardef--rsph-create-params-and-return alist nil nil)
                    pardef-docstring-style))))


(defun pardef--rsph-group-lines (param-block)
  "Returns a list of list from `PARAM-BLOCK'.
A line will be considered belong to previous line if its
indentation is greater than previous line.  Lines are grouped
directly, will not do any other modify."
  (let* ((result nil)
         (prev-ind #x1000)
         (prev-lines nil)
         (dump (lambda ()
                 (when prev-lines (push (nreverse prev-lines) result)))))
    (dolist (line param-block)
      (let ((ind (pardef-util-indent-of line)))
        (cond ((<= ind prev-ind)
               (funcall dump)
               (setq prev-ind ind
                     prev-lines (list line)))
              (t (push line prev-lines)))))
    (funcall dump)
    (nreverse result)))


(defun pardef--rsph-destruct-line (grouped-line)
  "Returns destructed `GROUPED-LINE'.
If succeed in destructing, return multiple value

  (type name rest)
      1    2    3

    :param x: My private x.
     ^     ^ ^
     1     2 3

First two components are string, REST is a list of string.  Both name and rest may be empty in this case:

  \"  :return:\"

If failed to parse, NIL will be returned.
See `pardef--rsph-group-lines' for more details about
`GROUPED-LINE'"
  (unless (null grouped-line)
    (let ((first-line (cl-first grouped-line)))
      (when (string-match pardef--rsph-destructing-regexp first-line)
        (cl-multiple-value-bind (type-spec name-spec rest-spec)
            (--map (match-string-no-properties it first-line) '(1 2 3))
          (cl-values type-spec
                     (string-trim-left (or name-spec (string)))
                     (cons (or rest-spec (string))
                           (cdr grouped-line))))))))


(defun pardef--rsph-rebuild-line (limbs &optional prefix)
  "Rebuild line from `LIMBS' and perfixed with `PREFIX'.
Returns lines as a list.

See `pardef--rsph-destruct-line' for more details."
  (setq prefix (or prefix (string)))
  (cl-multiple-value-bind (type name rest) limbs
      (let ((spec (format "%s:%s %s:" prefix type name))
            (first-rest (cl-first rest))
            (rest-rest (cdr rest)))
        (cons (concat spec first-rest)
              rest-rest))))


(defun pardef--rsph-collect-docs (compiled)
  "Collect existed documents from `SPECS-BLOCK'.
Returns multiple value DOC-ALIST and TYPE-ALIST and both of them
are formed like:

  ((\"param-name\" . (\"param-doc\")))

Each <param-doc> are a single line.

DOC-ALIST collects items have tag:
  - :param x:
  - :return:
TYPE-ALIST collects items have tag:
  - :type x:
  - :rtype:
Every documents for type must prefixed with (,)"
  (let ((doc-alist nil)
        (type-alist nil))
    (dolist (destructed compiled (cl-values doc-alist type-alist))
      (unless (null destructed)
        (cl-multiple-value-bind (type name rest) destructed
          (cond ((string= "param" type) (push (cons name rest) doc-alist))
                ((string= "return" type) (push (cons "return" rest) doc-alist))
                ((not (member type '("type" "rtype"))))
                (t (let ((first (cl-first rest))
                         (rest (cl-rest rest))
                         (name (if (string= "type" type) name "return")))
                     (-if-let (i (pardef--find-next-outside-par first ?\, 0))
                         (let ((first* (substring-no-properties first i)))
                           (push (cons name (cons first* rest)) type-alist))
                       (push (cons name rest) type-alist))))))))))


(defun pardef--rsph-collect-raises (compiled-lines)
  "Collects lines from `COMPILED-LINES' which don't have type in
`pardef--rsph-keywords'."
  (let ((raises nil))
    (dolist (lines compiled-lines)
      (when lines
        (unless (string-match-p pardef--rsph-keywords (car lines))
          (push lines raises))))
    (mapcar #'pardef--rsph-rebuild-line (nreverse raises))))


(defun pardef--rsph-update (alist docstring)
  (let ((i 0)
        (result nil)
        (z (- (length docstring) 1)))
    (dolist (line docstring)
      (push (cond ((= 0 i) (replace-regexp-in-string
                            "^\\(?:'''\\|\"\"\"\\)" (string) line))
                  ((= z i) (replace-regexp-in-string
                            "\\(?:'''\\|\"\"\"\\)$" (string) line))
                  (t (replace-regexp-in-string pardef-docstring-style ""
                                               line)))
            result))
    (setq docstring (nreverse result)))
  (let ((blocks (pardef-util-split-docstring-blocks docstring)))
    (cl-case (length blocks)
      (0 (pardef--rsph-create alist))
      (1 (let* ((first (caar blocks))
                (rest (cdar blocks)))
           (--> (cons (concat pardef-docstring-style first) rest)
                (list it (string)
                      (pardef--rsph-create-params-and-return alist)
                      pardef-docstring-style)
                -flatten)))
      (t (let* ((ignored-regexp "^\\s-*\\(?:'''\\|\"\"\"\\)\\s-*$")
                (params (--remove (string-match-p ignored-regexp it)
                                  (cl-second blocks)))
                (grouped-lines (pardef--rsph-group-lines params))
                (compiled (mapcar #'pardef--rsph-destruct-line grouped-lines)))
           (-when-let* ((first (cl-first compiled))
                        (nilidx (-find-index #'null compiled)))
             (pardef--user-error "Unable to parse: %s"
                                 (string-join (nth nilidx grouped-lines) "\n")))
           (let ((raises (pardef--rsph-collect-raises compiled))
                 (docs (pardef--rsph-collect-docs compiled))
                 (modifier (if (car compiled) #'-replace-at #'-insert-at)))
             (cl-multiple-value-bind (da ta) docs
               (--> (pardef--rsph-create-params-and-return alist da ta raises)
                    (funcall modifier 1 it blocks)
                    (-interpose (string) it)
                    -flatten
                    (--map-first it (concat pardef-docstring-style it) it)
                    (append it (list pardef-docstring-style))))))))))


(defun pardef-renderer-sphinx (alist docstring)
  "Simple renderer base on Sphinx document format.

See `pardef-gen' for more information about renderer.
See <https://sphinx-rtd-tutorial.readthedocs.io/en/latest/docstrings.html>
for the Sphinx docstring format."
  (cl-values
   ;; Docstring's content
   (if (null docstring)
       (pardef--rsph-create alist)
     (pardef--rsph-update alist docstring))
   ;; Cursor's relative location
   0 (length pardef-docstring-style)))


(defun pardef-sphinx ()
  (interactive)
  (pardef-gen #'pardef-renderer-sphinx))

(provide 'pardef)
;;; pardef.el ends here
