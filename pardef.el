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

(require 'cl)

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
                   (parlist (substring-no-properties definition parlist-begin parlist-end)))
              (list fun-name parlist return-spec)))))))


(defconst pardef--python-par-alist '((?\( . ?\))
                                     (?\[ . ?\])
                                     (?\{ . ?\})))

(defun pardef--parse-pyparlist-find-next-comma (parlist &optional start)
  (do ((idx start (+ 1 start))
       (len (length parlist))
       (par-stack 0)
       (par-char nil)
       (c (substring-no-properties idx (+ idx 1))
          (substring-no-properties idx (+ idx 1))))
      ((>= idx len) nil)
    (cond ((and (zerop par-stack) (char-equal ?, c)) (return idx))
          (t ;;; TODO COMPLETE ME
           ))))


(defun pardef--parse-python-parlist (parlist)
  "Parsing python-style function's parameter list(`PARLIST').
Returns the result as a list, whose each element is a list has 3
elements, each component corresponds name, default value and
type.  First element(i.e. name) will be a non-empty string, but
both type and value can be empty.
If `PARLIST' is empty or only contains white-space, empty
list(i.e. nil) will be returned.  As a result, we use STRING to
indicate a parse error: if invocation returns a string, it's mean
there is something wrong, and the STRING contains a short message
about the reason."
  )



(provide 'pardef)
;;; pardef.el ends here
