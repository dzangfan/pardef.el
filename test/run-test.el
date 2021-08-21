
(add-to-list 'load-path (expand-file-name ".."))
(require 'pardef)

(cd "data")
(setq pardef--line-count 0)
(setq pardef--test-lines nil)
(dolist (file (directory-files "."))
  (unless (member file '("." ".."))
    (let ((buffer (find-file-noselect file)))
      (with-current-buffer buffer
        (end-of-line)
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (push line pardef--test-lines)
            (cl-incf pardef--line-count))
          (forward-line)))
      (kill-buffer buffer))))
(cd "..")

(dolist-with-progress-reporter (line pardef--test-lines) "Testing.."
  (let ((parez (pardef-load-python-defun line)))
    (when (stringp parez)
      (user-error "Test failed due to '%s'" parez))))
(message "Passed %d tests." pardef--line-count)
