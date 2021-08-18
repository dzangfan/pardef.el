
(defun pardef--test-this-line ()
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

(add-to-list 'load-path (expand-file-name ".."))

(with-current-buffer (find-file-noselect "./data/TheAlgorithms-Python.git.qy")
  (require 'pardef)
  (end-of-buffer)
  (let ((sample-count (line-number-at-pos)))
    (beginning-of-buffer)
    (dotimes-with-progress-reporter (lidx sample-count)
        "Testing.."
      (let* ((line (pardef--test-this-line))
             (parez (pardef-load-python-defun line)))
        (when (stringp parez)
          (user-error "Test failed due to '%s' in %d" parez (+ 1 lidx))))
      (forward-line))
    (message "Passed %d tests" sample-count)))
