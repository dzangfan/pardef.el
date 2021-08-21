(ql:quickload :cl-ppcre)

(defun find-defun (string)
  (let ((result nil))
    (ppcre:register-groups-bind (content) ("^([^#]+)(?:#.*)?$" string)
      (ppcre:register-groups-bind (definition) ("^\\s*(def\\s+.+:)" content)
        (push definition result)))
    result))


(defun collect-from-file (path)
  (with-open-file (hnd path :direction :input)
    (loop :for line := (read-line hnd nil)
          :while line :collecting (find-defun line) :into def-collector
          :finally (return (apply #'nconc def-collector)))))


(defun collect-from-dir (path)
  (uiop:with-current-directory (path)
    (let ((sources (directory "**/*.py")))
      (apply #'nconc (mapcar #'collect-from-file sources)))))


(defun collect-to-file (output-filename &optional (directory #P"."))
  (let ((defun-seq (collect-from-dir directory)))
    (with-open-file (hnd output-filename :direction :output
                                         :if-exists :supersede)
      (mapc (lambda (entry) (write-line entry hnd)) defun-seq))))
