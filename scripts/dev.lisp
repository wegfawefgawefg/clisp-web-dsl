;; Run with: sbcl --script scripts/dev.lisp
;; Polls src/dsl.lisp and re-runs main whenever it changes.

(defparameter *target-file* (truename (merge-pathnames "../src/dsl.lisp" *load-truename*)))

(defun file-stamp (path)
    (or (ignore-errors (file-write-date path)) 0))

(defun run-once ()
    (handler-case
        (progn
            (load *target-file*)
            (main))
        (error (e)
            (format *error-output* "~&Run failed: ~a~%" e))))

(let ((last-stamp -1))
    (format t "Watching ~a~%" *target-file*)
    (loop
        (let ((stamp (file-stamp *target-file*)))
            (when (/= stamp last-stamp)
                (setf last-stamp stamp)
                (format t "~&== Reload: ~a ==~%" (get-universal-time))
                (run-once)
                (format t "~&== Waiting for changes ==~%")))
        (sleep 0.75)))
