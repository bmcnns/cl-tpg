(in-package :bes)

(defun store-team (team file-path)
  "Stores a serialized copy of TEAM to the file system."
  (with-open-file (stream file-path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (prin1 (serialize-team team) stream)))

(defun load-team (file-path)
  "Loads a TEAM from the file system."
  (with-open-file (stream file-path
			  :direction :input
			  :if-does-not-exist :error)
    (deserialize-team (read stream) (make-hash-table :test 'equal))))
