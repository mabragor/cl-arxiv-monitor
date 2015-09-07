;;;; cl-arxiv-monitor.lisp

(in-package #:cl-arxiv-monitor)

(cl-interpol:enable-interpol-syntax)

(defparameter *known-papers* (make-hash-table :test #'equal))

(defparameter *known-papers-pathname* ".arxiv-monitor-known-papers.txt")
(defparameter *email* "popolit@gmail.com")

(defun err-script (str)
  (multiple-value-bind (out err errno) (clesh:script str)
    (if (equal 0 errno)
	out
	(error err))))

(defun load-known-papers ()
  (iter (for line in-file (merge-pathnames *known-papers-pathname* (user-homedir-pathname))
	     using #'read-line)
	(setf (gethash (string-trim '(#\space #\tab #\newline) line) *known-papers*) t)))

(defun save-known-papers ()
  (let ((path (merge-pathnames #?"$(*known-papers-pathname*)" (user-homedir-pathname)))
	(new-path (merge-pathnames #?"$(*known-papers-pathname*).new" (user-homedir-pathname)))
	(bkup-path (merge-pathnames #?"$(*known-papers-pathname*).bkup" (user-homedir-pathname))))
    (with-open-file (stream new-path :direction :output :if-exists :supersede)
      (iter (for (key nil) in-hashtable *known-papers*)
	    (format stream "~a~%" key)))
    (err-script #?"mv $(path) $(bkup-path)")
    (err-script #?"mv $(new-path) $(path)")))


(defparameter *authors-of-interest*
  '(;; ("Morozov" ("Alexei" "A\\." "Al\\."))
    ("Popolitov" ("Alexandr" "A\\."))))

(defun get-author-recent-papers (author &optional (start 0))
  (arxiv-get `(:author ,author) :start start :max-results 10 :sort-by :submitted :sort-order :desc))

(defun get-authors (lst)
  (iter (for elt in lst)
	(if (eq :author (car elt))
	    (collect (cdr elt)))))

(defun matching-author-p (str author-spec)
  (let ((lst (cl-ppcre:split "(\\s|\\t)+" str))
	surname-matched)
    (destructuring-bind (surname white-names) author-spec
      (iter (for smth in lst)
	    (if (cl-ppcre:all-matches-as-strings #?"^$(surname)$" smth)
		(if (not surname-matched)
		    (setf surname-matched t)
		    (return-from matching-author-p nil))
		(let (name-matched)
		  (iter (for name in white-names)
			(when (cl-ppcre:all-matches-as-strings #?"^$(name)$" smth)
			  (setf name-matched t)
			  (terminate))
			(finally (if (not name-matched)
				     (return-from matching-author-p nil))))))
	    (finally (return surname-matched))))))
	  
(defun some-author-matches-p (authors author-spec)
  (iter (for author in authors)
	(if (matching-author-p author author-spec)
	    (return-from some-author-matches-p t)))
  nil)

(defun extract-id (str)
  (cl-ppcre:regex-replace-all "v(\\d)+$"
			      (cl-ppcre:regex-replace-all "^http://arxiv.org/abs/" str "")
			      ""))

(defun %get-author-unknown-papers (recent-papers author author-spec)
  (if (integerp recent-papers)
      (setf recent-papers (get-author-recent-papers author recent-papers)))
  (let (known-paper-found)
    (iter (for elt in recent-papers)
	  (if (not (eq :entry (car elt)))
	      (next-iteration))
	  (if (not (some-author-matches-p (get-authors (cdr elt)) author-spec))
	      (next-iteration))
	  (let ((id (extract-id (cdr (assoc :id (cdr elt))))))
	    (format t "ID: ~a~%" id)
	    (if (gethash id *known-papers*)
		(progn (setf known-paper-found t)
		       (terminate))
		(collect (cdr elt) into res)))
	  (finally (return (values res known-paper-found))))))

(defun get-author-unknown-papers (author)
  (let* ((recent-papers (get-author-recent-papers author))
	 (total-papers (or (cdr (assoc :total-results recent-papers))
			   (error "Response of arXiv does not contain total results field!")))
	 (author-spec (assoc author *authors-of-interest* :test #'equal)))
    (multiple-value-bind (res known-found) (%get-author-unknown-papers recent-papers author author-spec)
      (if known-found
	  res
	  (iter (for start from 10 by 10)
		(if (>= start total-papers)
		    (terminate))
		(multiple-value-bind (new-res known-found) (%get-author-unknown-papers start author author-spec)
		  (setf res (append res new-res))
		  (if known-found
		      (terminate)))
		(finally (return res)))))))

(defun generate-email-report (new-papers)
  nil)
  ;; ...)

(defun send-the-email (title body)
  (with-email (stream *email* :subject title :from "<noreply>@arxivmon.org")
    (format stream "~a" body)))

(defun get-new-papers ()
  (load-known-papers)
  (let ((new-papers (make-hash-table :test #'equal)))
    (iter (for (author . nil) in *authors-of-interest*)
	  (iter (for paper in (get-author-unknown-papers author))
		(setf (gethash (extract-id (cdr (assoc :id paper))) new-papers)
		      paper)))
    (iter (for (key nil) in-hashtable new-papers)
	  (setf (gethash key *known-papers*) t))
    (send-the-email (generate-email-header new-papers)
		    (generate-email-report new-papers))
    (save-known-papers)))
    