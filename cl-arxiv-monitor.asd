;;;; cl-arxiv-monitor.asd

(asdf:defsystem #:cl-arxiv-monitor
  :description "Monitor appearance of new papers on www.arXiv.org"
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "MIT"
  :serial t
  :version "0.1"
  :depends-on (#:cl-interpol #:cl-ppcre #:iterate #:cl-arxiv-api #:cl-smtp)
  :components ((:file "package")
               (:file "cl-arxiv-monitor")))

