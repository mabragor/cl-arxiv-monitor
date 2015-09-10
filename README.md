cl-arxiv-monitor
----------------

More fine-grained (daily) monitoring of www.arXiv.org, then
its mailing lists allow.

The main use-case (and the only one supported now) is:

  * keep track of papers from particular author(s)

Steps to setup cl-arxiv-monitor:

  * (prerequisite) make 'mail' command-line tool work (e.g. by setting up postfix)
  * save arxivmon core
  * write a configuration file
  * make a cron job

Make 'mail' command-line tool work
----------------------------------

Any solution will do, for example:
  * setting up send-only SMTP server with postfix
    https://www.digitalocean.com/community/tutorials/how-to-install-and-configure-postfix-as-a-send-only-smtp-server-on-ubuntu-14-04
  * configure sendmail to use GMail as relay SMTP server
    http://linuxconfig.org/configuring-gmail-as-sendmail-email-relay

Save arxivmon core
------------------

The following will create an sbcl core named 'arxivmon' in your home directory

```lisp
(ql:quickload 'cl-arxiv-monitor)
(cl-arxiv-monitor::make-cronable)
```

Write a configuration file
--------------------------

The configuration file for cl-arxiv-monitor is ~/.arxivmon.rc
and is supposed to be a file WITH *ARBITRARY* LISP CODE, which is EVALed.
So, be careful with permissions.

The file is initially read in cl-arxiv-monitor package.

The variables, you might want to configure:
  * \*authors-of-interest\* -- the list of authors to keep track of
  * \*email\* -- the email to send notifications to
  * \*known-papers-pathname\* -- a path to a file, where cl-arxiv-monitor
    saves the arXiv IDs of the papers it already "knows"

Example:
```lisp
(defparameter *email* "myname@mymail.com")
(defparameter *authors-of-interest*
  '(("Smith" ("John" "Agent" "J\\."))))
```

As seen from the example, \*authors-of-interest\* is just
a list of entries, each of which is a surname
(which must be present in author specifier of the paper)
+ several "names" (at least one of which must be present in author specifier of the paper).

Both surname and name are actually regexps (in syntax of CL-PPCRE).

TODO
----

* Support other Lisps than just SBCL
* Notifications of some papers based on keywords in title/abstract, not just authors