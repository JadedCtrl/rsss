(defpackage :rsss-testing
  (:use :cl)
  (:export

    :do-all))

(in-package :rsss-testing)


(defun do-all ()
  "Execute all tests."

  (rt:do-tests))



;; ----------------------------------------
;; DATA PREP
;; ----------------------------------------

(defvar *rss* "")
(with-open-file (file-stream "t/res/rss.xml")
  (do ((line (read-line file-stream nil)
             (read-line file-stream nil)))
    ((null line))

    (setq *rss*
          (concatenate 'string *rss* (format nil "~A~%" line)))))

(setq *rss* (xmls:parse-to-list *rss*))

(defvar *getf-strings-list*
  '(("apple" 1) ("pear" 2) ("plum" yuck) ("plum" yum) ("madoka" weeb 2)))



;; ----------------------------------------
;; MISC
;; ----------------------------------------

(rt:deftest getf-strings
            (rsss:getf-strings *getf-strings-list* "apple")
            (("apple" 1)))

(rt:deftest getf-strings-1
            (rsss:getf-strings *getf-strings-list* "plum")
            (("plum" yuck) ("plum" yum)))

(rt:deftest getf-strings-2
            (rsss:getf-strings *getf-strings-list* "madoka")

            (("madoka" weeb 2)))


(rt:deftest getf-string
            (rsss:getf-string *getf-strings-list* "plum")

            ("plum" yuck))



;; ----------------------------------------
;; FEED PARSING
;; ----------------------------------------

(rt:deftest feed-values
            (rsss:feed-values *rss* "item") 
            (("item" NIL ("title" NIL "Local Resident Takes a Nap") ("description" NIL "John was reported to be \"tired\" and \"irritable\" before his nap. Not so any more.") ("link" NIL "gopher://www.news.com.co.uk.cn/nap.cgi") ("guid" (("isPermaLink" "false")) "1102345") ("pubDate" NIL "Mon, 28 Aug 2018 09:00:00 -0400")) ("item" NIL ("title" NIL "London Bridge Has Burned Down") ("description" NIL "Oh no, not again! :(") ("link" NIL "https://londonbridge.co.uk.cn/bridge.php") ("guid" (("isPermaLink" "false")) "1102345") ("pubDate" NIL "Tue, 29 Aug 2018 09:00:00 -0400"))))

(rt:deftest feed-items
            (rsss:feed-items *rss*) 
            (("item" NIL ("title" NIL "Local Resident Takes a Nap") ("description" NIL "John was reported to be \"tired\" and \"irritable\" before his nap. Not so any more.") ("link" NIL "gopher://www.news.com.co.uk.cn/nap.cgi") ("guid" (("isPermaLink" "false")) "1102345") ("pubDate" NIL "Mon, 28 Aug 2018 09:00:00 -0400")) ("item" NIL ("title" NIL "London Bridge Has Burned Down") ("description" NIL "Oh no, not again! :(") ("link" NIL "https://londonbridge.co.uk.cn/bridge.php") ("guid" (("isPermaLink" "false")) "1102345") ("pubDate" NIL "Tue, 29 Aug 2018 09:00:00 -0400"))))

(rt:deftest feed-value
            (rsss:feed-value *rss* "title")
            ("title" NIL "RSS Sage Emergency Broadcast"))

(rt:deftest feed-value-listless
            (rsss:feed-value-listless *rss* "title")
            "RSS Sage Emergency Broadcast")

(rt:deftest title
            (rsss:title *rss*)
            "RSS Sage Emergency Broadcast")

(rt:deftest description
            (rsss:description *rss*)
            "Your daily dose of RSS Sage tests!")

(rt:deftest link
            (rsss:link *rss*)
            "https://rsss.eunichx.us")

(rt:deftest pubdate
            (rsss:pubdate *rss*)
            "Tue, 29 Aug 2018 09:00:00 -0400")

