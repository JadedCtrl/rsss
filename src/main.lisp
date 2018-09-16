(in-package :rsss)

;; ---------------------------------------- 
;; FEED PARSING
;; ---------------------------------------- 

(defun feed-values (data value)
  "Return all values from a feed matching a set query."

  (if (getf-string data "channel")
    (getf-strings (getf-string data "channel") value)
    (getf-strings data value)))



(defun feed-value (data value)
  "Return the first value from a feed matching a set query."

  (car (feed-values data value)))



(defun feed-value-listless (data value)
  "Return the first value from a feed matching a set query,
  but as an isolated string."

  (let ((result (feed-value data value)))

    (if (listp result)
      (car (last result))
      result)))




(defun feed-items (data)
  "Return a list of all RSS `<item>`s (articles)."

  (feed-values data "item"))



(defun title (data)
  "Return the title of a set of data.
  Accepts an entire XML file (for the `<channel>`'s data)
  or a single `<item>`."

  (feed-value-listless data "title"))



(defun description (data)
  "Return the description of a set of data.
  Accepts an entire XML file (for the `<channel>`'s data)
  or a single `<item>`."

  (feed-value-listless data "description"))



(defun link (data)
  "Return the link of a set of data.
  Accepts an entire XML file (for the `<channel>`'s data)
  or a single `<item>`."

  (feed-value-listless data "link"))



(defun pubdate (data)
  "Return the publish-date of a set of data.
  Accepts an entire XML file (for the `<channel>`'s data)
  or a single `<item>`."

  (feed-value-listless data "pubDate"))




;; ---------------------------------------- 
;; MISC
;; ---------------------------------------- 

(defun getf-string (list string)
  "Get an item from a list by an identifying string in `car`.
  I.E., if the string is 'apple', the first sublist like this:
  ('apple' 1 2 3)
  will be returned."

  (car (getf-strings list string)))



(defun getf-strings (list string &optional (stack '()))
  "Get items from list by an identifying string in `car`.
  I.E., if the string is 'apple', any sublists like this:
  ('apple' 1 2 3)
  will be returned."

  ;; just recurse through the list, adding each new matching
  ;; item to the `stack`

  (if (and (< 0 (length list)) (listp list))
    (if (ignore-errors     
          ;; the item might not be a list; for our purposes, let's ignore that.
          (equal
            (car (car list))    ;; '( ( here ) )
            string))
      (getf-strings (cdr list) string (concatenate 'list stack (list (car list))))
      (getf-strings (cdr list) string stack))
    stack))
