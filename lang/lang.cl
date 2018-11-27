#!/usr/bin/sbcl --script
#
# An attempt to identify the language
# of plain text files by calculating the euclidean
# distance between two vectors; one representing
# the frequency of letters for a particular language,
# the other representing actual frequency of letters
# in file.


(defparameter *alphabet '(#\a #\à #\â #\á #\å #\ä #\ã #\ą #\æ #\œ #\b #\c #\ç #\ĉ #\ć #\č #\d #\ď #\ð #\e #\è #\é #\ê #\ë #\ę #\ě #\f #\g #\ĝ #\ğ #\h #\ĥ #\i #\î #\ì #\í #\ï #\ı #\j #\ĵ #\ł #\k #\l #\m #\n #\ñ #\ń #\ň #\o #\ò #\ö #\ô #\ó #\ø #\p #\q #\r #\ř #\s #\ŝ #\ş #\ś #\š #\ß #\t #\ť #\þ #\u #\ù #\ú #\ŭ #\ü #\ů #\v #\w #\x #\y #\ý #\z #\ź #\ż #\ž))
(defparameter *langfile "lfreq.csv_utf8")

(defun args ()
	(cdr *posix-argv*))

(defun alphap (char)
	(member char *alphabet))

(defun hash-put (key value hashtable)
	(if (eq (type-of hashtable) (type-of (make-hash-table)))
		(setf (gethash key hashtable) value)))

(defun hash-get (key hashtable)
	(if (eq (type-of hashtable) (type-of (make-hash-table)))
		(if (gethash key hashtable)
			(gethash key hashtable)
			(+ 0))))

(defun hash-clear (hashtable)
	(loop for key in *alphabet do
		(remhash key hashtable)))

(defun hash-increment (key hashtable)
	(hash-put key (+ 1 (hash-get key hashtable)) hashtable))

(defun hash-init (hashtable)
	(loop for key in *alphabet do
		(hash-put key 0 hashtable)))

(defun hash-print (hashtable)
	(if (eq (type-of hashtable) (type-of (make-hash-table)))
		(loop for key in *alphabet
			do (format t "~s -> ~d~%" key (hash-get key hashtable)))))

; TODO: refactor to return hashtable from filename
(defun populate-hashtable (hashtable filename)
		(with-open-file (s filename)
	  (do ((c (read-char s nil :eof)
	          (read-char s nil :eof)))
	    ((eql c :eof) 'done)
	    (if (alphap (char-downcase c))
	    	(hash-increment (char-downcase c) hashtable)))))

(defun sum-table (hashtable)
	(let ((sum 0))
		(loop for key in *alphabet do
			(setf sum (+ sum (hash-get key hashtable)))) sum ))

; stolen from http://stackoverflow.com/questions/7508450/whats-the-best-way-to-sort-a-hashtable-by-value
(defun hash-table-alist (hashtable) 
	(let ((alist nil))
		(maphash (lambda (k v) (push (cons k v) alist))	hashtable)
		alist))

; stolen from http://stackoverflow.com/questions/7508450/whats-the-best-way-to-sort-a-hashtable-by-value
(defun hash-table-top-n-values (table n)
  (subseq (sort (hash-table-alist table) #'> :key #'cdr) 0 n))

(defun letter-frequency (hashtable)
	(let ((lettersum (sum-table hashtable)) (frequencytable (make-hash-table)))
		(loop for key in *alphabet do
			(hash-put key (/ (hash-get key hashtable) lettersum) frequencytable))
		frequencytable))

; stolen from http://cl-cookbook.sourceforge.net/strings.html
(defun split-by-one-space (string)
    "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

; stolen from https://groups.google.com/forum/#!topic/comp.lang.lisp/-W1FeuHq0DI
(defun parse-float (string)
  "Return a float read from string, and the index to the remainder of string."
  (multiple-value-bind (integer i)
      (parse-integer string :junk-allowed t)
    (multiple-value-bind (fraction j)
        (parse-integer string :start (+ i 1) :junk-allowed t)
      (values (float (+ integer (/ fraction (expt 10 (- j i 1)))))
      	j))))

(defun get-lang-stats (n)
	(let ((langtable (make-hash-table)))
		(with-open-file (handle *langfile)
			(read-line handle nil)
			(loop for line = (read-line handle nil) do
				(let ((key-val-mapping (split-by-one-space line)))
					(if (eq line nil)
						(return-from get-lang-stats langtable)
						(let ((key (char (nth 0 key-val-mapping) 0))
							(val (parse-float (nth n key-val-mapping))))
							(hash-put key val langtable))))))))

(defun euclidean_distance (hasht1 hasht2) 
	(let ((result 0))
		(loop for key in *alphabet do
			(setf result (+ result (expt
				(- (hash-get key hasht1) (hash-get key hasht2))
				 2)))) result))

(defun languages ()
	(with-open-file (handle *langfile)
		(let ((langlist (list nil)))
			(loop for element in (split-by-one-space (read-line handle)) do
				(if (> (length element) 1)
					(setf langlist (append langlist (list element)))))
			(return-from languages langlist))))

(defun lang-to-idx (langname)
	(with-open-file (handle *langfile)
		(let ((idx 0))
			(with-open-file (handle *langfile)
				(loop for element in (split-by-one-space (read-line handle nil)) do
					(if (string= element langname)
						(return-from lang-to-idx idx)
						(setf idx (+ 1 idx))))))))

(defparameter *hashtable (make-hash-table))
(populate-hashtable *hashtable (car (args)))
(defparameter *frequency (letter-frequency *hashtable))
(defparameter *lang-distances (make-hash-table))

(loop for language in (languages) do
	(if (> (length language) 1)
		(hash-put language (euclidean_distance *frequency (get-lang-stats (lang-to-idx language))) *lang-distances)))
(loop for top in (reverse (hash-table-top-n-values *lang-distances 15)) do
	(format t "~5$~c~s~%" (cdr top) #\tab (car top)))
