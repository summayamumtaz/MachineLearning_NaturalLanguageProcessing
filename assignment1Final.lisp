;; Assignment 1 Submission 14th Sept ( Author : Summaya)
;; Question 1 part a) select element pear from '(apple orange pear lemon)
(first (rest (rest '(apple orange pear lemon))))

;;b) select element pear from '((apple orange) (pear lemon))
(first (first (rest '((apple orange) (pear lemon)))))

;;c)  select element pear from '((apple) (orange) (pear))
(first (first (rest (rest '((apple) (orange) (pear))))))

;;d) use cons to create ((apple orange) (pear lemon))
(cons (cons 'apple (cons 'organe nil)) (cons (cons 'pear (cons 'lemon nil)) nil))

;;d2) use cons to create ( (apple) (orange) (pear))
(cons (cons 'apple nil) (cons (cons 'orange nil) (cons (cons 'pear nil) nil)))

;;e) Select next-to-last element of *foo*
;; option 1 using length and nth function
(nth (- (length *foo*) 2) *foo*)

;;option 2 using first rest and reverse functions
(first (rest (reverse *foo*)))

;; Question 2 Interpreting Common Lisp
;;;;purpose of function foo is to count total number of elements in the given list by using recursive calls. Symbol foo is used as function and also as list parameter passed to the function.It achieves its goals by calling foo function as many times as the list has elements. If the list has Nil , it will return 0 and start adding 1 with 0. With every recursive call except the base case 1 will be incremented and hence the count of elements in a list is returned.
;;(defun foo (foo) -----> first foo is function name and second list passed as function argument
; ( if foo ---> here foo acts as list
;     (+ 1 ( foo --> here foo is function call
;               (rest foo ----> here foo is used as list)


;; question 3 part a
(let ((foo (list 0 42 2 3)))
	   (setf (nth 0 foo) 42)
	   (setf (nth 1 foo) 0)
	   (first foo))

;;; question 3 part b
(let* ((keys '(:a :b :c)) (values '(0 1 2))
           (pairs (pairlis keys values)))
      (setf pairs (acons ':b 42 pairs))
      (rest (assoc :b pairs)))

;; question 3 part c)
(let ((foo (make-hash-table)))
	       (setf (gethash 'meaning foo) 41)
	       (setf (gethash 'meaning foo) (incf (gethash 'meaning foo)))
	       (gethash 'meaning foo))

;;question 3 part d)
(let ((foo (make-array 5)))
	   (setf (aref foo 0) 0)
	   (setf (aref foo 1) 1)
	   (setf (aref foo 2) 42)
	   (setf (aref foo 3) 5)
	   (setf (aref foo 4) 6)
	   (aref foo 2))

;; question  4 part a) count-member using recursion

(defun count-member (symbol list)
    (if list
     (if (equal symbol (first list))
	       (+ 1 (count-member symbol (rest list)))
	       (count-member symbol (rest list)))
     0))

;; question 4 part b) count-member using loop
(defun count-member ( symbol list)
	   (loop for i from 0 to (length list)
		counting ( equal symbol (nth i list))))

;; question 5 part a
;;;; The above experssion opens file "brown1.txt" using macro  with-open-file and creates a filestream called "stream" in this case.The :direction specifies either we want to read or write from file. In this case direction is :input ( we are reading from file). Inside loop read-line is used to store one line from file in variable "line". Loop continues untill line is nil and for each line tokenize function is called ( tokenize : returns separated tokens from the given line by using collect and based on ignoring spaces between words . Append is used to store the tokens everytime tokenize functions returns a list of tokens. The return value of the entire expression will be a list of tokens of the given input file.

;; question 5 part b

(defparameter *corpus* nil)

(setf *corpus* (with-open-file (stream "brown1.txt" :direction :input)
  (loop
      for line = (read-line stream nil)
      while line
     append (tokenize line))))


;; question 5 part c
;;;; the current strategy for tokenisation is to split each line into tokens by using "space". Group of words occurring before "space"  are tokenised into one token.Current tokenisation strategy has the flaws liking considering "(when" ,"goal)." as one token. The token "(when" is incorrect and it should be should be extracted as "when" by removing the punctuation marks. Further steps need to be added to remove starting and ending punctuation marks in tokens.

;; question 5 part d
;; hashtable is defined for the refined tokenize function  using "equalp"
(defparameter *hashtable* (make-hash-table :test 'equalp))

;; If the result of gethash is nil then value is set to 1 otherwise each time value is incremented using incf
(loop for x in *corpus*
	      when (equal nil (gethash x *hashtable*))
	      do (setf (gethash  x *hashtable*) 1)
	      else
	      do (setf (gethash x *hashtable*) (incf (gethash x *hashtable*)))
	      finally
     (return t))

;; question 5 part e : Unique word types(total keys)  in *corpus* 5920 using original tokenize function

;; question 5 part f : function tokenize2 takes string "s" as argument . First  cond removes the starting punction mark in a token by using subseq token 0 1 to check first index of token. If it is not alphanumeric , token is updated using string-left-trim.
;; Second cond checks for the ending punctuation mark by giving last index of token to subseq and if it is not  alphanumeric , token is trimmed using string-right-trim. output "goal.)" --> "goal."
(defun tokenize2 (s)
	   (loop
      for start = 0 then (+ space 1)
      for space = (position #\space s :start start)
      for token = (string-downcase (subseq s start space))
         ;; two separate conds for handling right and left punctuation marks as they might be different like ".(thus" "the;;"
	       do ( cond ( (equal "" token) nil) ;; if token is nil skip the cond
                   ;; loop until token is not empty AND all the left punctuation marks are trimmed ( handles and removes token like "--")
									 ;; for trimming first index of token is given input to alphanumericp , if it is not alphanumeric then token is updated by trimming first index
									 ( (loop while  (and (not (string= token "")) (not (alphanumericp (coerce (subseq token 0 1)  'character))) )
						            do (setf token  (string-left-trim (subseq token 0 1) token)))))

                  ;; removing any number of punctuation appearing on right side of token
				 do (cond ( (equal "" token) nil)  ;; after removing left punctuation , token like "--" may be empty so exit if token is empty
                  ;; take the last index of token and check for alphanumeric. Remaining logic is same as mentioned above
									( (loop while (and (not (string= token "")) (not (alphanumericp (coerce (subseq token (- (length token) 1)) 'character ))))
					          do (setf token (string-right-trim (subseq token (- (length token) 1)) token)))))


	       unless (string= token "") collect token
      until (not space)))


;; total number of entries after refining tokenise = 4839
;; count of unique word types is reduced based on refined approach. The most frequent token in both cases (original vs. Refined approach) is “the”. In original count for refine is 1781 and using refined approach it is 1782

;; finding the most frequent token based on value ( most frequent token)
(loop for k being the hash-keys in *hashtable1* using (hash-value v)
  maximize v
   return k)
