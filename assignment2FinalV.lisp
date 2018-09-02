
;; question 1 part a
;;;; Context can be defined using two approaches 1) Distributional representation : based on co-occurance of word and following strategies are used to find co-occurance
;; Document context : context of a word is all the documents in which it is appearing
;;; Window Context : Context of word conists of n words appearing on left or right of it
;;; Syntactic context : Context of words consists of all words that are connected with word using syntactic relations
;;; Famous context models based on distributional hypothesis
;;; Hyperspace Analogue to Language(HAL)
;;;  This model considers only words that immediately occurs with given word. It computes NxN Matrix , where N is the number of words in its lexicon. Using a 10-word reading frame is moved incrementally on a corpus and every time two words are in same frame their association is incremented. Words that are different have smaller associations in the Matrix.

;;;;Latent Semantic Analysis ( LSA)
;;; This model is also based on the idea that word occuring in similar pieces of text are similiar. A matrix is composed for representing word counts per paragraph. Rows represent the unique words and columns represent each paragraph.
;;; Reference : Wikipedia



;; question 2 part a
(defstruct vs
	   (matrix nil)
	   (similarity-fn nil))

;;question 2 part b
;;;; Matrix is represented as a superhash table--->  each key in superhash refers to one word (total 122 keys in superhash)  in "words.txt" file. Value for each key stores a feature-hash-table. Feature-hash-table stores name and count of each feature associated with the word in superhash.
;; Using n-dimensional arrays leads to look-time of O(n). As the matrix is sparse and only few elements are non-zero hence using n-dimensional array is not feasible both in terms of memory utilization and loop-up time. Whereas look-up time in hashtable is contant O(1). Hence 122 words are stored as keys in superhash and in the value of each key reference to feature hash table is stored. As each word has separe hash table for features hence the insertion , deletion and lookup time is constant.

;;; Note : in this implementation the word itself is not considered it's context(feature) eg. potato in sentence (this is potato) is ignored and context--> (this , is)

;;1  Read-corpus-to-vs function requires following helper function
;;  (dot-product) --- arguments assoclist1 assoclist 2 |( computes dot.product of two assoc lists)
;; (create-feature-hashes) --> arguments  key corpusfile (reference to feature hash table stored in superhash)
;; (filter-context) ----> argument line from corpus :  checks if the words appears in sentence then calls normalize-token
;; (normalize-token) --> token as string :  removes punctuation marks and check if the token appears in stop word list
;; ( create-hash) ---> create the hash table from list of stop words and *Stophash* global variable used in normalize-token function

;;;; above functions are defined below in reverse order
(defun create-hash (stoplist)
  (let ( (hashtable (make-hash-table :test 'equalp)))
    (loop for x in stoplist
       when (equal nil (gethash x hashtable))
       do (setf (gethash  x hashtable) 1)
       finally
	 (return hashtable))))

;;; stop list containing 319 english stop words downloaded from google
;;; while comparing efficieny, hash-table has maximum efficiency as compared to list. To matach a given token with list of stopwords , everytime entire list will be scanned hence resulting in scannig entire list total  O ( 122 * ( all the tokens in brown2.corpus)* (n stop words))
;;; while in hashtable , finding the token as key in hashtable gives a constant time O(1) instead of scanning all 319 words  and gives maximum efficieny

(defparameter *stophash* (create-hash '("a" "about" "above" "across" "after" "afterwards" "again" "against" "all" "almost" "alone" "along" "already" "also" "although" "always" "am" "among" "amongst" "amoungst" "amount" "an" "and" "another" "any" "anyhow" "anyone" "anything" "anyway" "anywhere" "are" "around" "as" "at" "back" "be" "became" "because" "become" "becomes" "becoming" "been" "before" "beforehand" "behind" "being" "below" "beside" "besides" "between" "beyond" "bill" "both" "bottom" "but" "by" "call" "can" "cannot" "cant" "co" "computer" "con" "could" "couldnt" "cry" "de" "describe" "detail" "do" "done" "down" "due" "during" "each" "eg" "eight" "either" "eleven" "else" "elsewhere" "empty" "enough" "etc" "even" "ever" "every" "everyone" "everything" "everywhere" "except" "few" "fifteen" "fify" "fill" "find" "fire" "first" "five" "for" "former" "formerly" "forty" "found" "four" "from" "front" "full" "further" "get" "give" "go" "had" "has" "hasnt" "have" "he" "hence" "her" "here" "hereafter" "hereby" "herein" "hereupon" "hers" "herself”" "him" "himself”" "his" "how" "however" "hundred" "i" "ie" "if" "in" "inc" "indeed" "interest" "into" "is" "it" "its" "itself" "keep" "last" "latter" "latterly" "least" "less" "ltd" "made" "many" "may" "me" "meanwhile" "might" "mill" "mine" "more" "moreover" "most" "mostly" "move" "much" "must" "my" "myself”" "name" "namely" "neither" "never" "nevertheless" "next" "nine" "no" "nobody" "none" "noone" "nor" "not" "nothing" "now" "nowhere" "of" "off" "often" "on" "once" "one" "only" "onto" "or" "other" "others" "otherwise" "our" "ours" "ourselves" "out" "over" "own" "part" "per" "perhaps" "please" "put" "rather" "re" "same" "see" "seem" "seemed" "seeming" "seems" "serious" "several" "she" "should" "show" "side" "since" "sincere" "six" "sixty" "so" "some" "somehow" "someone" "something" "sometime" "sometimes" "somewhere" "still" "such" "system" "take" "ten" "than" "that" "the" "their" "them" "themselves" "then" "thence" "there" "thereafter" "thereby" "therefore" "therein" "thereupon" "these" "they" "thick" "thin" "third" "this" "those" "though" "three" "through" "throughout" "thru" "thus" "to" "together" "too" "top" "toward" "towards" "twelve" "twenty" "two" "un" "under" "until" "up" "upon" "us" "very" "via" "was" "we" "well" "were" "what" "whatever" "when" "whence" "whenever" "where" "whereafter" "whereas" "whereby" "wherein" "whereupon" "wherever" "whether" "which" "while" "whither" "who" "whoever" "whole" "whom" "whose" "why" "will" "with" "within" "without" "would" "yet" "you" "your" "yours" "yourself" "yourselves")))

;; Normalize-token Function

(defun normalize-token (token)
  ( cond ( (equal "" token) nil) ;; if token is nil skip the cond
	 ;; removes all punctuation marks on left
	 (      (loop while  (and (not (string= token "")) (not (alphanumericp (coerce (subseq token 0 1)  'character))) )
		   do (setf token  (string-left-trim (subseq token 0 1) token)))))


  (cond ( (equal "" token) nil)  ;; after removing left punctuations , token like "--" may be empty exit if token is empty
	;; remove punctuations on right
	( (loop while (and (not (string= token "")) (not (alphanumericp (coerce (subseq token (- (length token) 1)) 'character ))))                            do (setf token (string-right-trim (subseq token (- (length token) 1)) token)))))
   ;; after removing punctuations check if token appears stop words ---> return empty token if found in stopword hashtable
  (if (not (equal nil (gethash token *stophash*)))
      ( setf token "")
      token))

;; Filter-context
;; Tokenize the input sentence and call Normalize-token. Return list of filtered tokens


(defun filter-context (s)
  (loop
     for start = 0 then (+ space 1)
     for space = (position #\space s :start start)
     for token = (string-downcase (subseq s start space))
     do

       (setf token   (normalize-token token))

     unless (string= token "") collect token
     until (not space)))


;; create-feature-hashes
;; Arguments
;; 1. k ---- ( word in "words.txt")
;; 2. corpusfile --- "borwn2.txt"
;; 3. fhash ---Reference to featurehashtable stored in Superhash  table

(defun create-feature-hashes ( k corpusfile fhash)
  (with-open-file (stream corpusfile :direction :input)
    (loop
       for context =  (read-line stream nil)
       while context
       do
	 (if  (search k ( string-downcase context)) ;; if the word appears in sentence ,proceed
	      (progn
		(loop for token in (filter-context context)
		   do (
		       cond ( (search k token) nil) ;; ignore if same words e.g k= potato token=potato

			    ( ( if (gethash token  fhash)  ;; if token is found , increment count value in fhash
				   (setf (gethash token  fhash) (incf (gethash token fhash)))
				   (setf (gethash  token fhash) 1))))   ;; otherwise insert feature in fhash and set count to 1
		   until(not token)))
	      ))))


;; DOT-Product (explained in detail in last quesiotn
;; arguments ; assoc list of 2 feature vectos

(defun dot-product1 (al al2)
  (let ((sum 0))
    (loop for ( key . val) in al
       do
	 ( let ((temp (assoc key al2 :test 'equalp)))
	   (if temp
	       (setf sum  (+ sum (* (rest temp) val)))))   ;; multiply only those dimenions that are common in both feature vectos
       finally (return sum))))




;; Read-corpus-to-vs


(defun read-corpus-to-vs (wordfile corpusfile)
  (let  (
	 (superhash  (make-hash-table :test 'equalp))
	 ( space    nil)  )

    ;; create hash containing all words as keys and for each key create separate empty feature hash table
    (with-open-file (stream wordfile :direction :input)
      (loop
	 for line = (read-line stream nil)
	 while line
	 do (  setf (gethash  (string-trim " " line) superhash) ( make-hash-table :test 'equalp))))

    ;; create feature hash table for each entry in Superhash
    (loop for k being the hash-keys in  superhash  using (hash-value val)
       do
	 ( create-feature-hashes k corpusfile val)

       finally
	 (return (setq space (make-vs :matrix  superhash :similarity-fn (function dot-product1))   )))))

;; Test code
;;CL-USER> (defparameter vecspace (read-corpus-to-vs "words.txt" "brown2.txt"))
;;VECSPACE
;;CL-USER> vecspace
;;#S(VS
;;   :MATRIX #<HASH-TABLE :TEST EQUALP :COUNT 122 {1003FBB093}>
;;   :SIMILARITY-FN #<FUNCTION DOT-PRODUCT1>)



;;;  question 2 part c)function that retrieves the feature vector for any given word in our list

(defun get-feature-vector (space item)  ;; returns assoication list composing of feature name and value

  (let ((featurehash (gethash item (vs-matrix space)))
	(alist nil))

    (loop for k being the hash-keys in  featurehash  using (hash-value v)
       do (push (cons k v) alist)
       finally
	 (return alist))))

;; test code
;;;;(get-feature-vector vecspace "mississippi")
;;;; (("expense" . 1) ("publish" . 1) ("obliged" . 1) ("albeit" . 1) ("street" . 1)
;; ("bartholf" . 1) ("novel" . 1) ("author" . 1) ("himself" . 1)
; ("censorship" . 1) ("postal" . 1) ("matters" . 1) ("deal" . 1) ("day" . 1)
; ("general" . 1) ("postmaster" . 1) ("wilson" . 1) ("surgeon" . 1)
; ("ship's" . 1) ("fight" . 1) ("picked" . 1) ("greene" . 1) ("protege" . 1)
; ("looking" . 1) ("hudson" . 1) ("plowing" . 1) ("stopped" . 1)
; ("faulkner" . 1) ("h" . 1) ("folly" . 1) ("plowman's" . 1) ("reading" . 1)
; ("industry" . 1) ("picture" . 1) ("motion" . 1) ("leaders" . 1) ("100" . 1)
; ("dinner" . 1) ("hollywood" . 1) ("came" . 1) ("agency" . 1)
; ("information" . 1) ("states" . 1) ("united" . 1) ("director" . 1)
; ("murrow" . 1) ("r" . 1) ("month" . 1) ("earlier" . 1) ("programs" . 1)
; ("squeeze" . 1) ("brinkley" . 1) ("huntley" . 1) ("douglas" . 1)
; ("snatches" . 1) ("got" . 1) ("hours" . 1) ("evening" . 1) ("run" . 1)
; ("tapes" . 1) ("night" . 1) ("war" . 1) ("cold" . 1) ("exacerbation" . 1)
; ("triumph" . 1) ("hour" . 1) ("enjoying" . 1) ("teller" . 1) ("dr" . 1)
; ("commission" . 1) ("energy" . 1) ("atomic" . 1) ("mc*cone" . 1)
; ("strauss" . 1) ("past" . 1) ("rockefeller" . 1) ("governor" . 1)
; ("nixon" . 1) ("richard" . 1) ("opposed" . 1) ("proposal" . 1)
; ("literature" . 1) ("soviet" . 1) ("discussion" . 1) ("writers" . 1)
; ("russian" . 1) ("table" . 1) ("round" . 1) ("moderates" . 1) ("weeks" . 1)
; ("editor" . 1) ("monthly" . 1) ("atlantic" . 1) ("tonight" . 1) ("club" . 1)
; ("book" . 1) ("members" . 1) ("entertain" . 1) ("app" . 1) ("sabol" . 1)
; ("kershbaum" . 1) ("alfred" . 1) ("bregman" . 1) ("oscar" . 1)
; ("blumberg" . 1) ("gordon" . 1) ("liss" . 1) ("langsdorf" . 1) ("jack" . 1)
; ("kamens" . 1) ("s" . 1) ("mr" . 1) ("schultz" . 1) ("henry" . 1)
; ("evelyn" . 1) ("zinman" . 1) ("jacques" . 1) ("rosen" . 2) ("morton" . 1)
; ("korman" . 1) ("berton" . 1) ("cushman" . 1) ("robert" . 1)
; ("fernberger" . 1) ("malmud" . 1) ("leonard" . 2) ("blum" . 1) ("alvin" . 1)
; ("committees" . 1) ("various" . 1) ("chairmen" . 2) ("assisting" . 1)
; ("tuesday" . 2) ("birthday" . 1) ("celebrates" . 1) ("bowman" . 1)
; ("clinton" . 1) ("honor" . 1) ("week" . 1) ("luncheon" . 1) ("planning" . 1)
; ("luette" . 1) ("paul" . 1) ("hackstaff" . 1) ("mrs" . 20) ("second" . 1)
; ("men" . 1) ("fouled" . 1) ("john" . 1) ("fizzled" . 1) ("drive" . 1)
; ("6-3" . 1) ("trailing" . 1) ("game" . 1) ("ball" . 1) ("reds" . 1)
; ("momentarily" . 1) ("commissioner" . 1) ("safety" . 1) ("public" . 1)
; ("roos" . 1) ("w" . 1) ("today" . 1) ("supported" . 1) ("county" . 1)
; ("essex" . 1) ("sheriff" . 1) ("nomination" . 1) ("republican" . 1)
; ("orange" . 2) ("west" . 2) ("sheeran" . 1) ("j" . 5) ("james" . 1)
; ("mayor" . 1) ("candidacy" . 1))
;;;; question 2 part d)  print-features, taking three arguments: a structure of type vs, a word (as a string) and a number k. The function should then print a sorted list of the k features with the highest count/value for the given word.

(defun print-features ( space word knumber)
  (let ( (featurehash (gethash word (vs-matrix space)))
	(alist nil))

    (maphash #'(lambda (k v)

		 (push (cons k v) alist)) featurehash)
    (subseq (sort alist  #'> :key #'cdr) 0 knumber)))

;; test code
;;CL-USER> (print-features vecspace "university" 9)
;;(("dr" . 17) ("college" . 15) ("state" . 14) ("students" . 12) ("emory" . 11)
 ;;("professor" . 11) ("school" . 11) ("new" . 10) ("j" . 9))



;;; question 3 part a) a function euclidean-length that computes the norm of a given feature vector

;;; helper function summation for function euclidean-length
;;; takes alist , takes square of each value and return summation of squares of all values

(defun summation (alist)

  (if (equal  alist nil)
      0
      (+ (* (rest (first alist)) (rest (first alist))) (summation (rest alist)))
      ))

 (defun euclidean-length (alist)
   (sqrt (summation alist)))   ;;  helper-fn summation to add square of all elements
;; test code
;; CL-USER> (euclidean-length (get-feature-vector vecspace "boston"))
;; 24.433584

;;; question 3 part b) function length-normalize-vs taking a vs structure as input and then destructively modifying its co-occurrence matrix so that all the feature vectors have unit length

(defun length-normalize-vs (space)
  (let  ( (superhash  (vs-matrix space))
	  (norm nil)
	  (featurehash nil))

    (loop for k being the hash-keys in  superhash  using (hash-value v)
       do
	 (setf norm (euclidean-length (get-feature-vector space k)))
	 (setf featurehash (gethash k superhash))

	 (loop for keyh being the hash-keys in featurehash  using (hash-value val)
	    do (setf (gethash keyh featurehash)  (/ val norm))
	     )
	  finally
	      (return space))))
;; test code
;;CL-USER> (length-normalize-vs vecspace)
;;; #S(VS
;;   :MATRIX #<HASH-TABLE :TEST EQUALP :COUNT 122 {1003FBB093}>
;;   :SIMILARITY-FN #<FUNCTION DOT-PRODUCT1>)
;;CL-USER> (euclidean-length (get-feature-vector vecspace "boston"))
;;1.0000039


;;; Question 3 part c

;;;function dot-product that computes this similarity score for two given feature vectors.
;;; Arguments al : association list containing feature vector al2: containing feature vector

(defun dot-product (al al2)
  (let ((sum 0))

    ;;; scan first pair in feature vector of word1
    (loop for ( key . val) in al
       do
	 ( let ((temp (assoc key al2 :test 'equalp)))  ;;; find same feature in feature vector of word2
	   (if temp
	       (setf sum  (+ sum (* (rest temp) val)))))   ;; multiply only those dimenions that are common in both feature vectos
       finally (return sum))))


;;; question 3 pard d

;; compute cosine similarity of two given words

(defun word-simmilarity ( vectorspace  word1  word2)
  (funcall (vs-similarity-fn vectorspace) (get-feature-vector vectorspace word1 ) (get-feature-vector vectorspace word2)))




;;; Results of cosine similarity

;;CL-USER> (word-similarity sp2 "college" "university")
;;0.53377223
;;CL-USER> (word-similarity sp2 "university" "bread")
;;0.112692624
