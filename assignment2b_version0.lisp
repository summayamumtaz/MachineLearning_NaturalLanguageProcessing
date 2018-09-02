
 ;;;; This solution is based on the soluation 2a provided by the instructor and requires all the functions in 2a for compilation
 ;;;; version used : sbcl/1.3.21

 (defstruct classinfo
 label ;; name of class
 members ;; list of classmembers
 centroid ;; centroid of class
 )

 (defstruct vs
   (matrix (make-hash-table :test #'equal))
   (similarity-fn #'dot-product)
   ;;The following slot will be used in exercise 2b:
   (classes  (make-array 6 :element-type 'classinfo))  ;; store each class as struct in array of size 6
   proximity-matrix)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; solution 2 b ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  question 1 part a

;;  Part a can be implemented using two approaches mentioned below.
;; Approach 1 :proximity-matrix can be stored as hash of hash tables. Superhash comprises of all the words and reference to proximity hash table ( which contains neighbour name and computed proximity.
;; Hash of hashtables implemented this way , will require the proximity to be updated at two places.

;;; Approach 2:  is to consider proximity matrix as symmatrical adjaceny matrix and compute  and store only upper half of matrix as M[i][j] = M[j][i] in our case. Hence comparing two words with string> function
;;; and if word1> word2 , we store the value in hashtable otherwise we dont compute.

;; solution based on approach 1 for question 1 part a

(defun compute-proximities (vspace)
	   ;;
	   (let ((proxMatrix (setf (vs-proximity-matrix vspace) (make-hash-table :test #'equal)))
		 (temphash nil)
		 (wordMatrix (vs-matrix vspace)))

     (loop for k being the hash-keys of wordMatrix using (hash-value v)
      do
    	(setf (gethash k proxMatrix) (make-hash-table :test #'equal)))
    	     ;; for each key in matrix , create hash composed of proximity and associated neighbour
    	     (loop for k being the hash-keys of wordMatrix  using (hash-value v)
      		  do
    		  ;; create hash table for k
      		  (setf temphash (gethash k proxMatrix))

    		  ;; loop matrix again to compute pairwise similarity for each k
    		  (loop for j being the hash-keys of wordMatrix using (hash-value v1)
    		       do
    		       ( if(not (equal k j))
       			   (progn
    			     (setf (gethash j temphash) (dot-product v v1))
    			   ))))))

;; get proximity for approach 1
             (Defun get-proximity (vspace word1 word2)
             	   (let
             	       ((proxMatrix (vs-proximity-matrix vspace)))
             	     (gethash word2 (gethash word1 proxMatrix))))

;; output
;;CL-USER> (get-proximity vspace "kennedy" "nixon")
;;0.49914196
;;T
;;CL-USER> (get-proximity vspace "nixon" "kennedy")
;;0.499142
;;T
;; solution based on approach 2 for question 1 part a

;; store only upper half of proximity matric by using "string>" function. If it return true only then compute and store dot product
(defun compute-proximities2 (vspace)
	   (let ((proxMatrix (setf (vs-proximity-matrix vspace) (make-hash-table :test #'equal) )))
	     (loop for k being the hash-keys of (vs-matrix vspace) using (hash-value v)
	      do(loop
		     for j being the hash-keys of (vs-matrix vspace) using (hash-value v1)
		     do(if (not (equal k j))
			   (if (string> k j)
				       (setf (gethash (cons k j) proxMatrix) (dot-product v v1)) nil) nil)))))


;;; getting proximity for approach 2
 (defun get-proximity (vspace word1 word2)
         	   (let
         	       ((proxMatrix (vs-proximity-matrix vspace)))
         	              (if (string> word1 word2)
         			  (gethash (cons word1 word2) proxMatrix)
         			  (gethash (cons word2 word1) proxMatrix))))






   ;;; 1 b find-knn based on approach 1

   (Defun find-knn (vspace word &optional (n 5))
	     "Prints a ranked list of n neighbours for a given word."
	     (let ((sorted
		    (loop
		       with vector = (gethash word (vs-proximity-matrix vspace))
		       for neighbours being the hash-keys of vector
		       using (hash-value val)
		       collect (cons val neighbours) into values
		       finally (return (sort values #'> :key #'first)))))
	       ;; print the top n neighbours:
	       (loop
		  for i from 1 to n
		  for (val . neighbour) in sorted
		  do (format t "~&~a " neighbour ))))

      ;; output
    ;;  CL-USER> (find-knn vspace "egypt")
    ;;congo
    ;;germany
    ;;europe
    ;;italy
    ;;america
    ;; NIL

    ;;CL-USER> (find-knn vspace "salt" 1)
    ;;pepper
    ;;NIL






     ;;;;;;;;; Implementing a Rocchio classifier

     (defstruct classinfo
	   label ;; name of class
	   members ;; list of classmembers
	   centroid ;; centroid of class
     )

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



          ;;;; question  2 part a) readclasses
         ;; vs-classes stores an array of "classinfo" structures and size is 6


          (defun read-classes (vspace clasfile )
          	   (let
          	       ( (clasarray  (vs-classes vspace))
          		)

          	     (with-open-file (stream clasfile :direction :input)
          	       (loop
                 ;; store  each class information in struct "Classinfo" and save in clasarray
          		  for sexp = ( read stream nil)
          		  for counter from 0 to 5
          		  while sexp
          		  do
          		    (setf (aref clasarray counter)  (make-classinfo :label (string-downcase (first sexp)) :members (first (rest sexp)) :centroid nil))
          		    ))))




;;;; quesiton 2 part b) compute class centroids

(defun compute-class-centroids (vspace)
    (let ((classarray  (vs-classes vspace))  )
      ;;; iterate through each class
      (loop
   for k from 0 to 5
   for clas = (aref classarray k)
         do

   ;; iterate through each member of class k
   (loop
      for x in (classinfo-members clas)
      for centroid =  (make-hash-table :test #'equal ) then centroid
     ;;; get feature vector of member x
      for hash1 = (get-feature-vector vspace  (string-downcase   x))
      do
      ;; iterate through entire feature vector and compute centroid
        (loop
     for id1 being the hash-keys of hash1 using (hash-value v1)
     for v2 = (gethash id1 centroid)
     do (if (gethash id1 centroid)
            (setf (gethash id1 centroid) (+ v1 v2))
            (setf (gethash id1 centroid) v1)))
      finally
        (return  (setf (classinfo-centroid clas) (length-normalize-vector centroid)))))))
;;;;;;;;;
;;;;;;;;;;;;

;;; question 2 part c

(Defun rocchio-classify (vspace)
	   (let ((classarray (vs-classes vspace))
		 (members (classinfo-members (aref  (vs-classes vspace) 5))) ;; get all member of "unknown" class
		 )

      ;;; iterate on each unknown test instance and compute cos similarity
	(loop for k in members
	 for values = nil
		when k
		do(
		   loop for i from 0 to 4
			do
			(let ((t1 (get-feature-vector vspace  (string-downcase k)))
			      (t2 (classinfo-centroid (aref classarray i)))
			      )
			  ( when( and t1 t2)
			    (push  (cons (cons k   (classinfo-label (aref classarray i)) )  (dot-product t1 t2)) values))))

         ;; sort values and print the highest match
		  (print (first (sort values #'> :key #'cdr))))))

  ;;; output of rocchio-classify

;;  CL-USER> (rocchio-classify vspace)

;;((FRUIT . "foodstuff") . 0.3667755)
;;((CALIFORNIA . "person_name") . 0.2996773)
;;((PETER . "person_name") . 0.3088593)
;;((EGYPT . "place_name") . 0.30741528)
;;((DEPARTMENT . "institution") . 0.5497037)
;;((HIROSHIMA . "place_name") . 0.23045647)
;;((ROBERT . "person_name") . 0.5956597)
;;((BUTTER . "foodstuff") . 0.3845364)
;;((PEPPER . "foodstuff") . 0.3638512)
;;((ASIA . "place_name") . 0.3986908)
;;((ROOSEVELT . "title") . 0.25974715)
;;((MOSCOW . "place_name") . 0.48412973)
;;((SENATOR . "title") . 0.35631225)
;;((UNIVERSITY . "institution") . 0.5805004)
;;((SHERIFF . "title") . 0.22805552)
;;NIL


;;;;; question 2 part d)
;;; Storing centroid as hashtable is a wise choice as for each unlabelled instance, we need to compute similarity based on the
;;; dimensions that are present in both un-labelled instance and centroid. Hence for each dimension in un-labelled instance , it requires o(1)
;;; to find the same dimension in centroid and retrieve its value. Using list or 2-D array would require o(n) for each dimension present in un-labelled instance.


             ;;;;;; question 3 Classification theory ;;;;;;;;
             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; part a: Rocchio classifier is a linear classifier in which a hyperplane is the decision boundary for separating classes.
  ;;Rocchio assumes that classes belongs to sphere of equal radii and fails to deal if
  ;; if the classes are dispersed over non-contiguous regions.While KNN belongs to non-linear classifiers and based on non-linear function
  ;;, the decision boundaries in this case are complex , non-linear and discontinous.
  ;; Rocchio classifier is robut and fast but may fail if the problem is not linearly separable. Whereas KNN is able to handle complex dependices
  ;;but is prone to overfitting.



;; part b) K-means clustering is unsupervised learning as points have no external classification while Rocchio classifier is based on supervised learning
;; where each training data points is already labelled with its class name. The basic idea for both algorithms is same , k-means tries to find the points
;; such that each point in a cluster tend to be near to each other. Similariy Rocchio finds the centre of mass for each class "centroid" and assign the
;; point to class if distance between point and class is minimum. Both techniques tries to maximize high intra-class or (intra-cluster) similarity by computing centroid for each class/cluster.
;; Both are based on linear-decision boundaries


;; part c) Evaluation

;; First step to evaluate a classifier is to partition the data into 2 sets 1) training data set 2) testing datset
;;  1.Holdout Method
;;  We can take our dataset and partition it into two, randomly selecting instances for a training set (usually 2/3 of the original dataset) and a test set (1/3 of the dataset)
;; Building the classifier on training set and evaluting using test set

;; 2. Repeated hold out : we can repeat this procss several times with randomly selecting testing and training dataset

;;3. Cross-validation : We can parition our datset into k-parition. Each subset is used in turn as the test set, with the remaining subsets being the training set.


;; after dividing our original dataset, using any of the above mentioned techniques, then we can apply several strategies for evalution.

;; We can evalute classifier performance by calculating a confusion matrix, also known as contingency table.
;;  TP: Number of True positives
;;  FP: Number of False positives
;;  TN: Number of True Negatives
;;  FN: Number of False Negative
;;  Accuracy = (TP + TN)/n
;;  Precision =  TP / ( TP + FP)
;;  Recall = TP / ( TP + FN )
;; (With c classes the confusion matrix becomes an c × c matrix containing the c correct classifications (the major diagonal entries) and c2 − c possible errors (the off-diagonal entries)

;;  The results of above mentioned measures varies from case to case and also distribution of data points in different classes will biase the results.

;; For multi-class text classification system  as in this exercise , we can use F-measure which is weighted combination of precision and recall. We can use micro and macro averaged F measures.
;; Micro F1 is a weighted combination of precision and recall that gives equal weight to every document.
;; Macro F1 gives equal weight to every class.
;; The traditional F-measure or balanced F-score (F1 score) is the harmonic mean of precision and recall.
;;
;; F = 2.  ( ( precision . recall) / (precision + recall ) )
