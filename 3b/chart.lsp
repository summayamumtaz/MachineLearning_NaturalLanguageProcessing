;;; Hey, emacs, this file is -*- Mode: common-lisp; -*- ... got that?

(in-package :common-lisp-user)

(defparameter *svn-directory*
  (make-pathname :directory "~/lib/teaching/uio.inf4820.2017/public/3b"))

(defstruct grammar
  ;;


  (lexemes (make-hash-table :test 'equalp))
  (rules (make-hash-table :test 'equalp))
  ;; _fix_me_
  ;; fill in the rest of what is needed: a place to store rules and lexemes at
  ;; least, possibly also indices to make finding rules or lexemes easier
  ;;
  )

(defstruct rule
  lhs rhs (probability 1))

(defstruct lexeme
  category (probability 1))

;;
;; a minimum count (i.e. raw frequency) required for inclusion of rules in the
;; grammar; increasing this value will make the grammar smaller and faster to
;; process, maybe at the cost of grammatical coverage of rare constructions.
;;
(defparameter *rule-frequency-threshold* 0)



(defun rules-starting-in (category grammar)
;; return a list containing all grammar rules with `category' as the first
;; thing on the right hand side (i.e. the first category after the arrow)
  (let ((list '()))

	     (if  (gethash category (grammar-rules grammar))
		  (loop for k being the hash-keys of (gethash category (grammar-rules grammar)) using (hash-value v)
		      do (push v list)
		  finally
		  (return list )))))




  (defun get-lexemes (word grammar)

  ;; return a list of lexemes (from the global grammar) for the given word
  ;;
  	    (if (gethash word (grammar-lexemes grammar))
  		   (loop for k being the hash-keys of (gethash word (grammar-lexemes grammar)) using (hash-value v)
  			do ( print v))))



(defun read-grammar (filename)

  ;;; (read-grammar) requires following helper functions
  ;;; add-lexemes
  ;;; add-rules
  ;;; parse-tree
  ;;; log-conversion


          (let ( (grammar (make-grammar :lexemes (make-hash-table :test 'equalp)
                :rules (make-hash-table :test 'equalp))))
           (with-open-file (stream filename :direction :input)
             (loop
          for
          line = (read stream nil :eof)
          while (consp line)
          do(let  ( (stree (cons 'start (cons line nil))))

                   ( parse-tree stree grammar))
            finally
            (return  (log-conversion  grammar)))))
  )



;;;;;;;;; add lexemes to grammar
(defun add-lexemes (lhs word g)

	   ;; check if word is present
	   (cond

	       ( (gethash word (grammar-lexemes g))
		 (let ( (valhash (gethash word (grammar-lexemes g))))
		        (if (gethash lhs valhash)
			    (incf (lexeme-probability (gethash lhs valhash)))
		            (setf (gethash lhs valhash) (make-lexeme :category lhs)))))



	       ( (not (gethash word (grammar-lexemes g)))
		 (setf (gethash word (grammar-lexemes g)) (make-hash-table :test 'equalp))
	      ;; add lexeme
		 (let ( (valhash  (gethash word (grammar-lexemes g))))
		      ;; set the lexeme and add category as key in hashtable
		      (setf (gethash lhs valhash)  (make-lexeme :category lhs ))))

	       ))

;;; add0rules

         (defun add-rules (lhs rhs grammar)
         	   ( if (equal lhs rhs)
         		nil          ;; ignore if unary recursive rule
         	   	(if (gethash (first rhs) (grammar-rules grammar))   ;; rhs found --> match entire RHS & increment count
         		    (let ( (minhash (gethash (first rhs) (grammar-rules grammar))))
         		         (if (gethash rhs minhash)
         			     (incf  (rule-probability (gethash rhs minhash)))
         			     (setf (gethash rhs minhash) (make-rule :lhs lhs :rhs rhs))))

         		     ;; create hashtable for all rules starting with same first RHS element
         		    (let
                                 ( (minhash (setf (gethash (first rhs) (grammar-rules grammar)) (make-hash-table :test 'equalp))))

         			(setf (gethash rhs minhash) (make-rule :lhs lhs :rhs rhs))))))

;;;;;;;;; parse tree


(defun parse-tree (tree grammar)
  (let ( (lhs (first tree))
	        (rhs))
	     (dolist (child (rest tree))
	     (if (consp child)    ;; change condition
		      (push (first child) rhs)
	        (add-lexemes lhs child grammar)))

	     (if (consp rhs)
	     (add-rules  lhs (reverse rhs) grammar )))

  (dolist (child (cdr tree))
    (if (consp child)
        (parse-tree child grammar))))

        ;;;;;;;;; log conversion rules n lexemes together

        (defun log-conversion (grammar)
        	   (let (( countlist '() ))
        	    (loop for k being the hash-keys of (grammar-rules grammar) using (hash-value v)
        	        do  (loop for k being the hash-keys of v using (hash-value v1)
        			 do(let

        			       ( (temp (rule-lhs v1)))
        			         (if (assoc temp countlist)
        				     (rplacd (assoc temp countlist) (+ (rest (assoc temp countlist)) (rule-probability v1)))
        				     (push (cons temp (rule-probability v1)) countlist))))
        	    )


        	    ;;;; loop again to divide total counts by probabilities
        	     (loop for k being the hash-keys of (grammar-rules grammar) using (hash-value v)
        	        do  (loop for k being the hash-keys of v using (hash-value v1)
        		     do(let
        			   ( (total (cdr  (assoc (rule-lhs v1) countlist))))
        			   (setf (rule-probability v1) #+:debug (/  (rule-probability v1)  total)
                                                               #-:debug (log (/ (rule-probability v1) total)))))
        	     ))


        	   ;;;;;; convert lexeme probabilities
                   (let (( countlist '() ))
        	    (loop for k being the hash-keys of (grammar-lexemes  grammar) using (hash-value v)
        	        do  (loop for j being the hash-keys of v using (hash-value v1)
        			 do(let

        			       ( (temp (lexeme-category  v1)))
        			         (if (assoc temp countlist)
        				     (rplacd (assoc temp countlist) (+ (rest (assoc temp countlist)) (lexeme-probability  v1)))
        				     (push (cons temp (lexeme-probability v1)) countlist))))
        	    )


        	     ;;;; loop again to divide total counts by probabilities
        	     (loop for k being the hash-keys of (grammar-lexemes grammar) using (hash-value v)
        	        do  (loop for j being the hash-keys of v using (hash-value v1)
        		     do(let
        			   ( (total (cdr  (assoc (lexeme-category  v1) countlist))))
        			   (setf (lexeme-probability v1) #+:debug (/  (lexeme-probability v1)  total)
                                                               #-:debug (log (/ (lexeme-probability v1) total)))))
        		 finally
        		  (return grammar))))




              ;;; question 2 part b

                          ;;;;;; rules-starting-in

                          (defun rules-starting-in (category grammar)
                            ;;
                             (gethash category (grammar-rules grammar)))

                          ;;;;; get-lexemes

                          (defun get-lexemes (word grammar)

                          ;; return a list of lexemes (from the global grammar) for the given word
                          ;;
                                (if (gethash word (grammar-lexemes grammar))
                                 (loop for k being the hash-keys of (gethash word (grammar-lexemes grammar)) using (hash-value v)
                                do ( print v)))
                            )



                          ;;;; output
                          ;;CL-USER> (setf toy (read-grammar "toy.mrg"))
                          ;;CL-USER> (rules-starting-in 'NP toy)
                          ;;(#S(RULE :LHS START :RHS (NP) :PROBABILITY -1.5404451)
                          ;;#S(RULE :LHS S :RHS (NP VP) :PROBABILITY 0.0))

                          ;; output
                          ;;(get-lexemes "flies" toy)

                          ;;#S(LEXEME :CATEGORY NNS :PROBABILITY -2.0794415)
                          ;;#S(LEXEME :CATEGORY VBZ :PROBABILITY -1.9459101)
                          ;;NIL

                          ;; output for larger file wsj.mrg
                          ;;CL-USER> (get-lexemes "flies" wsj)

                          ;;#S(LEXEME :CATEGORY VBZ :PROBABILITY -8.192017)
                          ;;NIL


                          ;;;;; question 3
                          ;;; part a)
                          ;;; print leaf nodes of tree

                           (defun leaves (tree)
                          	   (let
                          	       ( (lst '()))
                          	     (labels ( (collect-leaves  (tree )

                          	     (dolist (child (rest tree))
                          	     (if (consp child)    ;; change condition
                          		 nil
                          		(push child lst
                          		      )
                          	        ))

                          	    (dolist (child (cdr tree))
                          	      (if (consp child)
                          		  (collect-leaves child)))))

                          	       (collect-leaves tree))
                          	     (nreverse lst)))

                                 ;;; output

                            ;;     CL-USER> (leaves '(START
                            ;;       (S (NP (NNP "Frodo"))
                            ;;        (VP (VBZ "eats")
                            ;;         (NP (NN "wasabi") (PP (P "with") (NP (NNS "chopsticks"))))))))
                            ;;  ("Frodo" "eats" "wasabi" "with" "chopsticks")



                                    ;;;;;; part b ;;;;;;;;;;
                                    ;;;;;; part b ;;;;;;;;;


                          ;; requires helper function traverse that decomposed tree into list composed of brackets (category , index of first word, length)

                          ;;  stores category --> first element tree and find all leaves for rest of tree
                          ;;; convert thesese values into (categor , index , length )
                          					(defun traverse (tree)
                          						   (let
                          						       ( (lst )                       ;; stores category (index length)
                          							 (leavset (leaves tree)))           ;; to find index of leaves in sub-trees


                          						     (labels ( (collect-brackets  (tree )
                          					                        (if (consp (rest tree))
                          								 (let
                          								    ( (temp (leaves tree))
                          								         )
                          								 (push (cons (first tree)  (cons  (position  (first temp) leavset :test #'equal) (length temp))) lst)))

                          								 (dolist (child (rest tree))
                          								   (if (consp (first (rest child)))       ;; skip tag for leaf node --> (NP "frodo")
                          								   (collect-brackets child)
                          								   nil))
                          								 ))


                          						       (collect-brackets tree))
                          						      (reverse lst)))


                               ;;;;;;;;;;;;;;;;;;;;;

                          		 (defun parseval ( parsehypo goldstandard)
                          	      (let ( (goldst (traverse goldstandard))
                          	       (parsehyp (traverse parsehypo))
                          		     (count 0))
                          	   (loop for x in goldst
                          	    do (if (position x parsehyp :test #'equal)
                          		   (incf count))
                          		)
                          	   (values count (length goldst) (length parsehyp))))


                          		 ;;;;; output
                          ;
                          ;;		 CL-USER> (parseval '(START
                          ;;       (S (NP (NNP "Frodo"))
                          ;;        (VP (VBZ "eats")
                          ;;         (NP (NP (NN "wasabi")) (PP (P "with") (NP (NNS "chopsticks")))))))
                          ;;     '(START
                          ;;       (S (NP (NNP "Frodo"))
                          ;;        (VP (VP (VBZ "eats") (NP (NN "wasabi")))
                          ;;         (PP (P "with") (NP (NNS "chopsticks")))))))
                          ;;		7
                          ;;		8
                          ;;		8

                          ;; The label bracket  (NP (NP (NN "wasabi"))  is incorrect in parsehypothesis as gold standard has (NP (NN "wasabi")))
                          ;; Gold standard has the bracket (VP (VP (VBZ "eats") which is missing in parserhypothesis as it has (VP (VBZ "eats"). Vp first goes
                          ;; to vp --> VBZ -- eats while in parse there is only one VP going directly to VBX --> eats

                          ;; (rest of assignment will be upload after 3 days)
                          ;  Deadline extended by instructor due to medical reasons












;;;
;;; from here onwards, we provide most of the code (and generous comments),
;;; only requiring you to complete one function: fundamental-rule().  read
;;; through the rest of the code and make sure you understand how it implements
;;; the generalized chart parser we discussed in the lectures.
;;;

;;;
;;; the parse chart we use is a two-dimensional array indexed by string
;;; positions.  we use the second dimension to indicate whether we are indexing
;;; by start or end positions, and whether the edge is passive or active i.e.:
;;;
;;;   chart[i,0] is for passive edges starting at i,
;;;   chart[i,1] is for passive edges ending at i,
;;;   chart[i,2] is for active edges starting at i; and
;;;   chart[i,3] is for active edges ending at i
;;;

(defun chart-cell (from to chart &optional activep)
  ;;
  ;; given a start and end vertex (i.e. sub-string .from. and .to. indices),
  ;; retrieve the relevant chart edges (defaulting to passive edges only)
  ;;
  (loop
      for edge in (append
                   (aref chart from 0) (and activep (aref chart from 2)))
      when (= (edge-to edge) to) collect edge))

(defun passive-edges-from (index chart)
  ;;
  ;; for a given chart vertex (aka string from position), retrieve all the
  ;; passive edges from the chart that start at that vertex
  ;;
  (aref chart index 0))

(defun active-edges-to (index chart)
  ;;
  ;; for a given chart vertex (aka string to position), retrieve all the
  ;; active edges from the chart that end at that vertex
  ;;
  (aref chart index 3))

(defun chart-adjoin (edge chart)
  ;;
  ;; given the way we have organized our chart, inserting a new edge requires
  ;; adding it by both its from and to positions in two `rows' of our
  ;; chart implementation.
  ;;
  (let ((offset (if (passive-edge-p edge) 0 2)))
    (push edge (aref chart (edge-from edge) (+ offset 0)))
    (push edge (aref chart (edge-to edge) (+ offset 1)))))

(defstruct edge
  ;;
  ;; edges record their span and category, the daughters they have seen (in the
  ;; .daughters. slots) and the daughters they still require (.unanalyzed.).
  ;; the .alternates. slot holds other edges with the same span and category.
  ;; during forest construction, .probability. holds the (log) probability of
  ;; the associated rule.  The Viterbi function updates this to be the maximum
  ;; probability of the subtree represented by this edge.  the .cache. slot is
  ;; used in viterbi() to avoid recalculations.
  ;;
  from to category
  daughters unanalyzed
  alternates
  probability
  cache)

(defun edge-to-tree (edge)
  ;;
  ;; expands .edge. to a tree, recursing over daughters (but not alternates)
  ;;
  (if (edge-daughters edge)
    (cons (edge-category edge)
          (loop
              for daughter in (edge-daughters edge)
              collect (edge-to-tree daughter)))
    (edge-category edge)))

(defun passive-edge-p (edge)
  ;;
  ;; passive edges have seen all their daughters
  ;;
  (null (edge-unanalyzed edge)))

(defstruct agenda
  ;;
  ;; our agenda, for this exercise, is just a simple stack, but that could be
  ;; changed to implement another agenda strategy
  ;;
  contents
  popped)

(defun agenda-push (edge agenda)
  (push edge (agenda-contents agenda)))

(defun agenda-pop (agenda)
  (setf (agenda-popped agenda) (pop (agenda-contents agenda))))

(defun parse (input grammar)
  ;;
  ;; finally, our implementation of the the generalized chart parser
  ;;
  (let* ((agenda (make-agenda))
         (n (length input))
         (chart (make-array (list (+ n 1) 4) :initial-element nil)))

    ;;
    ;; create a `lexical' edge (one without daughters that is passive from the
    ;; start) for each word of the input sequence.  then add passive edges for
    ;; each possible word category to the  agenda.
    ;;
    (loop
        for i from 0
        for word in input
        for lexemes = (get-lexemes word grammar)
        for daughters = (list (make-edge :from i :to (+ i 1) :category word
                                         :probability 0.0))
        do
          ;;
          ;; if we have not seen all the words in training, fail immediately;
          ;; no point waisting time in filling a chart that can never complete.
          ;;
          (if (null lexemes)
            (return-from parse nil)
            (loop
                for lexeme in (get-lexemes word grammar)
                for edge = (make-edge :from i :to (+ i 1)
                                      :category (lexeme-category lexeme)
                                      :daughters daughters
                                      :probability (lexeme-probability lexeme))
                do (agenda-push edge agenda))))

    ;;
    ;; the main parser loop: explore all possible edge combintions
    ;;
    (loop
        for edge = (agenda-pop agenda)
        while edge do
          (cond
           ((passive-edge-p edge)
            ;;
            ;; for passive edges, we first try and pack into an existing edge
            ;; in the chart.  if there are no equivalent edges in the chart
            ;; yet, add this .edge., apply the fundamental rule, then predict
            ;; new edges and add them to the agenda also.
            ;;
            (unless (pack-edge edge chart)
              (chart-adjoin edge chart)
              (loop
                  for active in (active-edges-to (edge-from edge) chart)
                  do (fundamental-rule active edge agenda))
              (loop
                  with from = (edge-from edge) with to = (edge-to edge)
                  for rule in (rules-starting-in (edge-category edge) grammar)
                  for new = (make-edge :from from :to to
                                       :category (rule-lhs rule)
                                       :daughters (list edge)
                                       :unanalyzed (rest (rule-rhs rule))
                                       :probability (rule-probability rule))
                  do (agenda-push new agenda))))
           (t
            ;;
            ;; we do not attempt ambiguity packing on active edges, but instead
            ;; just add the edge to the chart and apply the fundamental rule.
            ;;
            (chart-adjoin edge chart)
            (loop
                for passive in (passive-edges-from (edge-to edge) chart)
                do (fundamental-rule edge passive agenda)))))

    ;;
    ;; the agenda is now empty, check for a passive edge that spans the input
    ;; and has a category equal to our start symbol.  seeing as there is only
    ;; one start symbol, and given the assumptions we make about equivalence
    ;; within each chart cell, there can be at most one such edge.
    ;;
    (loop
        for edge in (chart-cell 0 (length input) chart)
        when (eq (edge-category edge) (grammar-start grammar))
        return edge)))

(defun fundamental-rule (active passive agenda)
  ;;
  ;; the fundamental rule of chart parsing: given one active and one passive
  ;; edge (known to be adjacent already), check for compatibility of the two
  ;; edges and add a new edge to the agenda when successful.
  ;;
  (when (equal (edge-category passive) (first (edge-unanalyzed active)))
    (agenda-push
     (make-edge :from (edge-from active) :to (edge-to passive)
                :category (edge-category active)
                :daughters (append (edge-daughters active) (list passive))
                :unanalyzed (rest (edge-unanalyzed active))
                :probability (edge-probability active)) agenda)) )

(defun viterbi (edge)
  ;;
  ;; a recursive implementation of the Viterbi algorithm over packed forests
  ;;
  (or (edge-cache edge)
      (setf (edge-cache edge)
        (if (edge-daughters edge)
          (loop
              initially
                (setf (edge-probability edge)
                  (+ (edge-probability edge)
                     (loop
                         for daughter in (edge-daughters edge)
                         sum (edge-probability (viterbi daughter)))))
              for alternate in (edge-alternates edge)
              for probability = (edge-probability (viterbi alternate))
              when (> probability (edge-probability edge))
              do
                (setf (edge-probability edge) probability)
                (setf (edge-daughters edge) (edge-daughters alternate))
              finally (return edge))
          edge))))

(defun pack-edge (edge chart)
  ;;
  ;; if there is more than one way to derive a particular category for a
  ;; particular span, pack all alternatives into the first such edge we found.
  ;;
  (when (passive-edge-p edge)
    (loop
        ;;
        ;; look for a passive edge with the same span and category; there can
        ;; be at most one.
        ;;
        for host in (passive-edges-from (edge-from edge) chart)
        when (and (= (edge-to host) (edge-to edge))
                  (equal (edge-category host) (edge-category edge)))
        do
          ;;
          ;; if we found an equivalent edge in the chart, add the new .edge.
          ;; to our host, unless that would create a cycle, in which case,
          ;; discard our new edge.  return the `host', indicating no more
          ;; processing is necessary on this edge.
          ;;
          (unless (daughterp host edge)
            (push edge (edge-alternates host)))
          (return host))))

(defun daughterp (host edge)
  ;;
  ;; test whether .host. is (transitively) embedded as a daughter below .edge.,
  ;; to avoid creating cycles in the packed forest.
  ;;
  (loop
      for daughter in (edge-daughters edge)
      thereis (or (eq daughter host) (daughterp host daughter))))

(defun evaluate (file grammar &key (limit 10))
  ;;
  ;; read a test file, extracting gold trees and using their leaves as input
  ;; to our parser, for any sentence <= .limit. (for efficiency).  then compute
  ;; ParsEval scores to compare between the tree from the parser and the gold
  ;; tree, after first stripping our dummy start node
  ;;
  (with-open-file (stream file)
    (loop
        with inputs = 0 with analyses = 0
        with tcorrect = 0 with tfound = 0 with tgold = 0
        for gold = (read stream nil nil)
        while gold do
          (let* ((leaves (leaves gold))
                 (n (length leaves)))
            (when (<= n limit)
              (incf inputs)
              (let* ((start (get-internal-run-time))
                     (parse (parse leaves grammar))
                     (end (get-internal-run-time))
                     (tree (when parse (edge-to-tree (viterbi parse))))
                     ;;
                     ;; discard the top-level node, which is the start symbol
                     ;;
                     (tree (when (consp tree) (first (rest tree)))))
                (multiple-value-bind (correct found gold) (parseval tree gold)
                  (format
                   t "~a. [~a] |~{~a~^ ~}| (~,2fs) P=~,2f R=~,2f~%"
                   inputs n leaves
                   (/ (- end start) internal-time-units-per-second)
                   (if (zerop found) 0 (/ correct found)) (/ correct gold))
                  (when parse
                    (incf analyses)
                    (incf tcorrect correct)
                    (incf tfound found))
                  (incf tgold gold)))))
        finally
          (let* ((precision (if (zerop tfound) 1 (/ tcorrect tfound)))
                 (recall (/ tcorrect tgold))
                 (fscore (/ (* 2 precision recall) (+ precision recall))))
            (format
             t "== ~a input~p; ~,2f% coverage; P=~,2f R=~,2f F1=~,2f~%"
             inputs inputs (/ analyses inputs) precision recall fscore)
            (return (float fscore))))))

(defun parseval (tree gold)
  ;;
  ;; _fix_me_
  ;; ParsEval compares trees by extracting all constituents (identified by start
  ;; and end positions, and category) from each tree and counting the overlap
  ;; (correct) as well as the total constituents in each tree.
  ;;
  )

(defun leaves (tree)
  ;;
  ;; _fix_me_
  ;; extract the leaf nodes (i.e. the surface string) from a tree
  ;;
  )
