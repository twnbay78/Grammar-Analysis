;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  PROJECT 2
;;                           LL(1) Parser for Scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Requires R5RS to run

;; Loads the given utility functions in utilFuncs.ss
(load "utilFuncs.ss")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 1: Utility functions

;; getStartSymbol returns the start nonterminal of the grammar
;; As defined in the project description, the start nonterminal is the left-hand side nonterminal of the first rule of the grammar.
;; (This function is useful for generating initial FOLLOW sets)
;; Input: grammar
;; Output: the start non-terminal of the grammar.


(define getStartSymbol
  (lambda (grammar)
	;; YOUR CODE GOES HERE
    (car ;; grab first NT
		(car grammar)) ;; grab first NT + first production rule
   )
)



;; Returns a list of all nonterminals in the grammar
;; You may assume that every nonterminal in the grammar will have a rule.
;; input: the grammar
;; Output: the list of all nonterminals in the grammar: '(NT1 NT2 ... NTk)


(define getNonterminals	
  (lambda (grammar)
	;; YOUR CODE GOES HERE
    (if (null? grammar)	;; If grammar is empty...
      '()	;; return empty list
      (cons (getStartSymbol grammar) (getNonterminals (cdr grammar)))	;; else, append first NT with all other NT's
	)
  )
)


;; Returns a list of all production rules in the grammar.
;; If a rule happens to look like lhs ::= rhs1 | rhs2, getProductions will separate those two rules into different elements of the list
;; Input: the grammar
;; Output: the list of all production rules in the grammar
;; Output format example:
;;    Given an example grammar:
;;          NT1 ::= rhs1 | rhs2
;;          NT2 ::= rhs3
;;    Which is of the form
;;          '((NT1 (rhs1) (rhs2)) (NT2 (rhs3)))
;;    getProductions should return '((NT1 rhs1) (NT1 rhs2) (NT2 rhs3))


(define getProductions
  (lambda (grammar)
	;; YOUR CODE GOES HERE
    (if (null? grammar)	;; f grammar is empty...
        '()	;; return empty list
        (if (= (length (car grammar)) 1)	;; If there is only one production rule...
            (getProductions (cdr grammar))
            (cons (cons (car (car grammar)) (cons (car (cdr (car grammar))) '())) ;; appends NT to the first production rule
				(getProductions (cons (removeMatch (car grammar) (car (cdr (car grammar)))) (cdr grammar)))	;; remove the front production rule for every recursive call and appends the next
			)
		)
	)
  )
 )


;; An associative list assocList is a list of pairs of the form '((a List_a) (b List_b) ... (k List_k)).
;;    Each pair has the form '(symbol list), where, for example, list can be the symbol's FIRST set or FOLLOW set.
;; updateAssocList updates the associative list by union-ing set with List_s given symbol s and returns the modified assocList
;;    If (s List_s) is not in assocList, just return assocList
;; Inputs:
;;    assocList: the associative list, a list of pairs of the form '((a List_a) (b List_b) ... (k List_k)).
;;    symbol: the symbol whose list, List(symbol), will be union-ed with set
;;    set: the set to be union-ed to the corresponding symbol
;; Output: the new assocList where the corresponding list for the symbol, is now List(symbol) U set.
;; Output format example:
;;    Given the following associative list assocListEx = '((a (2 3 4)) (b (4 5)))
;;    (updateAssocList assocListEx a '(4 5)) should return '((a (2 3 4 5)) (b (4 5)))

(define updateAssocList
  (lambda (assocList symbol set)
	;; YOUR CODE GOES HERE
    (if (null? set)
        assocList ;; set is empty, nothing to update, return input 
        (if (eq? (car (car assocList)) symbol) ;; find which list to update 
            (cons (cons (car (car assocList)) (cons (union (Car (cdr (car assocList))) set) '())) (cdr assocList)) ;; unions the set with the list, and joins it with the symbol
            (cons (car assocList) (updateAssocList (cdr assocList) symbol set)) ;; else, continue searching the list and append the first lists to the rest of the lists (which may or may not need updating)
		)
	)
  )
)

;; Depending on the setting (either 'first or 'follow), getInitSets returns the initialized FIRST sets or FOLLOW sets for all non-terminals in the grammar
;; Inputs:
;;    NTS: the list of all nonterminals in the grammar
;;    startSymbol: the start nonterminal of the grammar
;;    setting: either 'first or 'follow
;; Output: Depending on what the setting is, initSets returns an initialized list of empty FIRST sets or FOLLOW sets for each nonterminal of the grammar
;; Output format example:
;;    Given an example grammar:
;;          NT1 ::= rhs1 | rhs2
;;          NT2 ::= rhs3
;;    Which is of the form
;;          '((NT1 (rhs1) (rhs2)) (NT2 (rhs3)))
;;    (getInitSets '(NT1 NT2) NT1 'first) returns '((NT1 ()) (NT2 ()))
;;    (getInitSets '(NT1 NT2) NT1 'follow) returns '((NT1 (eof)) (NT2 ()))
(define getInitSets
  (lambda (NTs startSymbol setting)
	;; YOUR CODE GOES HERE
    (if (eq? setting 'first) ;; if first set is calling
      (if (null? NTs) ;; no non-terminals left
          '()
          (if (eq? (car NTs) startSymbol) ;; if start symbol...
              (cons (cons startSymbol (cons '() '())) (getInitSets (cdr NTs) startSymbol setting)) ;; append start symbol to begining of list and recurse on remaining NT's
              (cons (cons (car NTs) (cons '() '())) (getInitSets (cdr NTs) startSymbol setting)))) ;; else, append rest of the list to the NT's 
      (if (null? NTs) ;; for follow sets.. if no NT's, return empty list
          '()
          (if (eq? (car NTs) startSymbol) ;; same as above....
              (cons (cons startSymbol (cons '(eof) '())) (getInitSets (cdr NTs) startSymbol setting)) ;;except start symbol is initizialized with eof for follow set
              (cons (cons (car NTs) (cons '() '())) (getInitSets (cdr NTs) startSymbol setting))
		  )
		)
	)
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 2: Generating FIRST sets

;; genFirstFunc returns a function that computes FIRST(symbolSeq) given a sequence of symbols (terminals and nonterminals) and an initial list of FIRST sets for all nonterminals in the grammar.
;; Input:
;;    NTs: the list of all nonterminals in the grammar
;; Output:
;;    a function first that takes as input a sequence of symbols symbolSeq, and an initial list of FIRST sets firstSets, and outputs FIRST(symbolSeq)
;;        Input:
;;            symbolSeq: sequence of symbols
;;            firstSets: a list of (potentially unfinished) FIRST sets for all nonterminals in the grammar
;;        Output:
;;            FIRST(SymbolSeq): (first symbolSeq firstSets)
(define genFirstFunc
  (lambda (NTs)
    (letrec ((first ;; binding the returned function 
              (lambda (symbolSeq firstSets);; returned function takes in symbolseq and firstsets 
                (if (null? symbolSeq);; no sequence of symbols in symbolseq... empty list...
                    '()
                    (if (contains? NTs (car symbolSeq)) ;; if the NT's in the grammar are matched with the symbols in symbolseq..
                        (car  (cdr (assoc (car symbolSeq) firstSets))) ;; make an associated pair of the first symbol of the sequence and the first set
                        (list (car symbolSeq))));; else return list of symbols 
              )))
      first)))
;; recurseFirstSets goes through each rule in the grammar and updates the FIRST sets based on the current rule
;; Inputs:
;;     rules: the list of all production rules in the grammar
;;     firstSets: a list of (potentially unfinished) FIRST sets
;;     firstFunc: a function that takes as input a sequence of symbols and list of FIRST sets, and returns FIRST(sequence of symbols).
;;        You will pass firstFunc as a function. (This should be written in genFirstFunc)
;; Output: an updated firstSets after making one pass through all the rules in the grammar
(define recurseFirstSets
  (lambda (rules firstSets firstFunc)
    (if (null? rules) ;; no rules left in the grammar... base case...
        firstSets  ;; return "current" first sets
        (let ((rhsRule (car (cdr (car rules))));; gets the first terminal of the first production rule and sets rhsrule 
              (lhsRule (car (car rules))));; NT of the first rule 
          (recurseFirstSets (cdr rules) (updateAssocList firstSets lhsRule (firstFunc rhsRule firstSets)) firstFunc);; gets other rules and updates the associated list with the updated first sets and calls recursively for every rule 
        ))))

;; getFirstSets returns the FIRST sets of all nonterminals in the grammar if the FIRST sets had no change
;; Inputs:
;;     rules: the list of all production rules in the grammar
;;     firstSets: a list of (potentially unfinished) FIRST sets
;;     firstFunc: a function that takes as input a sequence of symbols and list of FIRST sets, and returns FIRST(sequence of symbols).
;; Output: the updated firstSets, which is a list of the FIRST sets of every non-terminal in the grammar
(define getFirstSets
  (lambda (rules firstSets firstFunc)
	;; YOUR CODE GOES HERE
    (let ((newSet (recurseFirstSets rules firstSets firstFunc)))
      (if (equal? firstSets newSet)  ;; while the first set is not changing
          firstSets ;; return the current first set
          (getFirstSets rules newSet firstFunc)) ;; recurse for the new first sets that are changing (while (FIRST sets are still changing)) 
	)
  )
) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 3: Generating FOLLOW sets

;; genFollowFunc returns a function that computes FOLLOW(symbolSeq) given a sequence of symbols (terminals and nonterminals) and an initial list of FOLLOW sets for all nonterminals in the grammar.
;; Input:
;;    NTs: the list of all nonterminals in the grammar
;; Output:
;;    a function follow that takes as input a sequence of symbols symbolSeq, and an initial list of FOLLOW sets, the variable trailer, and the list of COMPLETED FIRST sets firstSets, and outputs FOLLOW(symbolSeq)
;;        Input:
;;            symbolSeq: sequence of symbols
;;            followSets: a list of (potentially unfinished) FOLLOW sets for all nonterminals in the grammar
;;            trailer: trailer variable(Check the algorithm on FOLLOW sets)
;;            firstSets: list of COMPLETED FIRST sets for all nonterminals in the grammar
;;        Output:
;;            FOLLOW(symbolSeq): (follow symbolSeq followSets trailer firstSets)
;;(define genFollowFunc
;;  (lambda (NTs)
	;; YOUR CODE GOES HERE
;;	)
;;)


;; recurseFollowSets goes through each rule in the grammar and updates the FOLLOW sets based on the current rule
;; Inputs:
;;     rules: the list of all production rules in the grammar
;;     followSets: a list of (potentially unfinished) FOLLOW sets
;;     followFunc: a function that takes as input a sequence of symbols and list of FOLLOW sets, and returns FOLLOW(sequence of symbols).
;;        You will pass followFunc as a function. (This should be written in genFollowFunc)
;; Output: an updated followSets after making one pass through all the rules in the grammar
(define recurseFollowSets
  (lambda (rules followSets firstSets followFunc)
    (if (null? rules);; base case: no rules left to recurse 
         followSets ;; return current follow set
        (let* ((rhsRule (reverse (car (cdr (car rules)))));; reverse rhs and get the front (which is the end of the rules)
               (lhsRule (car (car rules)));; get NT
               (trailer (cadr (assoc lhsRule followSets))));; TRAILER <- FOLLOW 
          (recurseFollowSets (cdr rules) (followFunc rhsRule followSets trailer firstSets) firstSets followFunc);; recurse for all rules in grammar
))))

;; getFollowSets returns the FOLLOW sets of all nonterminals in the grammar if the FOLLOW sets had no change
;; Inputs:
;;     rules: the list of all production rules in the grammar
;;     followSets: a list of (potentially unfinished) FOLLOW sets
;;     followFunc: a function that takes as input a sequence of symbols and list of FOLLOW sets, and returns FOLLOW(sequence of symbols).
;; Output: the updated followSets, which is a list of the FOLLOW sets of every non-terminal in the grammar
(define getFollowSets
  (lambda (rules followSets firstSets followFunc)
	;; YOUR CODE GOES HERE
    (let ((newSet (recurseFollowSets rules followSets firstSets followFunc))) 
        (if (equal? followSets newSet) ;;If the follow sets have no change
            followSets ;; then return the follow sets
            (getFollowSets rules newSet firstSets followFunc)
		)
	)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Section 4: Generating PREDICT sets

;; Generates the PREDICT sets for each rule in the grammar
;; Inputs:
;;     rules: the list of all production rules of the grammar
;;     NTs: the list of all nonterminals in the grammar
;;     firstSets: the FIRST sets of all nonterminals in the grammar
;;     followSets: the FOLLOW sets of all nonterminals in the grammar
;;     firstFunc: a function that takes as input a sequence of symbols and list of FIRST sets, and returns FIRST(sequence of symbols).
;; Output: a list of pairs, one for each production rule in the grammar, where the first element is the production rule output as a list, and the second element is the PREDICT set for that production rule
;; Output format example:
;;    Given an example grammar:
;;          A ::= xB | "eps"
;;          B ::= yA | "eps"
;;    Which is of the form
;;          ’((A (x B) ()) (B (y A) ()))
;;    getPredictSets should return
;;          '(
;;            ((A ::= (x B)) (x))
;;            ((A ::= ("eps")) (eof))
;;            ((B ::= (y A)) (y))
;;            ((B ::= ("eps")) (eof)))
;; You MUST use let* for this problem:
;;    How do you define A? How do you define delta? How do you define FIRST(delta)?
;;(define getPredictSets
;;  (lambda (rules firstSets followSets firstFunc)
	;; YOUR CODE GOES HERE
;;  )
;;)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 5: Tester functions
;; Example Grammars
(define symbol 'a)
(define sett 'follow)
(define assocListEx '((a (2 3 4)) (b (4 5))))
(define grammar0 '((start (a))))
(define grammar1 '((a (x b) ("eps")) (b (y a) ("eps"))))
(define grammar2 '((start (expr)) (expr ("+" term term)) (term (a) (b) (c))))
(define grammar3 '((start (stmts))
                   (stmts (assgn morestmts))
                   (morestmts ("," stmts) ("eps"))
                   (assgn (var "=" value))
                   (var (a) (b) (c) (d) (e))
                   (value (0) (1) (2) (3) (4) (5) (6) (7) (8) (9))))

;; For the following commands, remember to comment them out using ";" before handing in the assignment.
;;    Your proj2.ss file should NOT output anything. These commands are only used to help you test your functions

;; Testing the grammars
;; To test the example grammars, substitute "grammar0" in (define grammar grammar0) with the appropriate grammar<num>.
;; Comment out the appropriate commands to test each function individually.
(define grammar grammar3)
(define NTs (getNonterminals grammar))
(define rules (getProductions grammar))
(define startSymbol (getstartSymbol grammar))
(define firstFunc (genFirstFunc NTs))
;;(define followFunc (genFollowFunc NTs))
(define firstSets (getFirstSets rules (getInitSets NTs startSymbol 'first) firstFunc))
;;(define followSets (getFollowSets rules (getInitSets NTs startSymbol 'follow) firstSets followFunc))
;;(define predictSets (getPredictSets rules firstSets followSets firstFunc))
(display "First sets\n")
firstSets
;;(display "Follow sets\n")
;;followSets
;;(display "Predict sets\n")
;;predictSets