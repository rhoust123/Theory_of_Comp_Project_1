;;;;;;;;;;;;;;;;;;;
;;;; Utilities ;;;;
;;;;;;;;;;;;;;;;;;;

(defun hash-table-keys (hash-table)
  "Return the hash table's keys as a list."
  (let ((result nil))
    (maphash (lambda (key value)
               (declare (ignore value))
               (push key result))
             hash-table)
    result))

(defun make-symbol-hash-table ()
  "Convenience function to make a hash table for FA symbols."
  (make-hash-table :test #'equal))

(defun fold-left (function initial-value list)
  "Convenience function for fold-left with an initial value."
  (reduce function list :initial-value initial-value))

(defun TODO (thing)
  "Placeholder for code to implement."
  (error "Unimplemented: ~A" thing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; STARTER DEFINITIONS FOR FINITE AUTOMATA ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A structure type for finite automata
(defstruct finite-automaton
  "A Finite Automaton."
  states    ; state set as a list
  alphabet  ; input alphabet as a list
  edges     ; list of edges: (state-0 input-symbol state-1)
  delta     ; transition function : state-0 * input-symbol -> state-1
  start     ; start state
  accept)   ; accept set as a list

(defun make-fa (edges start accept)
  "Convenience constructor for finite automata"
  (flet ((add-state (hash state)
           (setf (gethash state hash) t)
           hash)
         (add-edge-states (hash edge)
           (destructuring-bind (state-0 input-symbol state-1) edge
             (declare (ignore input-symbol))
             (setf (gethash state-0 hash) t
                   (gethash state-1 hash) t))
           hash)
         (add-edge-input-symbol (hash edge)
           (destructuring-bind (state-0 input-symbol state-1) edge
             (declare (ignore state-0 state-1))
             (setf (gethash input-symbol hash) t))
           hash)
         (add-edge-transition (hash edge)
           (destructuring-bind (state-0 input-symbol state-1) edge
             (push state-1 (gethash (cons state-0 input-symbol) hash)))
           hash))
    (let ((state-hash (fold-left #'add-edge-states
                                 (fold-left #'add-state
                                            (add-state (make-symbol-hash-table)
                                                       start)
                                            accept)
                                 edges))
          (alphabet-hash (fold-left #'add-edge-input-symbol
                                    (make-symbol-hash-table)
                                    edges))
          (edge-hash (fold-left #'add-edge-transition
                                (make-symbol-hash-table)
                                edges)))
      (make-finite-automaton
       :states (hash-table-keys state-hash)
       :alphabet (hash-table-keys alphabet-hash)
       :edges edges
       :delta (lambda (state-0 input-symbol)
                (gethash (cons state-0 input-symbol) edge-hash))
       :start start
       :accept accept))))


;;; Higher-order conveience functions for Finite Automata ;;;

(defun map-fa-states (function fa)
  "Map FUNCTION over the FA's states."
  (map 'list function (finite-automaton-states fa)))

(defun map-fa-accept (function fa)
  "Map FUNCTION over the FA's accept states."
  (map 'list function (finite-automaton-accept fa)))

(defun fold-fa-states (function initial-value fa)
  "Fold FUNCTION over the FA's states."
  (fold-left function initial-value (finite-automaton-states fa)))

(defun fold-fa-alphabet (function initial-value fa)
  "Fold FUNCTION over the FA's alphabet."
  (fold-left function initial-value (finite-automaton-alphabet fa)))

(defun map-fa-edges (function fa)
  "Map FUNCTION over the FA's edges."
  (map 'list
       (lambda (edge)
         (destructuring-bind (state-0 input-symbol state-1) edge
           (funcall function state-0 input-symbol state-1)))
       (finite-automaton-edges fa)))

;;; Graphviz Output ;;;

(defun fa-dot (fa place)
  "Output a Graphviz dot file of FA."
  (let ((hash (make-symbol-hash-table)))
    ;; number the states
    (fold-fa-states (lambda (i state)
                      (setf (gethash state hash) i)
                      (1+ i))
                    0 fa)
    ;; Output the Graphviz dot file
    (labels ((state-number (state)
               (gethash state hash))
             (dot-symbol (thing) ; Pretty-print Greek letters
               (case thing
                 (:alpha "&alpha;")
                 (:beta "&beta;")
                 (:gamma "&gamma;")
                 (:delta "&delta;")
                 (:epsilon "&epsilon;")
                 (:zeta "&zeta;")
                 (:eta "&eta;")
                 (:theta "&theta;")
                 (:iota "&iota;")
                 (:kappa "&kappa;")
                 (:lambda "&lambda;")
                 (:mu "&mu;")
                 (:nu "&nu;")
                 (:xi "&xi;")
                 (:omicron "&omicron;")
                 (:pi "&pi;")
                 (:rho "&rho;")
                 (:sigma "&sigma;")
                 (:tau "&tau;")
                 (:upsilon "&upsilon;")
                 (:phi "&phi;")
                 (:chi "&chi;")
                 (:omega "&omega;")
                 (t thing)))
             (helper (stream)
               ;; output
               (format stream "~&digraph { ~%")
               ;; state labels
               (format stream "~:{~&  ~A[label=\"~A\"];~}"
                       (map-fa-states (lambda (state)
                                        (list (state-number state)
                                              state))
                                      fa))
               ;; start state
               (format stream "~&  start[shape=none];")
               (format stream "~&  start -> ~A;"
                       (state-number (finite-automaton-start fa)))
               ;; accept state
               (format stream "~:{~&  ~A [ shape=~A ];~}"
                       (map-fa-accept (lambda (q)
                                        (list (state-number q) "doublecircle"))
                                      fa))
               ;; edges
               (format stream "~:{~&  ~A -> ~A [fontsize=~D,label=\"~A\"];~%~}"
                       (map-fa-edges (lambda (state-0 input-symbol state-1)
                                       (list (state-number state-0)
                                             (state-number state-1)
                                             12 (dot-symbol input-symbol)))
                                     fa))
               ;; end
               (format stream "~&}~%")))
      (cond
        ((streamp place)
         (helper place))
        ((eq place t)
         (helper *standard-output*))
        ((null place)
         (with-output-to-string (s)
           (helper s)))
        ((or (stringp place)
             (pathnamep place))
         (with-open-file (stream place
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
         (helper stream)))
        (t (error "Unrecognized output type: ~A" place))))))



#+sbcl
(defun fa-pdf (fa filename)
  "Create a PDF of FA."
  (with-input-from-string (text (fa-dot fa nil))
    (sb-ext:run-program "dot" (list "-Tpdf")
                        :search t
                        :input text
                        :output filename
                        :if-output-exists :supersede)))


;;; Finite Automata Helper Functions ;;;

(defun fa-transition (fa state-0 input-symbol)
  "Return the list of successors of STATE-0 on INPUT-SYMBOL."
  (funcall (finite-automaton-delta fa)
           state-0 input-symbol))

(defun dfa-transition (fa state-0 input-symbol)
  "Return the successor of STATE-0 on INPUT-SYMBOL."
  (assert (not (eq input-symbol :epsilon)))
  (let ((successors (fa-transition fa state-0 input-symbol)))
    ;; DFA cannot have multiple successors, or it would be
    ;; nondeterministic
    (assert (or (null successors)
                (null (cdr successors))))
    (car successors)))

(defun newstate (&optional (arg "Q-"))
  "Construct a unique state for a finite automaton."
  (gensym arg))

(defun dfa-p (fa)
  "Is FA is deterministic?"
  (labels ((rec (hash edges)
             ;; Recurse over the edges and check for nondeterministic
             ;; transitions.
             ;; hash : (CONS state-0  input-symbol) -> (or T NIL)
             (if edges
                 (destructuring-bind ((state-0 input-symbol state-1) &rest edges) edges
                   (declare (ignore state-1))
                   (let ((key (list state-0 input-symbol)))
                     (unless (or (eq input-symbol :epsilon)
                                 (gethash key hash))
                       (setf (gethash key hash) t)
                       (rec hash edges))))
                 t)))
    (and (finite-automaton-p fa)
         (rec (make-symbol-hash-table)
              (finite-automaton-edges fa)))))

(defun dfa-add-reject (dfa &optional (alphabet (finite-automaton-alphabet dfa)))
  "Add a reject state to DFA."
  (assert (dfa-p dfa))
  ;; Index non-dead-state edges
  ;; hash : (CONS state-0 input-symbol) -> (OR T NIL)
  (let ((dead-state (newstate "dead")))
    ;; Create edges to a dead-state
    (let ((edges (fold-left (lambda (edges input-symbol) ; fold over alphabet
                              (fold-left (lambda (edges state) ; fold over states
                                           (if (fa-transition dfa state input-symbol)
                                               edges
                                               (cons (list state input-symbol dead-state)
                                                     edges)))
                                         ;; dead-state self transition
                                         (cons (list dead-state input-symbol dead-state)
                                               edges)
                                         (finite-automaton-states dfa)))
                            (finite-automaton-edges dfa)
                            alphabet)))
      ;; Result
      (make-fa  edges
                (finite-automaton-start dfa)
                (finite-automaton-accept dfa)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; COMPLETE THE FUNCTIONS BELOW ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 0: DFA Simulation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Finite Automata Lecture: Algorithm 1

(defun dfa-simulate (dfa sequence)

  "True if DFA accepts SEQUENCE."
  (assert (dfa-p dfa))
  (labels
    (
      (edelta (state list) ;; state: current state, list: remaining transitions

        (cond
          (
            (equal list nil)
            state
          )
          (
            t
            (edelta (dfa-transition dfa state (car list)) (cdr list))
          )
        )

      )
    )
    (let
      (
        (final-state ;; Coerce to list for simplicity
          (edelta (finite-automaton-start dfa) (coerce sequence 'list))
        )
      )
      (if
         (find final-state (finite-automaton-accept dfa) :test #'equal)
          t
          nil
      )
    )
  )

)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 1: NFA Subset Construction ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun e-closure (nfa s c)

  (labels
    (
      (visit (c q)

        (cond
          (
            (member q c)
            c
          )
          (
            t
            (e-closure nfa (fa-transition nfa q :epsilon) (cons q c))
          )
        )

      )
    )
    (fold-left #'visit c s)
  )

)

(defun move-e-closure (nfa s a)
  (labels ((visit (c q)
             (e-closure nfa (fa-transition nfa q a) c)))
    (fold-left #'visit nil (e-closure nfa s nil))))

(defun nfa-simulate (nfa sequence)
  "True if NFA accepts SEQUENCE."
  (labels ((edelta (subset list)
      (cond 
        (
          (equal list nil) subset
        )
        
        (
          t (edelta (move-e-closure nfa subset (car list)) (cdr list))
        )
      )))
    (let* ((q0 (finite-automaton-start nfa))
           (f (finite-automaton-accept nfa))
           (u (e-closure nfa (list q0) nil))
           (list (coerce sequence 'list))
           (final-subset (edelta u list)))
      (if (intersection final-subset f
                        :test #'equal)
          t
          nil))))

(defun state-predicate-atom (a b)
  "Predicate function to compare atomic states."
  (etypecase a
    ((or symbol string)
     (etypecase b
       ((or symbol string)
        (string-lessp a b))
       (number nil)))
    (number
     (etypecase b
       ((or symbol string)
        t)
       (number (<= a b))))))

(defun state-predicate (a b)
  "Predicate function to compare FA states."
  (etypecase a
    (atom (etypecase b
            (atom (state-predicate-atom a b))
            (list t)))
    (cons (etypecase b
            (atom nil)
            (cons (if (equal (car a) (car b))
                      (state-predicate (cdr a)
                                       (cdr b))
                      (state-predicate (car a)
                                       (car b))))))))

;; Subset Construction Lecture: Algorithm 5
(defun nfa->dfa (nfa)
  "Convert a nondeterministic finite automaton to a deterministic finite automaton."
  (let ((visited-hash (make-symbol-hash-table))
        (alphabet (remove :epsilon (finite-automaton-alphabet nfa))))
    (labels ((sort-subset (u)
               ;; sort subsets so we can quickly test for previously
               ;; visited subsets in the hash table
               (sort u #'state-predicate))
             (visit-symbol (edges subset-0 input-symbol)
               ;; (TODO 'nfa->dfa-visit-symbol))
               (let* ((next-subset (sort-subset (move-e-closure nfa subset-0 input-symbol))))
                 ;; Check if the next subset has already been visited
                 (unless (gethash next-subset visited-hash)
                   ;; Mark it as visited and process this new subset
                   (setf (gethash next-subset visited-hash) t)
                   (visit-subset edges next-subset))
                 ;; Add the transition to the DFA edges
                 (push (list subset-0 input-symbol next-subset) edges)))
             (visit-subset (edges subset)
               ;; (TODO 'nfa->dfa-visit-subset)))
               ;; Process each symbol in the alphabet for this subset
               (dolist (symbol alphabet)
                 (visit-symbol edges subset symbol))
                ;; Mark the subset as accepting if any of its states are accepting states in the NFA
                 (when (intersection subset (finite-automaton-accept nfa) :test #'equal)
                   (push subset dfa-accept)))
      ;; (TODO 'nfa->dfa))))
      (let ((start-subset (sort-subset (e-closure nfa (list (finite-automaton-start nfa))))))
        ;; Mark this initial subset as visited
        (setf (gethash start-subset visited-hash) t)
        ;; Begin recursive process to build the DFA by visiting all reachable subsets
        (visit-subset dfa-edges start-subset))
      ;; Build and return the DFA using the gathered edges and accepting states
      (make-fa dfa-edges start-subset dfa-accept))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 2: Regular Expressions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We represent regular expressions as S-expressions, using the
;; following operators:
;;
;; - :union
;; - :concatenation
;; - :kleene-closure
;;
;; The union and concatenation operators are n-ary.  Kleene-closure
;; takes a single argument.
;;
;; We can represent any regular language with a regular expression
;; using only the operators :union, :concatenation, :kleene-closure.
;; However, it is often convenient (and common practice) to define
;; additional operators. The SIMPLIFY-REGEX function will take as
;; input a regular expression with such additional operators and
;; return a simplified expression that contains only the operators
;; :union, :concatenation, ang :kleene-closure.  Specifically, it will
;; simplify the following operators:
;;
;; - :.     -> (:union alphabet...)
;; - (:? X) -> (:union X :epsilon)
;; - (:+ X) -> (:concatenation X (:kleene-closure X))

(defun simplify-regex (regex &optional alphabet)

  "Convert :., :?, :+ to only :union, :concatenation, :kleene-closure"
  (cond
    (
      (eq regex :.) ;; '0-ary' operator
      (assert alphabet)
      `(union ,@alphabet)
    )
    (
      (atom regex)
      regex
    )
    (
      t ;; at this point, car must be a unary or binary operator
      (labels
        (
          (process-mono (xpr)

            (simplify-regex (car (cdr xpr)) alphabet)

          )
          (process-duo (xpr)

            `(,(process-mono xpr) ,(simplify-regex (car (cdr (cdr xpr))) alphabet))

          )
        )
        (case (car regex)
          (
            :?
            `(:union :epsilon ,(process-mono regex))
          )
          (
            :+
            `(:concatenation ,(process-mono regex) (:kleene-closure ,(process-mono regex)))
          )
          (
            :kleene-closure
            `(:kleene-closure ,(process-mono regex))
          )
          (
            otherwise
            (cons (car regex) (process-duo regex))
          )
        )
      )
    )
  )

)


;;; The functions FA-CONCATENATE, FA-UNION, and FA-REPEAT apply the
;;; corresponding regular operation (union, concatenation, and
;;; kleene-closure, respectively) to finite automata.  We will next
;;; use these functions as subroutines to convert a regular expression
;;; to an NFA.

;; Regular Expression Lecture: Concatenation.
;; Provided in complete form as an example
(defun fa-concatenate (nfa-1 nfa-2)
  "Find the concatenation of NFA-1 and NFA-2."
  (assert (not (intersection (finite-automaton-states nfa-1)
                             (finite-automaton-states nfa-2))))
  (let ((start (newstate))
        (accept (newstate)))
    (make-fa (append (list (list start :epsilon (finite-automaton-start nfa-1)))
                     (map 'list (lambda (x)
                                  (list x :epsilon (finite-automaton-start nfa-2)))
                          (finite-automaton-accept nfa-1))
                     (map 'list (lambda (x)
                                  (list x :epsilon accept))
                          (finite-automaton-accept nfa-2))
                     (finite-automaton-edges nfa-1)
                     (finite-automaton-edges nfa-2))
             start
             (list accept))))


;; Regular Expression Lecture: Union
(defun fa-union (nfa-1 nfa-2)
  "Find the union of NFA-1 and NFA-2."
  (assert (not (intersection (finite-automaton-states nfa-1)
                             (finite-automaton-states nfa-2))))
  ;; (TODO 'fa-union))
  (let ((start (newstate))
        (accept (newstate)))
    (make-fa (append (list (list start :epsilon (finite-automaton-start nfa-1))
                           (list start :epsilon (finite-automaton-start nfa-2)))
                     (map 'list (lambda (x)
                                  (list x :epsilon accept))
                          (append (finite-automaton-accept nfa-1)
                                  (finite-automaton-accept nfa-2)))
                     (finite-automaton-edges nfa-1)
                     (finite-automaton-edges nfa-2))
             start
             (list accept))))

;; Regular Expression Lecture: Kleene-Closure
(defun fa-repeat (nfa)
  "Find the repetition / Kleene-closure of NFA."
  ;; (TODO 'fa-repeat))
  (let ((start (newstate))   ; New start state
        (accept (newstate))) ; New accept state
    (make-fa (append 
              ;; Epsilon transition from new start to the original start state
              (list (list start :epsilon (finite-automaton-start nfa))
                    ;; Epsilon transition from new start to new accept state, allowing for zero occurrences
                    (list start :epsilon accept))
              ;; Epsilon transitions from original accept states back to the original start state for repetition
              (map 'list (lambda (state)
              ;; (mapcar (lambda (state)
                           (list state :epsilon (finite-automaton-start nfa)))
                   (finite-automaton-accept nfa))
              ;; Epsilon transitions from original accept states to new accept state
              (map 'list (lambda (state)
              ;; (mapcar (lambda (state)
                           (list state :epsilon accept))
                   (finite-automaton-accept nfa))
              ;; Include all edges from the original NFA
              (finite-automaton-edges nfa))
             start
             (list accept))))

;; Convert a regular expression to a nondeterministic finite
;; automaton.
;;
;; The following are examples of possible regular expressions.
;;
;; - (:concatenation a b c)
;; - (:union a b c :epsilon)
;; - (:union)
;; - (:kleene-closure a)
;; - (:concatenation (:union a b) (:kleene-closure c))
(defun regex->nfa (regex)
  "Convert a regular expression to an NFA."
  (cond
    ((null regex) ; Base case for empty set
     (make-fa nil (newstate) (list (newstate))))
    ;; TODO: other base cases
        ;; Base case for a single symbol or :epsilon
    ((atom regex)
     (let ((start (newstate))
           (accept (newstate)))
       (if (eq regex :epsilon)
           ;; For :epsilon, create an NFA with a direct epsilon transition
           (make-fa (list (list start :epsilon accept)) start (list accept))
         ;; For any other symbol, create an NFA with a direct symbol transition
         (make-fa (list (list start regex accept)) start (list accept)))))

    ;; Case for :union - Combine NFAs for each subexpression
    ((eq (car regex) :union)
     (reduce #'fa-union (mapcar #'regex->nfa (cdr regex))))

    ;; Case for :concatenation - Concatenate NFAs for each subexpression
    ((eq (car regex) :concatenation)
     (reduce #'fa-concatenate (mapcar #'regex->nfa (cdr regex))))

    ;; Case for :kleene-closure - Apply Kleene closure on the subexpression
    ((eq (car regex) :kleene-closure)
     (fa-repeat (regex->nfa (cadr regex))))
    ;; (t
    ;;   (TODO 'regex->nfa))))
    (t (error "Unsupported operator in REGEX->NFA: ~A" (car regex))))) 
            ;; Not sure!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 3: Regular Decision and Closure Properties ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lecture: Decision Properties of Regular Languages, Emptiness
(defun fa-empty (fa)
  "Does FA represent the empty set?"
  (TODO 'fa-empty))

;; Lecture: Closure Properties of Regular Languages, State Minimization
(defun dfa-minimize (dfa)
  "Return an equivalent DFA with minimum state."
  (TODO 'dfa-minimize))

;; Lecture: Closure Properties of Regular Languages, Intersection
(defun dfa-intersection (dfa-0 dfa-1)
  "Return the intersection FA."
  (TODO 'dfa-intersection))

;; Lecture: Decision Properties of Regular Languages, Equivalence
(defun dfa-equivalent (dfa-0 dfa-1)
  "Do DFA-1 and DFA-2 recognize the same language?"
  (TODO 'dfa-equivalent))
