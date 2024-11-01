;; CSCI 561 Project 1
;;
;; Nathaniel Graves
;; Rhett Houston
;; Shadi Nourriz
;;

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
               (format stream "~&digraph~%{~%")
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
  (let
    (
      (visited-hash (make-symbol-hash-table))
      (alphabet (remove :epsilon (finite-automaton-alphabet nfa)))
    )
    (labels
      (
        (sort-subset (u)

          ;; sort subsets so we can quickly test for previously visited subsets in the hash table
          (sort u #'state-predicate)

        )
        (visit-symbol (dfa-edges this-subset input-symbol)

          (let
            (
              (next-subset (sort-subset (move-e-closure nfa this-subset input-symbol)))
            )
            (cond
              (
                (equal next-subset nil)
                dfa-edges
              )
              (
                t
                (visit-subset
                  (cons `(,this-subset ,input-symbol ,next-subset) dfa-edges)
                  next-subset
                )
              )
            )
          )

        )
        (visit-subset (dfa-edges subset)

          (cond
            (
              ;; Check if the next subset has already been visited
              (gethash subset visited-hash)
              dfa-edges
            )
            (
              ;; Mark it as visited and process this new subset
              t
              (setf (gethash subset visited-hash) t)
              (labels
                (
                  (h (sub-edges input-symbol)

                    (visit-symbol sub-edges subset input-symbol)

                  )
                )
                ;; Process each symbol in the alphabet for this subset
                (fold-left #'h dfa-edges alphabet)
              )
            )
          )

        )
        (find-accept (dfa-states found)

          ;; Mark as accepting if any of its states are accepting states in the NFA
          (cond
            (
              (equal dfa-states nil)
              found
            )
            (
              (intersection (car dfa-states) (finite-automaton-accept nfa) :test #'equal)
              (find-accept (cdr dfa-states) (cons (car dfa-states) found))
            )
            (
              t
              (find-accept (cdr dfa-states) found)
            )
          )

        )
      )
      (let*
        (
          (start-subset (sort-subset (e-closure nfa (list (finite-automaton-start nfa)) nil)))
          ;; Begin recursive process to build the DFA by visiting all reachable subsets
          (final-edges (visit-subset nil start-subset))
          (final-states (hash-table-keys visited-hash))
        )
        ;; Build and return the DFA using the gathered edges and accepting states
        (make-fa final-edges start-subset (find-accept final-states nil))
      )
    )
  )

)


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
      `(:union ,@alphabet)
    )
    (
      (atom regex)
      regex
    )
    (
      t ;; at this point, car must be at least a unary operator
      (labels
        (
          (process-vals (xpr out)

            (cond
              (
                (equal xpr nil)
                out
              )
              (
                t
                (process-vals (cdr xpr) (cons (simplify-regex (car xpr) alphabet) out))
              )
            )

          )
        )
        (let
          (
            (exclude-first (reverse (process-vals (cdr regex) nil)))
            (include-first (reverse (process-vals regex nil)))
          )
          (case (car regex)
            (
              :?
              `(:union :epsilon ,exclude-first)
            )
            (
              :+
              `(:concatenation ,exclude-first (:kleene-closure ,exclude-first))
            )
            (
              otherwise
              include-first
            )
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
  (assert
    (not (intersection (finite-automaton-states nfa-1)
                       (finite-automaton-states nfa-2)))
  )
  (let
    (
      (start (newstate))
      (accept (newstate))
    )
    (make-fa
      (append
        ;; start of AB -> start of A
        (list (list start :epsilon (finite-automaton-start nfa-1)))
        ;; every accept state of A -> start of B
        (map
          'list
          (lambda (x) (list x :epsilon (finite-automaton-start nfa-2)))
          (finite-automaton-accept nfa-1)
        )
        ;; every accept state of B -> end of AB
        (map
          'list
          (lambda (x) (list x :epsilon accept))
          (finite-automaton-accept nfa-2)
        )
        ;; internal edges
        (finite-automaton-edges nfa-1)
        (finite-automaton-edges nfa-2)
      )
      start
      (list accept)
    )
  )

)

;; Regular Expression Lecture: Union
(defun fa-union (nfa-1 nfa-2)

  "Find the union of NFA-1 and NFA-2."
  (assert
    (not (intersection (finite-automaton-states nfa-1)
                       (finite-automaton-states nfa-2)))
  )
  (let
    (
      (start (newstate))
      (accept (newstate))
    )
    (make-fa
      (append
        ;; start of A|B -> start of A, start of B
        (list
          (list start :epsilon (finite-automaton-start nfa-1))
          (list start :epsilon (finite-automaton-start nfa-2))
        )
        ;; every accept state of A, every accept state of B -> end of A|B
        (map
          'list
          (lambda (x) (list x :epsilon accept))
          (append
            (finite-automaton-accept nfa-1)
            (finite-automaton-accept nfa-2)
          )
        )
        ;; internal edges
        (finite-automaton-edges nfa-1)
        (finite-automaton-edges nfa-2)
      )
      start
      (list accept)
    )
  )

)

;; Regular Expression Lecture: Kleene-Closure
(defun fa-repeat (nfa)

  "Find the repetition / Kleene-closure of NFA."
  (let
    (
      (start (newstate))   ; New start state
      (accept (newstate))  ; New accept state
    )
    (make-fa
      (append
        (list
          ;; Epsilon transition from new start to the original start state
          (list start :epsilon (finite-automaton-start nfa))
          ;; Epsilon transition from new start to new accept state, allowing for zero occurrences
          (list start :epsilon accept)
        )
        ;; Epsilon transitions from original accept states back to the original start state
        (map
          'list
          (lambda (state) (list state :epsilon (finite-automaton-start nfa)))
          (finite-automaton-accept nfa)
        )
        ;; Epsilon transitions from original accept states to new accept state
        (map
          'list
          (lambda (state) (list state :epsilon accept))
          (finite-automaton-accept nfa)
        )
        ;; Include all edges from the original NFA
        (finite-automaton-edges nfa)
      )
      start
      (list accept)
    )
  )

)


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
    (
      (null regex) ; Base case for empty set
      (make-fa nil (newstate) nil)
    )
    ;; Base case for a single symbol or :epsilon
    (
      (atom regex)
      (let
        (
          (start (newstate))
          (accept (newstate))
        )
        (if
          (eq regex :epsilon)
          ;; For :epsilon, create an NFA with a direct epsilon transition
          (make-fa (list (list start :epsilon accept)) start (list accept))
          ;; For any other symbol, create an NFA with a direct symbol transition
          (make-fa (list (list start regex accept)) start (list accept))
        )
      )
    )
    ;; Case for :union - Combine NFAs for each subexpression
    (
      (eq (car regex) :union)
      (if
        (eq (cdr regex) nil)
        (make-fa nil (newstate) nil)
        (reduce #'fa-union (mapcar #'regex->nfa (cdr regex)))
      )
    )
    ;; Case for :concatenation - Concatenate NFAs for each subexpression
    (
      (eq (car regex) :concatenation)
      (if
        (eq (cdr regex) nil)
        (let ((one-state (newstate))) (make-fa nil one-state (list one-state)))
        (reduce #'fa-concatenate (mapcar #'regex->nfa (cdr regex)))
      )
    )
    ;; Case for :kleene-closure - Apply Kleene closure on the subexpression
    (
      (eq (car regex) :kleene-closure)
      (fa-repeat (regex->nfa (cadr regex)))
    )
    ;; List of one non-operator
    (
      (eq (cdr regex) nil)
      (regex->nfa (car regex))
    )
    ;; Not sure!
    (
      t
      (error "Unsupported operator in REGEX->NFA: ~A" (car regex))
    )
  )

)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 3: Regular Decision and Closure Properties ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-member (set item)
  (cond
    ((null set) nil)
    ((equal (car set) item) t)
    (t (set-member (cdr set) item))))

(defun xor (a b)
  (not (equal a b)))

;; Lecture: Decision Properties of Regular Languages, Emptiness
(defun fa-empty (fa)
  "Does FA represent the empty set?"
  (let ((visited (make-hash-table :test #'equal))
        (queue (list (finite-automaton-start fa))))
    (loop
      ;; Check if there are any states left to explore
      (when (null queue) (return t)) ;; If the queue is empty, the FA is empty
      (let ((state (pop queue)))
        ;; If we reach an accept state, return false (not empty)
        (when (set-member (finite-automaton-accept fa) state)
          (return nil))
        ;; Mark the state as visited
        (setf (gethash state visited) t)
        ;; Enqueue all reachable states from the current state
        (dolist (input (finite-automaton-alphabet fa))
          (dolist (next-state (fa-transition fa state input))
            (unless (gethash next-state visited)
              (push next-state queue))))))))


;; Lecture: Closure Properties of Regular Languages, State Minimization
(defun dfa-minimize (dfa)
  "Return an equivalent DFA with minimum state."
  (let* ((states (finite-automaton-states dfa))
         (accept-states (finite-automaton-accept dfa))
         (alphabet (finite-automaton-alphabet dfa))
         ;; Table to mark distinguishable state pairs
         (distinguished (make-hash-table :test #'equal)))

    ;; Step 1: Mark pairs of accepting and non-accepting states as distinguishable
    (dolist (s1 states)
      (dolist (s2 states)
        (when (and (not (equal s1 s2))
                   (xor (set-member accept-states s1) (set-member accept-states s2)))
          (setf (gethash (cons s1 s2) distinguished) t))))

    ;; Step 2: Propagate distinguishability based on transitions
    (loop
      (let ((updated nil))
        (dolist (s1 states)
          (dolist (s2 states)
            (unless (or (equal s1 s2) (gethash (cons s1 s2) distinguished))
              ;; Check transitions to see if they lead to distinguishable states
              (dolist (symbol alphabet)
                (let ((t1 (dfa-transition dfa s1 symbol))
                      (t2 (dfa-transition dfa s2 symbol)))
                  (when (or (xor (equal t1 nil) (equal t2 nil))
                            (and t1 t2
                                 (not (equal t1 t2))
                                 (gethash (cons t1 t2) distinguished)))
                    (setf (gethash (cons s1 s2) distinguished) t)
                    (setf (gethash (cons s2 s1) distinguished) t)
                    (setf updated t)))))))
        ;; Exit the loop when no more updates are made
        (unless updated (return))))

    ;; Step 3: Group indistinguishable states into equivalence classes
    (let ((state-groups (make-hash-table :test #'equal)))
      (dolist (s1 states)
        (dolist (s2 states)
          (unless (gethash (cons s1 s2) distinguished)
            ;; Put s1 and s2 in the same equivalence class
            (let ((group (or (gethash s1 state-groups)
                             (gethash s2 state-groups)
                             (gensym "group"))))
              (setf (gethash s1 state-groups) group)
              (setf (gethash s2 state-groups) group)))))

      ;; Step 4: Construct the minimized DFA
      (let ((new-start (gethash (finite-automaton-start dfa) state-groups))
            (new-accept-states (remove-duplicates
                                (mapcar (lambda (state)
                                          (gethash state state-groups))
                                        accept-states)
                                :test #'equal))
            (new-edges '()))

        ;; Map original DFA transitions to transitions in the minimized DFA
        (dolist (edge (finite-automaton-edges dfa))
          (destructuring-bind (s0 symbol s1) edge
            (let ((new-s0 (gethash s0 state-groups))
                  (new-s1 (gethash s1 state-groups)))
              (push (list new-s0 symbol new-s1) new-edges))))

        ;; Remove duplicate edges and create the minimized DFA
        (make-fa (remove-duplicates new-edges :test #'equal)
                 new-start
                 new-accept-states)))))


(defun set-union (set-1 set-2)
  (cond
    ((equal set-1 nil)
     set-2)
    ((set-member set-2 (car set-1))
     (set-union (cdr set-1) set-2))
    (t
     (cons (car set-1) (set-union (cdr set-1) set-2)))))

;; Lecture: Closure Properties of Regular Languages, Intersection
(defun product-dfa (dfa-0 dfa-1 predicate)

  ;; Define the alphabet for the new DFA
  (let* ((alphabet (set-union (finite-automaton-alphabet dfa-0)
                                     (finite-automaton-alphabet dfa-1)))
         ;; Generate product states as pairs (s0, s1)
         (states (loop for s0 in (finite-automaton-states dfa-0)
                       nconc (loop for s1 in (finite-automaton-states dfa-1)
                                   collect (cons s0 s1))))
         ;; Define the start state as the pair of start states from both DFAs
         (start (cons (finite-automaton-start dfa-0)
                      (finite-automaton-start dfa-1)))
         ;; Collect accept states where both states in the pair are accepting
         (accept-states '())
         ;; Initialize an empty list for edges
         (edges '()))

    ;; Identify accept states in the product DFA
    (dolist (s0 (finite-automaton-states dfa-0))
      (dolist (s1 (finite-automaton-states dfa-1))
        (when (funcall predicate dfa-0 dfa-1 s0 s1)
          (let ((product-accept (cons s0 s1)))
            (push product-accept accept-states)))))

    ;; Define transitions for each state pair and input symbol
    (dolist (state states)
      (let ((s0 (car state))
            (s1 (cdr state)))

        ;; Process each input symbol
        (dolist (input alphabet)
          ;; Find the transitions for s0 and s1 on the input symbol
          (let ((t0 (dfa-transition dfa-0 s0 input))
                (t1 (dfa-transition dfa-1 s1 input)))
            (when (and t0 t1)
              ;; Both states have transitions on this input, so add it to the edges
              (let ((target-state (cons t0 t1)))
                (push (list state input target-state) edges)))))))

    ;; Remove duplicates in edges and accept states for a clean DFA
    (let ((unique-edges (remove-duplicates edges :test #'equal))
          (unique-accept-states (remove-duplicates accept-states :test #'equal)))

      ;; Construct and return the new DFA representing the intersection
      (make-fa unique-edges start unique-accept-states))))


(defun dfa-intersection (dfa-0 dfa-1)
  "Return the intersection FA."
  (labels
    (
      (if-both-accept (dfa-0 dfa-1 state-0 state-1)
        ;; Only pairs where both states are accepting are added as accept states
        (and
          (set-member (finite-automaton-accept dfa-0) state-0)
          (set-member (finite-automaton-accept dfa-1) state-1)
        )
      )
    )
    (product-dfa dfa-0 dfa-1 #'if-both-accept)
  )
)

;; Lecture: Decision Properties of Regular Languages, Equivalence
(defun dfa-equivalent (dfa-0 dfa-1)
  "Do DFA-1 and DFA-2 recognize the same language?"
  (labels
    (
      (if-exactly-one-accepts (dfa-0 dfa-1 state-0 state-1)

        (xor
          (set-member (finite-automaton-accept dfa-0) state-0)
          (set-member (finite-automaton-accept dfa-1) state-1)
        )

      )
    )
    (fa-empty (product-dfa dfa-0 dfa-1 #'if-exactly-one-accepts))
  )
)
