;;;;;;;;;;;;;;;;;;;;
;;; FA structure ;;;
;;;;;;;;;;;;;;;;;;;;

;; Make a DFA
(defparameter *dfa-0*
  (make-fa '((q0 0 q0)
             (q0 1 q1)
             (q1 1 q1)
             (q1 0 q0))
           'q0
           '(q1)))

(defparameter *dfa-1*
  (make-fa '((q0 0 q1)
             (q1 1 q0))
           'q0
           '(q1)))

(defparameter *dfa-2*
  (make-fa '((q0 a q1)
             (q0 b q2)
             (q1 a q3)
             (q1 b q4)
             (q2 a q4)
             (q2 b q3)
             (q3 a q3)
             (q3 b q3)
             (q4 a q4)
             (q4 b q4))
           'q0
           '(q3 q4)))

(defparameter *dfa-3*
  (make-fa '((q0 a q1)
             (q1 b q2)
             (q1 d q4)
             (q2 c q3)
             (q3 b q2)
             (q3 d q4)
             (q4 b q2)
             (q4 d q4))
           'q0
           '(q1 q3 q4)))

(defparameter *dfa-4*
  (make-fa '((q0 0 q0)
             (q0 1 q1)
             (q1 0 q0)
             (q1 1 q0))
           'q0
           '(q1)))

(defparameter *dfa-5*
  (make-fa '((q0 1 q0)
             (q0 0 q1)
             (q1 1 q0))
           'q0
           '(q0)))

;; Make an NFA
(defparameter *nfa-0*
  (make-fa '((q0 0 q0)
             (q0 :epsilon q1)
             (q1 1 q1)
             (q1 :epsilon q2)
             (q2 2 q2))
           'q0
           '(q2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 0: DFA Simulation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dfa-simulate *dfa-0* '(0 1))
;; => t


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 1: NFA Subset Construction ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e-closure *nfa-0* '(q0) nil)
;; => (q0 q1 q2)

(move-e-closure *nfa-0* '(q0) 1)
;; => (q1 q2)

(nfa-simulate *nfa-0* '(0 1))
;; => t

(nfa->dfa *nfa-0*)
;; => the equivalent dfa


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 2: Regular Expressions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(simplify-regex '(:? (:union 0 1)) '(0 1))
;; => (:union :epsilon (:union 0 1))

(simplify-regex '(:+ :.) '(0 1))
;; => (:concatenation (:union 0 1) (:kleene-closure (:union 0 1)))

(regex->nfa '(:concatenation (:union 0 1) (:kleene-closure (:union 0 1))))
;; => the equivalent nfa


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 3: Regular Decision and Closure Properties ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fa-empty *dfa-0*)
;; => nil

(dfa-minimize *dfa-0*)
;; => the equivalent minimum-state dfa

(dfa-intersection *dfa-0* *dfa-1*)
;; => fa for the intersection of *dfa-0* and *dfa-1*

(dfa-equivalent *dfa-0* *dfa-1*)
;; => nil


;; DES for Question 2
(defparameter *des-2a*
  (make-fa '(("W: (1, 1)\\nH: (3, 2)" right "W: (1, 2)\\nH: (3, 3)")
             ("W: (1, 1)\\nH: (3, 2)"  down "W: (2, 1)\\nH: (3, 3)")
             ("W: (2, 1)\\nH: (3, 3)"    up "W: (1, 1)\\nH: (2, 3)")
             ("W: (2, 1)\\nH: (3, 3)"  down "W: (3, 1)\\nH: (2, 3)")
             ("W: (3, 1)\\nH: (2, 3)"    up "W: (2, 1)\\nH: (1, 3)")
             ("W: (3, 1)\\nH: (2, 3)" right "W: (3, 2)\\nH: (1, 3)")
             ("W: (1, 1)\\nH: (2, 3)"  down "W: (2, 1)\\nH: (1, 3)")
             ("W: (1, 1)\\nH: (2, 3)" right "W: (1, 2)\\nH: (1, 3)")
             ("W: (1, 2)\\nH: (3, 3)" right "W: (1, 3)\\nH: (3, 3)")
             ("W: (1, 2)\\nH: (3, 3)"  left "W: (1, 1)\\nH: (2, 3)")
             ("W: (1, 3)\\nH: (3, 3)"  left "W: (1, 2)\\nH: (2, 3)")
             ("W: (1, 3)\\nH: (3, 3)"  down "W: (2, 3)\\nH: (3, 2)")
             ("W: (1, 2)\\nH: (2, 3)"  left "W: (1, 1)\\nH: (1, 3)")
             ("W: (1, 2)\\nH: (2, 3)" right "W: (1, 3)\\nH: (3, 3)")
             ("W: (2, 3)\\nH: (3, 2)"    up "W: (1, 3)\\nH: (3, 3)")
             ("W: (2, 3)\\nH: (3, 2)"  down "W: (3, 3)\\nH: (3, 1)")
             ("W: (3, 3)\\nH: (3, 1)"    up "W: (2, 3)\\nH: (3, 2)")
             ("W: (3, 3)\\nH: (3, 1)"  left "W: (3, 2)\\nH: (2, 1)")
             ("W: (3, 2)\\nH: (2, 1)" right "W: (3, 3)\\nH: (3, 1)")
             ("W: (3, 2)\\nH: (2, 1)"  left "W: (3, 1)\\nH: (1, 1)")
             ("W: (3, 1)\\nH: (1, 1)" right "W: (3, 2)\\nH: (1, 2)")
             ("W: (3, 1)\\nH: (1, 1)"    up "W: (2, 1)\\nH: (1, 2)")
             ("W: (2, 1)\\nH: (1, 2)"    up "W: (1, 1)\\nH: (1, 3)")
             ("W: (2, 1)\\nH: (1, 2)"  down "W: (3, 1)\\nH: (1, 3)")
             ("W: (3, 2)\\nH: (1, 2)" right "W: (3, 3)\\nH: (1, 3)")
             ("W: (3, 2)\\nH: (1, 2)"  left "W: (3, 1)\\nH: (1, 3)"))
           "W: (1, 1)\\nH: (3, 2)"
           '()))

(defparameter *des-2b*
  (make-fa '(("H: (3, 3)\\nW: (2, 3)" remain "H: (3, 3)\\nW: (3, 3)")
             ("H: (3, 1)\\nW: (1, 2)" remain "H: (3, 1)\\nW: (1, 1)")
             ("H: (3, 1)\\nW: (1, 1)" remain "H: (3, 1)\\nW: (1, 2)")
             ("H: (2, 1)\\nW: (1, 1)" remain "H: (2, 1)\\nW: (2, 1)")
             ("H: (3, 2)\\nW: (1, 3)" remain "H: (3, 2)\\nW: (1, 2)")
             ("H: (3, 3)\\nW: (1, 3)" remain "H: (3, 3)\\nW: (2, 3)")
             ("H: (3, 2)\\nW: (1, 2)" remain "H: (3, 2)\\nW: (1, 3)")
             ("H: (3, 1)\\nW: (1, 1)"  right "H: (3, 2)\\nW: (1, 2)")
             ("H: (3, 2)\\nW: (1, 3)"  right "H: (3, 3)\\nW: (2, 3)")
             ("H: (3, 1)\\nW: (1, 2)"  right "H: (3, 2)\\nW: (1, 3)")
             ("H: (3, 2)\\nW: (1, 2)"  right "H: (3, 3)\\nW: (1, 3)")
             ("H: (3, 3)\\nW: (1, 3)"     up "H: (2, 3)\\nW: (2, 3)")
             ("H: (3, 1)\\nW: (1, 2)"     up "H: (2, 1)\\nW: (1, 1)")
             ("H: (3, 1)\\nW: (1, 1)"     up "H: (2, 1)\\nW: (2, 1)")
             ("H: (2, 1)\\nW: (1, 1)"   down "H: (3, 1)\\nW: (1, 2)")
             ("H: (3, 2)\\nW: (1, 3)"   left "H: (3, 1)\\nW: (1, 2)")
             ("H: (3, 3)\\nW: (2, 3)"   left "H: (3, 2)\\nW: (1, 3)")
             ("H: (3, 2)\\nW: (1, 2)"   left "H: (3, 1)\\nW: (1, 1)")
             ("H: (3, 3)\\nW: (1, 3)"   left "H: (3, 2)\\nW: (1, 2)"))
           "H: (3, 1)\\nW: (1, 1)"
           '()))



;; DES for question 1
(defparameter *des-1*
  (make-fa '(("H: (1, 1)\\nW: (1, 2)\\nNext: H" h-remain "H: (1, 1)\\nW: (1, 2)\\nNext: W")
             ("H: (1, 1)\\nW: (1, 2)\\nNext: H"   h-down "H: (2, 1)\\nW: (1, 2)\\nNext: W")
             ("H: (1, 1)\\nW: (1, 2)\\nNext: W"   w-left "H: (1, 1)\\nW: (1, 1)\\nDEATH")
             ("H: (1, 1)\\nW: (1, 2)\\nNext: W"  w-right "H: (1, 1)\\nW: (1, 3)\\nNext: H")
             ("H: (1, 1)\\nW: (1, 3)\\nNext: H" h-remain "H: (1, 1)\\nW: (1, 3)\\nNext: W")
             ("H: (1, 1)\\nW: (1, 3)\\nNext: H"  h-right "H: (1, 2)\\nW: (1, 3)\\nNext: W")
             ("H: (1, 1)\\nW: (1, 3)\\nNext: H"   h-down "H: (2, 1)\\nW: (1, 3)\\nNext: W")
             ("H: (1, 1)\\nW: (1, 3)\\nNext: W"   w-left "H: (1, 1)\\nW: (1, 2)\\nNext: H")
             ("H: (1, 1)\\nW: (1, 3)\\nNext: W"   w-down "H: (1, 1)\\nW: (2, 3)\\nNext: H")
             ("H: (1, 1)\\nW: (2, 1)\\nNext: H" h-remain "H: (1, 1)\\nW: (2, 1)\\nNext: W")
             ("H: (1, 1)\\nW: (2, 1)\\nNext: H"  h-right "H: (1, 2)\\nW: (2, 1)\\nNext: W")
             ("H: (1, 1)\\nW: (2, 1)\\nNext: W"     w-up "H: (1, 1)\\nW: (1, 1)\\nDEATH")
             ("H: (1, 1)\\nW: (2, 1)\\nNext: W"   w-down "H: (1, 1)\\nW: (3, 1)\\nNext: H")
             ("H: (1, 1)\\nW: (2, 3)\\nNext: H" h-remain "H: (1, 1)\\nW: (2, 3)\\nNext: W")
             ("H: (1, 1)\\nW: (2, 3)\\nNext: H"  h-right "H: (1, 2)\\nW: (2, 3)\\nNext: W")
             ("H: (1, 1)\\nW: (2, 3)\\nNext: H"   h-down "H: (2, 1)\\nW: (2, 3)\\nNext: W")
             ("H: (1, 1)\\nW: (2, 3)\\nNext: W"     w-up "H: (1, 1)\\nW: (1, 3)\\nNext: H")
             ("H: (1, 1)\\nW: (2, 3)\\nNext: W"   w-down "H: (1, 1)\\nW: (3, 3)\\nNext: H")
             ("H: (1, 1)\\nW: (3, 1)\\nNext: H" h-remain "H: (1, 1)\\nW: (3, 1)\\nNext: W")
             ("H: (1, 1)\\nW: (3, 1)\\nNext: H"  h-right "H: (1, 2)\\nW: (3, 1)\\nNext: W")
             ("H: (1, 1)\\nW: (3, 1)\\nNext: H"   h-down "H: (2, 1)\\nW: (3, 1)\\nNext: W")
             ("H: (1, 1)\\nW: (3, 1)\\nNext: W"     w-up "H: (1, 1)\\nW: (2, 1)\\nNext: H")
             ("H: (1, 1)\\nW: (3, 1)\\nNext: W"  w-right "H: (1, 1)\\nW: (3, 2)\\nNext: H")
             ("H: (1, 1)\\nW: (3, 2)\\nNext: H" h-remain "H: (1, 1)\\nW: (3, 2)\\nNext: W")
             ("H: (1, 1)\\nW: (3, 2)\\nNext: H"  h-right "H: (1, 2)\\nW: (3, 2)\\nNext: W")
             ("H: (1, 1)\\nW: (3, 2)\\nNext: H"   h-down "H: (2, 1)\\nW: (3, 2)\\nNext: W")
             ("H: (1, 1)\\nW: (3, 2)\\nNext: W"   w-left "H: (1, 1)\\nW: (3, 1)\\nNext: H")
             ("H: (1, 1)\\nW: (3, 2)\\nNext: W"  w-right "H: (1, 1)\\nW: (3, 3)\\nNext: H")
             ("H: (1, 1)\\nW: (3, 3)\\nNext: H" h-remain "H: (1, 1)\\nW: (3, 3)\\nNext: W")
             ("H: (1, 1)\\nW: (3, 3)\\nNext: H"  h-right "H: (1, 2)\\nW: (3, 3)\\nNext: W")
             ("H: (1, 1)\\nW: (3, 3)\\nNext: H"   h-down "H: (2, 1)\\nW: (3, 3)\\nNext: W")
             ("H: (1, 1)\\nW: (3, 3)\\nNext: W"     w-up "H: (1, 1)\\nW: (2, 3)\\nNext: H")
             ("H: (1, 1)\\nW: (3, 3)\\nNext: W"   w-left "H: (1, 1)\\nW: (3, 2)\\nNext: H")
             ("H: (1, 2)\\nW: (1, 1)\\nNext: H" h-remain "H: (1, 2)\\nW: (1, 1)\\nNext: W")
             ("H: (1, 2)\\nW: (1, 1)\\nNext: H"  h-right "H: (1, 3)\\nW: (1, 1)\\nFREEDOM")
             ("H: (1, 2)\\nW: (1, 1)\\nNext: W"  w-right "H: (1, 2)\\nW: (1, 2)\\nDEATH")
             ("H: (1, 2)\\nW: (1, 1)\\nNext: W"   w-down "H: (1, 2)\\nW: (2, 1)\\nNext: H")
             ("H: (1, 2)\\nW: (1, 3)\\nNext: H" h-remain "H: (1, 2)\\nW: (1, 3)\\nNext: W")
             ("H: (1, 2)\\nW: (1, 3)\\nNext: H"   h-left "H: (1, 1)\\nW: (1, 3)\\nNext: W")
             ("H: (1, 2)\\nW: (1, 3)\\nNext: W"   w-left "H: (1, 2)\\nW: (1, 2)\\nDEATH")
             ("H: (1, 2)\\nW: (1, 3)\\nNext: W"   w-down "H: (1, 2)\\nW: (2, 3)\\nNext: H")
             ("H: (1, 2)\\nW: (2, 1)\\nNext: H" h-remain "H: (1, 2)\\nW: (2, 1)\\nNext: W")
             ("H: (1, 2)\\nW: (2, 1)\\nNext: H"   h-left "H: (1, 1)\\nW: (2, 1)\\nNext: W")
             ("H: (1, 2)\\nW: (2, 1)\\nNext: H"  h-right "H: (1, 3)\\nW: (2, 1)\\nFREEDOM")
             ("H: (1, 2)\\nW: (2, 1)\\nNext: W"     w-up "H: (1, 2)\\nW: (1, 1)\\nNext: H")
             ("H: (1, 2)\\nW: (2, 1)\\nNext: W"   w-down "H: (1, 2)\\nW: (3, 1)\\nNext: H")
             ("H: (1, 2)\\nW: (2, 3)\\nNext: H" h-remain "H: (1, 2)\\nW: (2, 3)\\nNext: W")
             ("H: (1, 2)\\nW: (2, 3)\\nNext: H"   h-left "H: (1, 1)\\nW: (2, 3)\\nNext: W")
             ("H: (1, 2)\\nW: (2, 3)\\nNext: H"  h-right "H: (1, 3)\\nW: (2, 3)\\nFREEDOM")
             ("H: (1, 2)\\nW: (2, 3)\\nNext: W"     w-up "H: (1, 2)\\nW: (1, 3)\\nNext: H")
             ("H: (1, 2)\\nW: (2, 3)\\nNext: W"   w-down "H: (1, 2)\\nW: (3, 3)\\nNext: H")
             ("H: (1, 2)\\nW: (3, 1)\\nNext: H" h-remain "H: (1, 2)\\nW: (3, 1)\\nNext: W")
             ("H: (1, 2)\\nW: (3, 1)\\nNext: H"   h-left "H: (1, 1)\\nW: (3, 1)\\nNext: W")
             ("H: (1, 2)\\nW: (3, 1)\\nNext: H"  h-right "H: (1, 3)\\nW: (3, 1)\\nFREEDOM")
             ("H: (1, 2)\\nW: (3, 1)\\nNext: W"     w-up "H: (1, 2)\\nW: (2, 1)\\nNext: H")
             ("H: (1, 2)\\nW: (3, 1)\\nNext: W"  w-right "H: (1, 2)\\nW: (3, 2)\\nNext: H")
             ("H: (1, 2)\\nW: (3, 2)\\nNext: H" h-remain "H: (1, 2)\\nW: (3, 2)\\nNext: W")
             ("H: (1, 2)\\nW: (3, 2)\\nNext: H"   h-left "H: (1, 1)\\nW: (3, 2)\\nNext: W")
             ("H: (1, 2)\\nW: (3, 2)\\nNext: H"  h-right "H: (1, 3)\\nW: (3, 2)\\nFREEDOM")
             ("H: (1, 2)\\nW: (3, 2)\\nNext: W"   w-left "H: (1, 2)\\nW: (3, 1)\\nNext: H")
             ("H: (1, 2)\\nW: (3, 2)\\nNext: W"  w-right "H: (1, 2)\\nW: (3, 3)\\nNext: H")
             ("H: (1, 2)\\nW: (3, 3)\\nNext: H" h-remain "H: (1, 2)\\nW: (3, 3)\\nNext: W")
             ("H: (1, 2)\\nW: (3, 3)\\nNext: H"   h-left "H: (1, 1)\\nW: (3, 3)\\nNext: W")
             ("H: (1, 2)\\nW: (3, 3)\\nNext: H"  h-right "H: (1, 3)\\nW: (3, 3)\\nFREEDOM")
             ("H: (1, 2)\\nW: (3, 3)\\nNext: W"     w-up "H: (1, 2)\\nW: (2, 3)\\nNext: H")
             ("H: (1, 2)\\nW: (3, 3)\\nNext: W"   w-left "H: (1, 2)\\nW: (3, 2)\\nNext: H")
             ("H: (2, 1)\\nW: (1, 1)\\nNext: H" h-remain "H: (2, 1)\\nW: (1, 1)\\nNext: W")
             ("H: (2, 1)\\nW: (1, 1)\\nNext: H"   h-down "H: (3, 1)\\nW: (1, 1)\\nNext: W")
             ("H: (2, 1)\\nW: (1, 1)\\nNext: W"  w-right "H: (2, 1)\\nW: (1, 2)\\nNext: H")
             ("H: (2, 1)\\nW: (1, 1)\\nNext: W"   w-down "H: (2, 1)\\nW: (2, 1)\\nDEATH")
             ("H: (2, 1)\\nW: (1, 2)\\nNext: H" h-remain "H: (2, 1)\\nW: (1, 2)\\nNext: W")
             ("H: (2, 1)\\nW: (1, 2)\\nNext: H"     h-up "H: (1, 1)\\nW: (1, 2)\\nNext: W")
             ("H: (2, 1)\\nW: (1, 2)\\nNext: H"   h-down "H: (3, 1)\\nW: (1, 2)\\nNext: W")
             ("H: (2, 1)\\nW: (1, 2)\\nNext: W"   w-left "H: (2, 1)\\nW: (1, 1)\\nNext: H")
             ("H: (2, 1)\\nW: (1, 2)\\nNext: W"  w-right "H: (2, 1)\\nW: (1, 3)\\nNext: H")
             ("H: (2, 1)\\nW: (1, 3)\\nNext: H" h-remain "H: (2, 1)\\nW: (1, 3)\\nNext: W")
             ("H: (2, 1)\\nW: (1, 3)\\nNext: H"     h-up "H: (1, 1)\\nW: (1, 3)\\nNext: W")
             ("H: (2, 1)\\nW: (1, 3)\\nNext: H"   h-down "H: (3, 1)\\nW: (1, 3)\\nNext: W")
             ("H: (2, 1)\\nW: (1, 3)\\nNext: W"   w-left "H: (2, 1)\\nW: (1, 2)\\nNext: H")
             ("H: (2, 1)\\nW: (1, 3)\\nNext: W"   w-down "H: (2, 1)\\nW: (2, 3)\\nNext: H")
             ("H: (2, 1)\\nW: (2, 3)\\nNext: H" h-remain "H: (2, 1)\\nW: (2, 3)\\nNext: W")
             ("H: (2, 1)\\nW: (2, 3)\\nNext: H"     h-up "H: (1, 1)\\nW: (2, 3)\\nNext: W")
             ("H: (2, 1)\\nW: (2, 3)\\nNext: H"   h-down "H: (3, 1)\\nW: (2, 3)\\nNext: W")
             ("H: (2, 1)\\nW: (2, 3)\\nNext: W"     w-up "H: (2, 1)\\nW: (1, 3)\\nNext: H")
             ("H: (2, 1)\\nW: (2, 3)\\nNext: W"   w-down "H: (2, 1)\\nW: (3, 3)\\nNext: H")
             ("H: (2, 1)\\nW: (3, 1)\\nNext: H" h-remain "H: (2, 1)\\nW: (3, 1)\\nNext: W")
             ("H: (2, 1)\\nW: (3, 1)\\nNext: H"     h-up "H: (1, 1)\\nW: (3, 1)\\nNext: W")
             ("H: (2, 1)\\nW: (3, 1)\\nNext: W"     w-up "H: (2, 1)\\nW: (2, 1)\\nDEATH")
             ("H: (2, 1)\\nW: (3, 1)\\nNext: W"  w-right "H: (2, 1)\\nW: (3, 2)\\nNext: H")
             ("H: (2, 1)\\nW: (3, 2)\\nNext: H" h-remain "H: (2, 1)\\nW: (3, 2)\\nNext: W")
             ("H: (2, 1)\\nW: (3, 2)\\nNext: H"     h-up "H: (1, 1)\\nW: (3, 2)\\nNext: W")
             ("H: (2, 1)\\nW: (3, 2)\\nNext: H"   h-down "H: (3, 1)\\nW: (3, 2)\\nNext: W")
             ("H: (2, 1)\\nW: (3, 2)\\nNext: W"   w-left "H: (2, 1)\\nW: (3, 1)\\nNext: H")
             ("H: (2, 1)\\nW: (3, 2)\\nNext: W"  w-right "H: (2, 1)\\nW: (3, 3)\\nNext: H")
             ("H: (2, 1)\\nW: (3, 3)\\nNext: H" h-remain "H: (2, 1)\\nW: (3, 3)\\nNext: W")
             ("H: (2, 1)\\nW: (3, 3)\\nNext: H"     h-up "H: (1, 1)\\nW: (3, 3)\\nNext: W")
             ("H: (2, 1)\\nW: (3, 3)\\nNext: H"   h-down "H: (3, 1)\\nW: (3, 3)\\nNext: W")
             ("H: (2, 1)\\nW: (3, 3)\\nNext: W"     w-up "H: (2, 1)\\nW: (2, 3)\\nNext: H")
             ("H: (2, 1)\\nW: (3, 3)\\nNext: W"   w-left "H: (2, 1)\\nW: (3, 2)\\nNext: H")
             ("H: (2, 3)\\nW: (1, 1)\\nNext: H" h-remain "H: (2, 3)\\nW: (1, 1)\\nNext: W")
             ("H: (2, 3)\\nW: (1, 1)\\nNext: H"     h-up "H: (1, 3)\\nW: (1, 1)\\nFREEDOM")
             ("H: (2, 3)\\nW: (1, 1)\\nNext: H"   h-down "H: (3, 3)\\nW: (1, 1)\\nNext: W")
             ("H: (2, 3)\\nW: (1, 1)\\nNext: W"  w-right "H: (2, 3)\\nW: (1, 2)\\nNext: H")
             ("H: (2, 3)\\nW: (1, 1)\\nNext: W"   w-down "H: (2, 3)\\nW: (2, 1)\\nNext: H")
             ("H: (2, 3)\\nW: (1, 2)\\nNext: H" h-remain "H: (2, 3)\\nW: (1, 2)\\nNext: W")
             ("H: (2, 3)\\nW: (1, 2)\\nNext: H"     h-up "H: (1, 3)\\nW: (1, 2)\\nFREEDOM")
             ("H: (2, 3)\\nW: (1, 2)\\nNext: H"   h-down "H: (3, 3)\\nW: (1, 2)\\nNext: W")
             ("H: (2, 3)\\nW: (1, 2)\\nNext: W"   w-left "H: (2, 3)\\nW: (1, 1)\\nNext: H")
             ("H: (2, 3)\\nW: (1, 2)\\nNext: W"  w-right "H: (2, 3)\\nW: (1, 3)\\nNext: H")
             ("H: (2, 3)\\nW: (1, 3)\\nNext: H" h-remain "H: (2, 3)\\nW: (1, 3)\\nNext: W")
             ("H: (2, 3)\\nW: (1, 3)\\nNext: H"   h-down "H: (3, 3)\\nW: (1, 3)\\nNext: W")
             ("H: (2, 3)\\nW: (1, 3)\\nNext: W"   w-left "H: (2, 3)\\nW: (1, 2)\\nNext: H")
             ("H: (2, 3)\\nW: (1, 3)\\nNext: W"   w-down "H: (2, 3)\\nW: (2, 3)\\nDEATH")
             ("H: (2, 3)\\nW: (2, 1)\\nNext: H" h-remain "H: (2, 3)\\nW: (2, 1)\\nNext: W")
             ("H: (2, 3)\\nW: (2, 1)\\nNext: H"     h-up "H: (1, 3)\\nW: (2, 1)\\nFREEDOM")
             ("H: (2, 3)\\nW: (2, 1)\\nNext: H"   h-down "H: (3, 3)\\nW: (2, 1)\\nNext: W")
             ("H: (2, 3)\\nW: (2, 1)\\nNext: W"     w-up "H: (2, 3)\\nW: (1, 1)\\nNext: H")
             ("H: (2, 3)\\nW: (2, 1)\\nNext: W"   w-down "H: (2, 3)\\nW: (3, 1)\\nNext: H")
             ("H: (2, 3)\\nW: (3, 1)\\nNext: H" h-remain "H: (2, 3)\\nW: (3, 1)\\nNext: W")
             ("H: (2, 3)\\nW: (3, 1)\\nNext: H"     h-up "H: (1, 3)\\nW: (3, 1)\\nFREEDOM")
             ("H: (2, 3)\\nW: (3, 1)\\nNext: H"   h-down "H: (3, 3)\\nW: (3, 1)\\nNext: W")
             ("H: (2, 3)\\nW: (3, 1)\\nNext: W"     w-up "H: (2, 3)\\nW: (2, 1)\\nNext: H")
             ("H: (2, 3)\\nW: (3, 1)\\nNext: W"  w-right "H: (2, 3)\\nW: (3, 2)\\nNext: H")
             ("H: (2, 3)\\nW: (3, 2)\\nNext: H" h-remain "H: (2, 3)\\nW: (3, 2)\\nNext: W")
             ("H: (2, 3)\\nW: (3, 2)\\nNext: H"     h-up "H: (1, 3)\\nW: (3, 2)\\nFREEDOM")
             ("H: (2, 3)\\nW: (3, 2)\\nNext: H"   h-down "H: (3, 3)\\nW: (3, 2)\\nNext: W")
             ("H: (2, 3)\\nW: (3, 2)\\nNext: W"   w-left "H: (2, 3)\\nW: (3, 1)\\nNext: H")
             ("H: (2, 3)\\nW: (3, 2)\\nNext: W"  w-right "H: (2, 3)\\nW: (3, 3)\\nNext: H")
             ("H: (2, 3)\\nW: (3, 3)\\nNext: H" h-remain "H: (2, 3)\\nW: (3, 3)\\nNext: W")
             ("H: (2, 3)\\nW: (3, 3)\\nNext: H"     h-up "H: (1, 3)\\nW: (3, 3)\\nFREEDOM")
             ("H: (2, 3)\\nW: (3, 3)\\nNext: W"     w-up "H: (2, 3)\\nW: (2, 3)\\nDEATH")
             ("H: (2, 3)\\nW: (3, 3)\\nNext: W"   w-left "H: (2, 3)\\nW: (3, 2)\\nNext: H")
             ("H: (3, 1)\\nW: (1, 1)\\nNext: H" h-remain "H: (3, 1)\\nW: (1, 1)\\nNext: W")
             ("H: (3, 1)\\nW: (1, 1)\\nNext: H"     h-up "H: (2, 1)\\nW: (1, 1)\\nNext: W")
             ("H: (3, 1)\\nW: (1, 1)\\nNext: H"  h-right "H: (3, 2)\\nW: (1, 1)\\nNext: W")
             ("H: (3, 1)\\nW: (1, 1)\\nNext: W"  w-right "H: (3, 1)\\nW: (1, 2)\\nNext: H")
             ("H: (3, 1)\\nW: (1, 1)\\nNext: W"   w-down "H: (3, 1)\\nW: (2, 1)\\nNext: H")
             ("H: (3, 1)\\nW: (1, 2)\\nNext: H" h-remain "H: (3, 1)\\nW: (1, 2)\\nNext: W")
             ("H: (3, 1)\\nW: (1, 2)\\nNext: H"     h-up "H: (2, 1)\\nW: (1, 2)\\nNext: W")
             ("H: (3, 1)\\nW: (1, 2)\\nNext: H"  h-right "H: (3, 2)\\nW: (1, 2)\\nNext: W")
             ("H: (3, 1)\\nW: (1, 2)\\nNext: W"   w-left "H: (3, 1)\\nW: (1, 1)\\nNext: H")
             ("H: (3, 1)\\nW: (1, 2)\\nNext: W"  w-right "H: (3, 1)\\nW: (1, 3)\\nNext: H")
             ("H: (3, 1)\\nW: (1, 3)\\nNext: H" h-remain "H: (3, 1)\\nW: (1, 3)\\nNext: W")
             ("H: (3, 1)\\nW: (1, 3)\\nNext: H"     h-up "H: (2, 1)\\nW: (1, 3)\\nNext: W")
             ("H: (3, 1)\\nW: (1, 3)\\nNext: H"  h-right "H: (3, 2)\\nW: (1, 3)\\nNext: W")
             ("H: (3, 1)\\nW: (1, 3)\\nNext: W"   w-left "H: (3, 1)\\nW: (1, 2)\\nNext: H")
             ("H: (3, 1)\\nW: (1, 3)\\nNext: W"   w-down "H: (3, 1)\\nW: (2, 3)\\nNext: H")
             ("H: (3, 1)\\nW: (2, 1)\\nNext: H" h-remain "H: (3, 1)\\nW: (2, 1)\\nNext: W")
             ("H: (3, 1)\\nW: (2, 1)\\nNext: H"  h-right "H: (3, 2)\\nW: (2, 1)\\nNext: W")
             ("H: (3, 1)\\nW: (2, 1)\\nNext: W"     w-up "H: (3, 1)\\nW: (1, 1)\\nNext: H")
             ("H: (3, 1)\\nW: (2, 1)\\nNext: W"   w-down "H: (3, 1)\\nW: (3, 1)\\nDEATH")
             ("H: (3, 1)\\nW: (2, 3)\\nNext: H" h-remain "H: (3, 1)\\nW: (2, 3)\\nNext: W")
             ("H: (3, 1)\\nW: (2, 3)\\nNext: H"     h-up "H: (2, 1)\\nW: (2, 3)\\nNext: W")
             ("H: (3, 1)\\nW: (2, 3)\\nNext: H"  h-right "H: (3, 2)\\nW: (2, 3)\\nNext: W")
             ("H: (3, 1)\\nW: (2, 3)\\nNext: W"     w-up "H: (3, 1)\\nW: (1, 3)\\nNext: H")
             ("H: (3, 1)\\nW: (2, 3)\\nNext: W"   w-down "H: (3, 1)\\nW: (3, 3)\\nNext: H")
             ("H: (3, 1)\\nW: (3, 2)\\nNext: H" h-remain "H: (3, 1)\\nW: (3, 2)\\nNext: W")
             ("H: (3, 1)\\nW: (3, 2)\\nNext: H"     h-up "H: (2, 1)\\nW: (3, 2)\\nNext: W")
             ("H: (3, 1)\\nW: (3, 2)\\nNext: W"   w-left "H: (3, 1)\\nW: (3, 1)\\nDEATH")
             ("H: (3, 1)\\nW: (3, 2)\\nNext: W"  w-right "H: (3, 1)\\nW: (3, 3)\\nNext: H")
             ("H: (3, 1)\\nW: (3, 3)\\nNext: H" h-remain "H: (3, 1)\\nW: (3, 3)\\nNext: W")
             ("H: (3, 1)\\nW: (3, 3)\\nNext: H"     h-up "H: (2, 1)\\nW: (3, 3)\\nNext: W")
             ("H: (3, 1)\\nW: (3, 3)\\nNext: H"  h-right "H: (3, 2)\\nW: (3, 3)\\nNext: W")
             ("H: (3, 1)\\nW: (3, 3)\\nNext: W"     w-up "H: (3, 1)\\nW: (2, 3)\\nNext: H")
             ("H: (3, 1)\\nW: (3, 3)\\nNext: W"   w-left "H: (3, 1)\\nW: (3, 2)\\nNext: H")
             ("H: (3, 2)\\nW: (1, 1)\\nNext: H" h-remain "H: (3, 2)\\nW: (1, 1)\\nNext: W")
             ("H: (3, 2)\\nW: (1, 1)\\nNext: H"   h-left "H: (3, 1)\\nW: (1, 1)\\nNext: W")
             ("H: (3, 2)\\nW: (1, 1)\\nNext: H"  h-right "H: (3, 3)\\nW: (1, 1)\\nNext: W")
             ("H: (3, 2)\\nW: (1, 1)\\nNext: W"  w-right "H: (3, 2)\\nW: (1, 2)\\nNext: H")
             ("H: (3, 2)\\nW: (1, 1)\\nNext: W"   w-down "H: (3, 2)\\nW: (2, 1)\\nNext: H")
             ("H: (3, 2)\\nW: (1, 2)\\nNext: H" h-remain "H: (3, 2)\\nW: (1, 2)\\nNext: W")
             ("H: (3, 2)\\nW: (1, 2)\\nNext: H"   h-left "H: (3, 1)\\nW: (1, 2)\\nNext: W")
             ("H: (3, 2)\\nW: (1, 2)\\nNext: H"  h-right "H: (3, 3)\\nW: (1, 2)\\nNext: W")
             ("H: (3, 2)\\nW: (1, 2)\\nNext: W"   w-left "H: (3, 2)\\nW: (1, 1)\\nNext: H")
             ("H: (3, 2)\\nW: (1, 2)\\nNext: W"  w-right "H: (3, 2)\\nW: (1, 3)\\nNext: H")
             ("H: (3, 2)\\nW: (1, 3)\\nNext: H" h-remain "H: (3, 2)\\nW: (1, 3)\\nNext: W")
             ("H: (3, 2)\\nW: (1, 3)\\nNext: H"   h-left "H: (3, 1)\\nW: (1, 3)\\nNext: W")
             ("H: (3, 2)\\nW: (1, 3)\\nNext: H"  h-right "H: (3, 3)\\nW: (1, 3)\\nNext: W")
             ("H: (3, 2)\\nW: (1, 3)\\nNext: W"   w-left "H: (3, 2)\\nW: (1, 2)\\nNext: H")
             ("H: (3, 2)\\nW: (1, 3)\\nNext: W"   w-down "H: (3, 2)\\nW: (2, 3)\\nNext: H")
             ("H: (3, 2)\\nW: (2, 1)\\nNext: H" h-remain "H: (3, 2)\\nW: (2, 1)\\nNext: W")
             ("H: (3, 2)\\nW: (2, 1)\\nNext: H"   h-left "H: (3, 1)\\nW: (2, 1)\\nNext: W")
             ("H: (3, 2)\\nW: (2, 1)\\nNext: H"  h-right "H: (3, 3)\\nW: (2, 1)\\nNext: W")
             ("H: (3, 2)\\nW: (2, 1)\\nNext: W"     w-up "H: (3, 2)\\nW: (1, 1)\\nNext: H")
             ("H: (3, 2)\\nW: (2, 1)\\nNext: W"   w-down "H: (3, 2)\\nW: (3, 1)\\nNext: H")
             ("H: (3, 2)\\nW: (2, 3)\\nNext: H" h-remain "H: (3, 2)\\nW: (2, 3)\\nNext: W")
             ("H: (3, 2)\\nW: (2, 3)\\nNext: H"   h-left "H: (3, 1)\\nW: (2, 3)\\nNext: W")
             ("H: (3, 2)\\nW: (2, 3)\\nNext: H"  h-right "H: (3, 3)\\nW: (2, 3)\\nNext: W")
             ("H: (3, 2)\\nW: (2, 3)\\nNext: W"     w-up "H: (3, 2)\\nW: (1, 3)\\nNext: H")
             ("H: (3, 2)\\nW: (2, 3)\\nNext: W"   w-down "H: (3, 2)\\nW: (3, 3)\\nNext: H")
             ("H: (3, 2)\\nW: (3, 1)\\nNext: H" h-remain "H: (3, 2)\\nW: (3, 1)\\nNext: W")
             ("H: (3, 2)\\nW: (3, 1)\\nNext: H"  h-right "H: (3, 3)\\nW: (3, 1)\\nNext: W")
             ("H: (3, 2)\\nW: (3, 1)\\nNext: W"     w-up "H: (3, 2)\\nW: (2, 1)\\nNext: H")
             ("H: (3, 2)\\nW: (3, 1)\\nNext: W"  w-right "H: (3, 2)\\nW: (3, 2)\\nDEATH")
             ("H: (3, 2)\\nW: (3, 3)\\nNext: H" h-remain "H: (3, 2)\\nW: (3, 3)\\nNext: W")
             ("H: (3, 2)\\nW: (3, 3)\\nNext: H"   h-left "H: (3, 1)\\nW: (3, 3)\\nNext: W")
             ("H: (3, 2)\\nW: (3, 3)\\nNext: W"     w-up "H: (3, 2)\\nW: (2, 3)\\nNext: H")
             ("H: (3, 2)\\nW: (3, 3)\\nNext: W"   w-left "H: (3, 2)\\nW: (3, 2)\\nDEATH")
             ("H: (3, 3)\\nW: (1, 1)\\nNext: H" h-remain "H: (3, 3)\\nW: (1, 1)\\nNext: W")
             ("H: (3, 3)\\nW: (1, 1)\\nNext: H"     h-up "H: (2, 3)\\nW: (1, 1)\\nNext: W")
             ("H: (3, 3)\\nW: (1, 1)\\nNext: H"   h-left "H: (3, 2)\\nW: (1, 1)\\nNext: W")
             ("H: (3, 3)\\nW: (1, 1)\\nNext: W"  w-right "H: (3, 3)\\nW: (1, 2)\\nNext: H")
             ("H: (3, 3)\\nW: (1, 1)\\nNext: W"   w-down "H: (3, 3)\\nW: (2, 1)\\nNext: H")
             ("H: (3, 3)\\nW: (1, 2)\\nNext: H" h-remain "H: (3, 3)\\nW: (1, 2)\\nNext: W")
             ("H: (3, 3)\\nW: (1, 2)\\nNext: H"     h-up "H: (2, 3)\\nW: (1, 2)\\nNext: W")
             ("H: (3, 3)\\nW: (1, 2)\\nNext: H"   h-left "H: (3, 2)\\nW: (1, 2)\\nNext: W")
             ("H: (3, 3)\\nW: (1, 2)\\nNext: W"   w-left "H: (3, 3)\\nW: (1, 1)\\nNext: H")
             ("H: (3, 3)\\nW: (1, 2)\\nNext: W"  w-right "H: (3, 3)\\nW: (1, 3)\\nNext: H")
             ("H: (3, 3)\\nW: (1, 3)\\nNext: H" h-remain "H: (3, 3)\\nW: (1, 3)\\nNext: W")
             ("H: (3, 3)\\nW: (1, 3)\\nNext: H"     h-up "H: (2, 3)\\nW: (1, 3)\\nNext: W")
             ("H: (3, 3)\\nW: (1, 3)\\nNext: H"   h-left "H: (3, 2)\\nW: (1, 3)\\nNext: W")
             ("H: (3, 3)\\nW: (1, 3)\\nNext: W"   w-left "H: (3, 3)\\nW: (1, 2)\\nNext: H")
             ("H: (3, 3)\\nW: (1, 3)\\nNext: W"   w-down "H: (3, 3)\\nW: (2, 3)\\nNext: H")
             ("H: (3, 3)\\nW: (2, 1)\\nNext: H" h-remain "H: (3, 3)\\nW: (2, 1)\\nNext: W")
             ("H: (3, 3)\\nW: (2, 1)\\nNext: H"     h-up "H: (2, 3)\\nW: (2, 1)\\nNext: W")
             ("H: (3, 3)\\nW: (2, 1)\\nNext: H"   h-left "H: (3, 2)\\nW: (2, 1)\\nNext: W")
             ("H: (3, 3)\\nW: (2, 1)\\nNext: W"     w-up "H: (3, 3)\\nW: (1, 1)\\nNext: H")
             ("H: (3, 3)\\nW: (2, 1)\\nNext: W"   w-down "H: (3, 3)\\nW: (3, 1)\\nNext: H")
             ("H: (3, 3)\\nW: (2, 3)\\nNext: H" h-remain "H: (3, 3)\\nW: (2, 3)\\nNext: W")
             ("H: (3, 3)\\nW: (2, 3)\\nNext: H"   h-left "H: (3, 2)\\nW: (2, 3)\\nNext: W")
             ("H: (3, 3)\\nW: (2, 3)\\nNext: W"     w-up "H: (3, 3)\\nW: (1, 3)\\nNext: H")
             ("H: (3, 3)\\nW: (2, 3)\\nNext: W"   w-down "H: (3, 3)\\nW: (3, 3)\\nDEATH")
             ("H: (3, 3)\\nW: (3, 1)\\nNext: H" h-remain "H: (3, 3)\\nW: (3, 1)\\nNext: W")
             ("H: (3, 3)\\nW: (3, 1)\\nNext: H"     h-up "H: (2, 3)\\nW: (3, 1)\\nNext: W")
             ("H: (3, 3)\\nW: (3, 1)\\nNext: H"   h-left "H: (3, 2)\\nW: (3, 1)\\nNext: W")
             ("H: (3, 3)\\nW: (3, 1)\\nNext: W"     w-up "H: (3, 3)\\nW: (2, 1)\\nNext: H")
             ("H: (3, 3)\\nW: (3, 1)\\nNext: W"  w-right "H: (3, 3)\\nW: (3, 2)\\nNext: H")
             ("H: (3, 3)\\nW: (3, 2)\\nNext: H" h-remain "H: (3, 3)\\nW: (3, 2)\\nNext: W")
             ("H: (3, 3)\\nW: (3, 2)\\nNext: H"     h-up "H: (2, 3)\\nW: (3, 2)\\nNext: W")
             ("H: (3, 3)\\nW: (3, 2)\\nNext: W"   w-left "H: (3, 3)\\nW: (3, 1)\\nNext: H")
             ("H: (3, 3)\\nW: (3, 2)\\nNext: W"  w-right "H: (3, 3)\\nW: (3, 3)\\nDEATH"))

           "H: (3, 1)\\nW: (1, 1)\\nNext: H"

           '("H: (1, 3)\\nW: (1, 1)\\nFREEDOM"
             "H: (1, 3)\\nW: (1, 2)\\nFREEDOM"
             "H: (1, 3)\\nW: (2, 1)\\nFREEDOM"
             "H: (1, 3)\\nW: (2, 3)\\nFREEDOM"
             "H: (1, 3)\\nW: (3, 1)\\nFREEDOM"
             "H: (1, 3)\\nW: (3, 2)\\nFREEDOM"
             "H: (1, 3)\\nW: (3, 3)\\nFREEDOM")))



;; DES for question 3
(defparameter *des-3*
  (make-fa '(("H: (2, 1)\\nW: (1, 2)"  right "H: (2, 2)\\nW: (2, 2)")
             ("H: (2, 1)\\nW: (1, 2)" remain "H: (2, 1)\\nW: (2, 2)")
             ("H: (2, 1)\\nW: (2, 2)" remain "H: (2, 1)\\nW: (2, 1)"))
           "H: (2, 1)\\nW: (1, 2)"
           '()))

;; DES for question 4
(defparameter *des-3*
  (make-fa '(("W: (1, 1)\\nH: (2, 2)" right "W: (1, 2)\\nH: (2, 3)")
             ("W: (1, 1)\\nH: (2, 2)"  down "W: (2, 1)\\nH: (2, 3)"))
           "W: (1, 1)\\nH: (2, 2)"
           '()))
