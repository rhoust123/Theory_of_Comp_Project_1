# CSCI 561 --- Project 1

The purpose of this project is to learn about:

1. Algorithms for regular language manipulation.

2. The application of regular languages to:
   a. text processing,
   b. the control of discrete event systems.

## INSTRUCTIONS

### Setup
1. Form groups of 2-4 to work on this project.
    * "Groups" of one are not allowed.
        * The era of "cowboy coding" ended decades ago, all software engineering is a team effort.
        * You will receive a **-25% penalty** if you do not join a group.

1. From the github.com interface for this repo, click `Use this
   template` to import the starter code.  Create a new, **private** repo
   for your team.

1. Give all team members access to your new repo.

1. Clone the repo.

1. Complete the function stubs in `project-1.lisp`.

1. Answer the questions in `project-1-report.pdf`.
    * You **must** typeset your answers
    * ...and are *highly* recommended to use the provided `.tex` file

### Code

The code portion of the project is divided into four parts,
corresponding to different lecture segments.  For each function to
complete, the expected total length of the function (starter code plus
code you will write) is listed below.

- Part 0, Introduction.  Complete the following functions:
  - DFA-SIMULATE, Expected length: about 15 lines

- Part 1, Subset construction. Complete the following functions:
  - E-CLOSURE, Expected length: about 10 lines
  - MOVE-E-CLOSURE, Expected length: less than 10 lines
  - NFA->DFA, Expected length: about 30 lines
  - NFA-SIMULATE, Expected length: about 15 lines

- Part 2, Regular Expressions. Complete the following functions:
  - SIMPLIFY-REGEX, Expected length: about 25 lines
  - FA-UNION, Expected length: about 20 lines
  - FA-REPEAT, Expected length: about 15 lines
  - REGEX->NFA, Expected length: about 30 lines

- Part 3, Regular Language Properties, Complete the following
  functions:
  - FA-EMPTY, Expected length: about 15 lines
  - DFA-MINIMIZE, Expected length: about 30 lines
  - DFA-INTERSECTION, Expected length: about 50 lines
  - DFA-EQUIVALENT, Expected length: about 10 lines

## Submission

### Code
Submit `project-1.lisp` (and *only* that file) to [Project 1 - Code](https://www.gradescope.com/courses/814289/assignments/4973842) on Gradescope before the due date.

* Make sure to add your teammates to the submission.

Keep the same function names as given in the starter code. The grading scripts test your code based on these function names, and if you change them, your code will not pass the tests.

### Report
Submit `project-1-report.pdf` to [Project 1 - Report](https://www.gradescope.com/courses/814289/assignments/4973908) on Gradescope before the due date.

* Make sure to add your teammates to the submission.

### Grading

Your code will be run for grading using (approximately) the following procedure:

```sh
sbcl --load project-1.lisp --load testcases.lisp
```

Your code needs to produce the correct result to receive credit for each testcase. Make sure your code loads cleanly without compilation errors, extra I/O, or other side effects.


"Correct" for REGEX->NFA, NFA->DFA, and DFA-INTERSECTION means that
the language defined by the returned finite automaton is equivalent
and, for NFA->DFA, that the result is a DFA.  "Correct" for
DFA-MINIMIZE means that the language defined by the returned DFA is
equivalent and the state is minimized.

Please consider and appropriately handle potential edge cases in the
input.
