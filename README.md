# Automated Grading of Logical exercises
The main purpose of this program is to be able to grade various types of logical exercises.

## How to use
To build the program, first run:

```stack build```

The program runs using the following command:
```stack exec -- auto-grading -g (GRADER) -i (ANSWER TO BE GRADED) -a (MODEL ANSWER) -p (POINTS) -f``` 

Each flag does the following:

`-g`: 
Give the auto test to be used

    TESTS:

    - checkTF: Check if two booleans are equal to eachother.
        two are equivalent: 100% of points.
        two are not equivalent: 0% points.
    - propEquiv: Check if two propositional formula are equivalent.
        two are equivalent but not equal: 50% of points.
        two are equivalent and equal: 100% points.
        otherwise: 0% points.
    - setEquiv: Check if two sets are equivalent.
        two are equivalent but answer is not reduced: 50% of points.
        two are equivalent and answer is reduced: 100% of points.
        otherwise: 0% points.
    - validateND: Check if a natural deduction proof is valid and proves the correct statement.
        model input answer should contain a natural deduction proof with only the premises and the final derivation.

`-i`: 
Input answer to be graded, example:

    for propositional formula: 1 and 2

    for sets: {1,2,3}

For reading in a file: ```question.txt```. This would read out anything within the file ```question.txt```

`-a`:
Model answer to compare input to, uses same input as `-i`.

`-p`:
Determine what should be the maximum amount of points awarded, set to 1 if none are given.

`-f`: 
Use this option to read in from a file instad of the arguments. `-i` and `-a` should be directories instead of singular lines.

## examples
The examples folder contains an example model contains example files to test the automatic grader. Each grader contains an example file with a model answer to use, along with several files containing candidate answers. To run the automatic grader using the example files, simply execute the following command:

```stack exec -- auto-grading -g (GRADER) -i "examples/(QUESTIONTYPE)/(CANDIDATEANSWER)" -a "examples/(QUESTIONTYPE)/modelAnswer.txt" -p (POINTS) -f```
- GRADER: Which type of question is to be graded. See previous explaination for list of graders.
- QUESTIONTYPE: The system to which the question relates to. Can either be Propositional, Set or Ndeduction.
- POINTS: Amount of points that can be awarded.
- CANDIDATEANSWER: The answer to be graded. Can be either: notCorrect.txt, partiallyCorrect.txt or fullyCorrect.txt. Numbers are appended to the end of the filename in case multiple variants exist.

For example, to run the grader testing the partially correct answer for propositional logic, run the following command:

```stack exec -- auto-grading -g propEquiv -i "examples/Propositional/partiallyCorrect.txt" -a "examples/Propositional/modelAnswer.txt" -f```
