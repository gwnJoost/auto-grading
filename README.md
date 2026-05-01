# Automated Grading of Logical exercises
The main purpose of this program is to be able to grade various types of logical exercises.

## How to use
To build the program, first run:

```stack build```

The program runs using the following command:
```stack exec -- auto-grading-exe -g (GRADER) -i (ANSWER TO BE GRADED) -a (MODEL ANSWER) -p (POINTS) -f``` 

Each flag does the following:

`-g`: 
Give the auto test to be used

    TESTS:

    - checkTF: Check if two booleans are equal to eachother.
        two are equivalent: 100% of points.
        two are not equivalent: 0% points.
    - predEquiv: Check if two propositional formula are equivalent.
        two are equivalent but not equal: 50% of points.
        two are equivalent and equal: 100% points.
        otherwise: 0% points.
    - setEquiv: Check if two sets are equivalent.
        two are equivalent but answer is not reduced: 50% of points.
        two are equivalent and answer is reduced: 100% of points.
        otherwise: 0% points.

`-i`: 
Input answer to be graded.

`-a`:
Model answer to compare input to.

`-p`:
Determine what should be the maximum amount of points awarded, set to 1 if none are given.

`-f`: 
Use this option to read in from a file instad of the arguments. -i and -a should be directories instead of singular lines.