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

`-i`: 
Input answer to be graded.

`-a`:
Model answer to compare input to.

`-p`:
Determine what should be the maximum amount of points awarded, set to 1 if none are given.

`-f`: 
Use this option to read in from a file instad of the arguments. -i and -a should be directories instead of singular lines.