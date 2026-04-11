build:
	stack build

run:
	stack build && stack exec auto-grading-exe

exec:
	stack exec auto-grading-exe