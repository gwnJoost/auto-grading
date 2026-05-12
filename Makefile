build:
	stack build

run:
	stack build && stack exec auto-grading

exec:
	stack exec auto-grading

clean:
	stack clean --full