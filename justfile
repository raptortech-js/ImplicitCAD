build:
	@time stack build --verbosity warn

do:
    stack run --verbosity warn

test:
	time stack test

