all:
	stack init
	stack setup
	stack build

clean:
	stack clean