typecheck:
	find src/ test/ -name '*.gleam' | entr -s 'clear; gleam check'
