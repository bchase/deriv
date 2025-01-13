typecheck:
	find src/ test/ -name '*.gleam' | entr -s 'clear; gleam check'

watch-tests:
	find src/ test/ -name '*.gleam' | entr -s 'clear; gleam test'
