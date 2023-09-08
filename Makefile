check:
	guile -L ./tests -L ./src -c \
	'((@ (x-files utils tests) run!))'
