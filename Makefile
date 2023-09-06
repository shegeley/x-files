check:
	guile -L ./tests -L ./src -c \
	'((@ (x-files tests utils) run!))'
