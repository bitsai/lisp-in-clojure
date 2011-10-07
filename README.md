[Inspiration](http://onestepback.org/index.cgi/Tech/Ruby/LispInRuby.red)

eval.clj implements the 'eval' function described in Paul Graham's ["The Roots of Lisp" paper](http://lib.store.yahoo.net/lib/paulgraham/jmc.ps)

env.clj defines the environment, which contains additional functions added by evaluating 'defun' expressions:

* subst

* caar

* cadr

* cdar

* cadar

* caddr

* caddar

* list

* null

* and

* not

* append

* pair

* assoc

* eval (yep, you can run a Lisp interpreter inside the interpreter!)

* evcon

* evlis

reader.clj implements the simplified reader in ["An Introduction to Scheme and its Implementation"](http://www-pu.informatik.uni-tuebingen.de/users/sperber/pfg-2001/scheme/schintro-v14/schintro_115.html#SEC137) (with additional support for the quote special form)

repl.clj (clj repl.clj) launches a simple REPL (CTRL-C to quit).

tests.clj (clj tests.clj) tests the interpreter using examples from Paul Graham's paper.
