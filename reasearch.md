### What I learned about CL while doing this exercise

[03.03.2015]

- CL lists are much more like JSON objects than I thought.
- what a compound type is. A data type that is not an atom. Or a
  datatype that can contain another data types.
- `dotimes` has quite strange syntax. You should state what you want it
  to return as the third parameter
- `append` behaves like `concat` in JS.

[04.03.2015]

- lisp forces you to think a lot more about how the data is passed
  around
- mathematical opperations feel clunky. Maybe this is a matter of
  experience.
- learned what `dolist`, `make-list` and `make-array` do.
- the `loop` macro is a DSL on its own. Accordint to SO, some lispers
  like it and other hate it. I am in neither camp so far.
- `let` creates variable bindings IN PARALLEL. To create the
  bindigs sequentially you should use `let*`. Lost one lovely hour in debugging.

[05.03.2015]

- learned about functions that perform operations on lists: `nthcdr`, `pop`, `butlast`, `ldiff`, `reduce`...
- `nconc` has side effects. Use `append` instead.
- learn about bit operands and bit-related funtions. `integer-length` counts the number of bits an
  integer has.
- learned about documentation strings on function declaration. (Much like python's """ doc
strings """)
- to read the doc string of any function use `(documentation 'fn-name 'function)` to get the doc string only or `(describe 'fn-name)` for more throught information.
- you can have a variable and a function with the same name.
- functions are actually 'data objects'. Function defenition with
  `defun` just assigns a piece of code to a name.
- use `apply` to 'explode' a list of arguments. For example, to
  flatten an array: `(apply #'append 'array)`

[06.03.2015]

- use `when` and `unless` if the conditional doesn't have an
  `else`. Unlike `if` these statements allow multiple
statements in their bodies. To do the same with `if` you shoud use
`progn`.
