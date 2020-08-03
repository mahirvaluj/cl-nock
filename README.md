# cl-nock

This is an interpreter for
[nock](https://urbit.org/docs/tutorials/nock/definition/), Urbit's
purely functional machine code, that I wrote in an afternoon.

This is by no means production ready, as it does not respect hints
(opcode 11), but it does moderately non-trivial code, for example, the
decrement function [provided at the end of this
page](https://urbit.org/docs/tutorials/nock/example/) runs without
error, thus all the opcodes function properly, just slowly (again,
because of the lack of respecting hints).


Examples:

```lisp
CL-USER> #n[42 [[0 1] [1 0]]]
(42 (0 . 1) 1 . 0)

CL-USER> (nock:tar #n[42 [[0 1] [1 0]]])

(42 . 0)
CL-USER> (nock:tar #n[3 [8 [1 0] 8 [1 6 [5 [0 7] 4 0 6] [0 6] 9 2 [0 2] [4 0 6] 0 7] 9 2 0 1]])

2
```

The only exposed functions are a reader macro (`#n`), which will read
an `[]` expression, as demonstrated above, and `tar`, which will
interpret the machine code found. The combination of these two
functions allows for a well-integrated and easy development
experience.

