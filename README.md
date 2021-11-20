# opl

This is an attempt at producing x64 AT & T assembly from Wirth's original
PL/0. Some minor extensions might be supported. The code is unlikely to be of
proper ML nature, as this is really an OCaml exercise. The goal is to
successfully compile the attached `test.pl0` which should be an original
example from Wirth's book.

# Bits and pieces

Lots of unnecessary recursion is being done as all AST visitors operate on
their own pass for simplicity. To get faster compilation, these should be
combined into a single-pass traversal.

# TODO

- [x] parse `test.pl0` and generate the AST
- [x] do scope analysis for visible constants and variables
- [x] search for referring to not-in-scope identifiers
- [ ] a common logging interface with verbosity for AST handlers
- [ ] extend AST types to store source line:column
- [x] constant folding for integer arithmetic
- [x] constant folding for constant conditionals
- [x] common subexpression elimination
- [ ] report AST errors with line:column info
- [ ] loop-invariant hoisting
- [ ] dead store elimination
- [x] search for calling undeclared functions
- [x] search for assigning to constants
- [x] dumb stack machine codegen
- [ ] register-allocating codegen
