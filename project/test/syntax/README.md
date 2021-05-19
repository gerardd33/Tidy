# Syntax and parser tests

## Contents

These tests are designed to test the syntax defined in ``Tidy/project/src/Parser/Tidy.cf`` and the parser generated from it by BNFC over manually prepared Tidy source files.

The tests are a group of *.ty* files: *good* and *bad* syntax examples in the *unit* directory and larger *integration* tests for syntax.


## Testing

Before running any tests, run ``./prepare_syntax_test_env.sh``.

Then you can:

- Enter your own example manually in ``Test.ty`` and test its correctness by typing ``./single_test.sh``.
- Run the whole test suite by typing ``./run_all_tests.sh``.
