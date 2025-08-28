# Guide to the icosa-testing suite

## Basics

The test suite was migrated from using `testthat` to `tinytest` to minimize the dependencies. It was also completely reorganized for efficient testing of functions.

Note that despite having tens of thousands of individual test cases, the unit-testing suite is still not complete - and as with the rest of R. 


## Files

- `main.R`: entry point to the tests.
- `source.R`: worker initialization script.

### A. Directories with test-masters

*Directories, that are not pre-fixed*

These are the *second-order* entry points to the individual tests and they specify input data to the tests, and are called to from `main.R`.

These *second-order scripts* are dedicated to testing specific functions in specific settings, but since `icosa` is a closed ecosystem, many of these testing scripts utilize other functions as well - which have calls that are explicitly assessed. 

The *second-order script* files frequently define different data structures that represent a wide range of input. The correct run of functions is documented/tested the same way for these, so in order to maximize code reuse, **testing methods** are frequently implemented for these, rather than individual expectations. Such **testing methods** are loose scripts that expect certain objects in the namespace where the *second-order* scripts are running (they are sourced into the local environment). This allows the running of the same testing methods for iteratively created inputs, and a spectrum of input (e.g. grids with different tessellation). The testing methods can be found in the *`_methods`* directory.

Because many tests are run within loop constructs, if an error occurs, the proper diagnosis usually requires the interactive running of the *second-order scripts* in the R REPL. The boolean flag `diag` can be set to `TRUE` to force the printing of the individual test results with `tinytest`.

### B. Directories with underscores 

*The `_methods` directory*

Directory includes testing methods for various functions.

*The `_results` directory*

This directory includes some reference results with graphics that can be used to validate the numeric results, with which the actual testing will compare new results with. 
