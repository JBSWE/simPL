This code supports the Programming Language Implementation course
It contains a bunch of APIs for computing various semantics described in the course.
Examples for using these APIs are in the corresponding test packages.

In this tutorial, we will be practicing
-- find the free variables of SimPL code
-- type inference of SimPL programs
-- the Denotational Semantics of SimPL (to generate the virtual machine code).
To finish this tutorial, you need to:

1) compiler all the code and run all the test cases.
Upon cloning the project, simply do
```
sbt test
```
Alternatively, you may import the code into IntelliJ as an Scala `sbt`. Make sure that you have the Scala and Antlr plugins loaded before doing that. Then you can just run the tests (methods in classes whose names have the suffix 'Test') individually.
Each test is an example of how the language APIs can be used.

2) fill up all the pattern matching blanks in 3 files:
 1. src/main/scala/edu/nus/comp/pli/simpl/util/FreeVariables.scala
 2. src/main/scala/edu/nus/comp/pli/simpl/util/TypeInfer.scala
 3. src/main/scala/edu/nus/comp/pli/simpl/util/SimplEplDenotationalSemantics.scala

 (You are highly suggested to fill the file in the order of 1, 2, 3)

 To ease your work, all the blanks are marked with comment "// add you code here"

3) after programming, all the test cases are expected to be passed.

(Note: the AST of SimPL is defined in src/main/antlr4/simpl/SimplParser.g4
       all the test cases are stored in src/test/resources/simpl/)

---------------------------------------------------------------- *
Origin repository: https://github.com/razvanvoicu/pli
Revised by:        Yahui Song  (e0210374@u.nus.edu)
Date:              9/3/2019
Purpose:           Tutorial for CS4215 (2018/2019 sem2)
---------------------------------------------------------------- *
Submission: please submit a compressed folder which is renamed with: "Lab06_<ID>_<Name>"
eg. Lab05_A12345678_Yahui.zip

Deadline: 29/03/2019 23:59pm