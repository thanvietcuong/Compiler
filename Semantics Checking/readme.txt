Students: 
  Cuong Than, 
  Bashyam Srinidhi,
  Micheal Kovar,
  Alex Malyshev
------------------------------
PA4 - Semantic Analyzer
-------------------------------

Usage:

  First, copy the executable cool compiler to the current folder and name it "cool".  

  You can compile the program with the following command:
    make
  
  That command will generate a checker-pa4.exe file. This file can be run using:
    ./checker-pa4.exe <input Cool ast file>
  
  You can also run the program with a Cool file using the following command from the Makefile:
    make test file=<input Cool file>
  
  You can run the above command against the reference compiler by doing:
    make diff file=<input Cool file>
  
  To check all the tests in the folder, you can run the following command:
    make diff-all

Design:

Our program can be broken up into the following sections: reading and storing the Cool AST, parsing the AST for any method and class declaration issues, type checking all of the methods, and finally outputting the new annotated AST.

Our code for reading in the AST and storing it is fairly simple and follows the pattern we have for the lexing and parsing code seen before. There are no tricks involved.

For our code to check the declaration of classes and methods for errors, we first start by parsing the AST to store parent-child relationships (which is determined using a simple topological sort) as well as what methods and attributes each class has. After we set up this data, we then check the declarations of each of the classes to ensure that things like invalid class names or redeclaration of existing or forbidden classes do not occur. We then go on to populate the object environment for each class with its attributes.

After we populate a class's attributes and have ensured that the declaration of the class itself is fine, we go on to type check each method. For the type checking in, we recursively call our expression type checking method and make use of the method, object, and class environments to perform all of the type checking for each expression type. The only notable type checking tricks that we use involve the Dispatch and Let expressions. For the three Dispatch expressions, we use the same core helper method to check their arguments against the formal definitions and to check that the given method is present in the context of the caller. The Let expression is also interesting because we have to populate a new object environment based upon the old object environment with all of the new variables that are present in the new scope. Successive expression type checks in the current block scope following this code will use the new object environment that is created here.

After we complete all of the type checking, we follow the same general strategy used for outputting tokens in PA2 and PA3 to output our completed type-checked AST. 

We believe that our Semantic Analyzer is currently feature-complete and should fulfill all of the functional requirements, based upon our testing with our test cases and the instructor provided ones. The only remaining work that we would do for it involves some cleanup and optimization.

References:
1: Westley Weimer's - Video Guide - PA4c - OCaml at https://www.youtube.com/watch?v=Wa9zMygcv_M
2: Westley Weimer's - Video Guide - PA4 - OCaml at https://www.youtube.com/watch?v=Oxpgrkmsxhg
