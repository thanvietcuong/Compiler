Students: 
  Alex Malyshev,
  Cuong Than, 
  Bashyam Srinidhi  
------------------------------
PA5 - Cool Interpreter
-------------------------------

List of files:
  1: readme.txt: this README file
  2: Source files: interpreter.ml
  3: Makefile

Usage:
	1: Run the Makefile: 
	  	Commands:  
	    To clean: $ make clean
	    To build: $ make   
	    Result: creates interpreter.exe, and interpreter.o

    2: Test the interpreter: 
	  	Command:  
	    $ cool --type file.cl
	    $ ./interpreter.exe file.cl-type


Design:

	Given input: Cool Annotated AST
	Output: Evaluated (interpreted) result of the Cool program.

	The annotated AST is parsed into its corresponding data structures of the class-map and the implementation-map using worker functions. 

	The interpreter starts evaluating the program from the main method. Every expression is converted to a value. If the expression is not initialized, a default value is assumed according to the Cool Reference Manual (CRM). 

	Environment Map: To store the environment map, a association list is used to map the variable named to Cool address.

	Store Map: Like the Environment map, a store maps the Cool address to its Cool value.

	The expressions are evaluated using a recursive procedure that implements the operational semantics as per CRM. The procedure switches on every expression type, using a match statement and returns a tuple of a value and a store.

	Few complicated expressions are handled using following approach:
	1: Case: Case has a main expression and list of case elements. We get the type of the main expression(types for Object, String, Int and Bool. The Void type throws and error). We iterate through the ancestor list of the main expression and choose the closest type that matches the case element types. The ascestor list is created using the parent map from AST.
	Rest of the rules are straight forward and as per the CRM.

	2: Internal: 
		IO: "IO.out_int", "IO.in_int", "IO.out_string", "IO.in_string"
			The string and int inputs are read, parsed or printed accordingly using OCAML's equivalent functions like read_line, Int32.of_int, Int32.of_string
		Object: 	
			"Object.copy" -> for Cool Int, String, Bool, and Void, we just return the same value, but for Object, we copy all the attributes, create new location, update the store and environemt before returning the updated Object with new location.
		String: "String.concat" , "String.substr" 
			We make use of OCAML String library's String.concat and String.sub respectively

	3: Let: Let has a list of binding and an expression body. The list of bindings are iterated and each expression in the binding is evaluated and the store/environment maps are updated before the next iteration. Finally the body is evaluated using the updated store/env.

	4: New: We handle expression depending on the class name. If the type is Bool, Int, or String we create a Cool Bool(defaulted to false), Int(defaulted to 0), or String(defaulted to "") respectively. For an Object type, we create a new location for the attributes anf the object, update the store/environment mapts and return the Cool Obeject as the value.
		We also check for "Stack Overflow" runtime error if and only if there are 1000 (one thousand) or more outstanding Cool Activation Records using a global counter variable. 


	5: Dispatch: 
		All kinds of dispatches are handled similarly but with some expections on the type of object the method is evaluated. We get the formals, parent and body from the implementation map for that particular Class and Method. New locations are created for each arguments and the store/env. maps are updated. Finally the mehod body is evaluated on the updated stores according to the CRM rules.
		We divided dispatch into 3 categories:
			1: SelfDispatch: We check for the Self Dispatch on a Cool Object and throw error on Void.
			2: StaticDispatch: We check for the Static Dispatch on a Cool Object and throw error on Void.
			3: Dispatch (Dynamic): We check for the Static Dispatch on a Cool Object, String, Int and Bool and throw error on Void.
		The "Stack Overflow" runtime error is checked in all the dispatches.

References:
1: Westley Weimer's - Video Guide - PA5c - OCaml at https://youtu.be/i-nBIxE5Xcs
