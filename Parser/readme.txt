Some core design decisions have been inspired by Wes Weimer's ideas (https://youtu.be/3xCJMyawoxg). To print the abstract syntax tree in pre-order, we used a recursive algorithm. When our program reads the parent node, it would print the current node information fist, before visiting each child from left to right recursively.

The high-level design idea of a solution is to check the output of lexer against context-free grammar. We, firstly, saved input into a queue as tokens and then recursively process them through hierarchical grammar rules. The recursive nature of OCaml allows concisely to implement this logic.

To compile, use the command: make. For an input file <filename>.cl-lex, run the following command: 
	./main <filename>.cl-lex
This command will produce an output file <filename>.cl-ast. 

The most challenging part was building rules that match lists of arguments, expressions, etc. In this step, we followed the instructions on PA3 website.
