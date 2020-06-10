I use flex to generate a lexer in C. I choose flex because it supports C/C++. After reading some comments about flex and running some test programs, I decided to use C because flex supports this language better than C++. Flex can compile a .lex file (a C file with additional definition blocks for regular expression declaration) to a C file. After that, we can run it using the normal C compiler. The hardest part of C is strings handling. Since C use pointer to represent strings, so every string manipulation should be processed by using pointers.

Flex supports regular expression and context-free grammar, so a string can be recognized by the following set of rules:
        <INITIAL> <- "<STRING>"
        <STRING> <- .<STRING>
with "." is any character except the NULL or newline character. So if the program read those character while scanning a string, it reports an error and terminates. A special case is Cool string also accepts embedded double quotes in the form \". So we should add a new rule: 
        <STRING> <- \"<STRING>
This rule must have a higher priority than the terminate string rule in order to prevent error. 

Like the string, a comment like (* comment *) could be handled by a set of rules:
        <INITIAL> <- (*<COMMENT>*)
        <COMMENT> <- .<COMMENT>
But in this case, the comment can be nested, which means a comment like (*comment 1(*comment 2*)comment 3*) is also valid. So we should check the number of comment opening symbols (*, the number of closing symbol *). We only complete a comment if those two numbers are the same. While reading the comment, we also need to keep track of the newline. Whenever a newline symbol is read, the program should increase the line counter by 1. If the comment is a dash comment, the program reads the next line and increases the line number by 1. The regular expression for dash comment is --.*\n.

The other token can be handle directly by regular expressions. For example, RE for integer is [0-9]+, RE for identifier is [a-z]([a-zA-Z_]|[0-9])+, ...

The regular expression and rules must be declared in a special block. After declaration, all remaining part of the code is in C language. 

TEST CASE: 
The first good test case is file rosetta.cl. I chose this because rosetta.cl is quite long. And the longer the file is, the better we can detect the bugs in our program. I also added a nested comment, a valid string with embedded double quotes inside and all tokens to the file. If the file passes this test, there is a high probability that the code covers all the valid tokens.

The bad test case is a simple arithmetic program with an uncompleted nested comment in the end. This comment likes (*(*comment*) is a tricky case that can only be detected if the programmer sets up the comment recognition correctly. 
