# mxrlang

## Introduction
Mxrlang is a toy language which uses LLVM as its backend. It serves as an excercise in using LLVM compiler infrastructure to create a general purpose programming language. Mxrlang is a C-like language, although it supports just a basic set of C-like features (seeing as it was created for the purpose of learning).

## Features
### Type system
Mxrlang is a strongly, statically typed language. It has two basic types (BOOL and INT) which cannot be cast into each other. It also supports array and pointer types.

### Declarations
Variables can be declared on a global scope (outside of any functions), or on a local scope:

      VAR x : INT := 5;
      VAR y : BOOL := TRUE
      VAR px : INT* := &x;
      VAR arr : INT[3] := {1, 2, 3};
      
Functions are declared and defined all at once:

      FUN foo : INT(x : INT, y: BOOL)
        ...
        RETURN x;
      NUF
      
      FUN main : INT()
        ...
        RETURN 0;
      NUF
      
Every program **must have a main function declaration with the above signature**.
Every function **must have a single return statement at the end of its body**. Return statements in the middle of a function body are not yet permitted.

### Statements
Mxrlang supports **IF-THEN-ELSE** and **WHILE-DO** control flow statements:

      IF x THEN
        ...
      ELSE
        ...
      FI
      
      WHILE x DO
        ...
      ELIHW
      
Assignment is performed with the walrus (**:=**) operator:

      x := FALSE;

**PRINT** and **SCAN** builtin statements can be used to print or load (respectively) a single variable:

      PRINT x;
      SCAN y;
      
### Arithmetic and logical expressions
Mxrlang supports basic binary arithmetic operators: **+**, **-**, **\***, **/** - these can only be used on operands of type INT.
Supported binary comparison operators are: **=**, **!=**, **>**, **>=**, **<**, **<=**.
Supported binary boolean operators are: logical and (**&&**), and logical or (**||**) - these can only be used on operands of type BOOL.
Two unary negation operators are present: **!** - negation for BOOL type; **-** - negation for INT type.
Expressions can be grouped together using parentheses: **(**, **)**.

### Arrays
Arrays of any type can be declared like so:

      VAR arr1 : INT[3] := {1,2,3};
      VAR arr2 : BOOL[3] := {FALSE,TRUE,FALSE};
      VAR arr3 : INT*[3];
      VAR arr4 : INT[2][3] := {{1,2,3},{4,5,6}};
      
Arrays can be accessed using squared brackets **\[**, **\]**:

      VAR x : INT := arr[2];
      
### Pointers
Mxrlang supports pointers to basic types, or to other pointers:

      VAR p1 : INT*;
      VAR p2 : BOOL*;
      VAR pp1 : INT** := &p1;
      
C-like dereferencing and address-of operators are supported:

      VAR x : INT := *p1;
      VAR px : INT* := &x;
      
## How to build
Mxrlang, as other similar LLVM projects, uses CMake build system (minimum version 3.4.3).
The user must have LLVM 14.0.0. installed on the system.
The instructions to build the compiler are as follows:

  * Clone the repository to your local machine.
  * Create a build/ directory and position yourself in it.
  * Run the following command: cmake -DCMAKE_CXX_FLAGS="-fexceptions" ../
  * After CMake configures the project, build it with: make
  * The compiler executable can be found inside the build/tools/driver directory.

## How to use
Mxrlang files must have **.mxr** extension. To compile a .mxr file, run the following command:

      mxrlang file.mxr
      
This will produce an assembly file **file.s**. To compile the .s file into an executable, you must use gcc or clang:

      gcc file.s -o file
      
To emit the LLVM IR of the program, run the compiler with **-emit-llvm** flag. This will produce an .ll file instead of an .s file.
To print out the AST of the program, run the compiler with **-print-ast** flag.
