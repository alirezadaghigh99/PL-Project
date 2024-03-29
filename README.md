# CE40364 Project
An interpreter written in Racket for a given programming language. This code is developed for [CE40364 (Design of Programming Languages) Course Project](http://ce.sharif.edu/courses/99-00/1/ce364-1/index.php).

## Getting Started
- Download and install DrRacket based on your platform.([here](https://download.racket-lang.org/))
- Open terminal and insert following command

    `git clone https://github.com/alirezadaghigh99/PL-Project.git`
- Run [interpreter.rkt](https://github.com/alirezadaghigh99/PL-Project/blob/master/interpreter.rkt)

## Source Code
Source code is consisted of four parts. Functionality of each part will be explained in this part.

### parser-lexer.rkt
As its name shows, this module does lexical analysis process and then creates parser according to given grammar.

### environment.rkt
This module defines environment functions to store variables and their values.

### operations.rkt
This module is used for handling defined operations, such as addition, multiplication, comparison, list operations, etc. Error handling is done in this module too.

### interpreter.rkt
This module is main module and combines all other parts to compute value of given program by interpreting input language to Racket.

## Outputs
In this section you can see two codes in given language and their corresponding output in racket.

![alt text](https://github.com/alirezadaghigh99/PL-Project/blob/master/inputs_outputs/a_input.png)

![alt text](https://github.com/alirezadaghigh99/PL-Project/blob/master/inputs_outputs/a_output.png)

![alt text](https://github.com/alirezadaghigh99/PL-Project/blob/master/inputs_outputs/b_input.png)

![alt text](https://github.com/alirezadaghigh99/PL-Project/blob/master/inputs_outputs/b_output.png)

## Author
- GitHub: [@nimajam41](https://github.com/nimajam41)
- LinkedIn: [Nima Jamali](https://www.linkedin.com/in/nima-jamali-5b1521195/)

## Contributors
- [@nimajam41](https://github.com/nimajam41)
- [@alirezadaghigh99](https://github.com/alirezadaghigh99)

This project is available thanks to all contributors.

