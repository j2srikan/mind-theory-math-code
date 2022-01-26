## Chris Longley, Sixuan Chen
# Intro

Racket is a functional programming language descended from Lisp and Scheme. Noted for its elegance and power, Racket closely follows mathematical algorithms. Some tutorials we came across stressed learning about recursion and functional style. We are interested in exploring how these characteristics and points of emphasis may be relevant to the issue of whether Racket is a useful tool for neural modeling. 

# Practical

**Install Racket**

Open the website [Racket](https://racket-lang.org) and click on the download button at the top right. Then choose the Racket version that suits your operating system.
 
After downloading, you may see a number of different application icons, but just remember that Racket’s main tools are:
 
Racket, the core compiler, interpreter, and run-time system;
 
DrRacket, the programming environment; and,
 
Raco, a command-line tool for executing Racket commands that install packages, build libraries, and more.

A guide to using Racket is available at: [RacketGuide](https://docs.racket-lang.org/guide/intro.html)

DrRacket is a programming environment in which one can create their own art, games, compilers, interpreters, and more. 

**Our First Program in IDE DrRacket**

DrRacket is an IDE suitable for many proper languages and variants of Racket. It requires that one specify the language they are using. If we want to use Racket, we can type
 
#lang racket
 
in DrRacket’s top text box, and then click the “Run” button above the text area. We can define our functions in the top text box, and use them later in the bottom text box, which will display the outputs. We could think of this as working as a kind of calculator. See greeting.rkt for a demonstration of how to print "hello world!" using Racket.

# Theoretical

During this course, we will be exploring the idea of code as data through Racket. Utilized in the context of theoretical neuroscience, we will evaluate how this maps on to the idea of the mind as software that rewrites itself.

In Racket, everything is a function, and everything returns a value. Every function call is written in brackets. Following the “( ” will be an operator. This operator could be either built in operators or any function you defined yourself. Then following the operator, you give this function some variables. 

For example, consider “+” the built in operator. We have: 

3+7 

This will be coded as

`(+ 3 7)`

in Racket. 

Racket also facilitates the coding of functions within functions, as demonstrated in our example, twice.rkt. In this example, twice is a function that can take two inputs. One is a function F and another is a variable that is the parameter for F. The result of twice is ‘do whatever the input function is twice on the input variable.’


