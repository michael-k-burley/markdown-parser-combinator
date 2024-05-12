# markdown-parser-combinator

### This is a simple "markdown" parsing library 

#### Intention
I built this project in order to gain an even better nderstanding of how parsing and parser combinators work.
I was working through the book Functional Progrogramming in Scala when I reached the chapter on parser combinators and was fascinated.

In writing this library, 
I gained a deeper understanding of the idea of a state action. ie. S => (A, S)
I also attempted to follow a test driven model.  
I learned more about the scala language. Specifically, concerning boxing, the sized trait and about dynamic dispatch.   


If 


I included some example Markdown input and the corresponding parsed HTML output files for illustration.
No doubt there are various improvements that could be made to this project. 


#### Note
There are a few significant differences between the markdown specification and the implementation that I have written here. 
In general, the markdown specification is more permissive in what it allows. 
Many of the that I have required in order to be considered valid markdown are widely considered to be best practise.
Often, by enforcing these more stringent requirements, the task of specifying what constitutes a valid component was somewhat simplified.
The difference have been listed below.

All markdown elements are assumed to start without leading spaces on line, although markdown technically allows for up to four spaces before the start of line content.

Paragraph, header, blockquote, list & codeblock are all "hard-wrapped" ie. they must be followed by a blank line.  
Each horizontal rule must end with zero or more optional spaces and a newline.  
  
Paragraph, header, bolckquote, list, horizontal rule & codeblock are all block elements
Links, emphasis, inline code and images are all span elements

#### How to Use
Program can be ran using rust's package manager, with cmd: cargo run

#### Libraries used:
+ scalatest

#### Here is a short list of some resources that I found useful:

+ [Chapter 9 of *the red book*](https://www.manning.com/books/functional-programming-in-scala-second-edition "By Michael Pilquist, Rúnar Bjarnason, and Paul Chiusano")
+ [Markdown Syntax Specification](https://daringfireball.net/projects/markdown/syntax "By John Gruber")
+ [CommonMark Website](https://commonmark.org/)
+ [Live Testing Tool for CommonMark](https://spec.commonmark.org/dingus/)

