# markdown-parser-combinator

### This is a simple "markdown" parsing library 

#### Intention
I built this project in order to gain an even better understanding of how parser combinators work. As well as to learn more about markdown and its syntax.  
I was working through the book **Functional Programming in Scala** when I reached the chapter on parser combinators and was fascinated.

In writing this library, I
+ learned a lot about the scala language. Specifically, about the behaviour of certain types like string and what for comprehensions get desugared to.
+ gained a deeper understanding of the notion of sequencing monadic actions and concerning the idea of a state transition. ie. S => (A, S)
+ learned more about TDD as I attempted to follow a test driven model. The test suite is included in the project. 

Admittedly, the formatting of the output oftentimes leaves something to be desired.  
Starting fresh, I would pay more attention to string types, use a buffered iterator and try incorporating other libraries like cats.

For illustration purposes, I have included some example markdown input files and the corresponding parsed HTML output files.  
No doubt there are various improvements that could be made to this project. 


#### Note
There are a few significant differences between the markdown specification and the implementation that I have written here.  
In general, the official markdown specification is more permissive in terms of what it allows.  
Many of the additional syntactic expectations that I have required here, in order to be considered valid markdown and be parsed successfully, are widely considered to be best practise.  
Admittedly, often by enforcing these more stringent requirements the task of specifying what constitutes a valid syntax for a markdown component was somewhat simplified.  
These difference have been listed below, in no particular order.  

1. All markdown elements are assumed to start without leading spaces on a line. Although markdown technically allows for up to four spaces (exclusive) before the start of the line content.
2. Most block elements, such as paragraph, header, blockquote, list & codeblock are all "hard-wrapped". Said another way, they must be followed by a blank line. 
3. Each horizontal rule must end with zero or more optional spaces and a newline.  
4. The bullet for a list item or the greater than symbol for a blockquote must be followed by a single space before the content for that item.

In the interest of doing someting else, some common markdown features such lazy block quotes, reference links and images, as well as the repeated nesting of certain markdown elements are not supported.

#### How to Use
Program can be ran using scala build tool, with cmd: sbt run

#### Libraries used:
+ ScalaTest - for writing unit tests

#### Here is a short list of some resources that I found useful:

+ [Chapter 9 of *the red book*](https://www.manning.com/books/functional-programming-in-scala-second-edition "By Michael Pilquist, Rúnar Bjarnason, and Paul Chiusano")
+ [Markdown Syntax Specification](https://daringfireball.net/projects/markdown/syntax "By John Gruber")
+ [CommonMark Website](https://commonmark.org/)
+ [Live Testing Tool](https://spec.commonmark.org/dingus/)

