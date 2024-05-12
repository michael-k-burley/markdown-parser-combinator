file://<WORKSPACE>/src/main/scala/main.scala
### java.lang.IndexOutOfBoundsException: 0

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 3.3.3
Classpath:
<HOME>/.cache/coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.3.3/scala3-library_3-3.3.3.jar [exists ], <HOME>/.cache/coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.12/scala-library-2.13.12.jar [exists ]
Options:



action parameters:
offset: 1343
uri: file://<WORKSPACE>/src/main/scala/main.scala
text:
```scala

//import Parser.*  // Testing

import Parser.*//{ParserInput, Parser}
import MarkDown.*
import MarkDown.Markdown

import scala.io.Source

// NOTE: C[_] is a shorthand for the type lambda [X] =>> C[X] 
// existential type VS. wildcard parameter
// ? is replacement symbol for wildcard types
// Null is type of null value
// Nothing extends Any type, so we can use Nothing in place of any Scala type both reference types and value types.
// 

import scala.util.matching.Regex
import scala.util.Using // Using for self closing files, like bracket or resource

import cats.Monad
import scala.annotation.tailrec

type Input = (Int, String)
type Result[A] = (Either[String, A], Input)
type Farser[A] = Input => Result[A]


@main   
def main(args: String*): Unit = 

	println(">>> START")

	def farser(v: String): Farser[String] = 
		(i, s) => if(s.startsWith(v)) then (Right(v), (i + v.length, s)) else (Left(s"Error: @ $i"), (i, s))

	val hello = farser("hello ")
	val goodbye = farser("goodbye")

	val input = (0, "hello goodbye you")


	given farserMonad: Monad[Farser] with {
		
		override def pure[A](a: A): Farser[A] = pr => (Right(a), pr) // ?
		
		override def flatMap[A, B](fa: Farser[A])(f: A => Farser[B]): Farser[B] = //app.map(fa)(f).flatten
			case in @ (i: Int, s: String) =>

				val (value ,in2) = fa(in)
				value.fold(e => (Left(@@e, in2), a => f(a)(in2))
				//if(value.isLeft) then (Left(s"ERROR: at $i"), in2) /* ??? */ else f(value.toOption.get)(in2)


		// @annotation.tailrec
		// def tailRecM[A, B](init: A)(fn: A => Farser[Either[A, B]]): Farser[B] =
		// fn(init) match {
		// 	case None => None
		// 	case Some(Right(b)) => Some(b)
		// 	case Some(Left(a)) => tailRecM(a)(fn)
		// }
	}
	
	println(hello(input))
	//println(hello(input).flatMap( in => goodbye(in) ))

	// val pr = 
	// for {
	// 	x <- hello
	// 	y <- goodbye
	// } println(y)

	// pr(input)

	// val paragraph_folder = "./src/test/test_cases/samples"
	// val input = Using{ Source.fromFile(paragraph_folder + "/sample_2.txt") } { _.iter.mkString }.get
	
	// import java.io._ ///{BufferedWriter, FileWriter}

	// import scala.io.Source
	// import scala.util.{Try, Using} // Using for self closing files, like bracket or resource

	// def writeFile(filename: String, content: String): Try[Unit] =
	// 	Using { BufferedWriter(FileWriter(new File(filename))) } { bufferedWriter => bufferedWriter.write(content) }
	// writeFile("output.txt", result)
	// println("------------")
	
	println(">>> END")

end main


```



#### Error stacktrace:

```
scala.collection.LinearSeqOps.apply(LinearSeq.scala:131)
	scala.collection.LinearSeqOps.apply$(LinearSeq.scala:128)
	scala.collection.immutable.List.apply(List.scala:79)
	dotty.tools.dotc.util.Signatures$.countParams(Signatures.scala:501)
	dotty.tools.dotc.util.Signatures$.applyCallInfo(Signatures.scala:186)
	dotty.tools.dotc.util.Signatures$.computeSignatureHelp(Signatures.scala:94)
	dotty.tools.dotc.util.Signatures$.signatureHelp(Signatures.scala:63)
	scala.meta.internal.pc.MetalsSignatures$.signatures(MetalsSignatures.scala:17)
	scala.meta.internal.pc.SignatureHelpProvider$.signatureHelp(SignatureHelpProvider.scala:51)
	scala.meta.internal.pc.ScalaPresentationCompiler.signatureHelp$$anonfun$1(ScalaPresentationCompiler.scala:414)
```
#### Short summary: 

java.lang.IndexOutOfBoundsException: 0