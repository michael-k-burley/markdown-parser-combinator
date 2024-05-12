file://<WORKSPACE>/src/main/scala/main.scala
### java.lang.OutOfMemoryError: Java heap space

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 3.3.3
Classpath:
<HOME>/.cache/coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.3.3/scala3-library_3-3.3.3.jar [exists ], <HOME>/.cache/coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.12/scala-library-2.13.12.jar [exists ]
Options:



action parameters:
offset: 1354
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
				value.fold((Left(_), in2), a => f@@(a)(in2))
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
java.base/java.util.Arrays.copyOf(Arrays.java:3537)
	java.base/java.lang.AbstractStringBuilder.ensureCapacityInternal(AbstractStringBuilder.java:228)
	java.base/java.lang.AbstractStringBuilder.append(AbstractStringBuilder.java:582)
	java.base/java.lang.StringBuilder.append(StringBuilder.java:179)
	scala.collection.mutable.StringBuilder.append(StringBuilder.scala:143)
	dotty.tools.dotc.printing.Texts$Text.print(Texts.scala:125)
	dotty.tools.dotc.printing.Texts$Text.print$$anonfun$1(Texts.scala:130)
	dotty.tools.dotc.printing.Texts$Text$$Lambda$5256/0x00007f3eb8df4848.applyVoid(Unknown Source)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:333)
	dotty.tools.dotc.printing.Texts$Text.print(Texts.scala:132)
	dotty.tools.dotc.printing.Texts$Text.mkString(Texts.scala:143)
	dotty.tools.dotc.core.Decorators$.show(Decorators.scala:240)
	dotty.tools.dotc.printing.Showable.show(Showable.scala:23)
	dotty.tools.dotc.printing.Showable.show$(Showable.scala:9)
	dotty.tools.dotc.core.Types$Type.show(Types.scala:94)
	dotty.tools.dotc.core.Decorators$.tryToShow(Decorators.scala:278)
	dotty.tools.dotc.printing.Formatting$StringFormatter.showArg(Formatting.scala:123)
	dotty.tools.dotc.printing.Formatting$StringFormatter.treatArg(Formatting.scala:133)
	dotty.tools.dotc.printing.Formatting$StringFormatter.$anonfun$3(Formatting.scala:146)
	dotty.tools.dotc.printing.Formatting$StringFormatter$$Lambda$3088/0x00007f3eb8a659b0.apply(Unknown Source)
	scala.collection.LazyZip2$$anon$1$$anon$2.next(LazyZipOps.scala:42)
	scala.collection.mutable.Growable.addAll(Growable.scala:62)
	scala.collection.mutable.Growable.addAll$(Growable.scala:57)
	scala.collection.mutable.ArrayBuilder.addAll(ArrayBuilder.scala:67)
	scala.collection.IterableOnceOps.toArray(IterableOnce.scala:1346)
	scala.collection.IterableOnceOps.toArray$(IterableOnce.scala:1339)
	scala.collection.AbstractIterable.toArray(Iterable.scala:933)
	scala.Array$.from(Array.scala:73)
	scala.collection.immutable.ArraySeq$.from(ArraySeq.scala:282)
	scala.collection.immutable.ArraySeq$.from(ArraySeq.scala:273)
```
#### Short summary: 

java.lang.OutOfMemoryError: Java heap space