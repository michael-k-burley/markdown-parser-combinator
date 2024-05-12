file://<WORKSPACE>/src/main/scala/main.scala
### java.lang.AssertionError: assertion failed: position error, parent span does not contain child span
parent      =  extends Monad[Farser] {
  override def pur = null
} # -1,
parent span = <1010..1052>,
child       = override def pur = null # -1,
child span  = [1036..1049..2238]

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 3.3.3
Classpath:
<HOME>/.cache/coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.3.3/scala3-library_3-3.3.3.jar [exists ], <HOME>/.cache/coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.12/scala-library-2.13.12.jar [exists ]
Options:



action parameters:
offset: 1052
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
type Farser = Input => (String, Input) // A

@main   
def main(args: String*): Unit = 

	println(">>> START")

	def farser(v: String): Function1[Input, (String, Input)] = 
		(i, s) => if(s.startsWith(v)) then (v, (i + v.length, s)) else (v, (i, s))

	val hello = farser("hello ")
	val goodbye = farser("goodbye")

	val input = (0, "hello goodbye you")


	given farserMonad: Monad[Farser] with {
		
		override def pur@@/*[A](a: A): Farser[A] = app.pure(a)

    	override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = app.map(fa)(f).flatten

	// 	// @annotation.tailrec
	// 	// def tailRecM[A, B](init: A)(fn: A => Option[Either[A, B]]): Option[B] =
	// 	// fn(init) match {
	// 	// 	case None => None
	// 	// 	case Some(Right(b)) => Some(b)
	// 	// 	case Some(Left(a)) => tailRecM(a)(fn)
	// 	// }
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
scala.runtime.Scala3RunTime$.assertFailed(Scala3RunTime.scala:8)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:175)
	dotty.tools.dotc.ast.Positioned.check$1$$anonfun$3(Positioned.scala:205)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:333)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:205)
	dotty.tools.dotc.ast.Positioned.checkPos(Positioned.scala:226)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:200)
	dotty.tools.dotc.ast.Positioned.checkPos(Positioned.scala:226)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:200)
	dotty.tools.dotc.ast.Positioned.check$1$$anonfun$3(Positioned.scala:205)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:333)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:205)
	dotty.tools.dotc.ast.Positioned.checkPos(Positioned.scala:226)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:200)
	dotty.tools.dotc.ast.Positioned.checkPos(Positioned.scala:226)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:200)
	dotty.tools.dotc.ast.Positioned.check$1$$anonfun$3(Positioned.scala:205)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:333)
	dotty.tools.dotc.ast.Positioned.check$1(Positioned.scala:205)
	dotty.tools.dotc.ast.Positioned.checkPos(Positioned.scala:226)
	dotty.tools.dotc.parsing.Parser.parse$$anonfun$1(ParserPhase.scala:39)
	dotty.tools.dotc.parsing.Parser.parse$$anonfun$adapted$1(ParserPhase.scala:40)
	scala.Function0.apply$mcV$sp(Function0.scala:42)
	dotty.tools.dotc.core.Phases$Phase.monitor(Phases.scala:440)
	dotty.tools.dotc.parsing.Parser.parse(ParserPhase.scala:40)
	dotty.tools.dotc.parsing.Parser.runOn$$anonfun$1(ParserPhase.scala:49)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:333)
	dotty.tools.dotc.parsing.Parser.runOn(ParserPhase.scala:49)
	dotty.tools.dotc.Run.runPhases$1$$anonfun$1(Run.scala:246)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.ArrayOps$.foreach$extension(ArrayOps.scala:1323)
	dotty.tools.dotc.Run.runPhases$1(Run.scala:262)
	dotty.tools.dotc.Run.compileUnits$$anonfun$1(Run.scala:270)
	dotty.tools.dotc.Run.compileUnits$$anonfun$adapted$1(Run.scala:279)
	dotty.tools.dotc.util.Stats$.maybeMonitored(Stats.scala:71)
	dotty.tools.dotc.Run.compileUnits(Run.scala:279)
	dotty.tools.dotc.Run.compileSources(Run.scala:194)
	dotty.tools.dotc.interactive.InteractiveDriver.run(InteractiveDriver.scala:165)
	scala.meta.internal.pc.MetalsDriver.run(MetalsDriver.scala:45)
	scala.meta.internal.pc.PcCollector.<init>(PcCollector.scala:44)
	scala.meta.internal.pc.PcDocumentHighlightProvider.<init>(PcDocumentHighlightProvider.scala:16)
	scala.meta.internal.pc.ScalaPresentationCompiler.documentHighlight$$anonfun$1(ScalaPresentationCompiler.scala:179)
```
#### Short summary: 

java.lang.AssertionError: assertion failed: position error, parent span does not contain child span
parent      =  extends Monad[Farser] {
  override def pur = null
} # -1,
parent span = <1010..1052>,
child       = override def pur = null # -1,
child span  = [1036..1049..2238]