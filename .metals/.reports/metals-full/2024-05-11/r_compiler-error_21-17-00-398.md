file://<WORKSPACE>/src/main/scala/main.scala
### java.lang.IndexOutOfBoundsException: 0

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 3.3.3
Classpath:
<HOME>/.cache/coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.3.3/scala3-library_3-3.3.3.jar [exists ], <HOME>/.cache/coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.12/scala-library-2.13.12.jar [exists ]
Options:



action parameters:
offset: 369
uri: file://<WORKSPACE>/src/main/scala/main.scala
text:
```scala

import Parser.*
import MarkDown.*
import MarkDown.Markdown

import scala.io.Source
import java.io.{File, FileWriter, BufferedWriter}
import scala.util.{Try, Using} // Using for self closing files, like bracket or resource
import scala.util.Failure
import scala.util.Success

@main   
def main(args: String*): Unit = 

	println(">>> START")

	val x = (0 until 5)

	for(@@)
	println()

	// Read input markdown files
	def readFile(fileName: String): String = 
		Using{ Source.fromFile(fileName) } { _.iter.mkString }.get
	val input = readFile("./examples/input/input_3.txt")

	// Process markdown text
	val result_str = Markdown.parse_markdown(input)
	
	// Output html to output file
	def writeFile(filename: String, content: String): Try[Unit] =
		Using { BufferedWriter(FileWriter(new File(filename))) } { bufferedWriter => bufferedWriter.write(content) }
	
	writeFile("./examples/output/output_3.txt", result_str)
		.fold(e => println(s"Error: ${e}"), _ => println("File parsed successfully."))
		
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