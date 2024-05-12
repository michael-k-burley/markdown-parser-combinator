
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

	for(i <- (1 to 5)) {

		// Read input markdown files
		def readFile(fileName: String): String = 
			Using{ Source.fromFile(fileName) } { _.iter.mkString }.get
		val input = readFile(s"./examples/input/input_$i.txt")

		// Process markdown text
		val result = Markdown.parse_markdown(input)

		if(result.isEmpty)
			println(s"Error: Unable to parse file $i")
		else
			// Output html to output file
			def writeFile(filename: String, content: String): Try[Unit] =
				Using { BufferedWriter(FileWriter(new File(filename))) } { bufferedWriter => bufferedWriter.write(content) }
			writeFile(s"./examples/output/output_$i.txt", result.get)
				.fold(e => println(s"Error: ${e}"), _ => println(s"File $i written successfully."))
	}

	println(">>> END")

end main

