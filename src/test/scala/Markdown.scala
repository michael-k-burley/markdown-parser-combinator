
package MarkDown

/* 
    Most error cases only check that an appropriate type error was returned, 
    not that the error msg accurately represents the point of failure.
*/

import Parser._
import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source
import scala.util.Using 


class Markdown_Suite extends AnyFunSuite {

    test("Paragraph") {
        val paragraph = Markdown.paragraph()
        val paragraph_folder = "./src/test/test_cases/paragraph"

        val input_1 = Using{ Source.fromFile(paragraph_folder + "/pass1.txt") } { _.iter.mkString }.get
        assert(paragraph(ParserInput(input_1)) == (ParseSuccess(Markdown.Paragraph(Vector("one or more","lines of text"))), ParserInput(input_1, input_1.length)))

        val input_2 = Using{ Source.fromFile(paragraph_folder + "/pass2.txt") } { _.iter.mkString }.get
        assert(paragraph(ParserInput(input_2)) == (ParseSuccess(Markdown.Paragraph(Vector("this is a paragraph  ","with a line break"))), ParserInput(input_2, input_2.length)))

        val input_3 = Using{ Source.fromFile(paragraph_folder + "/pass3.txt") } { _.iter.mkString }.get
        assert(paragraph(ParserInput(input_3)) == (ParseSuccess(Markdown.Paragraph(Vector("this","is","another","paragraph"))), ParserInput(input_3, input_3.length)))

        val input_4 = Using{ Source.fromFile(paragraph_folder + "/pass4.txt") } { _.iter.mkString }.get
        assert(paragraph(ParserInput(input_4)) == (ParseSuccess(Markdown.Paragraph(Vector("this","is","yet  ","another","paragraph"))), ParserInput(input_4, input_4.length)))

        // Fail if leading whitespaces <<< Is this useful (?)
        val failing_test1 = Source.fromFile(paragraph_folder + "/fail1.txt").mkString 
        assert(paragraph(ParserInput(failing_test1))._1.isInstanceOf[ParseError])

        val failing_test2 = Source.fromFile(paragraph_folder + "/fail1.txt").mkString 
        assert(paragraph(ParserInput(failing_test2))._1.isInstanceOf[ParseError])
    }

    test("Header Setext") { 
        val header = Markdown.header()
        val header_folder = "./src/test/test_cases/header/setext"
        
        val passing_test1 = Using{ Source.fromFile(header_folder + "/pass1.txt") } { _.iter.mkString }.get
        val (input_1, result_1, level_1) = (passing_test1, passing_test1.takeWhile(_ != '\n'), 1)
        assert(header(ParserInput(input_1)) == (ParseSuccess(Markdown.Header(result_1, level_1)), ParserInput(input_1, input_1.length)))
       
        val passing_test2 = Using{ Source.fromFile(header_folder + "/pass2.txt") } { _.iter.mkString }.get
        val (input_2, result_2, level_2) = (passing_test2, passing_test2.takeWhile(_ != '\n'), 2)
        assert(header(ParserInput(input_2)) == (ParseSuccess(Markdown.Header(result_2, level_2)), ParserInput(input_2, input_2.length)))
       
        val failing_test1 = Using{ Source.fromFile(header_folder + "/fail1.txt") } { _.iter.mkString }.get
        assert(header(ParserInput(failing_test1))._1.isInstanceOf[ParseError])

        val failing_test2 = Using{ Source.fromFile(header_folder + "/fail2.txt") } { _.iter.mkString }.get
        assert(header(ParserInput(failing_test2))._1.isInstanceOf[ParseError])

    }
    
    test("Header ATX") {
        val header = Markdown.header()
        val header_folder = "./src/test/test_cases/header/atx"
        
        def isPrintable(c: Char) = c >= 0x20 && c <= 0x7E

        val passing_test1 = Using{ Source.fromFile(header_folder + "/pass1.txt") } { _.iter.mkString }.get
        val (input_1, level_1) = (passing_test1, passing_test1.takeWhile(_ == '#').length)
        val result_1 = input_1.dropWhile(_ == '#').takeWhile(c => isPrintable(c) && c != '#').trim
        assert(header(ParserInput(input_1)) == (ParseSuccess(Markdown.Header(result_1, level_1)), ParserInput(input_1, input_1.length)))
     
        val passing_test2 = Using{ Source.fromFile(header_folder + "/pass2.txt") } { _.iter.mkString }.get
        val (input_2, level_2) = (passing_test2, passing_test2.takeWhile(_ == '#').length)
        val result_2 = input_2.dropWhile(_ == '#').takeWhile(c => isPrintable(c) && c != '#').trim
        assert(header(ParserInput(input_2)) == (ParseSuccess(Markdown.Header(result_2, level_2)), ParserInput(input_2, input_2.length)))
       
        val failing_test1 = Using{ Source.fromFile(header_folder + "/fail1.txt") } { _.iter.mkString }.get 
        assert(header(ParserInput(failing_test1))._1.isInstanceOf[ParseError])

        val failing_test2 = Using{ Source.fromFile(header_folder + "/fail2.txt") } { _.iter.mkString }.get 
        assert(header(ParserInput(failing_test2))._1.isInstanceOf[ParseError])
    }

    test("BlockQuote"){
        val blockquote = Markdown.blockquote()
        val blockquote_folder = "./src/test/test_cases/blockquote"

        val input_1 = Using{ Source.fromFile(blockquote_folder + "/pass1.txt") } { _.iter.mkString }.get
        assert(blockquote(ParserInput(input_1)) == (ParseSuccess(Markdown.BlockQuote(Vector("here","is a","block quote"))), ParserInput(input_1, input_1.length)))
        
        val input_2 = Using{ Source.fromFile(blockquote_folder + "/pass2.txt") } { _.iter.mkString }.get
        assert(blockquote(ParserInput(input_2)) == (ParseSuccess(Markdown.BlockQuote(Vector("This is a block quote"," ","With 2 seperate paragraphs"))), ParserInput(input_2, input_2.length)))
        
        val input_3 = Using{ Source.fromFile(blockquote_folder + "/pass3.txt") } { _.iter.mkString }.get
        assert(blockquote(ParserInput(input_3)) == (ParseSuccess(Markdown.BlockQuote(Vector("here is a block quote  ","with a break btwn lines"))), ParserInput(input_3, input_3.length)))
        
        // Lazy block quote with hard wrapped paragraph
        // val input_4 = Using{ Source.fromFile(blockquote_folder + "/pass5.txt") } { _.iter.mkString }.get
        // assert(blockquote(ParserInput(input_4)) == (ParseSuccess(Markdown.BlockQuote(Vector("This is a","> nested","block quote"))), ParserInput(input_4, input_4.length)))

        // Nested Block Quotes (Probably not entirely correct)
        val input_5 = Using{ Source.fromFile(blockquote_folder + "/pass5.txt") } { _.iter.mkString }.get
        assert(blockquote(ParserInput(input_5)) == (ParseSuccess(Markdown.BlockQuote(Vector("This is a","> nested","block quote"))), ParserInput(input_5, input_5.length)))

        val failing_test1 = Using{ Source.fromFile(blockquote_folder + "/fail1.txt") } { _.iter.mkString }.get
        assert(blockquote(ParserInput(failing_test1))._1.isInstanceOf[ParseError])

        val failing_test2 = Using{ Source.fromFile(blockquote_folder + "/fail2.txt") } { _.iter.mkString }.get
        assert(blockquote(ParserInput(failing_test2))._1.isInstanceOf[ParseError])

        val failing_test3 = Using{ Source.fromFile(blockquote_folder + "/fail3.txt") } { _.iter.mkString }.get
        assert(blockquote(ParserInput(failing_test3))._1.isInstanceOf[ParseError])
    }

    test("Ordered List"){ 
        val list = Markdown.list()
        val list_folder = "./src/test/test_cases/list/ordered"

        val passing_test1: String = Using{ Source.fromFile(list_folder + "/pass1.txt") } { _.iter.mkString }.get
        assert(list(ParserInput(passing_test1)) == (ParseSuccess(Markdown.List(Vector("Bird","McHale","Parish"),true)),ParserInput(passing_test1, passing_test1.length)))

        val passing_test2: String = Using{ Source.fromFile(list_folder + "/pass2.txt") } { _.iter.mkString }.get
        assert(list(ParserInput(passing_test2)) == (ParseSuccess(Markdown.List(Vector("Bird","McHale","Parish"),true)),ParserInput(passing_test2, passing_test2.length)))

        val failing_test1: String = Using{ Source.fromFile(list_folder + "/fail1.txt") } { _.iter.mkString }.get
        assert(list(ParserInput(failing_test1))._1.isInstanceOf[ParseError])

        val failing_test2: String = Using{ Source.fromFile(list_folder + "/fail2.txt") } { _.iter.mkString }.get
        assert(list(ParserInput(failing_test2))._1.isInstanceOf[ParseError])
    }

    test("Unordered List"){ 
        val list = Markdown.list()
        val list_folder = "./src/test/test_cases/list/unordered"

        val passing_test1: String = Using{ Source.fromFile(list_folder + "/pass1.txt") } { _.iter.mkString }.get
        assert(list(ParserInput(passing_test1)) == (ParseSuccess(Markdown.List(Vector("Red","Green","Blue"),false)),ParserInput(passing_test1, passing_test1.length)))

        val passing_test2: String = Using{ Source.fromFile(list_folder + "/pass2.txt") } { _.iter.mkString }.get
        assert(list(ParserInput(passing_test2)) == (ParseSuccess(Markdown.List(Vector("Red","Green","Blue"),false)),ParserInput(passing_test2, passing_test2.length)))

        val passing_test3: String = Using{ Source.fromFile(list_folder + "/pass3.txt") } { _.iter.mkString }.get
        assert(list(ParserInput(passing_test3)) == (ParseSuccess(Markdown.List(Vector("Red","Green","Blue"),false)),ParserInput(passing_test3, passing_test3.length)))

        val failing_test1: String = Using{ Source.fromFile(list_folder + "/fail1.txt") } { _.iter.mkString }.get
        assert(list(ParserInput(failing_test1))._1.isInstanceOf[ParseError])

        val failing_test2: String = Using{ Source.fromFile(list_folder + "/fail2.txt") } { _.iter.mkString }.get
        assert(list(ParserInput(failing_test2))._1.isInstanceOf[ParseError])
    }

    test("CodeBlock"){  
        val codeblock = Markdown.codeblock()
        val codeblock_folder = "./src/test/test_cases/codeblock"

        val input_1 = Using { Source.fromFile(codeblock_folder + "/pass1.txt") } { _.iter.mkString }.get 
        val result_1 = input_1.split("\n").map(_.drop(4)).mkString("\n") // drop first 4 spaces
        assert(codeblock(ParserInput(input_1)) == (ParseSuccess(Markdown.CodeBlock(result_1)),ParserInput(input_1, input_1.length)))

        val input_2 = Using { Source.fromFile(codeblock_folder + "/pass2.txt") } { _.iter.mkString }.get 
        val result_2 = input_2.split("\n").map(_.drop(4)).mkString("\n") // drop first 4 spaces
        assert(codeblock(ParserInput(input_2)) == (ParseSuccess(Markdown.CodeBlock(result_2)),ParserInput(input_2, input_2.length)))

        val input_3 = Using { Source.fromFile(codeblock_folder + "/pass3.txt") } { _.iter.mkString }.get 
        val result_3 = input_3.split("\n").map(_.drop(4)).mkString("\n") // drop first 4 spaces
        assert(codeblock(ParserInput(input_3)) == (ParseSuccess(Markdown.CodeBlock(result_3)),ParserInput(input_3, input_3.length)))

        val failing_test1: String = Using { Source.fromFile(codeblock_folder + "/fail1.txt") } { _.iter.mkString }.get
        assert(codeblock(ParserInput(failing_test1))._1.isInstanceOf[ParseError])

        val failing_test2: String = Using { Source.fromFile(codeblock_folder + "/fail2.txt") } { _.iter.mkString }.get
        assert(codeblock(ParserInput(failing_test1))._1.isInstanceOf[ParseError])
    }

    test("HorizontalRule"){ 
        val horinzontal_rule = Markdown.horizontal_rule()
        val horizontal_folder = "./src/test/test_cases/horizontal"
        
        val input_pass = Using{ Source.fromFile(horizontal_folder + "/pass1.txt") } { _.iter.mkString }.get
        val passing_tests = input_pass.split("\n").map(_ + "\n").iterator 
        for(testCase <- passing_tests){ 
            assert(horinzontal_rule(ParserInput(testCase)) == (ParseSuccess(Markdown.HorizontalRule()), ParserInput(testCase, testCase.length)))
        }

        val input_fail = Using{ Source.fromFile(horizontal_folder + "/fail1.txt") } { _.iter.mkString }.get
        val failing_tests = input_fail.split("\n").map(_ + "\n").iterator
        for(testCase <- failing_tests){ 
            val res: Tuple2[ParserResult[Markdown], ParserInput] = horinzontal_rule(ParserInput(testCase))
            assert(res._1.isInstanceOf[ParseError])
        }
    }

    test("Emphasis"){
        val emphasis = Markdown.emphasis()
        val emphasis_folder = "./src/test/test_cases/emphasis"

        /* ITALICS */
        val input_1i = Using{ Source.fromFile(emphasis_folder + "/italics/pass1.txt") } { _.iter.mkString }.get
        assert(emphasis(ParserInput(input_1i)) == (ParseSuccess(Markdown.Emphasis("single asterisks", true, false)), ParserInput(input_1i, input_1i.length)))
    
        val input_2i = Using{ Source.fromFile(emphasis_folder + "/italics/pass2.txt") } { _.iter.mkString }.get
        assert(emphasis(ParserInput(input_2i)) == (ParseSuccess(Markdown.Emphasis("single underscores", true, false)), ParserInput(input_2i, input_2i.length)))
    
        val failing_1i = Using{ Source.fromFile(emphasis_folder + "/italics/fail1.txt") } { _.iter.mkString }.get
        assert(emphasis(ParserInput(failing_1i))._1.isInstanceOf[ParseError])

        val failing_2i = Using{ Source.fromFile(emphasis_folder + "/italics/fail2.txt") } { _.iter.mkString }.get
        assert(emphasis(ParserInput(failing_2i))._1.isInstanceOf[ParseError])

        /* BOLD */
        val input_1b = Using{ Source.fromFile(emphasis_folder + "/bold/pass1.txt") } { _.iter.mkString }.get
        assert(emphasis(ParserInput(input_1b)) == (ParseSuccess(Markdown.Emphasis("double asterisks", false, true)), ParserInput(input_1b, input_1b.length)))
    
        val input_2b = Using{ Source.fromFile(emphasis_folder + "/bold/pass2.txt") } { _.iter.mkString }.get
        assert(emphasis(ParserInput(input_2b)) == (ParseSuccess(Markdown.Emphasis("double underscores", false, true)), ParserInput(input_2b, input_2b.length)))
    
        val failing_1b = Using{ Source.fromFile(emphasis_folder + "/bold/fail1.txt") } { _.iter.mkString }.get
        assert(emphasis(ParserInput(failing_1b))._1.isInstanceOf[ParseError])

        val failing_2b = Using{ Source.fromFile(emphasis_folder + "/bold/fail2.txt") } { _.iter.mkString }.get
        assert(emphasis(ParserInput(failing_2b))._1.isInstanceOf[ParseError])

        /* BOLD & ITALICS */
        val input_1 = Using{ Source.fromFile(emphasis_folder + "/bold_italics/pass1.txt") } { _.iter.mkString }.get
        assert(emphasis(ParserInput(input_1)) == (ParseSuccess(Markdown.Emphasis("triple asterisks", true, true)), ParserInput(input_1, input_1.length)))
    
        val input_2 = Using{ Source.fromFile(emphasis_folder + "/bold_italics/pass2.txt") } { _.iter.mkString }.get
        assert(emphasis(ParserInput(input_2)) == (ParseSuccess(Markdown.Emphasis("triple underscores", true, true)), ParserInput(input_2, input_2.length)))
    
        val failing_1 = Using{ Source.fromFile(emphasis_folder + "/bold_italics/fail1.txt") } { _.iter.mkString }.get
        assert(emphasis(ParserInput(failing_1))._1.isInstanceOf[ParseError])

        val failing_2 = Using{ Source.fromFile(emphasis_folder + "/bold_italics/fail2.txt") } { _.iter.mkString }.get
        assert(emphasis(ParserInput(failing_2))._1.isInstanceOf[ParseError])
    }

    test("Link"){
        val link = Markdown.link()
        val link_folder = "./src/test/test_cases/link"

        val input_1 = Using{ Source.fromFile(link_folder + "/pass1.txt") } { _.iter.mkString }.get
        assert(link(ParserInput(input_1)) == (ParseSuccess(Markdown.Link("This link","http://example.net/",None)), ParserInput(input_1, input_1.length)))
       
        val input_2 = Using{ Source.fromFile(link_folder + "/pass2.txt") } { _.iter.mkString }.get // escaped below
        assert(link(ParserInput(input_2)) == (ParseSuccess(Markdown.Link("an example","http://example.com/",Some("Title"))), ParserInput(input_2, input_2.length)))
       
        val failing_test1: String = Using{ Source.fromFile(link_folder + "/fail1.txt") } { _.iter.mkString }.get
        assert(link(ParserInput(failing_test1))._1.isInstanceOf[ParseError])

        // Ill-formed URL
        // val failing_test2: String = Using{ Source.fromFile(link_folder + "/fail2.txt") } { _.iter.mkString }.get
        // assert(link(ParserInput(failing_test2))._1.isInstanceOf[ParseError])
    }

    test("Image"){
        val image = Markdown.image()
        val image_folder = "./src/test/test_cases/image"

        val input_1 = Using{ Source.fromFile(image_folder + "/pass1.txt") } { _.iter.mkString }.get
        assert(image(ParserInput(input_1)) == (ParseSuccess(Markdown.Image("Alt text","/path/to/img.jpg",None)), ParserInput(input_1, input_1.length)))
       
        val input_2 = Using{ Source.fromFile(image_folder + "/pass2.txt") } { _.iter.mkString }.get // escaped below
        assert(image(ParserInput(input_2)) == (ParseSuccess(Markdown.Image("Alt text","/path/to/img.jpg",Some("Optional title"))), ParserInput(input_2, input_2.length)))
       
        val failing_test1: String = Using{ Source.fromFile(image_folder + "/fail1.txt") } { _.iter.mkString }.get
        assert(image(ParserInput(failing_test1))._1.isInstanceOf[ParseError])

        // Ill-formed URL
        // val failing_test2: String = Using{ Source.fromFile(image_folder + "/fail2.txt") } { _.iter.mkString }.get
        // assert(image(ParserInput(failing_test2))._1.isInstanceOf[ParseError])

    }

    test("InlineCode"){
        val inline_code = Markdown.inline_code()
        val inline_folder = "./src/test/test_cases/inlinecode"

        val input_1 = Using{ Source.fromFile(inline_folder + "/pass1.txt") } { _.iter.mkString }.get
        assert(inline_code(ParserInput(input_1)) == (ParseSuccess(Markdown.InlineCode("printf()")), ParserInput(input_1, input_1.length)))
        
        val input_2 = Using{ Source.fromFile(inline_folder + "/pass2.txt") } { _.iter.mkString }.get 
        assert(inline_code(ParserInput(input_2)) == (ParseSuccess(Markdown.InlineCode("this should pass")), ParserInput(input_2, input_2.length)))
        
        val input_3 = Using{ Source.fromFile(inline_folder + "/pass3.txt") } { _.iter.mkString }.get 
        assert(inline_code(ParserInput(input_3)) == (ParseSuccess(Markdown.InlineCode("`foo`")), ParserInput(input_3, input_3.length)))

        val input_4 = Using{ Source.fromFile(inline_folder + "/pass4.txt") } { _.iter.mkString }.get 
        assert(inline_code(ParserInput(input_4)) == (ParseSuccess(Markdown.InlineCode("There is a literal backtick (`) here.")), ParserInput(input_4, input_4.length)))

        val failing_test1: String = Using{ Source.fromFile(inline_folder + "/fail1.txt") } { _.iter.mkString }.get
        assert(inline_code(ParserInput(failing_test1))._1.isInstanceOf[ParseError])

        val failing_test2: String = Using{ Source.fromFile(inline_folder + "/fail2.txt") } { _.iter.mkString }.get
        assert(inline_code(ParserInput(failing_test2))._1.isInstanceOf[ParseError])

        // Triple backticks [not failing]
        //val failing_test3: String = Using{ Source.fromFile(inline_folder + "/fail3.txt") } { _.iter.mkString }.get
        //assert(inline_code(ParserInput(failing_test3))._1.isInstanceOf[ParseError])
    }

}

