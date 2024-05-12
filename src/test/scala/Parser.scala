
/*
  Apparently anyFlatSuite is the scalatest recommended test suite

  To only run one class of tests use: testOnly *CLASS_NAME
  To only run tests affected by latest code changes use: testQuick

  To run a specifc test use: testOnly *CLASS_NAME -- -n TAG_NAME
  To run a specifc test use: testOnly *CLASS_NAME -- -z PART_OF_TAG_NAME

*/

package Parser

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.propspec.AnyPropSpec


class Parsers_Suite extends AnyFunSuite {

  val hello = "hello"
  val bye = "bye"

  val hello_input = ParserInput(hello, 0)
  val bye_input = ParserInput(bye, 0)

  val hello_parser = Parser.parseString(hello)
  val bye_parser = Parser.parseString(bye)

  test("String_Parser") {
    assert(hello_parser(hello_input) == (ParseSuccess(hello), ParserInput(hello, hello.length)))
    assert(bye_parser(hello_input) == (ParseError(s"Error: Expected string '${bye}' at position 0"), hello_input))
  }

  test("Char Predicate Parser") {
    val parsePrintable = Parser.parseCharPredicate((c: Char) => c >= 0x20 && c <= 0x7E, s"Error in Test Predicate")

    val input_1 = "h" 
    assert(parsePrintable(ParserInput(input_1)) == (ParseSuccess(input_1.charAt(0)), ParserInput(input_1, input_1.length)))
    
    val input_2 = "3 7"
    val output_2 = "3"
    assert(parsePrintable(ParserInput(input_2)) == (ParseSuccess(output_2.charAt(0)), ParserInput(input_2, output_2.length)))
  }

  test("String Predicate Parser") {
    val parsePrintable = Parser.parseStringPredicate((c: Char) => c >= 0x20 && c <= 0x7E, s"Error in Test Predicate")

    val input_1 = "hello 123 ~+-="
    assert(parsePrintable(ParserInput(input_1)) == (ParseSuccess(input_1), ParserInput(input_1, input_1.length)))
    
    val input_2 = "hello 123 ~+-= \n"
    val output_2 = "hello 123 ~+-= "
    assert(parsePrintable(ParserInput(input_2)) == (ParseSuccess(output_2), ParserInput(input_2, output_2.length)))
  }

}


class Combinators_Suite extends AnyFunSuite {

  val hello = "hello"
  val bye = "bye"

  val hello_input = ParserInput(hello, 0)
  val bye_input = ParserInput(bye, 0)

  val hello_parser = Parser.parseString(hello)
  val bye_parser = Parser.parseString(bye)

  test("AND combinator") {
    var helloAndBye = hello_parser.and(bye_parser)
    var input_1 = hello ++ bye
    assert(helloAndBye(ParserInput(input_1)) == (ParseSuccess((hello, bye)), ParserInput(input_1, input_1.length)) )
    
    val byeAndHello = bye_parser.and(hello_parser)
    val input_2 = bye ++ hello
    assert(byeAndHello(ParserInput(input_2)) == (ParseSuccess((bye, hello)), ParserInput(input_2, input_2.length)) )
  }

  test("OR combinator") {
    var helloOrBye = hello_parser.or(bye_parser)
    assert(helloOrBye(hello_input) == (ParseSuccess(hello), ParserInput(hello, hello.length)))
    assert(helloOrBye(bye_input) == (ParseSuccess(bye), ParserInput(bye, bye.length)))
  }

  test("zeroOrMore combinator") {
    val zormBye = bye_parser.zeroOrMore()
    assert(zormBye(ParserInput("byebyebye")) == (ParseSuccess(Vector("bye","bye","bye")), ParserInput("byebyebye" ,9)))
    assert(zormBye(ParserInput("bye")) == (ParseSuccess(Vector("bye")), ParserInput("bye",3)))
    assert(zormBye(ParserInput("")) == (ParseSuccess(Vector()), ParserInput("" ,0)))
  }

  test("onceOrMore combinator") {
    val oormBye = bye_parser.onceOrMore()
    assert(oormBye(ParserInput("byebyebye")) == (ParseSuccess(Vector("bye","bye","bye")), ParserInput("byebyebye" ,9)))
    assert(oormBye(ParserInput("bye")) == (ParseSuccess(Vector("bye")), ParserInput("bye",3)))
    assert(oormBye(ParserInput("")) == (ParseError("Error: Expected string 'bye' at position 0"), ParserInput("" ,0)))
  }

}
