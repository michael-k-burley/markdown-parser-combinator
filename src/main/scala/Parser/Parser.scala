
package Parser

import scala.annotation.tailrec

trait ParserResult[+A] 
case class ParseSuccess[A](value: A) extends ParserResult[A] 
case class ParseError(errorMsg: String) extends ParserResult[Nothing]
case class ParserInput(input: String, pos: Int = 0) 
type Parser[A] = ParserInput => (ParserResult[A], ParserInput) 


extension[A](p: Parser[A]) 
	
    def foreach(f: A => Unit): Unit = p.map(f) 

    def map[B](f: A => B): Parser[B] = 
        (s: ParserInput) => 
            val (result, s2) = p(s)
            result match {
				case ParseSuccess(value) => (ParseSuccess(f(value)), s2)
				case ParseError(errorMsg) => (ParseError(errorMsg), s2) 
			} 
		
    def flatMap[B](f: A => Parser[B]): Parser[B] =
        (s: ParserInput) => 
            val (result, s2) = p(s) 
            result match {
				case ParseSuccess(value) => f(value)(s2)
				case ParseError(errorMsg) => (ParseError(errorMsg), s2) 
			}	


/* --- COMBINATORS --- */

    /*
        @ Purpose: Function can try both parsers, only if 1st parser succeeds. Returns error msg of first failure
        @ Inputs: 
        @ Outputs:
        @ Remarks: 
    */
    def and[B](p2: Parser[B]): Parser[(A,B)] = 
        (s: ParserInput) => 
            p(s) match { 
                case (ParseError(em1), s2) => (ParseError(em1), s2)
                case (ParseSuccess(v1), s2) =>  { 
                    p2(s2) match { 
                        case (ParseError(em2), s3) => (ParseError(em2), s3)
                        case (ParseSuccess(v2), s3) => (ParseSuccess((v1, v2)), s3) 
                    }
                }
            }

    /*
        @ Purpose: Function can try both parsers, only if 1st parser fails. If both fail concats error msgs
        @ Inputs: 
        @ Outputs:
        @ Remarks: Could use monoid here for errors
    */
    def or(p2: Parser[A]): Parser[A] = 
        (s: ParserInput) => 
            p(s) match {
                case pr1 @ (ParseSuccess(_), _) => pr1
                case (ParseError(em1), _)  =>  { 
                    p2(s) match {
                        case pr2 @ (ParseSuccess(_), _) => pr2
                        case (ParseError(em2), s2)  => (ParseError( em1 +" "+ em2 ), s2)
                    }
                }
            }

    /*
        @ Purpose: Function returns a vector of strings from match on input string else an empty vector
        @ Inputs: 
        @ Outputs:
        @ Remarks: Always succeeds
    */
    def zeroOrMore(): Parser[Vector[A]] = 
        (s: ParserInput) => 
            @tailrec
            def loop(vek: Vector[A], si: ParserInput): (Vector[A], ParserInput)  = 
                p(si) match { 
                    case (ParseSuccess(value), sj) => loop(vek :+ value, sj)
                    case (ParseError(em), _) => (vek, si)
                }
            val (vek, sn) = loop(Vector(), s)
            (ParseSuccess(vek), sn) 

    /*
        @ Purpose: Function returns a vector of strings from match on input string once or more
        @ Inputs: 
        @ Outputs:
        @ Remarks: Flattens result
    */
    def onceOrMore(): Parser[Vector[A]] = 
        (s: ParserInput) => 
            val oneOrMore = p.and(p.zeroOrMore())
            oneOrMore(s) match {
                case (ParseSuccess((a, vek_a)), s2) => (ParseSuccess(a +: vek_a), s2) 
                case (ParseError(em), s2) => (ParseError(em), s2)
            }

    /*
        @ Purpose: Function converts a parser to one which if fails returns a success value with no progress made
        @ Inputs: 
        @ Outputs:
        @ Remarks: Avoid default parser behaviour of only succeeding on non-empty string or if not at end of string
    */
    def optional(a: A): Parser[A] =        
        case s @ ParserInput(input, pos) => { 
            val alwaySucceeds: Parser[A] = (s: ParserInput) => (ParseSuccess(a), ParserInput(input, pos)) 
            p.or(alwaySucceeds)(s)
        }


/* COMPANION OBJECT */
object Parser {

    /*
        @ Purpose: Function returns char if matched the start of input string
        @ Inputs: 
        @ Outputs:
        @ Remarks:
    */
    def parseChar(c: Char): Parser[Char] =  
        case pi @ ParserInput(input: String, pos: Int) => 

            if(pos < input.length && (input.charAt(pos) == c)) then 
                ( ParseSuccess(c), ParserInput(input , pos + 1)) 
            else
                ( ParseError(s"Error: Expected char '${c}' at position $pos"), pi) 

    /*
        @ Purpose: Function returns string if matched the start of input string
        @ Inputs: 
        @ Outputs:
        @ Remarks:
    */
    def parseString(s: String): Parser[String] =  
        case pi @ ParserInput(input: String, pos: Int) => 

            if(pos < input.length && input.startsWith(s, pos)) then 
                ( ParseSuccess(s), ParserInput(input , pos + s.length)) 
            else
                ( ParseError(s"Error: Expected string '${s}' at position $pos"), pi) 

    /*
        @ Purpose: Function returns char that matched while predicate was true 
        @ Inputs: 
        @ Outputs:
        @ Remarks: 
    */
     def parseCharPredicate(pred: Function1[Char, Boolean], errorMsg: String): Parser[Char] = 
        case s @ ParserInput(input, pos) =>
            
            if(pos < input.length && pred(input(pos))) then
                (ParseSuccess(input(pos)), ParserInput(input, pos + 1))
            else
                (ParseError(errorMsg + s" $pos"), s) 
    
    /*
        @ Purpose: Function returns string that matched while predicate was true 
        @ Inputs: 
        @ Outputs:
        @ Remarks: 
    */
    def parseStringPredicate(pred: Function1[Char, Boolean], errorMsg: String): Parser[String] = 
        case s @ ParserInput(input, pos) =>
            
            val parsedStr: String = input.substring(pos).takeWhile(pred)  
            if(parsedStr.isEmpty) then
                (ParseError(errorMsg + s" $pos"), s) 
            else 
                (ParseSuccess(parsedStr), ParserInput(input, pos + parsedStr.length))
    
     /*
        @ Purpose: Function to halt parsing when encountering double backticks. Uses bufferesIterator.
        @ Inputs: 
        @ Outputs:
        @ Remarks: Added only for inline code parser, specifically for parsing double backticks
    */
    def parseUntil(pred: Function1[Char, Boolean], errorMsg: String): Parser[String] = 
        case s @ ParserInput(input, pos) =>
            
            val shiftedInput: String = input.substring(pos)
            val bufit = shiftedInput.grouped(1).buffered // Convert input string to buffered iterator over chars

            val parsedStr = shiftedInput.takeWhile(c => pred(c) && bufit.hasNext && !(bufit.next == "`" && bufit.head == "`"))            
            
            if(parsedStr.isEmpty) then
                (ParseError(errorMsg + s" $pos"), s) 
            else 
                (ParseSuccess(parsedStr), ParserInput(input, pos + parsedStr.length ))


    /* --- CONVENIENCE PARSERS --- */

    val singleSpace = Parser.parseChar(' ') 
    val newline = Parser.parseChar('\n') 
    
    // Function to return a parser that consumes all whitespace
    def whitespaceParser(caller: String, tag: String): Parser[String] = 
        case s @ ParserInput(input, pos) =>
            Parser.parseStringPredicate(c => " \n\r\t".contains(c), s"Error[$caller]: $tag -> ")(s)
    
    // Convience function to return a parser that optionally consumes all whitespace, always succeeds
    val optionalWhiteSpace: Parser[String] =
        case s @ ParserInput(input, pos) => 
            Parser.whitespaceParser("optionalWhiteSpace", " Failed at position -> ").optional("")(s)

    // Helper function for below methods
    // Checks that value of char is within the range of printable ascii characters
    private def isPrintable(includeSpace: Boolean, opt_f: Option[Function1[Char,Boolean]] = None) = 
        (includeSpace, opt_f) match {
            case (true, None)     => (c: Char) => (c >= 0x20 && c <= 0x7E)
            case (true, Some(f))  => (c: Char) => (c >= 0x20 && c <= 0x7E) && f(c)
            case (false, None)    => (c: Char) => (c > 0x20 && c <= 0x7E)
            case (false, Some(f)) => (c: Char) => (c > 0x20 && c <= 0x7E) && f(c)
        }
        
    val printableCharsWithSpace: Parser[String] = 
        parseStringPredicate(isPrintable(true), s"Error: print chars w space")

    val printableCharsWithoutSpace: Parser[String] = 
        parseStringPredicate(isPrintable(false), s"Error: print chars wo space")
   
    def printableCharsWithSpaceCompare(f: Function1[Char,Boolean]): Parser[String] = 
        parseStringPredicate(isPrintable(true, Some(f)), s"Error: print chars w space - compare")
   
    def printableCharsWithoutSpaceCompare(f: Function1[Char,Boolean]): Parser[String] = 
        parseStringPredicate(isPrintable(false, Some(f)), s"Error: print chars wo space - compare")
    
}
