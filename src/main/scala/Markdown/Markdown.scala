
package MarkDown

import Parser.*
import scala.collection.mutable.StringBuilder

/*
    Paragraph, header, bolckquote, list, horizontal rule & codeblock are all block elements
    Links, emphasis, inline code and images are all span elements
*/

/* Enumeration */            
enum Markdown: // Could make private again after finished testing

    case Paragraph(value: Vector[String])  
    case Header(value: String, level: Int) 
    case BlockQuote(value: Vector[String]) 
    case List(value: Vector[String], isOrdered: Boolean) 
    case CodeBlock(value: String)
    case HorizontalRule() 

    case Emphasis(value: String, italic: Boolean, bold: Boolean)
    case Link(link_text: String, url: String, title: Option[String])   
    case Image(alt_text: String, url: String, title: Option[String])   
    case InlineCode(value: String)  


object Markdown {
    
    /*
        @ Purpose: Parses markdown or at least tries to. 
        @ Inputs: A string to try to parse
        @ Outputs: A string which is valid html  
        @ Remarks: Assumes that paragraphs are left aligned. ie. no leading spaces for a line of text
                   Paragraph, header, blockquote & list are all "hard-wrapped" ie. must be followed by a blank line
    */
    def parse_markdown(input: String): Option[String] =  

        val paragraph       = Markdown.paragraph()
        val header          = Markdown.header()
        val blockquote      = Markdown.blockquote()
        val list            = Markdown.list()
        val codeblock       = Markdown.codeblock()
        val horizontal_rule = Markdown.horizontal_rule()

        val emphasis        = Markdown.emphasis()
        val link            = Markdown.link()
        val image           = Markdown.image()
        val inline_code     = Markdown.inline_code()

        // Order matters, paragraph is at end because most permissive. 
        val all_markdowns = header.or(blockquote)
                        .or(list).or(codeblock)
                        .or(horizontal_rule).or(emphasis)
                        .or(link).or(image)
                        .or(inline_code).or(paragraph)

        val sequenced_parser = 
        for {
            _             <- Parser.optionalWhiteSpace
            vek_markdown  <- all_markdowns.onceOrMore()
        } yield {

            val html_header = { // multi-line a.k.a. herodoc string
            """
            |<!DOCTYPE html>
            |<html>
            |    <head>
            |        <title>Markdown to get down!</title>
            |    </head>
            |    <body>
            |""". stripMargin.replaceFirst("\n", "")
            }

            var tabsLevel = 2    // Clearly vars
            var isNested = false // are not ideal
            val isSpan = (el: Markdown) => el.isInstanceOf[Emphasis] || el.isInstanceOf[Image] || el.isInstanceOf[Link] || el.isInstanceOf[InlineCode]
            val html_result: StringBuilder = new StringBuilder(html_header) 

            //Iterate over vector of markdown elements, converting to appropriate html tags
            for(element <- vek_markdown.iterator){

                // If span tag matched at top level, needs to be "pre"-wrapped in p tag. Indentation is also adjusted
                if(!isNested && isSpan(element)) then
                    html_result ++= s"${"\t"*tabsLevel}<p>\n"
                    tabsLevel += 1
                    isNested = true
                else if(isNested && !isSpan(element)) then
                    html_result ++= s"${"\t"*tabsLevel}</p>\n"
                    tabsLevel -= 1
                    isNested = false

                // Wrap markdown elements in appropriate tag
                html_result ++= wrapInHTMLtags(element, tabsLevel) 
            } 

            // Close p tag from top level span elements
            html_result ++= { if(isNested) then "\t\t</p>\n" else "" }

            // Add closing tabs with formatting
            html_result ++= "\t</body>\n<html>"
            
            // Convert stringbuilder to string and return result
            html_result.toString
        }

        // Promote user string to parser input
        val parser_input = ParserInput(input)

        // Run markdown parser on user input to get parser output
        val parse_result: ParserResult[String] = sequenced_parser(parser_input)._1  

        // Convert parser result type to string
        parse_result match {
            case ParseError(errorMsg) =>  None
            case ParseSuccess(value)  =>  Some(value)
        } 
    

    /*
        @ Purpose: Converts each individual markdown element to its appropriate wrapped html tagged string. 
        @ Inputs: A markdown element 
        @ Outputs: That markdown element converted to html
        @ Remarks: Private helper function for function: parseMarkdown
    */
    private def wrapInHTMLtags(element: Markdown, tabsIndent: Int): String = 

        val wrapped_element: StringBuilder = new StringBuilder(s"${"\t"*tabsIndent}") // Add formating 
        
        // Append string to element string builder
        wrapped_element ++= { 

            element match { 

                /* BLOCK TAGS */
                case Paragraph(value)       => { 
                    
                    // Function to insert line break if line ends with 2 spaces
                    val f = (line: String) =>  if(line.endsWith("  ")) then line.trim + "<br />" else line + " "
                    
                    checkForSpanTags(value, tabsIndent+1, "<p>", "</p>", f)
                }
                case Header(value, level)   => s"<h$level>" + value + s"</h$level>"
                case BlockQuote(value)      => { 

                    // Function to ...
                    val f = (line: String) =>  
                        if(line.endsWith("  ")) then // Insert line break if line ends with 2 spaces
                            line.trim + "<br />"
                        else if(line.isBlank()) then  // Insert new paragraph  
                            "</p><p>"
                        // else if (nested block)
                        else
                            line + " "
                    
                    checkForSpanTags(value, tabsIndent+1, "<blockquote><p>", "</p></blockquote>", f) 
                }
                case List(value, isOrdered) => { 

                    // Function to insert line break if line ends with 2 spaces
                    val f = (line: String) =>  s"${"\t"*(tabsIndent+1)}<li>" + line + "</li>\n"

                    // Determine tag based on type of list
                    val tag = if(isOrdered) then "ol" else "ul"
            
                    checkForSpanTags(value, tabsIndent+1, s"<$tag>\n", s"${"\t"*tabsIndent}</$tag>", f)
                }
                case CodeBlock(value)       => "<pre><code>" + value + "</code></pre>" 
                case HorizontalRule()       => "<hr>"

                /* SPAN TAGS */
                case Emphasis(value, italic, bold) => { 
                    val text = if(italic) then "<em>" + value + "</em>" else value
                    if(bold) then "<strong>" + text + "</strong>" else text
                }
                case Link(link_text, url, title)   => raw"<a href='$url' title='${title.getOrElse("")}' >$link_text</a>"
                case Image(alt_text, url, title)   => raw"<img src='$url' alt='$alt_text' title='${title.getOrElse("")}' >"
                case InlineCode(value)             => "<p><code>" + value + "</code></p>"
            }
        }

        // Formatting so each tag on new line
        wrapped_element ++= "\n"

        // Return html tag wrapped string
        wrapped_element.toString

    
    /*
        @ Purpose: Turns multiline block elements into single string. 
                   Also checks for span tags nested with certain block elements (paragraph, blockquote, list)
        @ Inputs: vek_strs is a vector for block element where each element in the vector is a line
                  f is a function that maps the input string to its desired html counterpart
        @ Outputs: A string where the span elements within the block have been resolved
        @ Remarks: Used only by function directly above
    */
    def checkForSpanTags( md_lines: Vector[String], tabsIndent: Int, 
                            openTag: String, closeTag: String, f: Function1[String, String]): String = 

        val result: StringBuilder = new StringBuilder(openTag) 

            // Converts an input string to output string depending on user given conditions
            for(line <- md_lines.iterator){

                var prev = false // If previous character was special then don't add current character 

                // Get a list of all the indices of the starting span characters in the line
                val all_indices = line
                                .zipWithIndex
                                .filter((c, i) => { 
                                        (prev, "*[`_".contains(c)) match {
                                            case (true, false)  => { prev = false; prev }
                                            case (false, true)  => { prev = true; prev }
                                            case (_, _) => false
                                        }
                                    }
                                )
                                .partition((c, i) => c == '[' || c == '`') 
                                // ._1 contains the indices of all chars that match [ or `
                                // ._2 contains the indices of all chars that match * or _
                
                // Collect all the emphasis elements at odd indices (since they should be paired ideally)          
                val emphasis_indices = all_indices._2.grouped(2).map(_.head).toVector

                // Get a vector of all unique indices 
                val filtered_indices = { emphasis_indices ++ all_indices._1 }.map((c,i) => i)

                if(filtered_indices.isEmpty) then // If no indices then no special characters to try parsing
                    
                    result ++= f(line)
                else
                    val spanParsers = (s:String) => emphasis().or(image()).or(link()).or(inline_code())(ParserInput(s))
                    var modified_line = line // Using var here, clearly not ideal
                    var offset = 0

                    // Try to parse each span character at each of the given indices
                    filtered_indices.foreach( index => {

                        // Try using span parsers at each index
                        val parseResult/*: ParserResult[Markdown]*/ = spanParsers(line.substring(index)) // substring vs slice (?)

                        // Match on the result of parsing // Note: Don't add emphasis inside codeblocks or inline code
                        parseResult._1 match {
                            case ParseError(msg)     => () // Do nothing since no span elements at tried index
                            case ParseSuccess(value) => { 

                                // Get html string of matched span tag
                                val newEmphasizedString: String = wrapInHTMLtags(value, 0).trim // bc dont want nested newlines

                                // Index that was successfully parsed until, from starting point of index above, will be 0 if failed to parse
                                val numberCharsParsed: Int = parseResult._2._2 - 1 // Minus One since inclusive

                                // Modify line with new span element tags
                                modified_line = modified_line.patch(index + offset, newEmphasizedString, numberCharsParsed)
                                
                                // Accounts for change of indices if element to be inserted differs in size 
                                offset = offset + { if(newEmphasizedString.length > numberCharsParsed) then { newEmphasizedString.length - numberCharsParsed } else 0 }   
                            }
                        }

                    }) 

                    // Add modified line once after inserting span elements
                    result ++= f(modified_line)               
            }

            // Close opening tag
            result ++= closeTag 

            // Return html tag wrapped string
            result.toString

    /*
        @ Purpose: Parse paragraphs. Assumes that paragraphs are left aligned. ie. no leading spaces for line of text
        @ Inputs: 
        @ Outputs:
        @ Remarks: A paragraph is simply one or more consecutive lines of text, separated by one or more blank lines. 
                   Breaks within a paragraph can be either two spaces followed by a return
                   or the last non-whitespace character on the line immeadiately followed by a blackslash
                   2 Paragraphs are seperate by a blank line. Note: Only double space after last word has any effect
    */
    def paragraph(): Parser[Markdown] = 

        (s: ParserInput) => 

            // Matches optional trailing spaces followed by a newline, ignores newline return vector of spaces
            val trailing_spaces = Parser.singleSpace.zeroOrMore().and(Parser.newline).map((vek, n) => vek) 

            // Single word other than first word can have leading spaces
            val singleWord = for { 
                _       <- Parser.singleSpace.zeroOrMore()
                word    <- Parser.printableCharsWithoutSpace
            } yield word
                          
            // Single line of text
            val textLine = for {

                first_word      <- Parser.printableCharsWithoutSpace
                vek_raw_words   <- singleWord.zeroOrMore() 
                spaces          <- trailing_spaces

            } yield { 

                // If trailing spaces for line is less than 2 then no line break [(Magik Constant Alert)]
                if(spaces.length < 2) 
                    (first_word +: vek_raw_words).mkString(" ")
                
                else 
                    vek_raw_words.length match { // Re-insert 2 trailing spaces on line for line break
                        case 0 => first_word ++ "  "
                        case _ => first_word ++ " " ++ vek_raw_words.mkString(" ") ++ "  " 
                    }        
            }

            val sequenced_parser = 
            for { 
                vek_raw_text    <- textLine.onceOrMore()
                _               <- trailing_spaces.zeroOrMore()
            } yield {  

                // Each element of the vector is a line of text, line breaks are preserved as 2 spaces at end of line
                Paragraph(vek_raw_text)
            }

            // Calculate and return result of above sequence of parsers on initial input
            sequenced_parser(s)


    /* 
        @ Purpose: Parse headers in markdown. Header must be hard-wrapped. ie. followed by a blankline
        @ Inputs:
        @ Outputs: 
        @ Remarks:  Supports ATX and Setext headers as well as optional closing hashes
                    The number of opening hashes determines the header level
                    Opening hashes must be followed by a single space
                    Optional closing hashes don’t even need to match the number of hashes used to open the header
                    For setext headers underlines must be connected
    */
    def header(): Parser[Markdown] = 

        (s: ParserInput) => 
            
            val octothorpe = Parser.parseChar('#')
            val header_hashes = octothorpe.onceOrMore()
            val trailing_hashes = octothorpe.zeroOrMore()

            val ATX_header = 
            for {
                hvek    <- header_hashes    //vector of hashes
                _       <- Parser.singleSpace
                header  <- Parser.printableCharsWithSpaceCompare((c: Char) => c != '#') // Stop when encounters '#'

                _       <- Parser.singleSpace.zeroOrMore() 
                _       <- trailing_hashes
                _       <- Parser.singleSpace.zeroOrMore() 
                _       <- Parser.newline.onceOrMore()  // Force header to be hard wrapped

                _       <- Parser.optionalWhiteSpace
            } yield { 

                val numberOfHashes = hvek.length    // Calculate number of hashes from number of matches
                
                // Find the "weight" of header ie. H1 ... H6
                val level = 
                    if(numberOfHashes >= 7) then   
                        6  // 6 == Max Number of hashes for header  [(Magik Constant Alert)]
                    else
                        numberOfHashes

                val text = s"${header}".trim 

                Header(text, level)  
            }
            
            /* Setext */
            val singleDash = Parser.parseChar('-').onceOrMore()
            val doubleDash = Parser.parseChar('=').onceOrMore()
            val underlines = singleDash.or(doubleDash)

            val Setext_header = 
            for {
                header  <- Parser.printableCharsWithSpace
                _       <- Parser.newline
                unders  <- underlines
            
                _       <- Parser.singleSpace.zeroOrMore() 
                _       <- Parser.newline.onceOrMore()  // Force header to be hard wrapped

                _       <- Parser.optionalWhiteSpace
            } yield {

                val text = s"${header}".trim // Trim trailing whitespace

                if(unders(0) == '=') then // length > 0 since otherwise parser exits early 
                    Header(text, 1) 
                else
                    Header(text, 2) 
            }

            // Combine both types of headers
            val headers = ATX_header.or(Setext_header)

            // Calculate and return result of above sequence of parsers on initial input
            headers(s)


    /*
        @ Purpose: Parse blockquotes in markdown. Nested block quotes are not handled seperately.
                    An empty space element in returned vector is meant to signify the end of a paragraph
        @ Inputs: 
        @ Outputs:
        @ Remarks:  Doesn't currently support lazy block quotes with hard wrapped paragraphs
                    Best if you hard wrap the text and put a > before every line
                    Markdown allows you to be lazy and only put the > before the first line of a hard-wrapped paragraph.
                    Blockquotes can be nested, and can contain other md elements; lists, headers and code blocks.
    */
    def blockquote(): Parser[Markdown] = 

        (s: ParserInput) => 
            
            val greaterThan = Parser.parseString(">") // Must be string since vek of lines must be heterogenous
            val startOfLine = (greaterThan.and(Parser.singleSpace))
            val emptyLine = greaterThan 
                            .and(Parser.singleSpace.zeroOrMore())   // Return space but ignore vek            
                            .and(Parser.newline)                    // since it is just full 
                            .map{ case ((s,v),n) => " "}            // of spaces from empty line

            // Single block line
            val textLine = for { 
                _           <- startOfLine
                blockquote  <- Parser.printableCharsWithSpace
                _           <- Parser.newline
            } yield blockquote

            // Combine text line with empty line and map results to string
            val lines = textLine.or(emptyLine)

            // Lazy block quote with hardwrapped paragraph
            // val lazyParagraph = ...

            // Multiple block lines
            val sequenced_parser = 
            for {
                vek_raw_text      <- lines.onceOrMore()
                _                 <- Parser.optionalWhiteSpace
            } yield { // Note: Only double space after last word has any effect

                // raw_text is either a line of text following a > or else an empty line
                BlockQuote(vek_raw_text)
            }

            // Calculate and return result of above sequence of parsers on initial input
            sequenced_parser(s)


    /*
        @ Purpose: Parse lists in mardown. Doesn't handle sub lists yet. Requires single space after bullet.
        @ Inputs: 
        @ Outputs:
        @ Remarks:  Unordered lists use asterisks, pluses, and hyphens — interchangably — as list markers.
                    Ordered lists use any numeric character, in any order.
                    Lists must have a single space after their bullet point.
    */
    def list(): Parser[Markdown] = 

        (s: ParserInput) =>

            val asterisk = Parser.parseChar('*')
            val plus = Parser.parseChar('+')
            val hyphen = Parser.parseChar('-')

            /* Unordered List */
            def unordered_line(c: Char) = 
            for {
                _           <- Parser.parseChar(c) //unordered_bullet 
                _           <- Parser.singleSpace
                raw_text    <- Parser.printableCharsWithSpace
                _           <- Parser.newline

            } yield raw_text.trim

            def unordered_list(c: Char) =
            for {
                vek_lines   <- unordered_line(c).onceOrMore()
                _           <- Parser.optionalWhiteSpace
            } yield List(vek_lines, false) // is not Ordered

            // Create a combined parser of each unordered symbol
            val unordered_lists = unordered_list('*').or(unordered_list('-')).or(unordered_list('+'))

            /* Ordered List */
            val singleDigit = Parser.parseCharPredicate(c => "0123456789".contains(c), s"Error[List]: Ordered  -") 
            val period = Parser.parseChar('.')
            val ordered_bullet = singleDigit.and(period) 
            
            val ordered_line =
            for {
                _           <- ordered_bullet 
                _           <- Parser.singleSpace
                raw_text    <- Parser.printableCharsWithSpace
                _           <- Parser.newline
            } yield raw_text.trim

            val ordered_list = 
            for {
                vek_lines   <- ordered_line.onceOrMore()
                _           <- Parser.optionalWhiteSpace
            } yield List(vek_lines, true) // is Ordered
            
            // Combine each list parser into one parser 
            val lists = unordered_lists.or(ordered_list) 

            // Calculate and return result of above sequence of parsers on initial input
            lists(s)

    
    /*
        @ Purpose: Parse codeblocks in markdown.
        @ Inputs: 
        @ Outputs:
        @ Remarks:  Code blocks must start with either 4 spaces or a tab. All whitespace in block is preserved.
                    Pre-formatted code blocks are used for writing about programming or markup source code. 
                    Rather than forming normal paragraphs, the lines of a code block are interpreted literally. 
                    Markdown wraps a code block in both <pre> and <code> tags.
    */
    def codeblock(): Parser[Markdown] = 

        (s: ParserInput) => 

            val tab = Parser.parseChar('\t')
            val fourSpaces = Parser.singleSpace.and(Parser.singleSpace).and(Parser.singleSpace).and(Parser.singleSpace)

            val code_block_line = 
            for {
                _           <- fourSpaces.or(tab)
                raw_text    <- Parser.printableCharsWithSpace
                _           <- Parser.newline  

            } yield raw_text
            
            val code_block = 
            for {
                line           <- code_block_line.onceOrMore()
                _              <- Parser.optionalWhiteSpace
            } yield {

                val text = line mkString "\n"

                CodeBlock(text)
            }
        
            // Calculate and return result of above sequence of parsers on initial input
            code_block(s)


    /*
        @ Purpose: Parse horizontal rules in markdown.
        @ Inputs: 
        @ Outputs:
        @ Remarks: Must be 3 matching characters from ['-', '*', '_'], possibly with a single space between 
                   each of the first 3 characters followed by zero or more spaces and a newline
    */
    def horizontal_rule(): Parser[Markdown] = 

        (s: ParserInput) => 

            def horizontalRuleHelper(horizontalRule: Parser[Char]): Parser[Markdown] = 

                val rule1 = // Consecutive Characters ie. ----- or  ***
                for {
                    _       <- horizontalRule
                    _       <- horizontalRule
                    _       <- horizontalRule.onceOrMore()
                    _       <- Parser.singleSpace.zeroOrMore()
                    _       <- Parser.newline

                    _       <- Parser.optionalWhiteSpace

                } yield HorizontalRule()

                val rule2 = // Non-Consecutive Characters ie. - - - or * * *
                for {
                    _       <- horizontalRule
                    _       <- Parser.singleSpace
                    _       <- horizontalRule
                    _       <- Parser.singleSpace
                    _       <- horizontalRule
                    _       <- Parser.singleSpace.zeroOrMore()
                    _       <- Parser.newline

                    _       <- Parser.optionalWhiteSpace

                } yield HorizontalRule()

                // Combine both types of horizontal rule parsers
                rule1.or(rule2)


            // Create both types of horizontal rule  parsers for each possible character. ie. ['-', '*', '_']
            val dash_rule = horizontalRuleHelper(Parser.parseChar('-'))
            val star_rule = horizontalRuleHelper(Parser.parseChar('*'))
            val under_rule = horizontalRuleHelper(Parser.parseChar('_'))

            // Combine parsers for each possible character 
            val horizontal_rules = dash_rule.or(star_rule).or(under_rule)

            // Calculate and return result of above sequence of parsers on initial input
            horizontal_rules(s)

    
    /*
        @ Purpose: Parse emphasized markdown. 
        @ Inputs:
        @ Outputs: 
        @ Remarks:  Text wrapped with one * or _ will be wrapped with an HTML <em> tag; 
                    Text wrapped with two *’s or _’s will be wrapped with an HTML <strong> tag.
                    Text wrapped with three *’s or _’s will be wrapped with an HTML <em> and <strong> tag.
                    Emphasis can be used in the middle of a word, But if you surround 
                    an * or _ with spaces, it’ll be treated as a literal asterisk or underscore.
                    To produce a literal asterisk or underscore at a position where it would otherwise 
                    be used as an emphasis delimiter, you can backslash escape it. ie. \*
    */
    def emphasis(): Parser[Markdown] =

        (s: ParserInput) => 
            
            val asterisk = Parser.parseChar('*')
            val underscore = Parser.parseChar('_')

            // Works, maybe not bullet proof
            def getBoldtalics(i: Int, j: Int): (Boolean, Boolean) = 
                (i, j) match {
                    case (0,_) | (_,0) => (false, false)
                    case (1,_) | (_,1) => (true, false)
                    case (2,_) | (_,2) => (false, true)
                    case (3,_) | (_,3) => (true, true)
                    case (_,_) => (false, false)
                }

            val asterisk_word =
            for {
                word  <- Parser.printableCharsWithoutSpaceCompare((c: Char) => c != '*') // Stop when encounters '*'
                _     <- Parser.singleSpace
            } yield word

            val asterisk_emphasis = 
            for {
                vek_o       <- asterisk.onceOrMore()
                raw_text    <- asterisk_word.zeroOrMore()
                last_word   <- Parser.printableCharsWithoutSpaceCompare((c: Char) => c != '*') // Stop when encounters '*'
                vek_c       <- asterisk.onceOrMore()

                _           <- Parser.optionalWhiteSpace
            } yield { 

                // Take lesser of 2 lengths as level of emphasis tags.
                val (isItalicized, isBold) = getBoldtalics(vek_o.length, vek_c.length)

                // Append last word to vector then convert to string
                val text = (raw_text :+ last_word).mkString(" ")

                Emphasis(text, isItalicized, isBold) 
            }

            /* UNDERSCORES */
            val underscore_word =
            for {
                word  <- Parser.printableCharsWithoutSpaceCompare((c: Char) => c != '_') // Stop when encounters '_'
                _     <- Parser.singleSpace
            } yield word

            val underscore_emphasis = 
            for {
                vek_o       <- underscore.onceOrMore()
                raw_text    <- underscore_word.zeroOrMore()
                last_word   <- Parser.printableCharsWithoutSpaceCompare((c: Char) => c != '_') // Stop when encounters '_'
                vek_c       <- underscore.onceOrMore()

                _           <- Parser.optionalWhiteSpace
            } yield { 

                // Take lesser of 2 lengths as level of emphasis tags.
                val (isItalicized, isBold) = getBoldtalics(vek_o.length, vek_c.length)

                // Append last word to vector then convert to string
                val text = (raw_text :+ last_word).mkString(" ")

                Emphasis(text, isItalicized, isBold) 
            }

            // Combine both types of emphasis
            val emphasize = asterisk_emphasis.or(underscore_emphasis)

            // Calculate and return result of above sequence of parsers on initial input
            emphasize(s)


    /*
        @ Purpose: Parse link markdown. Only works with inline links, not reference links 
        @ Inputs: 
        @ Outputs:
        @ Remarks: Markdown supports two style of links: inline and reference.
    */
    def link(): Parser[Markdown] = 
        
        (s: ParserInput) => 
       
            val underscore = Parser.parseChar('_')
            val double_quote = Parser.parseChar('\"')
            val optional_title = (Parser.singleSpace
                                .and(double_quote)
                                .and(Parser.printableCharsWithSpaceCompare((c: Char) => c != '\"' && c != ')' ))
                                .and(double_quote)
                                .map(pr => pr._1._2)) // Ignore space & quotes, return title text // Left Assoc
                                .optional("")

            val inline_link = 
            for {
                _           <- Parser.parseChar('[')
                raw_link    <- Parser.printableCharsWithSpaceCompare((c: Char) => c != ']') // Stop when encounters ']'
                _           <- Parser.parseChar(']')

                _           <- Parser.parseChar('(')
                raw_url     <- Parser.printableCharsWithoutSpaceCompare((c: Char) => c != ')') // Stop when encounters ')'
                raw_title   <- optional_title
                _           <- Parser.parseChar(')')

                _           <- Parser.optionalWhiteSpace
            } yield {
                
                // Check if URL if valid

                // Replace quotes around title to avoid nested quotes
                val title = raw_title.replace("\"", "") 
                val opt_title: Option[String] = if(raw_title.length > 0) then Some(title) else None
                
                Link(raw_link, raw_url, opt_title)
            }

            // Combine each link parser into one parser 
            val links = inline_link//.or(reference_link) 

            // Calculate and return result of above sequence of parsers on initial input
            links(s)
            

    /*
        @ Purpose: Parse image markdown. Only supports inline images.
        @ Inputs: 
        @ Outputs:
        @ Remarks: Markdown uses an image syntax meant to resemble links syntax, allowing 2 styles: inline and reference.
    */
    def image(): Parser[Markdown] = 

        (s: ParserInput) => 
       
            val underscore = Parser.parseChar('_')
            val double_quote = Parser.parseChar('\"')
            val optional_title = (Parser.singleSpace
                                .and(double_quote)
                                .and(Parser.printableCharsWithSpaceCompare((c: Char) => c != '\"' && c != ')' ))
                                .and(double_quote)
                                .map(pr => pr._1._2)) // Ignore space & quotes, return title text // Left associative
                                .optional("")


            val inline_image = 
            for {
                _           <- Parser.parseChar('!')
                _           <- Parser.parseChar('[')
                raw_text    <- Parser.printableCharsWithSpaceCompare((c: Char) => c != ']') // Alternative text for image
                _           <- Parser.parseChar(']')

                _           <- Parser.parseChar('(')
                raw_url     <- Parser.printableCharsWithoutSpaceCompare((c: Char) => c != ')') // Stop when encounters ')'
                raw_title   <- optional_title
                _           <- Parser.parseChar(')')

                _           <- Parser.optionalWhiteSpace
            } yield {

                // Check if URL if valid

                // Replace quotes around title to avoid nested quotes
                val title = raw_title.replace("\"", "") 
                val opt_title: Option[String] = if(raw_title.length > 0) then Some(title) else None
             
                Image(raw_text, raw_url, opt_title)
            }

            // Combine each image parser into one parser 
            val images = inline_image 

            // Calculate and return result of above sequence of parsers on initial input
            images(s)
            

    /*
        @ Purpose: Parse inline code markdown. Should fail if blankline in middle of inline code block but doesn't.
        @ Inputs: 
        @ Outputs:
        @ Remarks:  To indicate a span of code, wrap it with backticks. 
                    Unlike a pre-formatted code block, a code span indicates code within a normal paragraph.
                    To include a literal backtick character within a code span, you can use multiple backticks as the opening and closing delimiters
    */
    def inline_code(): Parser[Markdown] = 

        (s: ParserInput) => 

            val backtick = Parser.parseChar('`')

            /* SINGLE BACKTICKS */
            val single_word = 
            for {
                word    <- Parser.printableCharsWithoutSpaceCompare((c: Char) => c != '`') // Stop when encounters '`'
                _       <- Parser.optionalWhiteSpace 
            } yield word 

            val single_backtick = 
            for {
                _           <- backtick
                _           <- Parser.optionalWhiteSpace
                vek_words   <- single_word.onceOrMore()
                _           <- backtick
                
                _           <- Parser.optionalWhiteSpace
            } yield { 

                // 2 spaces trailing the last word does not induce a line break
                val text = vek_words.mkString(" ")

                InlineCode(text)
            }

            /* DOUBLE BACKTICKS */
            val escapedIsPrintable = (c: Char) => (c >= 0x20 && c <= 0x7E) 
            val escaped_inline_code_text = Parser.parseUntil(escapedIsPrintable, s"Error[Inline Code]: double - ")

            val double_backtick = 
            for {
                _           <- backtick.and(backtick)
                _           <- Parser.optionalWhiteSpace
                raw_text    <- escaped_inline_code_text
                _           <- backtick.and(backtick)
                
                _           <- Parser.optionalWhiteSpace
            } yield {

                // 2 spaces trailing the last word does not induce a line break
                val text = s"${raw_text}".trim

                InlineCode(text)
            }

            // Combine both types of inline code parsers
            val inline_codes = single_backtick.or(double_backtick)

            // Calculate and return result of above sequence of parsers on initial input
            inline_codes(s)

}


