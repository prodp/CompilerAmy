package amyc
package parsing

import utils._
import scala.io.Source
import java.io.File

// The lexer for Amy.
// Transforms an iterator coming from scala.io.Source to a Stream of (Char, Position),
// then uses a functional approach to consume the stream into a Stream of Token.
object Lexer extends Pipeline[List[File], Stream[Token]] {
  import Tokens._

  /** Maps a string s to the corresponding keyword,
    * or None if it corresponds to no keyword
    */
  private def keywords(s: String): Option[Token] = s match {
    case "abstract" => Some(ABSTRACT())
    case "App"      => Some(APP())
    case "Boolean"  => Some(BOOLEAN())
    case "case"     => Some(CASE())
    case "class"    => Some(CLASS())
    case "def"      => Some(DEF())
    case "else"     => Some(ELSE())
    case "error"    => Some(ERROR())
    case "extends"  => Some(EXTENDS())
    case "false"    => Some(FALSE())
    case "if"       => Some(IF())
    // Update
    case "while"    => Some(WHILE())
    case "Int"      => Some(INT())
    case "match"    => Some(MATCH())
    case "object"   => Some(OBJECT())
    case "String"   => Some(STRING())
    case "true"     => Some(TRUE())
    case "Unit"     => Some(UNIT())
    case "val"      => Some(VAL())
      // Update
    case "var"      => Some(VAR())
    case _          => None
  }

  // Update: Boolean values acting as flags to indicate whether the next token cannot be a variable reassignment
  // Doesn't necessarily mean that the next token is a variable reassignment if this value is true, it is only a mere
  // indicator that the next token cannot be a variable reassignment, helps to deal with cases where a token might
  // be mistaken for a variable reassignment when it is not (such as providing the return type of a function ": type = {" )
  // Prior knowledge of the language syntax is required.
  val REASSIGNIMPOSSIBLE = false
  val REASSIGNPOSSIBLE = true

  private def lexFile(ctx: Context)(f: File): Stream[Token] = {
    import ctx.reporter._

    // Special character which represents the end of an input file
    val EndOfFile: Char = java.lang.Character.MAX_VALUE

    val source = Source.fromFile(f)

    // Useful type alias:
    // The input to the lexer will be a stream of characters,
    // along with their positions in the files
    type Input = (Char, Position)

    def mkPos(i: Int) = Position.fromFile(f, i)

    // The input to the lexer
    val inputStream: Stream[Input] =
      source.toStream.map(c => (c, mkPos(source.pos))) #::: Stream((EndOfFile, mkPos(source.pos)))

    /** Gets rid of whitespaces and comments and calls readToken to get the next token.
      * Returns the first token and the remaining input that did not get consumed
      */
    @scala.annotation.tailrec
    def nextToken(stream: Stream[Input])(implicit reassignPossible: Boolean): (Token, Stream[Input], Boolean) = {
      require(stream.nonEmpty)

      val (currentChar, currentPos) #:: rest = stream

      // Use with care!
      def nextChar = rest.head._1

      def multiLineSkip(s: Stream[Input]): Stream[Input] = {
        val (currentC, currentP) #:: streamRest = s
        if(currentC == EndOfFile) {
          ctx.reporter.error("multiLineskip error", currentP)
          ctx.reporter.terminateIfErrors()
        }

        val nextC = streamRest.head._1
        if(currentC == '*' && nextC == '/' ) streamRest.tail else multiLineSkip(streamRest)
      }

      if (Character.isWhitespace(currentChar)) {
        nextToken(stream.dropWhile{ case (c, _) => Character.isWhitespace(c) } )
      } else if (currentChar == '/' && nextChar == '/') {
        // Line comment
        val s = stream.dropWhile{ case (c, _) => !(c == '\n' || c == '\r' || c == EndOfFile) }
        if(s.head._1 == EndOfFile) nextToken(s) else nextToken(s.tail)
      } else if (currentChar == '/' && nextChar == '*') {
        // Multiline comment
        // tail of tail to get rid of the * and /
        // write own function
        //nextToken(rest.dropWhile{ case (c, _) => c != '*' || stream.tail.head._1 != '/' }.tail.tail)

        // In case /*/, it should fail, that's why we skid 2 chars and take the rest of the stream
        nextToken(multiLineSkip(stream.tail.tail))
      } else {
        readToken(stream)
      }
    }
    /** Reads the next token from the stream. Assumes no whitespace or comments at the beginning.
      * Returns the first token and the remaining input that did not get consumed.
      */
    def readToken(stream: Stream[Input])(implicit reassignPossible: Boolean): (Token, Stream[Input], Boolean) = {
      require(stream.nonEmpty)

      val (currentChar, currentPos) #:: rest = stream

      // Use with care!
      def nextChar = rest.head._1

      // Returns input token with correct position and uses up one character of the stream
      def useOne(t: Token, reassignPossible: Boolean) = (t.setPos(currentPos), rest, reassignPossible)
      // Returns input token with correct position and uses up two characters of the stream
      def useTwo(t: Token, reassignPossible: Boolean) = (t.setPos(currentPos), rest.tail, reassignPossible)

      currentChar match {
        case EndOfFile => useOne(EOF(), REASSIGNPOSSIBLE)
 
        // Reserved word/ Identifier
        case _ if Character.isLetter(currentChar) =>
          val (wordLetters, afterWord) = stream.span { case (ch, _) =>
            Character.isLetterOrDigit(ch) || ch == '_'
          }
          val word = wordLetters.map(_._1).mkString
          // Hint: Decide if it's a letter or reserved word (use our infrastructure!),
          // and return the correct token, along with the remaining input stream.
          // Make sure you set the correct position for the token.
          val tokenOption = keywords(word)
          tokenOption match {
            case Some(token) => (token.setPos(currentPos), afterWord, REASSIGNPOSSIBLE)
              // Update: Check if it's a variable reassignment (i.e id = )
            case None => {
              if(!reassignPossible) {
                (ID(word).setPos(currentPos), afterWord, REASSIGNPOSSIBLE)
              } else {
                val s = afterWord.dropWhile{ case (c, _) => Character.isWhitespace(c)}
                // Check if next character after id is '=' in case of reassign and not == as in a condition
                // or a => as what comes after a case match in pattern matching
                if(s.head._1 == '=' && !(s.tail.head._1 == '=' || s.tail.head._1 == '>')) {
                  (VARIDREASSIGN(word).setPos(currentPos), s.tail, REASSIGNPOSSIBLE)
                }
                  // Update: Lexing Reassignment operators
                else if (s.head._1 == '+' && s.tail.head._1 == '='){
                  (VARPLUSEQUALS(word).setPos(currentPos), s.tail.tail, REASSIGNPOSSIBLE)
                } else if (s.head._1 == '-' && s.tail.head._1 == '='){
                  (VARMINUSEQUALS(word).setPos(currentPos), s.tail.tail, REASSIGNPOSSIBLE)
                } else if (s.head._1 == '*' && s.tail.head._1 == '='){
                  (VARTIMESEQUALS(word).setPos(currentPos), s.tail.tail, REASSIGNPOSSIBLE)
                } else if (s.head._1 == '/' && s.tail.head._1 == '='){
                  (VARDIVEQUALS(word).setPos(currentPos), s.tail.tail, REASSIGNPOSSIBLE)
                } else if (s.head._1 == '%' && s.tail.head._1 == '='){
                  (VARMODEQUALS(word).setPos(currentPos), s.tail.tail, REASSIGNPOSSIBLE)
                } else if (s.head._1 == '+' && s.tail.head._1 == '+' && s.tail.tail.head._1 == '='){
                  (VARCONCATEQUALS(word).setPos(currentPos), s.tail.tail.tail, REASSIGNPOSSIBLE)
                } else {
                  (ID(word).setPos(currentPos), afterWord, REASSIGNPOSSIBLE)
                }
              }
            }
          }

        // Int literal
        case _ if Character.isDigit(currentChar) =>
          val (wordLetters, afterWord) = stream.span { case (ch, _) =>
            Character.isDigit(ch)
          }
          val digitWord = wordLetters.map(_._1).mkString

          // CHECK FOR OVERFLOW
          try{
            (INTLIT(digitWord.toInt).setPos(currentPos), afterWord, REASSIGNPOSSIBLE)
          } catch {
            case e :Exception => {
              ctx.reporter.error("Bad Int Literal", currentPos)
              (BAD().setPos(currentPos), afterWord, REASSIGNPOSSIBLE)
            }
          }

        // String Literal
        case '"' =>
          // We take the rest not stream to ignore the " char, we only need the string literal
          val (wordLetters, afterWord) = rest.span { case (ch, _) =>
            // String must end with another " and end on the same line
            if(ch == '\n' || ch == '\r' || ch == EndOfFile){
              ctx.reporter.error("Error lexing string Literal", currentPos)
              return (BAD().setPos(currentPos), rest.tail, REASSIGNPOSSIBLE)
            }
            ch != '"'
          }

          val stringLit = wordLetters.map(_._1).mkString

          // We take the tail of afterword since the " char stays after the stream span
          (STRINGLIT(stringLit).setPos(currentPos), afterWord.tail, REASSIGNPOSSIBLE)


        case '{' => useOne(LBRACE(), REASSIGNPOSSIBLE)

        case '}' => useOne(RBRACE(), REASSIGNPOSSIBLE)

        case '(' => useOne(LPAREN(), REASSIGNPOSSIBLE)

        case ')' => useOne(RPAREN(), REASSIGNPOSSIBLE)

        case ',' => useOne(COMMA(), REASSIGNPOSSIBLE)

        case ':' => useOne(COLON(), REASSIGNIMPOSSIBLE)

        case '.' => useOne(DOT(), REASSIGNIMPOSSIBLE)

        case '=' => {
          if(nextChar == '>') useTwo(RARROW(), REASSIGNPOSSIBLE)
          else if(nextChar == '=') useTwo(EQUALS(), REASSIGNPOSSIBLE)
          else useOne(EQSIGN(), REASSIGNPOSSIBLE)
        }

        case '_' => useOne(UNDERSCORE(), REASSIGNPOSSIBLE)

        case '+' => {
          if(nextChar == '+') useTwo(CONCAT(), REASSIGNPOSSIBLE) else useOne(PLUS(), REASSIGNPOSSIBLE)
        }

        case ';' => useOne(SEMICOLON(), REASSIGNPOSSIBLE)

        case '-' => useOne(MINUS(), REASSIGNPOSSIBLE)

        case '*' => useOne(TIMES(), REASSIGNPOSSIBLE)

        case '/' => useOne(DIV(), REASSIGNPOSSIBLE)

        case '%' => useOne(MOD(), REASSIGNPOSSIBLE)

        case '<' => {
          if(nextChar == '=') useTwo(LESSEQUALS(), REASSIGNPOSSIBLE) else useOne(LESSTHAN(), REASSIGNPOSSIBLE)
        }

        case '&' => {
          if(nextChar == '&') useTwo(AND(), REASSIGNPOSSIBLE) else {
            ctx.reporter.error("Unresolved & symbol", currentPos)
            useOne(BAD(), REASSIGNPOSSIBLE)
          }
        }

        case '|' => {
          if(nextChar == '|') useTwo(OR(), REASSIGNPOSSIBLE) else {
            ctx.reporter.error("Unresolved | symbol", currentPos)
            useOne(BAD(), REASSIGNPOSSIBLE)
          }
        }

        case '!' => useOne(BANG(), REASSIGNPOSSIBLE)

        case _ => {
          ctx.reporter.error("Unresolved symbol", currentPos)
          useOne(BAD(), REASSIGNPOSSIBLE)
        }
              // There should be a case for all remaining (invalid) characters in the end
      }
    }

    // To lex a file, call nextToken() until it returns the empty Stream as "rest"
    def tokenStream(s: Stream[Input])(implicit reassignPossible: Boolean): Stream[Token] = {
      if (s.isEmpty) Stream()
      else {
        val (token, rest, rp) = nextToken(s)(reassignPossible)
        token #:: tokenStream(rest)(rp)
      }
    }

    // Update: default value for the variable reassignment indicator flag is false, given the knowledge
    // a module or a program cannot start with a variable reassignment
    tokenStream(inputStream)(REASSIGNIMPOSSIBLE)
  }

  // Lexing all input files means putting the tokens from each file one after the other
  def run(ctx: Context)(files: List[File]): Stream[Token] = {
    files.toStream flatMap lexFile(ctx)
  }
}

/** Extracts all tokens from input and displays them */
object DisplayTokens extends Pipeline[Stream[Token], Unit] {
  def run(ctx: Context)(tokens: Stream[Token]): Unit = {
    tokens.toList foreach { t => println(s"$t(${t.position.withoutFile})") }
  }
}
