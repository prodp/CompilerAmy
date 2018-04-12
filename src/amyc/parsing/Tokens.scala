package amyc
package parsing

import grammarcomp.grammar.CFGrammar.TerminalClass
import utils.Positioned

sealed class Token extends Positioned

// Defines tokens for Lexer
object Tokens {

  /* Utility tokens */
  case class BAD() extends Token // represents incorrect tokens.
  case class EOF() extends Token // Signifies end-of-file

  /* Keywords */
  case class ABSTRACT() extends Token
  case class APP()      extends Token
  case class BOOLEAN()  extends Token
  case class CASE()     extends Token
  case class CLASS()    extends Token
  case class DEF()      extends Token
  case class ELSE()     extends Token
  case class ERROR()    extends Token
  case class EXTENDS()  extends Token
  case class FALSE()    extends Token
  case class IF()       extends Token
   // Update
  case class WHILE()    extends Token
  case class INT()      extends Token
  case class MATCH()    extends Token
  case class OBJECT()   extends Token
  case class STRING()   extends Token
  case class TRUE()     extends Token
  case class UNIT()     extends Token
  case class VAL()      extends Token
  // Update
  case class VAR()      extends Token

  /* Operators */
  case class SEMICOLON()  extends Token // ;
  case class PLUS()       extends Token // +
  case class MINUS()      extends Token // -
  case class TIMES()      extends Token // *
  case class DIV()        extends Token // /
  case class MOD()        extends Token // %
  case class LESSTHAN()   extends Token // <
  case class LESSEQUALS() extends Token // <=
  case class AND()        extends Token // &&
  case class OR()         extends Token // ||
  case class EQUALS()     extends Token // ==
  case class CONCAT()     extends Token // ++
  case class BANG()       extends Token // !
  // Update
  case class PLUSEQUALS() extends Token // +=
  case class MINUSEQUALS() extends Token // -=
  case class TIMESEQUALS() extends Token // *=
  case class DIVEQUALS() extends Token // /=
  case class MODEQUALS() extends Token // %=
  case class CONCATEQUALS() extends Token // ++=

  /* Delimiters and wildcard */
  case class LBRACE()     extends Token // {
  case class RBRACE()     extends Token // }
  case class LPAREN()     extends Token // (
  case class RPAREN()     extends Token // )
  case class COMMA()      extends Token // ,
  case class COLON()      extends Token // :
  case class DOT()        extends Token // .
  case class EQSIGN()     extends Token // =
  case class RARROW()     extends Token // =>
  case class UNDERSCORE() extends Token // _

  // Identifiers
  case class ID(value: String) extends Token with TerminalClass

  // Integer literals
  case class INTLIT(value: Int) extends Token with TerminalClass

  // Update: Variable reassignment
  case class VARIDREASSIGN(name: String) extends Token with TerminalClass // Id =

  // Update: Variable reassignment operators
  case class VARPLUSEQUALS(name: String) extends Token with TerminalClass // Id +=
  case class VARMINUSEQUALS(name: String) extends Token with TerminalClass // Id -=
  case class VARTIMESEQUALS(name: String) extends Token with TerminalClass // Id *=
  case class VARDIVEQUALS(name: String) extends Token with TerminalClass // Id /=
  case class VARMODEQUALS(name: String) extends Token with TerminalClass // Id %=
  case class VARCONCATEQUALS(name: String) extends Token with TerminalClass // Id ++=

  // String literals
  case class STRINGLIT(value: String) extends Token with TerminalClass

  // These three tokens are meant to represent their respective category in the parser
  val IDSENT = ID("")
  val INTLITSENT = INTLIT(0)
  val STRINGLITSENT = STRINGLIT("")
  // Update: Sentinel of a variable id that's being reassigned
  val VARIDSENT = VARIDREASSIGN("")
  // Update: Sentinel values for reassignment operators
  val VARPLUSEQSENT = VARPLUSEQUALS("")
  val VARMINUSEQSENT = VARMINUSEQUALS("")
  val VARTIMESEQSENT = VARTIMESEQUALS("")
  val VARDIVEQSENT = VARDIVEQUALS("")
  val VARMODEQSENT = VARMODEQUALS("")
  val VARCONCATEQSENT = VARCONCATEQUALS("")
}
