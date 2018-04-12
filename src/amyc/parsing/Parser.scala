package amyc
package parsing

import grammarcomp.grammar.CFGrammar._
import grammarcomp.grammar.GrammarDSL._
import grammarcomp.grammar.GrammarUtils.InLL1
import grammarcomp.grammar._
import grammarcomp.parsing._
import amyc.utils._
import ast.NominalTreeModule._
import Tokens._

// The parser for Amy
// Absorbs tokens from the Lexer and then uses grammarcomp to generate parse trees.
// Defines two different grammars, a naive one which does not obey operator precedence (for demonstration purposes)
// and an LL1 grammar that implements the true syntax of Amy
object Parser extends Pipeline[Stream[Token], Program] {

  /* This grammar does not implement the correct syntax of Amy and is not LL1
   * It is given as an example
   */
  val amyGrammar = Grammar('Program, List[Rules[Token]](
    'Program ::= 'ModuleDefs,
    'ModuleDefs ::= 'ModuleDef ~ 'ModuleDefs | epsilon(),
    'ModuleDef ::= OBJECT() ~ 'Id ~ EXTENDS() ~ APP() ~ LBRACE() ~ 'Definitions ~ 'OptExpr ~ RBRACE() ~ EOF(),
    'Definitions ::= 'Definition ~ 'Definitions | epsilon(),
    'Definition ::= 'AbstractClassDef | 'CaseClassDef | 'FunDef,
    'AbstractClassDef ::= ABSTRACT() ~ CLASS() ~ 'Id,
    'CaseClassDef ::= CASE() ~ CLASS() ~ 'Id ~ LPAREN() ~ 'Params ~ RPAREN() ~ EXTENDS() ~ 'Id,
    'FunDef ::= DEF() ~ 'Id ~ LPAREN() ~ 'Params ~ RPAREN() ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'Expr ~ RBRACE(),
    'Params ::= epsilon() | 'Param ~ 'ParamList,
    'ParamList ::= epsilon() | COMMA() ~ 'Param ~ 'ParamList,
    'Param ::= 'Id ~ COLON() ~ 'Type,
    'OptExpr ::= 'Expr | epsilon(),
    'Type ::= INT() | STRING() | BOOLEAN() | UNIT() | 'QName,
    'QName ::= 'Id | 'Id ~ DOT() ~ 'Id,
    'Expr ::= 'Id | 'Literal | 'Expr ~ 'BinOp ~ 'Expr | BANG() ~ 'Expr | MINUS() ~ 'Expr |
              'QName ~ LPAREN() ~ 'Args ~ RPAREN() | 'Expr ~ SEMICOLON() ~ 'Expr |
              VAL() ~ 'Param ~ EQSIGN() ~ 'Expr ~ SEMICOLON() ~ 'Expr |
              IF() ~ LPAREN() ~ 'Expr ~ RPAREN() ~ LBRACE() ~ 'Expr ~ RBRACE() ~ ELSE() ~ LBRACE() ~ 'Expr ~ RBRACE() |
              'Expr ~ MATCH() ~ LBRACE() ~ 'Cases ~ RBRACE() |
              ERROR() ~ LPAREN() ~ 'Expr ~ RPAREN() |
              LPAREN() ~ 'Expr ~ RPAREN(),
    'Literal ::= TRUE() | FALSE() | LPAREN() ~ RPAREN() | INTLITSENT | STRINGLITSENT,
    'BinOp ::= PLUS() | MINUS() | TIMES() | DIV() | MOD() | LESSTHAN() | LESSEQUALS() |
               AND() | OR() | EQUALS() | CONCAT(),
    'Cases ::= 'Case | 'Case ~ 'Cases,
    'Case ::= CASE() ~ 'Pattern ~ RARROW() ~ 'Expr,
    'Pattern ::= UNDERSCORE() | 'Literal | 'Id | 'QName ~ LPAREN() ~ 'Patterns ~ RPAREN(),
    'Patterns ::= epsilon() | 'Pattern ~ 'PatternList,
    'PatternList ::= epsilon() | COMMA() ~ 'Pattern ~ 'PatternList,
    'Args ::= epsilon() | 'Expr ~ 'ExprList,
    'ExprList ::= epsilon() | COMMA() ~ 'Expr ~ 'ExprList,
    'Id ::= IDSENT
  ))


  val amyGrammarLL1 = Grammar('Program, List[Rules[Token]](
    'Program ::= 'ModuleDefs,
    'ModuleDefs ::= 'ModuleDef ~ 'ModuleDefs | epsilon(),
    'ModuleDef ::= OBJECT() ~ 'Id ~ EXTENDS() ~ APP() ~ LBRACE() ~ 'Definitions ~ 'OptExpr ~ RBRACE() ~ EOF(),
    'Definitions ::= 'Definition ~ 'Definitions | epsilon(),
    'Definition ::= 'AbstractClassDef | 'CaseClassDef | 'FunDef,
    'AbstractClassDef ::= ABSTRACT() ~ CLASS() ~ 'Id,
    'CaseClassDef ::= CASE() ~ CLASS() ~ 'Id ~ LPAREN() ~ 'Params ~ RPAREN() ~ EXTENDS() ~ 'Id,
    'FunDef ::= DEF() ~ 'Id ~ LPAREN() ~ 'Params ~ RPAREN() ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'Expr ~ RBRACE(),
    'Params ::= epsilon() | 'Param ~ 'ParamList,
    'ParamList ::= epsilon() | COMMA() ~ 'Param ~ 'ParamList,
    'Param ::= 'Id ~ COLON() ~ 'Type,
    'OptExpr ::= 'Expr | epsilon(),
    'Type ::= INT() | STRING() | BOOLEAN() | UNIT() | 'QName,

    // QName only used in Type
    'QName ::= 'Id ~ 'QNameHelper,
    'QNameHelper ::= DOT() ~ 'Id | epsilon(),

    // List by priorities
    'Expr ::= VAL() ~ 'Param ~ EQSIGN() ~ 'ExprMatchDef ~ SEMICOLON() ~ 'Expr |
      // Update: variable has to be initialized
      VAR() ~ 'Param ~ EQSIGN() ~ 'ExprMatchDef ~ SEMICOLON() ~ 'Expr |
      'VarIdEquals ~ 'ExprMatchDef ~ 'ExprHelper |
      'VarPlusEquals ~ 'ExprMatchDef ~ 'ExprHelper |
      'VarMinusEquals ~ 'ExprMatchDef ~ 'ExprHelper |
      'VarTimesEquals ~ 'ExprMatchDef ~ 'ExprHelper |
      'VarDivEquals ~ 'ExprMatchDef ~ 'ExprHelper |
      'VarModEquals ~ 'ExprMatchDef ~ 'ExprHelper |
      'VarConcatEquals ~ 'ExprMatchDef ~ 'ExprHelper |
      'ExprMatchDef ~ 'ExprHelper,
    'ExprHelper ::= SEMICOLON() ~ 'Expr | epsilon(),

    'ExprMatchDef ::= 'ExprOrDef ~ 'ExprMatch,
    'ExprMatch ::= MATCH() ~ LBRACE() ~ 'Cases ~ RBRACE() | epsilon(),

    'ExprOrDef ::= 'ExprAndDef ~ 'OrExpr,
    'OrExpr ::= OR() ~ 'ExprOrDef | epsilon(),

    'ExprAndDef ::= 'ExprEqDef ~ 'AndExpr,
    'AndExpr ::= AND() ~ 'ExprAndDef | epsilon(),

    'ExprEqDef ::= 'ExprCompDef ~ 'EqExpr,
    'EqExpr ::= EQUALS() ~ 'ExprEqDef | epsilon(),

    'ExprCompDef ::= 'ExprAddDef ~ 'CompExpr,
    'CompExpr ::= LESSEQUALS() ~ 'ExprCompDef | LESSTHAN() ~ 'ExprCompDef | epsilon(),

    'ExprAddDef ::= 'ExprMultDef ~ 'AddExpr,
    'AddExpr ::= PLUS() ~ 'ExprAddDef| MINUS() ~ 'ExprAddDef | CONCAT() ~ 'ExprAddDef| epsilon(),

    'ExprMultDef ::= 'ExprUnary ~ 'MultExpr,
    'MultExpr ::= DIV() ~ 'ExprMultDef | TIMES() ~ 'ExprMultDef | MOD() ~ 'ExprMultDef | epsilon(),

    // -- not allowed
    'ExprUnary ::= MINUS() ~ 'OrdExpr | BANG() ~ 'OrdExpr | 'OrdExpr,

    /*
      QExpr can be id or qname expression
      LiteralParen can be () or (expr)
     */
     // Update
    'OrdExpr ::= 'QExpr | 'CondExpr | 'ErrExpr | 'Literal | 'LiteralParen | 'WhileExpr,

    'CondExpr ::= IF() ~ LPAREN() ~ 'Expr ~ RPAREN() ~ LBRACE() ~ 'Expr ~ RBRACE() ~ ELSE() ~ LBRACE() ~ 'Expr ~ RBRACE(),
    
     // Update
    'WhileExpr ::= WHILE() ~ LPAREN() ~ 'Expr ~ RPAREN() ~ LBRACE() ~ 'Expr ~ RBRACE(),

    'ErrExpr ::= ERROR() ~ LPAREN() ~ 'Expr ~ RPAREN(),

    'QExpr ::= 'Id ~ 'QExprHelper,
    'QExprHelper ::= 'QNameHelper ~ LPAREN() ~ 'Args ~ RPAREN() |epsilon(),

    // Fix first (Expr) and Literal ()
    'Literal ::= TRUE() | FALSE() | INTLITSENT | STRINGLITSENT,
    'LiteralParen ::= LPAREN() ~ 'LiteralParenHelper,
    'LiteralParenHelper ::= RPAREN() | 'Expr ~ RPAREN(),

    'BinOp ::= PLUS() | MINUS() | TIMES() | DIV() | MOD() | LESSTHAN() | LESSEQUALS() |
      AND() | OR() | EQUALS() | CONCAT(),

    // Fix first cases
    'Cases ::= 'Case ~ 'CasesPrime,
    'CasesPrime ::= 'Cases | epsilon(),
    'Case ::= CASE() ~ 'Pattern ~ RARROW() ~ 'Expr,

    /*
      Fix first of id and QName
      Here Pattern can be one of 3 cases : - simple id (if PatternPrime is epsilon)
                                           - QName with function id (if PatternPrime is not empty and QNameHelper is empty)
                                           - QName with module and function id (otherwise)
     */
    'Pattern ::= UNDERSCORE() | 'Literal | LPAREN() ~ RPAREN() | 'Id ~ 'PatternPrime,
    'PatternPrime ::= 'QNameHelper ~ LPAREN() ~ 'Patterns ~ RPAREN() | epsilon(),
    'Patterns ::= epsilon() | 'Pattern ~ 'PatternList,
    'PatternList ::= epsilon() | COMMA() ~ 'Pattern ~ 'PatternList,
    'Args ::= epsilon() | 'Expr ~ 'ExprList,
    'ExprList ::= epsilon() | COMMA() ~ 'Expr ~ 'ExprList,
    'Id ::= IDSENT,
    // Update: for reassign token (i.e id = )
    'VarIdEquals ::= VARIDSENT,
    // Update: for reassignment operators (e.g id += )
    'VarPlusEquals ::= VARPLUSEQSENT,
    'VarMinusEquals ::= VARMINUSEQSENT,
    'VarTimesEquals ::= VARTIMESEQSENT,
    'VarDivEquals ::= VARDIVEQSENT,
    'VarModEquals ::= VARMODEQSENT,
    'VarConcatEquals ::= VARCONCATEQSENT
  ))

  def run(ctx: Context)(tokens: Stream[Token]): Program = {
    val (grammar, constructor) = (amyGrammarLL1, new ASTConstructorLL1)
    //val (grammar, constructor) = (amyGrammar, new ASTConstructor)

    import ctx.reporter._
    implicit val gc = new GlobalContext()
    implicit val pc = new ParseContext()

    GrammarUtils.isLL1WithFeedback(grammar) match {
      case InLL1() =>
        // info("Grammar is in LL1")
      case other =>
        warning(other)
    }

    val feedback = ParseTreeUtils.parseWithTrees(grammar, tokens.toList)
    feedback match {
      case s: Success[Token] =>
        constructor.constructProgram(s.parseTrees.head)
      case err@LL1Error(_, Some(tok)) =>
        fatal(s"Parsing failed: $err", tok.obj.position)
      case err =>
        fatal(s"Parsing failed: $err")
    }
  }

}
