package amyc
package parsing

import grammarcomp.parsing._
import utils.Positioned
import ast.NominalTreeModule._
import Tokens._

// Implements the translation from parse trees to ASTs for the LL1 grammar.
// Corresponds to Parser.msGrammarLL1
// This extends the plain ASTConstructor as some things will be the same.
// You should override whatever has changed.
// Make sure to use ASTConstructor as an example
class ASTConstructorLL1 extends ASTConstructor {

  // Update :: construct variable id when reassigning
  @Override
  override def constructName(ptree: NodeOrLeaf[Token]): (String, Positioned) = {
    ptree match {
      case Node('Id ::= _, List(Leaf(id@ID(name)))) =>
        (name, id)
      case Node('VarIdEquals ::= _, List(Leaf(vid@VARIDREASSIGN(name)))) =>
        (name, vid)
      case Node('VarPlusEquals ::= _, List(Leaf(vid@VARPLUSEQUALS(name)))) =>
        (name, vid)
      case Node('VarMinusEquals ::= _, List(Leaf(vid@VARMINUSEQUALS(name)))) =>
        (name, vid)
      case Node('VarTimesEquals ::= _, List(Leaf(vid@VARTIMESEQUALS(name)))) =>
        (name, vid)
      case Node('VarDivEquals ::= _, List(Leaf(vid@VARDIVEQUALS(name)))) =>
        (name, vid)
      case Node('VarModEquals ::= _, List(Leaf(vid@VARMODEQUALS(name)))) =>
        (name, vid)
      case Node('VarConcatEquals ::= _, List(Leaf(vid@VARCONCATEQUALS(name)))) =>
        (name, vid)
    }
  }

  @Override
  override def constructQname(pTree: NodeOrLeaf[Token]): (QualifiedName, Positioned) = {
    pTree match {
      case Node('QName ::= _, List(id, Node('QNameHelper ::= _ , List(_, id2)))) =>
        val (module, pos) = constructName(id)
        val (name, _) = constructName(id2)
        (QualifiedName(Some(module), name), pos)
      case Node('QName ::= _, List(id, Node(_, List()))) =>
        val (name, pos) = constructName(id)
        (QualifiedName(None, name), pos)

    }
  }

  @Override
  override def constructLiteral(pTree: NodeOrLeaf[Token]): Literal[_] = {
    pTree match {
      case Node('Literal ::= List(INTLITSENT), List(Leaf(it@INTLIT(i)))) =>
        IntLiteral(i).setPos(it)
      case Node('Literal ::= List(STRINGLITSENT), List(Leaf(st@STRINGLIT(s)))) =>
        StringLiteral(s).setPos(st)
      case Node('Literal ::= _, List(Leaf(tt@TRUE()))) =>
        BooleanLiteral(true).setPos(tt)
      case Node('Literal ::= _, List(Leaf(tf@FALSE()))) =>
        BooleanLiteral(false).setPos(tf)
      case Node('LiteralParen ::= _, List(Leaf(lp@LPAREN()), Node('LiteralParenHelper ::= _, List(Leaf(rp@RPAREN()))))) =>
        UnitLiteral().setPos(lp)
    }
  }

  override def constructExpr(ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
        // module.f()
      case Node('OrdExpr ::= List('QExpr),
      List(Node('QExpr ::= _, List(id,
      Node('QExprHelper ::= _, List(Node('QNameHelper ::= _, List(Leaf(DOT()), id2)), Leaf(lp@LPAREN()), as, Leaf(rp@RPAREN()))))))) =>
        val (module, pos) = constructName(id)
        val (name, _) = constructName(id2)
        val (qname, qpos) = (QualifiedName(Some(module), name), pos)
        val args = constructList(as, constructExpr, hasComma = true)
        Call(qname, args).setPos(qpos)
        // f()
      case Node('OrdExpr ::= List('QExpr),
      List(Node('QExpr ::= _,
      List(id, Node('QExprHelper ::= _, List(Node('QNameHelper ::= _, List()), Leaf(lp@LPAREN()), as, Leaf(rp@RPAREN()))))))) =>
        val (name, pos) = constructName(id)
        val (qname, qpos) = (QualifiedName(None, name), pos)
        val args = constructList(as, constructExpr, hasComma = true)
        Call(qname, args).setPos(qpos)
        // Just id
      case Node('OrdExpr ::= List('QExpr), List(Node('QExpr::= _, List(id, Node('QExprHelper::= _, List()))))) =>
        val (name, pos) = constructName(id)
        Variable(name).setPos(pos)
      case Node('OrdExpr ::= List('Literal), List(lit)) =>
        constructLiteral(lit)
      case Node('OrdExpr ::= List('LiteralParen),
      List(Node('LiteralParen::= _, List(Leaf(lp@LPAREN()), Node('LiteralParenHelper ::= _, List(expr, Leaf(rp@RPAREN()))))))) =>
        constructExpr(expr).setPos(lp)
      // Construct Literal Parenthesis
      case Node('OrdExpr ::= List('LiteralParen), List(lit)) =>
        constructLiteral(lit)
      case Node('ExprMultDef::= List('ExprUnary, 'MultExpr), List(eu, opexpr)) =>
        val pe1 = constructExpr(eu)
        constructOpExpr(pe1, opexpr)
      case Node('ExprAddDef ::= _, List(em, opexpr)) =>
        val pe1 = constructExpr(em)
        constructOpExpr(pe1, opexpr)
      case Node('ExprCompDef ::= _, List(ea, opexpr)) =>
        val pe1 = constructExpr(ea)
        constructOpExpr(pe1, opexpr)
      case Node('ExprEqDef ::= _, List(ec, opexpr)) =>
        val pe1 = constructExpr(ec)
        constructOpExpr(pe1, opexpr)
      case Node('ExprAndDef ::= _, List(eeq, opexpr)) =>
        val pe1 = constructExpr(eeq)
        constructOpExpr(pe1, opexpr)
      case Node('ExprOrDef ::= _, List(ean, opexpr)) =>
        val pe1 = constructExpr(ean)
        constructOpExpr(pe1, opexpr)
      case Node('ExprUnary ::= List(BANG(), _), List(Leaf(bt), e)) =>
        Not(constructExpr(e)).setPos(bt)
      case Node('ExprUnary ::= List(MINUS(), _), List(Leaf(mt), e)) =>
        Neg(constructExpr(e)).setPos(mt)
      case Node('ExprUnary::= List('OrdExpr), List(e)) =>
        constructExpr(e)
        //construct Sequence
      case Node('Expr ::= List('ExprMatchDef, 'ExprHelper), List(e1, Node('ExprHelper::= _, List(_, e2)))) =>
        val expr1 = constructExpr(e1)
        val expr2 = constructExpr(e2)
        Sequence(expr1, expr2).setPos(expr1)
      case Node('Expr ::= List('ExprMatchDef, 'ExprHelper), List(e1, Node('ExprHelper::= _, List()))) =>
        constructExpr(e1)
      case Node('Expr ::= (VAL() :: _), List(Leaf(vt), param, _, value, _, body)) =>
        Let(constructParam(param), constructExpr(value), constructExpr(body), NOTVARIABLE).setPos(vt)
        // Update :: Construct variable
      case Node('Expr ::= (VAR() :: _), List(Leaf(vt), param, _, value, _, body)) =>
        Let(constructParam(param), constructExpr(value), constructExpr(body), VARIABLE).setPos(vt)
        // Update :: Construct Reassign case
      case Node('Expr ::= ('VarIdEquals :: _), List(id, newValue, Node('ExprHelper::= _, List()))) =>{
        val (varName, pos) = constructName(id)
        Reassign(varName, constructExpr(newValue)).setPos(pos)
      }
      case Node('Expr ::= ('VarIdEquals :: _), List(id, newValue, Node('ExprHelper::= _, List(_, e)))) =>{
        val (varName, pos) = constructName(id)
        Sequence(Reassign(varName, constructExpr(newValue)).setPos(pos), constructExpr(e)).setPos(pos)
      }
        // Update: Construct variable reassignment operators
      case Node('Expr ::= ('VarPlusEquals :: _), List(id, value, Node('ExprHelper::= _, List()))) =>{
        val (varName, pos) = constructName(id)
        PlusEquals(varName, constructExpr(value)).setPos(pos)
      }
      case Node('Expr ::= ('VarPlusEquals :: _), List(id, value, Node('ExprHelper::= _, List(_, e)))) =>{
        val (varName, pos) = constructName(id)
        Sequence(PlusEquals(varName, constructExpr(value)).setPos(pos), constructExpr(e)).setPos(pos)
      }
      case Node('Expr ::= ('VarMinusEquals :: _), List(id, value, Node('ExprHelper::= _, List()))) =>{
        val (varName, pos) = constructName(id)
        MinusEquals(varName, constructExpr(value)).setPos(pos)
      }
      case Node('Expr ::= ('VarMinusEquals :: _), List(id, value, Node('ExprHelper::= _, List(_, e)))) =>{
        val (varName, pos) = constructName(id)
        Sequence(MinusEquals(varName, constructExpr(value)).setPos(pos), constructExpr(e)).setPos(pos)
      }
      case Node('Expr ::= ('VarTimesEquals :: _), List(id, value, Node('ExprHelper::= _, List()))) =>{
        val (varName, pos) = constructName(id)
        TimesEquals(varName, constructExpr(value)).setPos(pos)
      }
      case Node('Expr ::= ('VarTimesEquals :: _), List(id, value, Node('ExprHelper::= _, List(_, e)))) =>{
        val (varName, pos) = constructName(id)
        Sequence(TimesEquals(varName, constructExpr(value)).setPos(pos), constructExpr(e)).setPos(pos)
      }
      case Node('Expr ::= ('VarDivEquals :: _), List(id, value, Node('ExprHelper::= _, List()))) =>{
        val (varName, pos) = constructName(id)
        DivEquals(varName, constructExpr(value)).setPos(pos)
      }
      case Node('Expr ::= ('VarDivEquals :: _), List(id, value, Node('ExprHelper::= _, List(_, e)))) =>{
        val (varName, pos) = constructName(id)
        Sequence(DivEquals(varName, constructExpr(value)).setPos(pos), constructExpr(e)).setPos(pos)
      }
      case Node('Expr ::= ('VarModEquals :: _), List(id, value, Node('ExprHelper::= _, List()))) =>{
        val (varName, pos) = constructName(id)
        ModEquals(varName, constructExpr(value)).setPos(pos)
      }
      case Node('Expr ::= ('VarModEquals :: _), List(id, value, Node('ExprHelper::= _, List(_, e)))) =>{
        val (varName, pos) = constructName(id)
        Sequence(ModEquals(varName, constructExpr(value)).setPos(pos), constructExpr(e)).setPos(pos)
      }
      case Node('Expr ::= ('VarConcatEquals :: _), List(id, value, Node('ExprHelper::= _, List()))) =>{
        val (varName, pos) = constructName(id)
        ConcatEquals(varName, constructExpr(value)).setPos(pos)
      }
      case Node('Expr ::= ('VarConcatEquals :: _), List(id, value, Node('ExprHelper::= _, List(_, e)))) =>{
        val (varName, pos) = constructName(id)
        Sequence(ConcatEquals(varName, constructExpr(value)).setPos(pos), constructExpr(e)).setPos(pos)
      }

      case Node('OrdExpr ::= List('CondExpr), List(Node('CondExpr::= _, List(Leaf(it), _, cond, _, _, thenn, _, _, _, elze, _)))) =>
        Ite(
          constructExpr(cond),
          constructExpr(thenn),
          constructExpr(elze)
        ).setPos(it)
      /****/
      case Node('OrdExpr ::= List('WhileExpr), List(Node('WhileExpr::= _, List(Leaf(it), _, cond, _, _, thenn, _)))) =>
        While(
          constructExpr(cond),
          constructExpr(thenn)
        ).setPos(it)
      case Node('ExprMatchDef ::= List('ExprOrDef, 'ExprMatch), List(sc, Node('ExprMatch::= (MATCH() :: _), List(_, _, cases,_)))) =>
        val scrut = constructExpr(sc)
        Match(scrut, constructCasesList1(cases, constructCase))
      case Node('ExprMatchDef ::= List('ExprOrDef, 'ExprMatch), List(sc, Node('ExprMatch::= _, List()))) =>
        constructExpr(sc)
      case Node('OrdExpr ::= List('ErrExpr), List(Node('ErrExpr ::= _,List(Leaf(ert), _, msg, _)))) =>
        Error(constructExpr(msg)).setPos(ert)

    }
  }


  @Override
  override def constructPattern(pTree: NodeOrLeaf[Token]): Pattern = {
    pTree match {
      case Node('Pattern ::= List(UNDERSCORE()), List(Leaf(ut))) =>
        WildcardPattern().setPos(ut)
      case Node('Pattern ::= List('Literal), List(lit)) =>
        val literal = constructLiteral(lit)
        LiteralPattern(literal).setPos(literal)
        //Add special case left right parenthesis literal
      case Node('Pattern ::= List(LPAREN(), RPAREN()), List(Leaf(lp@LPAREN()), Leaf(rp@RPAREN()))) =>
        val literal = UnitLiteral().setPos(lp)
        LiteralPattern(literal).setPos(literal)
      case Node('Pattern ::= List('Id, 'PatternPrime), List(id, Node('PatternPrime ::= _, List(Node('QNameHelper::= _, List(_, id2)), Leaf(lp@LPAREN()), patts, Leaf(rp@RPAREN()))))) =>
        val (module, pos) = constructName(id)
        val (name, _) = constructName(id2)
        val (qname, qpos) = (QualifiedName(Some(module), name), pos)
        val patterns = constructList(patts, constructPattern, hasComma = true)
        CaseClassPattern(qname, patterns).setPos(pos)
      case Node('Pattern ::= List('Id, 'PatternPrime), List(id, Node('PatternPrime ::= _, List(Node('QNameHelper::= _, List()), Leaf(lp@LPAREN()), patts, Leaf(rp@RPAREN()))))) =>
        val (name, pos) = constructName(id)
        val (qname, qpos) = (QualifiedName(None, name), pos)
        val patterns = constructList(patts, constructPattern, hasComma = true)
        CaseClassPattern(qname, patterns).setPos(pos)
      case Node('Pattern ::= List('Id, 'PatternPrime), List(id, Node(_, List()))) =>
        val (name, pos) = constructName(id)
        IdPattern(name).setPos(pos)
    }
  }

  /*
  Helper function to construct cases
   */
  def constructCasesList1[A](ptree: NodeOrLeaf[Token], constructor: NodeOrLeaf[Token] => A, hasComma: Boolean = false): List[A] = {
    ptree match {
      case Node(_, List(t, Node(_, List()))) => List(constructor(t))
      case Node(_, List(t, Node(_, List(ts)))) =>
        constructor(t) :: constructCasesList1(ts, constructor, hasComma)
      case Node(_, List(t, Node(_, List(Leaf(COMMA()), ts)))) if hasComma =>
        constructor(t) :: constructCasesList1(ts, constructor, hasComma)
    }
  }

  // Important helper method:
  // Because LL1 grammar is not helpful in implementing left associativity,
  // we give you this method to reconstruct it.
  // This method takes the left operand of an operator (leftopd)
  // as well as the tree that corresponds to the operator plus the right operand (ptree)
  // It parses the right hand side and then reconstruct the operator expression
  // with correct associativity.
  // If ptree is empty, it means we have no more operators and the leftopd is returned.
  // Note: You may have to override constructOp also, depending on your implementation
  def constructOpExpr(leftopd: Expr, ptree: NodeOrLeaf[Token]): Expr = {
    ptree match {
      case Node(_, List()) => //epsilon rule of the nonterminals
        leftopd
      case Node(sym ::= _, List(op, rightNode))
        if Set('OrExpr, 'AndExpr, 'EqExpr, 'CompExpr, 'AddExpr, 'MultExpr) contains sym =>
        rightNode match {
          case Node(_, List(nextOpd, suf)) => // 'Expr? ::= Expr? ~ 'OpExpr,
            val nextAtom = constructExpr(nextOpd)
            constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf) // captures left associativity
        }
    }
  }

  @Override
  override def constructOp(ptree: NodeOrLeaf[Token]): (Expr, Expr) => Expr = {
    ptree match {
      case Leaf(t) =>
        tokenToExpr(t)
    }
  }

}

