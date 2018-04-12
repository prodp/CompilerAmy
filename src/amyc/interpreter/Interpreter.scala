package amyc
package interpreter

import utils._
import ast.SymbolicTreeModule._
import ast.Identifier
import analyzer.SymbolTable

// An interpreter for MiniScala programs, implemented in Scala
object Interpreter extends Pipeline[(Program, SymbolTable), Unit] {

  // A class that represents a value computed by interpreting an expression
  abstract class Value {
    def asInt: Int = this.asInstanceOf[IntValue].i
    def asBoolean: Boolean = this.asInstanceOf[BooleanValue].b
    def asString: String = this.asInstanceOf[StringValue].s
    

    override def toString: String = this match {
      case IntValue(i) => i.toString
      case BooleanValue(b) => b.toString
      case StringValue(s) => s
      case UnitValue => "()"
      case CaseClassValue(constructor, args) =>
        constructor.name + "(" + args.map(_.toString).mkString(", ") + ")"
    }
  }
  case class IntValue(i: Int) extends Value
  case class BooleanValue(b: Boolean) extends Value
  case class StringValue(s: String) extends Value
  case object UnitValue extends Value
  case class CaseClassValue(constructor: Identifier, args: List[Value]) extends Value

  def run(ctx: Context)(v: (Program, SymbolTable)): Unit = {
    val (program, table) = v

    // These built-in functions do not have a MiniScala implementation in the program,
    // instead their implementation is encoded in this map
    val builtIns: Map[(String, String), (List[Value]) => Value] = Map(
      ("Std", "printInt")      -> { args => println(args.head.asInt); UnitValue },
      ("Std", "printString")   -> { args => println(args.head.asString); UnitValue },
      ("Std", "readString")    -> { args => StringValue(scala.io.StdIn.readLine()) },
      ("Std", "readInt")       -> { args =>
        val input = scala.io.StdIn.readLine()
        try {
          IntValue(input.toInt)
        } catch {
          case ne: NumberFormatException =>
            ctx.reporter.fatal(s"""Could not parse "$input" to Int""")
        }
      },
      ("Std", "intToString")   -> { args => StringValue(args.head.asInt.toString) },
      ("Std", "digitToString") -> { args => StringValue(args.head.asInt.toString) }
    )

    // Utility functions to interface with the symbol table.
    def isConstructor(name: Identifier) = table.getConstructor(name).isDefined
    def findFunctionOwner(functionName: Identifier) = table.getFunction(functionName).get.owner.name
    def findFunction(owner: String, name: String) = {
      program.modules.find(_.name.name == owner).get.defs.collectFirst {
        case fd@FunDef(fn, _, _, _) if fn.name == name => fd
      }.get
    }

    // Interprets a function, using evaluations for local variables contained in 'locals'
    def interpret(expr: Expr)(implicit locals: Map[Identifier, Value]): Value = {
      expr match {
        case Variable(name) => 
          locals(name)
        case IntLiteral(i) =>
          IntValue(i)
        case BooleanLiteral(b) =>
          BooleanValue(b)
        case StringLiteral(s) =>
          StringValue(s)
        case UnitLiteral() =>
          UnitValue
        case Plus(lhs, rhs) =>
          IntValue(interpret(lhs).asInt + interpret(rhs).asInt)
        case Minus(lhs, rhs) =>
          IntValue(interpret(lhs).asInt - interpret(rhs).asInt)
        case Times(lhs, rhs) =>
          IntValue(interpret(lhs).asInt * interpret(rhs).asInt)
        case Div(lhs, rhs) =>
          IntValue(interpret(lhs).asInt / interpret(rhs).asInt)
        case Mod(lhs, rhs) =>
          IntValue(interpret(lhs).asInt % interpret(rhs).asInt)
        case LessThan(lhs, rhs) =>
          BooleanValue(interpret(lhs).asInt < interpret(rhs).asInt)
        case LessEquals(lhs, rhs) =>
          BooleanValue(interpret(lhs).asInt <= interpret(rhs).asInt)
        case And(lhs, rhs) =>
          BooleanValue(interpret(lhs).asBoolean && interpret(rhs).asBoolean)
        case Or(lhs, rhs) =>
          BooleanValue(interpret(lhs).asBoolean || interpret(rhs).asBoolean)
        case Equals(lhs, rhs) =>
          interpret(lhs) match {
            case IntValue(i) => BooleanValue(interpret(rhs).asInt == i)
            case BooleanValue(b) => BooleanValue(interpret(rhs).asBoolean == b)
            case s @ StringValue(string) => BooleanValue(s.eq(interpret(rhs)))
            case UnitValue => BooleanValue(true)
            case cc @ CaseClassValue(c, args) => BooleanValue(cc.eq(interpret(rhs)))
          } // Hint: Take care to implement Amy equality semantics
        case Concat(lhs, rhs) =>
          StringValue(interpret(lhs).asString.concat(interpret(rhs).asString))
        case Not(e) =>
          BooleanValue(!interpret(e).asBoolean)
        case Neg(e) =>
          IntValue(-interpret(e).asInt)
        case Call(qname, args) =>
          if(isConstructor(qname)) {
            CaseClassValue(qname, args.map(x => interpret(x)))
          } /*else if(builtIns.contains(("Std", qname.name))){
            builtIns(("Std", qname.name))(args.map(interpret(_)))
          }*/ else {
            val fOwner = findFunctionOwner(qname)
            if (builtIns.contains((fOwner, qname.name))) {
              builtIns((fOwner, qname.name))(args.map(interpret(_)))
            } else {
              val f = findFunction(fOwner, qname.name)
              val l = f.paramNames.zip(args.map(interpret(_)))
              val newLocals = locals ++ l.toMap
              interpret(f.body)(newLocals)
            }
          }
        case Sequence(e1, e2) =>
          interpret(e1)
          interpret(e2)
        case Let(df, value, body, NOTVARIABLE) =>
          interpret(body)(locals + (df.name -> interpret(value)))
        case Ite(cond, thenn, elze) =>
          if(interpret(cond).asBoolean) interpret(thenn) else interpret(elze)
          
        // Update
        case While(cond, body) =>
          while(interpret(cond).asBoolean){
            interpret(body)
          }
          UnitValue
        case Match(scrut, cases) =>

          val evS = interpret(scrut)
          // Returns a list of pairs id -> value,
          // where id has been bound to value within the pattern.
          // Returns None when the pattern fails to match.
          // Note: Only works on well typed patterns (since the problem is well typed).
          def matchesPattern(v: Value, pat: Pattern): Option[List[(Identifier, Value)]] = {
            ((v, pat): @unchecked) match {
              case (_, WildcardPattern()) => Some(Nil)
              case (_, IdPattern(name)) => Some(List(name -> v))
              case (IntValue(i1), LiteralPattern(IntLiteral(i2))) =>
                if(i1 == i2) Some(Nil) else None
              case (BooleanValue(b1), LiteralPattern(BooleanLiteral(b2))) =>
                if(b1 == b2) Some(Nil) else None
              case (StringValue(_), LiteralPattern(StringLiteral(_))) =>
                None
              case (UnitValue, LiteralPattern(UnitLiteral())) =>
                Some(Nil)
              case (CaseClassValue(con1, realArgs), CaseClassPattern(con2, formalArgs)) =>
                if(con1 != con2) {
                  None
                } else {
                  Some(realArgs.zip(formalArgs).flatMap{
                    case (v,p) => {
                      if(matchesPattern(v,p) != None) {
                        matchesPattern(v, p).get
                      } else return None
                    }

                  })
                }
            }
          }

          // Main "loop" of the implementation: Go through every case,
          // check if the pattern matches, and if so return the evaluation of the case expression
          for {
            MatchCase(pat, rhs) <- cases
            moreLocals <- matchesPattern(evS, pat)
          } {
            return interpret(rhs)(locals ++ moreLocals)
          }
          // No case matched: The program fails with a match error
          ctx.reporter.fatal(s"Match error: ${evS.toString}@${scrut.position}")

        case Error(msg) =>
          ctx.reporter.fatal(interpret(msg))
      }
    }

    // Body of the interpreter: Go through every module in order
    // and evaluate its expression if present
    for {
      m <- program.modules
      e <- m.optExpr
    } {
      interpret(e)(Map())
    }
  }
}
