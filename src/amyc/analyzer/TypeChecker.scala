package amyc
package analyzer

import utils._
import ast.SymbolicTreeModule._
import ast.Identifier

// The type checker for Amy
// Takes a symbolic program and rejects it if it does not follow the Amy typing rules.
object TypeChecker extends Pipeline[(Program, SymbolTable), (Program, SymbolTable)] {

  // Compute the least upped bound of two type.
  // This is defined if and only if the types are equal or one is NothingType
  def leastUpperBound(t1: Type, t2: Type): Option[Type] = (t1, t2) match {
    case (NothingType, t2) => Some(t2)
    case (t1, NothingType) => Some(t1)
    case (t1, t2) if t1 == t2 => Some(t1)
    case _ => None
  }

  // leastUpperBound for a list of types
  def leastUpperBound(tps: List[Type]): Option[Type] = {
    require(tps.nonEmpty)
    tps.foldLeft[Option[Type]](Some(NothingType)) {
      case (soFar, current) => soFar flatMap (leastUpperBound(_, current))
    }
  }

  def run(ctx: Context)(v: (Program, SymbolTable)) = {
    import ctx.reporter._

    val (program, table) = v

    // Type check an expression
    // 'expected' is the type 'expr' should have. If None, 'expr' can be of any type
    // Make sure the helper methods provided.
    // Remember to add local variables to the symbol table to use them later!
    def tc(expr: Expr, expected: Option[Type]): Type = {
      // Check a type 'actual' against the expected type if present,
      // and emit an error if it is not equal
      def check(actual: Type)(implicit pos: Positioned) = {
        expected.foreach { exp =>
          if (actual != exp) {
            error(s"Type error: expected $exp, found $actual", pos)
          }
        }
        actual
      }

      // Compute least upper bound, emit an error and return the first one if not defined
      def lub(t1: Type, t2: Type)(pos: Positioned) = leastUpperBound(t1, t2).getOrElse{
        error(s"Incompatible types $t1 and $t2", pos)
        t1
      }

      // Same as lub, but for a list of types
      def lub_*(ts: List[Type])(pos: Positioned): Type = {
        require(ts.nonEmpty)
        ts.reduceLeft[Type](lub(_, _)(pos))
      }

      // Implicit position for 'check'
      implicit val pos: Positioned = expr

      expr match {
        // Literals
        case IntLiteral(_) => check(IntType)
        case BooleanLiteral(_) => check(BooleanType)
        case StringLiteral(_) => check(StringType)
        case UnitLiteral() => check(UnitType)

        //Binary Operators
        case Plus(lhs, rhs) => {
          tc(lhs, Some(IntType))
          tc(rhs, Some(IntType))
          check(IntType)
        }

        case Minus(lhs, rhs) => {
          tc(lhs, Some(IntType))
          tc(rhs, Some(IntType))
          check(IntType)
        }

        case Times(lhs, rhs) => {
          tc(lhs, Some(IntType))
          tc(rhs, Some(IntType))
          check(IntType)
        }

        case Div(lhs, rhs) => {
          tc(lhs, Some(IntType))
          tc(rhs, Some(IntType))
          check(IntType)
        }

        case Mod(lhs, rhs) => {
          tc(lhs, Some(IntType))
          tc(rhs, Some(IntType))
          check(IntType)
        }

        case And(lhs, rhs) => {
          tc(lhs, Some(BooleanType))
          tc(rhs, Some(BooleanType))
          check(BooleanType)
        }

        case Or(lhs, rhs) => {
          tc(lhs, Some(BooleanType))
          tc(rhs, Some(BooleanType))
          check(BooleanType)
        }

        case LessThan(lhs, rhs) => {
          tc(lhs, Some(IntType))
          tc(rhs, Some(IntType))
          check(BooleanType)
        }

        case LessEquals(lhs, rhs) => {
          tc(lhs, Some(IntType))
          tc(rhs, Some(IntType))
          check(BooleanType)
        }

        case Concat(lhs, rhs) => {
          tc(lhs, Some(StringType))
          tc(rhs, Some(StringType))
          check(StringType)
        }

        // lub emits error if not the same type
        case Equals(lhs, rhs) => {
          lub(tc(lhs, None), tc(rhs, None))(lhs)
          check(BooleanType)
        }

        //Unary Operatos
        case Neg(e) => {
          tc(e, Some(IntType))
          check(IntType)
        }

        case Not(e) => {
          tc(e, Some(BooleanType))
          check(BooleanType)
        }

          // Error expression
        case Error(msg) => {
          tc(msg, Some(StringType))
          expected.getOrElse(NothingType)
        }

        case Ite(cond, thenn, elze) => {
          tc(cond, Some(BooleanType))
          val t1 = tc(thenn, expected)
          val t2 = tc(elze, expected)
          lub(t1, t2)(cond)
        }
        
         // Update
        case While(cond, body) =>
          tc(cond, Some(BooleanType))
          tc(body, Some(UnitType))
          check(UnitType)

        case Sequence(e1, e2) => {
          tc(e1, None)
          tc(e2, expected)
        }

        // Local variable defintion
        case Let(df, value, body, _) => {
          table.addLocal(df.name, df.tpe.tpe)
          tc(value, Some(df.tpe.tpe))
          tc(body, expected)
        }

        // Update :: typecheck reassignment
        case Reassign(name, newValue) => {
          val t = table.getLocal(name)
          tc(newValue, t)
          check(UnitType)
        }

        // Update :: typecheck reassignment operators
        case PlusEquals(name, newValue) => {
          val t = table.getLocal(name)
          if(t.get != IntType) error(s"Plus equals only applied to variables of type Int", pos)
          tc(newValue, t)
          check(UnitType)
        }

        case MinusEquals(name, newValue) => {
          val t = table.getLocal(name)
          if(t.get != IntType) error(s"Minus equals only applied to variables of type Int", pos)
          tc(newValue, t)
          check(UnitType)
        }

        case TimesEquals(name, newValue) => {
          val t = table.getLocal(name)
          if(t.get != IntType) error(s"Times equals only applied to variables of type Int", pos)
          tc(newValue, t)
          check(UnitType)
        }

        case DivEquals(name, newValue) => {
          val t = table.getLocal(name)
          if(t.get != IntType) error(s"Divide equals only applied to variables of type Int", pos)
          tc(newValue, t)
          check(UnitType)
        }

        case ModEquals(name, newValue) => {
          val t = table.getLocal(name)
          if(t.get != IntType) error(s"Modulus equals only applied to variables of type Int", pos)
          tc(newValue, t)
          check(UnitType)
        }

        case ConcatEquals(name, newValue) => {
          val t = table.getLocal(name)
          if(t.get != StringType) error(s"Concat equals only applied to variables of type String", pos)
          tc(newValue, t)
          check(UnitType)
        }

        // Function/Constructor Call
        case cal@Call(qname, args) => {
          table.getFunction(qname) match {
            case Some(FunSig(argTypes, retType, _)) => {
              argTypes zip args map {
                case (formalArg, arg) => tc(arg, Some(formalArg))
              }
              check(retType)
            }
            case None => table.getConstructor(qname) match {
              case Some(cs@ConstrSig(argTypes, _, _)) => {
                argTypes zip args map {
                  case(formalArg, arg) => tc(arg, Some(formalArg))
                }
                check(cs.retType)
              }
              // Should not arrive here since Name Analysis already ensures name of function or constructor is valid
            }
          }
        }

          // Compiler should fail normally if can't find the variable
        case Variable(name) => {
          check(table.getLocal(name).get)
        }

        case mat@Match(scrut, cases) => {
          val scrutType = tc(scrut, None)

          def tcPat(pat: Pattern, expected: Option[Type]): Type = {
            pat match {
              case wc@WildcardPattern() => expected.getOrElse(NothingType)
              case idp@IdPattern(name) => {
                table.addLocal(name, expected.getOrElse(NothingType))
                expected.getOrElse(NothingType)
              }
              case lp@LiteralPattern(lit) => tc(lit, expected)
              case ccp@CaseClassPattern(constr, args) => {
                val constrSg = table.getConstructor(constr).get
                constrSg match {
                  case cs@ConstrSig(argTypes, _, _) =>
                    argTypes zip args foreach {
                      case (formalArg, arg) => {
                        val ccpType = tcPat(arg, Some(formalArg))
                        lub(ccpType, formalArg)(ccp)
                      }
                    }
                    cs.retType
                }
              }
            }
          }

          val patTypes = cases.map {
            case MatchCase(pat, _) => tcPat(pat, Some(scrutType))
          }

          lub_*(scrutType :: patTypes)(mat)

          val patExprTypes = cases.map {
            case MatchCase(_, expr) => tc(expr, expected)
          }

          lub_*(patExprTypes)(mat)

        }

        // At this point compiler should crash
      }
    }

    // Putting it all togetehr:
    program.modules.foreach { mod =>
      // put function parameters to the symbol table, then typecheck them against the return type
      mod.defs.collect { case FunDef(_, params, retType, body) =>
        params foreach { case ParamDef(name, TypeTree(tpe)) => table.addLocal(name, tpe) }
        tc(body, Some(retType.tpe))
      }
      // Typecheck expression if present, not providing expected type.
      mod.optExpr.foreach(tc(_, None))
    }

    v

  }
}
