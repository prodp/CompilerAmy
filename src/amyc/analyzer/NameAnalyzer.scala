package amyc
package analyzer

import amyc.ast.NominalTreeModule.QualifiedName
import utils._
import ast.{Identifier, NominalTreeModule => N, SymbolicTreeModule => S}
import sun.security.pkcs11.Secmod.Module

// Name analyzer for Amy
// Takes a nominal program (names are plain string, qualified names are string pairs)
// and returns a symbolic program, where all names have been resolved to unique Identifiers.
// Rejects programs that violate the Amy naming rules.
// Also populates and returns symbol table.
object NameAnalyzer extends Pipeline[N.Program, (S.Program, SymbolTable)] {
  def run(ctx: Context)(p: N.Program): (S.Program, SymbolTable) = {
    import ctx.reporter._
    
    // Step 0: Initialize symbol table
    val table = new SymbolTable

    // Helper method: will transform a nominal type 'tt' to a symbolic type,
    // given we are within module 'inModule'
    def transformType(tt: N.TypeTree, inModule: String): S.Type = {
      tt.tpe match {
        case N.IntType => S.IntType
        case N.BooleanType => S.BooleanType
        case N.StringType => S.StringType
        case N.UnitType => S.UnitType
        case N.ClassType(qn@N.QualifiedName(module, name)) =>
          table.getType(module getOrElse inModule, name) match {
            case Some(symbol) =>
              S.ClassType(symbol)
            case None =>
              fatal(s"Could not find type $qn", tt)
          }
      }
    }

    // Step 1: Add modules to table 
    val modNames = p.modules.groupBy(_.name)
    modNames.foreach{ case (name, modules) =>
      if (modules.size > 1) {
        fatal(s"Two modules named $name in program", modules.head.position)
      }
    }
    modNames.keys.toList foreach table.addModule


    // Step 2: Check name uniqueness of definitions in each module
    val moduleDefinitions = p.modules.map(m => m.defs.groupBy(_.name))
    moduleDefinitions.foreach(md => md.foreach{
      case (name, defs) =>
        if(defs.size > 1) {
          fatal(s"Two defintions named $name in same module", defs.head.position)
        }
    })

    // Step 3: Discover types and add them to symbol table
    p.modules.foreach{
      case N.ModuleDef(moduleName, defs, _) => {
        defs.foreach{
          case N.AbstractClassDef(aname) => table.addType(moduleName, aname)
          case _ =>
        }
      }
    }

    // Step 4: Discover type constructors, add them to table
    p.modules.foreach{
      case N.ModuleDef(moduleName, defs, _) => {
        defs.foreach{
          case N.CaseClassDef(className, fields, parentName) => {
            val parentClass = table.getType(moduleName, parentName)
            parentClass match {
              case Some(pc) => table.addConstructor(moduleName, className,
                fields.map(nf => transformType(nf, moduleName)), pc)
              case None => fatal(s"Trying to extend $parentName not in module $moduleName", defs.head.position)
            }
          }
          case _ =>
        }
      }
    }

    // Step 5: Discover functions signatures, add them to table
    p.modules.foreach{
      case N.ModuleDef(moduleName, defs, _) => {
        defs.foreach{
          case N.FunDef(fname, params, retType, _) => {
            table.addFunction(moduleName, fname,
              params.map(param => transformType(param.tpe, moduleName)), transformType(retType, moduleName))
          }
          case _ =>
        }
      }
    }

    // Step 6: We now know all definitions in the program.
    //         Reconstruct modules and analyse function bodies/ expressions
    
    // This part is split into three transfrom functions,
    // for definitions, FunDefs, and expressions.
    // You will need to have in mind we transform constructs of the
    // NominalTreeModule 'N' to respective constructs of the SymbolicTreeModule 'S'.
    // transformFunDef is given as an example, as well as some code for the other ones

    def transformDef(df: N.ClassOrFunDef, module: String): S.ClassOrFunDef = { df match {
      case acd@N.AbstractClassDef(name) =>
        S.AbstractClassDef(table.getType(module, name).get).setPos(acd)
      case ccd@N.CaseClassDef(name, fields, _) => {
        // symbol and constructor signature
        val Some((sym, sig)) = table.getConstructor(module, name)
        val newFields = fields zip sig.argTypes map { case (nt, st) =>
          S.TypeTree(st).setPos(nt)
        }

        S.CaseClassDef(sym, newFields, sig.parent).setPos(ccd)
      }
      case fd: N.FunDef =>
        transformFunDef(fd, module)
    }}.setPos(df)

    def transformFunDef(fd: N.FunDef, module: String): S.FunDef = {
      val N.FunDef(name, params, retType, body) = fd
      val Some((sym, sig)) = table.getFunction(module, name)

      params.groupBy(_.name).foreach { case (name, ps) =>
        if (ps.size > 1) {
          fatal(s"Two parameters named $name in function ${fd.name}", fd)
        }
      }

      val paramNames = params.map(_.name)

      val newParams = params zip sig.argTypes map { case (pd@N.ParamDef(name, tt), tpe) =>
        val s = Identifier.fresh(name)
        S.ParamDef(s, S.TypeTree(tpe).setPos(tt)).setPos(pd)
      }

      val paramsMap = paramNames.zip(newParams.map(_.name)).toMap

      S.FunDef(
        sym,
        newParams,
        S.TypeTree(sig.retType).setPos(retType),
        transformExpr(body)(module, (paramsMap, Map()))
      ).setPos(fd)
    }

    // This function takes as implicit a pair of two maps:
    // The first is a map from names of parameters to their unique identifiers,
    // the second is similar for local variables.
    // Make sure to update them correctly if needed given the scoping rules of Amy
    def transformExpr(expr: N.Expr)
                     (implicit module: String, names: (Map[String, Identifier],
                       Map[String, (Identifier, Boolean)])): S.Expr = {
      val (params, locals) = names
      val res = expr match {
        case N.Match(scrut, cases) =>
          def transformCase(cse: N.MatchCase) = {
            val mc@N.MatchCase(pat, rhs) = cse
            val (newPat, moreLocals) = transformPattern(pat)
            // Check for duplicate id patterns already defined in top pattern
            moreLocals.foreach{ case(s, _) =>
              if(locals.contains(s)) fatal(s"Pattern identifier $s is already defined", mc)
            }
            // Converting moreLocals list to Map should be safe, since every key value pair of local variable is unique
            // Update
            S.MatchCase(newPat, transformExpr(rhs)(module, (params, locals ++
              moreLocals.map{case (name, sym) => (name, (sym, false))}.toMap))).setPos(mc)
          }

          // Returns a transformed pattern along with all bindings
          // from strings to unique identifiers for names bound in the pattern.
          // Also, calls 'fatal' if a new name violates the Amy naming rules.
          def transformPattern(pat: N.Pattern): (S.Pattern, List[(String, Identifier)]) = {
            pat match {
              case wc@N.WildcardPattern() => (S.WildcardPattern().setPos(wc), List())

              case lp@N.LiteralPattern(lit) => lit match {
                case N.IntLiteral(value) => (S.LiteralPattern(S.IntLiteral(value)).setPos(lp), List())
                case N.BooleanLiteral(value) => (S.LiteralPattern(S.BooleanLiteral(value)).setPos(lp), List())
                case N.StringLiteral(value) => (S.LiteralPattern(S.StringLiteral(value)).setPos(lp), List())
                case N.UnitLiteral() => (S.LiteralPattern(S.UnitLiteral()).setPos(lp), List())
              }
              case idp@N.IdPattern(name) => {
                // Emit Nullary constructor warning
                table.getConstructor(module, name) match {
                  case Some((_, sig)) if (sig.argTypes.isEmpty) =>
                    warning(s"Identifier pattern has same name of that of the nullary constructor $name", idp)
                  case _ =>
                }
                val s = Identifier.fresh(name)
                (S.IdPattern(s).setPos(idp), List((name, s)))
              }

              case ccp@N.CaseClassPattern(constr, args) => {
                val (constrModule, cname) = constr match {
                  case N.QualifiedName(Some(otherModule), constructorName) => (otherModule, constructorName)
                  case N.QualifiedName(None, constructorName) => (module, constructorName)
                }

                // Check for constructor and correct number of arguments
                val sym = table.getConstructor(constrModule, cname) match {
                  case None => fatal(s"Constructor ${constrModule}.${cname} not found", ccp)
                  case Some((sym, sig)) => {
                    if(sig.argTypes.size != args.size) fatal(s"Wrong number of arguments for constructor ${cname}", ccp)
                    else sym
                  }
                }

                val newArgs = args map transformPattern
                val newPatterns = newArgs.map(arg => arg._1)
                val newIdentifiers = newArgs.flatMap(arg => arg._2)

                //Check for binder duplicates
                newIdentifiers.groupBy(_._1).foreach{ case (s, binds) =>
                  if(binds.size > 1) fatal(s"Two binders named $s in case constructor ${cname}", ccp)
                }

                (S.CaseClassPattern(sym, newPatterns).setPos(ccp), newIdentifiers)

              }
            }
          }

          S.Match(transformExpr(scrut), cases map transformCase)

        // Literals
        case il@N.IntLiteral(value) => S.IntLiteral(value).setPos(il)
        case bl@N.BooleanLiteral(value) => S.BooleanLiteral(value).setPos(bl)
        case sl@N.StringLiteral(value) => S.StringLiteral(value).setPos(sl)
        case ul@N.UnitLiteral() => S.UnitLiteral().setPos(ul)

        // Variable case
        case v@N.Variable(vname) => locals.get(vname) match {
          case Some((sym, _)) => S.Variable(sym).setPos(v)
          case None => params.get(vname) match {
            case None => fatal(s"Variable $vname not found", v)
            case Some(sym) => S.Variable(sym).setPos(v)
          }
        }

        // Binary Operators
        case pl@N.Plus(lhs, rhs) => S.Plus(transformExpr(lhs), transformExpr(rhs)).setPos(pl)
        case mi@N.Minus(lhs, rhs) => S.Minus(transformExpr(lhs), transformExpr(rhs)).setPos(mi)
        case ti@N.Times(lhs, rhs) => S.Times(transformExpr(lhs), transformExpr(rhs)).setPos(ti)
        case dv@N.Div(lhs, rhs) => S.Div(transformExpr(lhs), transformExpr(rhs)).setPos(dv)
        case md@N.Mod(lhs, rhs) => S.Mod(transformExpr(lhs), transformExpr(rhs)).setPos(md)
        case lt@N.LessThan(lhs, rhs) => S.LessThan(transformExpr(lhs), transformExpr(rhs)).setPos(lt)
        case le@N.LessEquals(lhs, rhs) => S.LessEquals(transformExpr(lhs), transformExpr(rhs)).setPos(le)
        case an@N.And(lhs, rhs) => S.And(transformExpr(lhs), transformExpr(rhs)).setPos(an)
        case or@N.Or(lhs, rhs) => S.Or(transformExpr(lhs), transformExpr(rhs)).setPos(or)
        case eq@N.Equals(lhs, rhs) => S.Equals(transformExpr(lhs), transformExpr(rhs)).setPos(eq)
        case cnc@N.Concat(lhs, rhs) => S.Concat(transformExpr(lhs), transformExpr(rhs)).setPos(cnc)

        // Unary Operators
        case not@N.Not(e) => S.Not(transformExpr(e)).setPos(not)
        case neg@N.Neg(e) => S.Neg(transformExpr(e)).setPos(neg)

        case seq@N.Sequence(e1, e2) => S.Sequence(transformExpr(e1), transformExpr(e2)).setPos(seq)

          // Call can be to constructor or function
        case cn@N.Call(qname, args) => {
          val (callModule, cname) = qname match {
            case N.QualifiedName(Some(otherModule), cname) => (otherModule, cname)
            case N.QualifiedName(None, cname) => (module, cname)
          }
          // Check if function + Check number of arguments
          val sym = table.getFunction(callModule, cname) match {
            case Some((symb, sig)) => {
              if(sig.argTypes.size != args.size) fatal(s"Wrong number of arguments for function ${cname}", cn)
              else symb
            }
            case None => {
              // Check if Constructor + Check number of arguments
              table.getConstructor(callModule, cname) match {
                case Some((symb, sig)) => {
                  if(sig.argTypes.size != args.size) fatal(s"Wrong number of arguments for constructor ${cname}", cn)
                  else symb
                }
                case None => fatal(s"$cname doesn't correspond to function or constructor in module ${callModule}", cn)
              }
            }
          }
          S.Call(sym, args map transformExpr).setPos(cn)
        }

        case ite@N.Ite(exprIf, exprThen, exprElse) => S.Ite(transformExpr(exprIf), transformExpr(exprThen),
          transformExpr(exprElse)).setPos(ite)
        
         // Update
        case whileExpr@N.While(cond, body) => 
          S.While(transformExpr(cond), 
              transformExpr(body)).setPos(whileExpr)

          // Local variable definition case
          // Update
        case let@N.Let(df, value, body, variable) => {
          // Check for duplicate local variables
          if(locals.contains(df.name)) {
            fatal(s"Duplicate local variable ${df.name}", let)
          } else {
            // Warning if local variable shadow parameter
            if(params.contains(df.name)) warning(s"Local variable shadows param ${df.name}", df)
            val s = Identifier.fresh(df.name)
            S.Let(S.ParamDef(s, S.TypeTree(transformType(df.tpe, module)).setPos(df.tpe)),
              transformExpr(value),
              transformExpr(body)(module, (params, locals + (df.name -> (s, variable)))), variable).setPos(let)
          }
        }

          // Update :: Check if we are reassigning a variable and not a val
        case rsgn@N.Reassign(name, newValue) => {
          locals.get(name) match {
            case Some((sym, variable)) => {
              if (variable) S.Reassign(sym, transformExpr(newValue)).setPos(rsgn)
              else fatal(s"Trying to reassign val $name", rsgn)
            }
            case None => fatal(s"Variable $name not found", rsgn)
          }
      }

          // Update :: Name analysis for reassinment operators analogous to variable reassignment
        case peq@N.PlusEquals(name, value) => {
          locals.get(name) match {
            case Some((sym, variable)) => {
              if (variable) S.PlusEquals(sym, transformExpr(value)).setPos(peq)
              else fatal(s"Trying to reassign val $name", peq)
            }
            case None => fatal(s"Variable $name not found", peq)
          }
        }

        case meq@N.MinusEquals(name, value) => {
          locals.get(name) match {
            case Some((sym, variable)) => {
              if (variable) S.MinusEquals(sym, transformExpr(value)).setPos(meq)
              else fatal(s"Trying to reassign val $name", meq)
            }
            case None => fatal(s"Variable $name not found", meq)
          }
        }

        case teq@N.TimesEquals(name, value) => {
          locals.get(name) match {
            case Some((sym, variable)) => {
              if (variable) S.TimesEquals(sym, transformExpr(value)).setPos(teq)
              else fatal(s"Trying to reassign val $name", teq)
            }
            case None => fatal(s"Variable $name not found", teq)
          }
        }

        case deq@N.DivEquals(name, value) => {
          locals.get(name) match {
            case Some((sym, variable)) => {
              if (variable) S.DivEquals(sym, transformExpr(value)).setPos(deq)
              else fatal(s"Trying to reassign val $name", deq)
            }
            case None => fatal(s"Variable $name not found", deq)
          }
        }

        case modeq@N.ModEquals(name, value) => {
          locals.get(name) match {
            case Some((sym, variable)) => {
              if (variable) S.ModEquals(sym, transformExpr(value)).setPos(modeq)
              else fatal(s"Trying to reassign val $name", modeq)
            }
            case None => fatal(s"Variable $name not found", modeq)
          }
        }

        case ceq@N.ConcatEquals(name, value) => {
          locals.get(name) match {
            case Some((sym, variable)) => {
              if (variable) S.ConcatEquals(sym, transformExpr(value)).setPos(ceq)
              else fatal(s"Trying to reassign val $name", ceq)
            }
            case None => fatal(s"Variable $name not found", ceq)
          }
        }

        // Error
        case err@N.Error(msg) => S.Error(transformExpr(msg)).setPos(err)

      }
      res.setPos(expr)
    }

    // Putting it all together to construct the final program for step 6.
    val newProgram = S.Program(
      p.modules map { case mod@N.ModuleDef(name, defs, optExpr) =>
        S.ModuleDef(
          table.getModule(name).get,
          defs map (transformDef(_, name)),
          optExpr map (transformExpr(_)(name, (Map(), Map())))
        ).setPos(mod)
      }
    ).setPos(p)

    (newProgram, table)

  }
}
