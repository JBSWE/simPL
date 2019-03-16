package edu.nus.comp.pli.simpl.util

import edu.nus.comp.pli.simpl.parser.SimplAST._



object TypeInfer {

  def get_type(env: Seq[(Expression, Type)], exp: Expression): Option[Type] =
    env match {
      case Seq() => None
      case head :: rest =>
        if (head._1 == exp)
          Some(head._2)
        else
          get_type(rest, exp)
    }

  def extr_arg_type[A](te: Type, formalArgs: Seq[A]): Option[(Seq[(A, Type)], Type)] = {
    def helper(env: Seq[(A, Type)], t: Type, args: Seq[A]): Option[(Seq[(A, Type)], Type)] = {
      (args, t) match {
        case (Seq(), _) => Some(env, t)
        case (head :: rest, Arrow(t1, t2)) => helper(env :+ (head, t1), t2, rest)
        case (_, _) => None
      }
    }

    helper(Seq(), te, formalArgs)
  }

  def testTwoTypes(type_1: Type, type_2: Type):Boolean = (type_1, type_2) match {
    case (Arrow(type_a, type_b), Arrow(type_c, type_d) )=> testTwoTypes (type_b, type_d) && testTwoTypes (type_a ,type_c)
    case (type_a, type_b) => type_1 == type_2
  }

  def type_infer(env: Seq[(Expression, Type)], expr: Expression):Option[Type]= {

      expr match {
        case Num(_) => Some(TypeInt)
        case Bool(_) => Some(TypeInt)
        case Var(name) => get_type (env, Var(name))

        case Bin(op, leftExpr, rightExpr) =>
          op match {
            case Add | Sub | Mul | Div =>
              val at1 = type_infer(env, leftExpr)
              val at2 = type_infer(env, rightExpr)
              (at1, at2) match {
                case (Some(TypeInt), Some(TypeInt)) => Some(TypeInt)
                case _ => None
              }
            case Lt| Gt | Eq|NEq| Gte|Lte =>
              val at1 = type_infer(env, leftExpr)
              val at2 = type_infer(env, rightExpr)
              (at1, at2) match {
                case (Some(TypeInt), Some(TypeInt)) => Some(TypeBool)
                case _ => None
              }
            case And | Or =>
              val at1 = type_infer(env, leftExpr)
              val at2 = type_infer(env, rightExpr)
              (at1, at2) match {
                case (Some(TypeBool), Some(TypeBool)) => Some(TypeBool)
                case _ => None
              }
            case _ => None
          }

        case Ury(op, expression) =>
          op match {
            case Minus =>
              val at2 = type_infer(env, expression)
              at2 match {
                case Some(TypeInt) => at2
                case _ => None
              }
            case Not =>
              val at2 = type_infer(env, expression)
              at2 match {
                case Some(TypeBool) => at2
                case _ => None
              }
            case _ => None
          }

        case Cond(condition, consequent, alternative) =>
          /*
           * condition must be bool type
           * consequent,alternative must be of the same inferred type
           */
          // add you code here
          None


        case Func(te, formalArgs, body) =>
          /*
          (* te is the inferred function type *)
          (* infer the types of args and body *)
          (* formalArgs and body type must be consistent with te *)
          (* extend the env when checking type of body *)
           */
          // add you code here
          None

        case RecFunc(te, name, formalArgs, body) =>
          /*
          (* te is the inferred function type *)
          (* infer the types of args and body *)
          (* formalArgs and body type must be consistent with te*)
          (* extend the env when checking type of body *)
           */
          // add you code here
          None

        case Appln(func, actualArgs) =>
          /*
          (* infer the type of e1 first *)
          (* infer the types of args *)
          (* check that args are consistent with inferred type *)
          (* remember to update _ with inferred type of e1 *)
           */
          // add you code here
          None


        case Let(ldecl,te,body) =>
          // the implementation for Let is given *)
          // pick the type of local vars from ldecl *)
          val env2 =ldecl.map((ele:(Type,Var,Expression)) => (ele._2,ele._1))
          //build an extended type environment for checking body
          val new_env = env2 ++ env
          //infer the type of body
          val nt1 = type_infer(new_env, body)
          //infer the type of local definitions
          val ls_res = ldecl.map((ele:(Type,Var,Expression)) => (type_infer( env, ele._3),ele._2,ele._1) )
          //why did we use env rather than nenv when checking ldecl?
          nt1 match {
            case Some (t1) =>
              if (t1 == te)
                if (ls_res.forall((ele:(Option[Type],Var, Type)) => ele._1.contains(ele._3))) nt1
                else None
              else None
            case _ => None

          }
      }
    }
  }
