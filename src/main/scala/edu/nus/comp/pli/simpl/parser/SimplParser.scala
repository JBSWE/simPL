package edu.nus.comp.pli.simpl.parser

import org.antlr.v4.runtime.{ANTLRInputStream, CommonTokenStream, ParserRuleContext}
import edu.nus.comp.pli.epl.parser.SimplParserParser.{ ExprContext => ASTNode}
import edu.nus.comp.pli.epl.parser.SimplParserParser.{TypeContext => TYPENode}

import SimplAST._
import org.antlr.v4.runtime.tree.ParseTree
import edu.nus.comp.pli.epl.parser.{SimplParserLexer, SimplParserParser}
import org.antlr.v4.runtime.tree.TerminalNode

import collection.JavaConverters._

object SimplParser {

  def parse(input: String): Expression = {
    val charStream = new ANTLRInputStream(input)
    val lexer = new SimplParserLexer(charStream)
    val tokens = new CommonTokenStream(lexer)
    val parser = new SimplParserParser(tokens)
    exprTreeWalk(parser.expr)
  }

  private implicit class ASTNodePimp(val node: ParserRuleContext) extends AnyVal {
    /*



*/
    def idNode (index:Int):String  = node.getChild(index).getText

    def exprNode (index:Int):ASTNode  = node.getChild(index).asInstanceOf[ASTNode]
    def typeNode (index:Int):TYPENode = node.getChild(index).asInstanceOf[TYPENode]
    def argList(index:Int):Seq[ASTNode] ={
      val child = node.getChild(index)
      val argnum = child.getChildCount
      def helper (num :Int, ret:Seq[ASTNode]) : Seq[ASTNode] = num match{
        case -1 =>
          ret
        case _ =>
          helper (num-1, child.getChild(num).asInstanceOf[ASTNode] +: ret)
      }
      helper (argnum-1, Seq())
    }

    def idList(index:Int):Seq[String] ={
      val child = node.getChild(index)
      val argnum = child.getChildCount
      def helper (num :Int, ret:Seq[String]) : Seq[String] = num match{
        case -1 =>
          ret
        case _ =>
          helper (num-1, child.getChild(num).getText +: ret)
      }
      helper (argnum-1, Seq())
    }


    def letDefList:Seq[(Type, Var, Expression)] ={
      val child = node.getChild(1)
      val argnum = child.getChildCount
      def helper (num :Int, ret:Seq[(Type, Var, Expression)]) : Seq[(Type, Var, Expression)] = num match{
        case -1 =>
          ret
        case _ =>
          helper (num-1, letDefs(child.getChild(num)) +: ret)
      }
      helper (argnum-1, Seq())
    }




    def letDefs(context: ParseTree): (Type, Var, Expression) =
      (
      typeTreeWalk(context.getChild(1).asInstanceOf[TYPENode]),
      Var(context.getChild(3).getText),
      exprTreeWalk(context.getChild(5).asInstanceOf[ASTNode])
    )

  }

  private def typeTreeWalk(context: TYPENode): Type = {
    import NodeType._
    classifyType(context) match {
      case IntType => TypeInt
      case BoolType => TypeBool
      case ParenType => typeTreeWalk (context.getChild(1).asInstanceOf[TYPENode])
      case ArrowType => Arrow(typeTreeWalk (context.getChild(0).asInstanceOf[TYPENode]), typeTreeWalk (context.getChild(2).asInstanceOf[TYPENode]))
    }
  }



  private def exprToVar (name:String) :Var = Var (name.toString)


  private def exprTreeWalk(context: ParserRuleContext): Expression = {
    import NodeType._
    val textMatch = context.getChild(if (context.getChildCount == 3) 1 else 0).getText
    (classify(context.asInstanceOf[ASTNode]), textMatch) match {
      case (LetExpr, _) => Let(context.letDefList,typeTreeWalk(context.typeNode(4)),exprTreeWalk(context.exprNode(6)))/*Let(
        context.getChild(1).getChild(0).asInstanceOf[ParserRuleContext].children.asScala.map(c => letDefsTreeWalk(c.asInstanceOf[ParserRuleContext])),
        typeTreeWalk(context.getChild(4).asInstanceOf[ParserRuleContext]),
        exprTreeWalk(context.getChild(6).asInstanceOf[ParserRuleContext])
      )*/
      case (CondExpr, _) => Cond(exprTreeWalk(context.exprNode(1)), exprTreeWalk(context.exprNode(3)), exprTreeWalk(context.exprNode(5)))
      case (FuncExpr, _) => Func(typeTreeWalk(context.typeNode(2)), context.idList(4).map(exprToVar), exprTreeWalk(context.exprNode(6)))
      case (RecFuncExpr, _) => RecFunc(typeTreeWalk(context.typeNode(3)),Var (context.idNode(1).toString), context.idList(5).map(exprToVar), exprTreeWalk(context.exprNode(7)))

      case (ApplnExpr, _) => Appln(exprTreeWalk(context.exprNode(1)), context.argList(2).map(exprTreeWalk))

        /*
  case class Cond(condition: Expression, consequent: Expression, alternative: Expression) extends Expression

  case class Func(typ: Type, formalArgs: Seq[Var], body: Expression) extends Expression

  case class RecFunc(typ: Type, name: Var, formalArgs: Seq[Var], body: Expression) extends Expression

  case class Appln(func: Expression, actualArgs: Seq[Expression]) extends Expression

  case class Let(defs: Seq[(Type, Var, Expression)], typ: Type, body: Expression) extends Expression
         */

      case (IntConst, text) => Num(text.toInt)
      case (BoolConst, text) => Bool(text.toBoolean)
      case (VarExpr, text) => Var(text.toString)
      case (ParenExpr, _) => exprTreeWalk(context.exprNode(1))
      case (UnaryExpr, Minus.opString) => Ury(Minus, exprTreeWalk(context.exprNode(1)))
      case (UnaryExpr, Not.opString) => Ury(Not, exprTreeWalk(context.exprNode(1)))
      case (BinaryExpr, Add.opString) => Bin(Add, exprTreeWalk(context.exprNode(0)), exprTreeWalk(context.exprNode(2)))
      case (BinaryExpr, Sub.opString) => Bin(Sub, exprTreeWalk(context.exprNode(0)), exprTreeWalk(context.exprNode(2)))
      case (BinaryExpr, Mul.opString) => Bin(Mul, exprTreeWalk(context.exprNode(0)), exprTreeWalk(context.exprNode(2)))
      case (BinaryExpr, Div.opString) => Bin(Div, exprTreeWalk(context.exprNode(0)), exprTreeWalk(context.exprNode(2)))
      case (BinaryExpr, Lt.opString) => Bin(Lt, exprTreeWalk(context.exprNode(0)), exprTreeWalk(context.exprNode(2)))
      case (BinaryExpr, Lte.opString) => Bin(Lte, exprTreeWalk(context.exprNode(0)), exprTreeWalk(context.exprNode(2)))
      case (BinaryExpr, Gt.opString) => Bin(Gt, exprTreeWalk(context.exprNode(0)), exprTreeWalk(context.exprNode(2)))
      case (BinaryExpr, Gte.opString) => Bin(Gte, exprTreeWalk(context.exprNode(0)), exprTreeWalk(context.exprNode(2)))
      case (BinaryExpr, Eq.opString) => Bin(Eq, exprTreeWalk(context.exprNode(0)), exprTreeWalk(context.exprNode(2)))
      case (BinaryExpr, NEq.opString) => Bin(NEq, exprTreeWalk(context.exprNode(0)), exprTreeWalk(context.exprNode(2)))
      case (BinaryExpr, And.opString) => Bin(And, exprTreeWalk(context.exprNode(0)), exprTreeWalk(context.exprNode(2)))
      case (BinaryExpr, Or.opString) => Bin(Or, exprTreeWalk(context.exprNode(0)), exprTreeWalk(context.exprNode(2)))
    }
  }

  object NodeType extends Enumeration {
    type NodeType = Value

    val BoolConst, IntConst, VarExpr, UnaryExpr, BinaryExpr, ParenExpr,
          CondExpr, FuncExpr, RecFuncExpr, ApplnExpr, LetExpr, IntType, BoolType, ArrowType, ParenType = Value


    def classify(node: ASTNode): NodeType =
      (
        Option(node.getChild(0).getText),
        Option(node.INT),
        Option(node.BOOL),
        Option(node.ID),
        node.getChildCount,
        node.getChild(0).isInstanceOf[TerminalNode]
      ) match {
        case (Some("let"), _, _, _, _, _) => LetExpr
        case (Some("if"), _, _, _, _, _) => CondExpr
        case (Some("fun"), _, _, _, _, _) => FuncExpr
        case (Some("recfun"), _, _, _, _ , _) => RecFuncExpr
        case (_, _, None, None, 4, _) => ApplnExpr

        case (_, _, None, None, 1, _) => IntConst
        case (_, None, _, None, 1, _) => BoolConst
        case (_, None, None, _, 1, _) => VarExpr
        case (_, None, None, None, 2, _) => UnaryExpr
        case (_, None, None, None, 3, false) => BinaryExpr
        case (_, None, None, None, 3, true) => ParenExpr

      }
    def classifyType(node: TYPENode): NodeType =
      (
        Option(node.getChild(0).getText), node.getChildCount,
      ) match {
        case (Some("int"),1) => IntType
        case (Some("bool"),1) => BoolType
        case (Some("("),3) => ParenType
        case (_, 3) => ArrowType

      }
  }


}
