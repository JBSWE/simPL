package edu.nus.comp.pli.simpl.parser

package object SimplAST {

  sealed class Operator(val opString: String)

  sealed class UnaryOperator(op: String) extends Operator(op)

  case object Not extends UnaryOperator("!")

  case object Minus extends UnaryOperator("~")

  sealed class BinaryOperator(op: String) extends Operator(op)

  case object And extends BinaryOperator("&")

  case object Or extends BinaryOperator("|")

  case object Mul extends BinaryOperator("*")

  case object Div extends BinaryOperator("/")

  case object Add extends BinaryOperator("+")

  case object Sub extends BinaryOperator("-")

  case object Lt extends BinaryOperator("<")

  case object Lte extends BinaryOperator("<=")

  case object Gt extends BinaryOperator(">")

  case object Gte extends BinaryOperator(">=")

  case object Eq extends BinaryOperator("=")

  case object NEq extends BinaryOperator("!=")

  val binaryOperators = List(Mul, Div, Add, Sub, Lt, Lte, Gt, Gte, Eq, NEq)
  val unaryOperators = List(Not, Minus)

  sealed trait  Type { def name: String }

  case object TypeInt extends Type { def name: String = "int"}

  case object TypeBool extends Type { def name: String = "bool" }

  trait ArrowMatcher extends Type {def name: String = "->" }

  case object Arrow extends ArrowMatcher

  case class Arrow(from: Type, to: Type) extends Type with ArrowMatcher

  sealed trait Expression

  case class Bin(op: BinaryOperator, lhs: Expression, rhs: Expression) extends Expression // Binary operator

  case class Ury(op: UnaryOperator, operand: Expression) extends Expression // Unary operator

  case class Num(value: Int) extends Expression

  case class Bool(value: Boolean) extends Expression

  case class Var(name: String) extends Expression

  case class Cond(condition: Expression, consequent: Expression, alternative: Expression) extends Expression

  case class Func(typ: Type, formalArgs: Seq[Var], body: Expression) extends Expression

  case class RecFunc(typ: Type, name: Var, formalArgs: Seq[Var], body: Expression) extends Expression

  case class Appln(func: Expression, actualArgs: Seq[Expression]) extends Expression

  case class Let(defs: Seq[(Type, Var, Expression)], typ: Type, body: Expression) extends Expression

}
