package edu.nus.comp.pli.simpl.util

package object SimplEvmInstruction {
  sealed trait SimInstruction

  case class LDCI(n: Int) extends SimInstruction
  case class LDCB(b: Boolean) extends SimInstruction
  case object NEG extends SimInstruction
  case object PLUS extends SimInstruction
  case object MINUS extends SimInstruction
  case object TIMES extends SimInstruction
  case object DIV extends SimInstruction
  case object AND extends SimInstruction
  case object OR extends SimInstruction
  case object NOT extends SimInstruction
  case object LT extends SimInstruction
  case object LE extends SimInstruction
  case object GT extends SimInstruction
  case object GE extends SimInstruction
  case object EQ extends SimInstruction
  case object NEQ extends SimInstruction
  case object DONE extends SimInstruction

  case class GOTO(label: String) extends SimInstruction
  case class LABEL(label: String) extends SimInstruction

  case class JOF(label: String) extends SimInstruction
  case class LD(sys:(String,Int)) extends SimInstruction
  case class CALL(n: Int) extends SimInstruction
  case class TAILCALL(n: Int) extends SimInstruction



  case object RTN extends SimInstruction

  case class LDF(sysL:Seq[(String,Int)],numA:Int,label: String) extends SimInstruction
  case class LDFR(sysL:Seq[(String,Int)],sys:(String,Int), numA:Int,label: String) extends SimInstruction


}
