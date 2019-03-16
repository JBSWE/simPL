package edu.nus.comp.pli.simpl.util

import edu.nus.comp.pli.simpl.parser.SimplAST._
import edu.nus.comp.pli.simpl.parser.SimplParser._
import edu.nus.comp.pli.simpl.util.TypeInfer._
import edu.nus.comp.pli.simpl.util.SimplEplDenotationalSemantics._
import edu.nus.comp.pli.simpl.util.SimplEvmInstruction._
import org.scalatest.FlatSpec

import scala.io.Source

class CompileTest extends FlatSpec{

  "test01.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test01.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==  List(LDF(Vector(),1,"labal_1"), LDCI(3), LDF(Vector(),2,"labal_0"), CALL(2), DONE, LABEL("labal_0"), LDCI(4), LD(("AboutPi",0)), TIMES, LDCI(6731), LD(("Square",1)), CALL(1), TIMES, RTN, LABEL("labal_1"), LD(("x",0)), LD(("x",0)), TIMES, RTN)
      }
    )
  }

  "test02.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test02.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDF(Vector(),1,"labal_1"), LDCI(3), LDF(Vector(),2,"labal_0"), CALL(2), DONE, LABEL("labal_0"), LDCI(4), LD(("AboutPi",0)), TIMES, LDCI(6371), LD(("Square",1)), CALL(1), TIMES, RTN, LABEL("labal_1"), LD(("x",0)), LD(("x",0)), TIMES, RTN)

      }
    )
  }
  "test03.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test03.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDFR(Vector(),("power",0),2,"labal_0"), DONE, LABEL("labal_0"), LDCI(1), RTN)

      }
    )
  }
  "test04.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test04.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDFR(Vector(),("recurse",0),4,"labal_0"), DONE, LABEL("labal_0"), LD(("y",1)), LDCI(0), EQ, JOF("labal_1"), LD(("iv",3)), GOTO("labal_2"), LABEL("labal_1"), LD(("iv",3)), LD(("op",4)), LD(("y",1)), LDCI(1), MINUS, LD(("x",0)), LD(("recurse",2)), CALL(4), LD(("x",0)), LD(("op",4)), CALL(2), LABEL("labal_2"), RTN)

      }
    )
  }
  "test05.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test05.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          true
        case Some (t) => false
      }
    )
  }
  "test06.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test06.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDCI(9), LDCI(8), LDCI(7), LDF(Vector(),2,"labal_0"), CALL(3), DONE, LABEL("labal_0"), LDF(Vector(("y",1), ("x",0)),1,"labal_1"), RTN, LABEL("labal_1"), LD(("y",0)), LD(("z",2)), PLUS, LD(("x",1)), MINUS, RTN)

      }
    )
  }
  "test07.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test07.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDF(Vector(),2,"labal_3"), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDF(Vector(),2,"labal_2"), LDF(Vector(("f",0)),1,"labal_1"), CALL(1), RTN, LABEL("labal_1"), LDCI(4), LDCI(3), LD(("f",0)), CALL(1), LD(("apply",1)), CALL(2), RTN, LABEL("labal_2"), LD(("x",1)), LD(("g",0)), CALL(1), RTN, LABEL("labal_3"), LD(("x",0)), LD(("y",1)), PLUS, RTN)

      }
    )
  }
  "test08.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test08.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDFR(Vector(),("mypower",0),2,"labal_1"), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDCI(3), LDCI(17), LD(("power",0)), CALL(2), RTN, LABEL("labal_1"), LD(("y",2)), LDCI(0), EQ, JOF("labal_2"), LDCI(1), GOTO("labal_3"), LABEL("labal_2"), LD(("x",1)), LD(("y",2)), LDCI(1), MINUS, LD(("x",1)), LD(("mypower",0)), CALL(2), TIMES, LABEL("labal_3"), RTN)

      }
    )
  }
  "test09.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test09.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDFR(Vector(),("recurse",0),4,"labal_2"), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDCI(1), LDF(Vector(),2,"labal_1"), LDCI(3), LDCI(2), LD(("recurse",0)), CALL(4), RTN, LABEL("labal_1"), LD(("x",0)), LD(("z",1)), TIMES, RTN, LABEL("labal_2"), LD(("y",2)), LDCI(0), EQ, JOF("labal_3"), LD(("initvalue",1)), GOTO("labal_4"), LABEL("labal_3"), LD(("initvalue",1)), LD(("operation",4)), LD(("y",2)), LDCI(1), MINUS, LD(("x",0)), LD(("recurse",3)), CALL(4), LD(("x",0)), LD(("operation",4)), CALL(2), LABEL("labal_4"), RTN)

      }
    )
  }
  "test10.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test10.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDFR(Vector(),("recurse",0),4,"labal_2"), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDCI(0), LDF(Vector(),2,"labal_1"), LDCI(3), LDCI(2), LD(("recurse",0)), CALL(4), RTN, LABEL("labal_1"), LD(("x",0)), LD(("z",1)), PLUS, RTN, LABEL("labal_2"), LD(("y",2)), LDCI(0), EQ, JOF("labal_3"), LD(("initvalue",1)), GOTO("labal_4"), LABEL("labal_3"), LD(("initvalue",1)), LD(("operation",4)), LD(("y",2)), LDCI(1), MINUS, LD(("x",0)), LD(("recurse",3)), CALL(4), LD(("x",0)), LD(("operation",4)), CALL(2), LABEL("labal_4"), RTN)

      }
    )
  }
  "test11.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test11.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDFR(Vector(),("recurse",0),4,"labal_2"), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDCI(128), LDF(Vector(),2,"labal_1"), LDCI(3), LDCI(2), LD(("recurse",0)), CALL(4), RTN, LABEL("labal_1"), LD(("z",1)), LD(("x",0)), DIV, RTN, LABEL("labal_2"), LD(("y",2)), LDCI(0), EQ, JOF("labal_3"), LD(("initvalue",1)), GOTO("labal_4"), LABEL("labal_3"), LD(("initvalue",1)), LD(("operation",4)), LD(("y",2)), LDCI(1), MINUS, LD(("x",0)), LD(("recurse",3)), CALL(4), LD(("x",0)), LD(("operation",4)), CALL(2), LABEL("labal_4"), RTN)

      }
    )
  }
  "test12.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test12.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDF(Vector(),1,"labal_2"), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDCI(3), LDF(Vector(("square",0)),1,"labal_1"), CALL(1), RTN, LABEL("labal_1"), LDCI(4), LD(("x",1)), LD(("square",0)), CALL(1), TIMES, RTN, LABEL("labal_2"), LD(("x",0)), LD(("x",0)), TIMES, RTN)

      }
    )
  }
  "test13.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test13.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDCI(5), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDF(Vector(("s",0)),1,"labal_3"), LDF(Vector(),1,"labal_1"), CALL(1), RTN, LABEL("labal_1"), LDCI(10), LDF(Vector(("f",0)),1,"labal_2"), CALL(1), RTN, LABEL("labal_2"), LDCI(3), LD(("f",0)), CALL(1), RTN, LABEL("labal_3"), LD(("x",1)), LD(("s",0)), PLUS, RTN)

      }
    )
  }
  "test14.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test14.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDFR(Vector(),("fac",0),1,"labal_0"), DONE, LABEL("labal_0"), LD(("n",1)), LDCI(2), LT, JOF("labal_1"), LDCI(1), GOTO("labal_2"), LABEL("labal_1"), LD(("n",1)), LD(("n",1)), LDCI(1), MINUS, LD(("fac",0)), CALL(1), TIMES, LABEL("labal_2"), RTN)

      }
    )
  }
  "test15.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test15.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          true
        case Some (t) => false
      }
    )
  }
  "test16.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test16.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDCI(3), LDFR(Vector(),("f",0),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LD(("n",1)), LD(("f",0)), CALL(1), RTN)

      }
    )
  }
  "test17.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test17.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDCI(3), LDF(Vector(),1,"labal_1"), LDF(Vector(),2,"labal_0"), CALL(2), DONE, LABEL("labal_0"), LD(("y",1)), LD(("x",0)), CALL(1), RTN, LABEL("labal_1"), LD(("x",0)), LDCI(1), PLUS, RTN)

      }
    )
  }
  "test18.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test18.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDF(Vector(),2,"labal_1"), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDCI(3), RTN, LABEL("labal_1"), LDCI(1), RTN)

      }
    )
  }
  "test19.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test19.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDFR(Vector(),("facloop",0),2,"labal_3"), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDF(Vector(("facloop",0)),1,"labal_2"), LDF(Vector(),1,"labal_1"), CALL(1), RTN, LABEL("labal_1"), LDCI(4), LD(("fac",0)), CALL(1), RTN, LABEL("labal_2"), LDCI(1), LD(("n",1)), LD(("facloop",0)), CALL(2), RTN, LABEL("labal_3"), LD(("n",1)), LDCI(1), EQ, JOF("labal_4"), LD(("acc",2)), GOTO("labal_5"), LABEL("labal_4"), LD(("acc",2)), LD(("n",1)), TIMES, LD(("n",1)), LDCI(1), MINUS, LD(("facloop",0)), CALL(2), LABEL("labal_5"), RTN)

      }
    )
  }
  "test20.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test20.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDF(Vector(),1,"labal_1"), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDCI(3), LD(("fac",0)), CALL(1), RTN, LABEL("labal_1"), LD(("n",0)), LDCI(1), PLUS, RTN)

      }
    )
  }
  "test21.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test21.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDFR(Vector(),("foo",0),1,"labal_1"), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDCI(4), LD(("f",0)), CALL(1), RTN, LABEL("labal_1"), LD(("n",1)), LDCI(0), EQ, JOF("labal_2"), LDCI(0), GOTO("labal_3"), LABEL("labal_2"), LDCI(2), LD(("n",1)), LDCI(1), MINUS, LD(("foo",0)), CALL(1), PLUS, LABEL("labal_3"), RTN)

      }
    )
  }
  "test22.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test22.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDFR(Vector(),("foo",0),1,"labal_1"), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDCI(4), LD(("f",0)), CALL(1), RTN, LABEL("labal_1"), LD(("n",1)), LDCI(0), EQ, JOF("labal_2"), LDCI(0), GOTO("labal_3"), LABEL("labal_2"), LD(("n",1)), LDCI(1), MINUS, LD(("foo",0)), CALL(1), LD(("n",1)), PLUS, LABEL("labal_3"), RTN)

      }
    )
  }
  "test23.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test23.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDFR(Vector(),("foo",0),1,"labal_1"), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDCI(1), LDCI(30), LD(("f",0)), CALL(2), RTN, LABEL("labal_1"), LD(("n",1)), LDCI(0), EQ, JOF("labal_2"), LDF(Vector(),1,"labal_4"), GOTO("labal_3"), LABEL("labal_2"), LDF(Vector(("foo",0), ("n",1)),1,"labal_5"), LABEL("labal_3"), RTN, LABEL("labal_4"), LD(("acc",0)), RTN, LABEL("labal_5"), LD(("acc",2)), LD(("n",1)), TIMES, LD(("n",1)), LDCI(1), MINUS, LD(("foo",0)), CALL(2), RTN)

      }
    )
  }
  "test24.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test24.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDCI(3), LDF(Vector(),1,"labal_1"), CALL(1), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDCI(4), LD(("f",0)), CALL(1), RTN, LABEL("labal_1"), LDF(Vector(("x",0)),1,"labal_2"), RTN, LABEL("labal_2"), LD(("x",0)), LD(("y",1)), PLUS, RTN)

      }
    )
  }
  "test25.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test25.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDCI(3), LDF(Vector(),1,"labal_1"), CALL(1), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDCI(4), LD(("f",0)), CALL(1), RTN, LABEL("labal_1"), LDF(Vector(("x",0)),1,"labal_2"), RTN, LABEL("labal_2"), LD(("x",0)), LD(("y",1)), PLUS, RTN)

      }
    )
  }
  "test26.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test26.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDCI(3), LDF(Vector(),1,"labal_1"), CALL(1), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDCI(4), LD(("f",0)), CALL(1), RTN, LABEL("labal_1"), LDF(Vector(("x",0)),1,"labal_2"), RTN, LABEL("labal_2"), LD(("x",0)), LD(("y",1)), PLUS, RTN)

      }
    )
  }
  "test27.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test27.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDF(Vector(),1,"labal_1"), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDCI(4), LD(("f",0)), CALL(1), RTN, LABEL("labal_1"), LDF(Vector(("x",0)),1,"labal_2"), RTN, LABEL("labal_2"), LD(("x",0)), LD(("y",1)), PLUS, RTN)

      }
    )
  }
  "test28.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test28.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) == List(LDF(Vector(),1,"labal_2"), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDF(Vector(("f",0)),1,"labal_1"), RTN, LABEL("labal_1"), LD(("pa",1)), LDCI(4), LD(("f",0)), CALL(2), RTN, LABEL("labal_2"), LDF(Vector(("x",0)),1,"labal_3"), RTN, LABEL("labal_3"), LD(("x",0)), LD(("y",1)), PLUS, RTN)

      }
    )
  }
  "test29.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test29.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) == List(LDF(Vector(),1,"labal_3"), LDF(Vector(),1,"labal_0"), CALL(1), DONE, LABEL("labal_0"), LDCI(3), LDF(Vector(("f",0)),1,"labal_2"), CALL(1), LDF(Vector(),1,"labal_1"), CALL(1), RTN, LABEL("labal_1"), LDCI(4), LD(("g",0)), CALL(1), RTN, LABEL("labal_2"), LDCI(3), LD(("f",0)), CALL(1), RTN, LABEL("labal_3"), LDF(Vector(("x",0)),1,"labal_4"), RTN, LABEL("labal_4"), LD(("x",0)), LD(("y",1)), PLUS, RTN)

      }
    )
  }
  "test30.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test30.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)) match{
        case None =>
          print ("type inference error")
          false
        case Some (t) => compile(parse(source)) ==List(LDCI(2), LDCI(2), TIMES, LDCI(4), PLUS, DONE)
      }
    )
  }
}
