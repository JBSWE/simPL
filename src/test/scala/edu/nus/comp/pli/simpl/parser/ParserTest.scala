package edu.nus.comp.pli.simpl.parser

import org.scalatest.FlatSpec
import SimplParser._
import SimplAST._

import scala.io.Source

class ParserTest extends FlatSpec {

  "test01.simpl" should "parse correctly" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test01.simpl")).mkString
    assert(
      parse(source)== Let(List((TypeInt,Var("AboutPi"),Num(3)), (Arrow(TypeInt,TypeInt),Var("Square"),Func(Arrow(TypeInt,TypeInt),List(Var("x")),Bin(Mul,Var("x"),Var("x"))))),TypeInt,Bin(Mul,Bin(Mul,Num(4),Var("AboutPi")),Appln(Var("Square"),List(Num(6731)))))
    )
  }

  "test02.simpl" should "parse correctly" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test02.simpl")).mkString
    assert(
      parse(source)== Appln(Func(Arrow(TypeInt,Arrow(Arrow(TypeInt,TypeInt),TypeInt)),List(Var("AboutPi"), Var("Square")),Bin(Mul,Bin(Mul,Num(4),Var("AboutPi")),Appln(Var("Square"),List(Num(6371))))),List(Num(3), Func(Arrow(TypeInt,TypeInt),List(Var("x")),Bin(Mul,Var("x"),Var("x")))))

    )
  }

  "test03.simpl" should "parse correctly" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test03.simpl")).mkString
    assert(
      parse(source)== RecFunc(Arrow(TypeInt,Arrow(TypeInt,TypeInt)),Var("power"),List(Var("x"), Var("y")),Num(1))

    )
  }

  "test04.simpl" should "parse correctly" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test04.simpl")).mkString
    assert(
      parse(source)== RecFunc(Arrow(TypeInt,Arrow(TypeInt,Arrow(Arrow(TypeInt,Arrow(TypeInt,TypeInt)),Arrow(TypeInt,TypeInt)))),Var("recurse"),List(Var("x"), Var("y"), Var("op"), Var("iv")),Cond(Bin(Eq,Var("y"),Num(0)),Var("iv"),Appln(Var("op"),List(Var("x"), Appln(Var("recurse"),List(Var("x"), Bin(Sub,Var("y"),Num(1)), Var("op"), Var("iv")))))))

    )
  }
  "test05.simpl" should "parse correctly" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test05.simpl")).mkString
    assert(
      parse(source)== RecFunc(Arrow(TypeInt,Arrow(TypeInt,Arrow(Arrow(TypeInt,TypeInt),Arrow(TypeInt,TypeInt)))),Var("recurse"),List(Var("x"), Var("y"), Var("op"), Var("iv")),Cond(Bin(Eq,Var("y"),Num(0)),Var("iv"),Appln(Var("op"),List(Var("x"), Appln(Var("recurse"),List(Var("x"), Bin(Sub,Var("y"),Num(1)), Var("op"), Var("iv")))))))

    )
  }

  "test06.simpl" should "parse correctly" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test06.simpl")).mkString
    assert(
      parse(source)== Appln(Func(Arrow(TypeInt,Arrow(TypeInt,Arrow(TypeInt,TypeInt))),List(Var("x"), Var("y")),Func(Arrow(TypeInt,TypeInt),List(Var("z")),Bin(Sub,Bin(Add,Var("y"),Var("z")),Var("x")))),List(Num(7), Num(8), Num(9)))

    )
  }

  "test07.simpl" should "parse correctly" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test07.simpl")).mkString
    assert(
      parse(source)== Let(List((Arrow(TypeInt,Arrow(TypeInt,TypeInt)),Var("f"),Func(Arrow(TypeInt,Arrow(TypeInt,TypeInt)),List(Var("x"), Var("y")),Bin(Add,Var("x"),Var("y"))))),TypeInt,Let(List((Arrow(Arrow(TypeInt,TypeInt),Arrow(TypeInt,TypeInt)),Var("apply"),Func(Arrow(Arrow(TypeInt,TypeInt),Arrow(TypeInt,TypeInt)),List(Var("g"), Var("x")),Appln(Var("g"),List(Var("x")))))),TypeInt,Appln(Var("apply"),List(Appln(Var("f"),List(Num(3))), Num(4)))))

    )
  }
  "test08.simpl" should "parse correctly" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test08.simpl")).mkString
    assert(
      parse(source)== Let(List((Arrow(TypeInt,Arrow(TypeInt,TypeInt)),Var("power"),RecFunc(Arrow(TypeInt,Arrow(TypeInt,TypeInt)),Var("mypower"),List(Var("x"), Var("y")),Cond(Bin(Eq,Var("y"),Num(0)),Num(1),Bin(Mul,Var("x"),Appln(Var("mypower"),List(Var("x"), Bin(Sub,Var("y"),Num(1))))))))),TypeInt,Appln(Var("power"),List(Num(17), Num(3))))

    )
  }

  "test09.simpl" should "parse correctly" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test09.simpl")).mkString
    assert(
      parse(source)== Let(List((Arrow(TypeInt,Arrow(TypeInt,Arrow(Arrow(TypeInt,Arrow(TypeInt,TypeInt)),Arrow(TypeInt,TypeInt)))),Var("recurse"),RecFunc(Arrow(TypeInt,Arrow(TypeInt,Arrow(Arrow(TypeInt,Arrow(TypeInt,TypeInt)),Arrow(TypeInt,TypeInt)))),Var("recurse"),List(Var("x"), Var("y"), Var("operation"), Var("initvalue")),Cond(Bin(Eq,Var("y"),Num(0)),Var("initvalue"),Appln(Var("operation"),List(Var("x"), Appln(Var("recurse"),List(Var("x"), Bin(Sub,Var("y"),Num(1)), Var("operation"), Var("initvalue"))))))))),TypeInt,Appln(Var("recurse"),List(Num(2), Num(3), Func(Arrow(TypeInt,Arrow(TypeInt,TypeInt)),List(Var("x"), Var("z")),Bin(Mul,Var("x"),Var("z"))), Num(1))))

    )
  }

  "test10.simpl" should "parse correctly" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test10.simpl")).mkString
    assert(
      parse(source)== Let(List((Arrow(TypeInt,Arrow(TypeInt,Arrow(Arrow(TypeInt,Arrow(TypeInt,TypeInt)),Arrow(TypeInt,TypeInt)))),Var("recurse"),RecFunc(Arrow(TypeInt,Arrow(TypeInt,Arrow(Arrow(TypeInt,Arrow(TypeInt,TypeInt)),Arrow(TypeInt,TypeInt)))),Var("recurse"),List(Var("x"), Var("y"), Var("operation"), Var("initvalue")),Cond(Bin(Eq,Var("y"),Num(0)),Var("initvalue"),Appln(Var("operation"),List(Var("x"), Appln(Var("recurse"),List(Var("x"), Bin(Sub,Var("y"),Num(1)), Var("operation"), Var("initvalue"))))))))),TypeInt,Appln(Var("recurse"),List(Num(2), Num(3), Func(Arrow(TypeInt,Arrow(TypeInt,TypeInt)),List(Var("x"), Var("z")),Bin(Add,Var("x"),Var("z"))), Num(0))))

    )
  }
  "test11.simpl" should "parse correctly" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test11.simpl")).mkString
    assert(
      parse(source)== Let(List((Arrow(TypeInt,Arrow(TypeInt,Arrow(Arrow(TypeInt,Arrow(TypeInt,TypeInt)),Arrow(TypeInt,TypeInt)))),Var("recurse"),RecFunc(Arrow(TypeInt,Arrow(TypeInt,Arrow(Arrow(TypeInt,Arrow(TypeInt,TypeInt)),Arrow(TypeInt,TypeInt)))),Var("recurse"),List(Var("x"), Var("y"), Var("operation"), Var("initvalue")),Cond(Bin(Eq,Var("y"),Num(0)),Var("initvalue"),Appln(Var("operation"),List(Var("x"), Appln(Var("recurse"),List(Var("x"), Bin(Sub,Var("y"),Num(1)), Var("operation"), Var("initvalue"))))))))),TypeInt,Appln(Var("recurse"),List(Num(2), Num(3), Func(Arrow(TypeInt,Arrow(TypeInt,TypeInt)),List(Var("x"), Var("z")),Bin(Div,Var("z"),Var("x"))), Num(128))))

    )
  }

  "test12.simpl" should "parse correctly" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test12.simpl")).mkString
    assert(
      parse(source)== Let(List((Arrow(TypeInt,TypeInt),Var("square"),Func(Arrow(TypeInt,TypeInt),List(Var("x")),Bin(Mul,Var("x"),Var("x"))))),TypeInt,Appln(Func(Arrow(TypeInt,TypeInt),List(Var("x")),Bin(Mul,Num(4),Appln(Var("square"),List(Var("x"))))),List(Num(3))))

    )
  }

  "test13.simpl" should "parse correctly" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test13.simpl")).mkString
    assert(
      parse(source)== Let(List((TypeInt,Var("s"),Num(5))),TypeInt,Let(List((Arrow(TypeInt,TypeInt),Var("f"),Func(Arrow(TypeInt,TypeInt),List(Var("x")),Bin(Add,Var("x"),Var("s"))))),TypeInt,Let(List((TypeInt,Var("s"),Num(10))),TypeInt,Appln(Var("f"),List(Num(3))))))

    )
  }
  "test14.simpl" should "parse correctly" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test14.simpl")).mkString
    assert(
      parse(source)== RecFunc(Arrow(TypeInt,TypeInt),Var("fac"),List(Var("n")),Cond(Bin(Lt,Var("n"),Num(2)),Num(1),Bin(Mul,Var("n"),Appln(Var("fac"),List(Bin(Sub,Var("n"),Num(1)))))))

    )
  }

  "test15.simpl" should "parse correctly" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test15.simpl")).mkString
    assert(
      parse(source)== Appln(RecFunc(Arrow(TypeInt,TypeInt),Var("f"),List(Var("n")),Appln(Var("f"),List(Var("n")))),List(Num(3), Num(4)))

    )
  }

  "test16.simpl" should "parse correctly" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test16.simpl")).mkString
    assert(
      parse(source)== Appln(RecFunc(Arrow(TypeInt,TypeInt),Var("f"),List(Var("n")),Appln(Var("f"),List(Var("n")))),List(Num(3)))
    )
  }
  "test17.simpl" should "parse correctly" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test17.simpl")).mkString
    assert(
      parse(source)== Appln(Func(Arrow(Arrow(TypeInt,TypeInt),Arrow(TypeInt,TypeInt)),List(Var("x"), Var("y")),Appln(Var("x"),List(Var("y")))),List(Func(Arrow(TypeInt,TypeInt),List(Var("x")),Bin(Add,Var("x"),Num(1))), Num(3)))
    )
  }

  "test18.simpl" should "parse correctly" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test18.simpl")).mkString
    assert(
      parse(source)== Let(List((Arrow(TypeInt,Arrow(TypeInt,TypeInt)),Var("facloop"),Func(Arrow(TypeInt,Arrow(TypeInt,TypeInt)),List(Var("n"), Var("acc")),Num(1)))),TypeInt,Num(3))
    )
  }

  "test19.simpl" should "parse correctly" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test19.simpl")).mkString
    assert(
      parse(source)== Let(List((Arrow(TypeInt,Arrow(TypeInt,TypeInt)),Var("facloop"),RecFunc(Arrow(TypeInt,Arrow(TypeInt,TypeInt)),Var("facloop"),List(Var("n"), Var("acc")),Cond(Bin(Eq,Var("n"),Num(1)),Var("acc"),Appln(Var("facloop"),List(Bin(Sub,Var("n"),Num(1)), Bin(Mul,Var("acc"),Var("n")))))))),TypeInt,Let(List((Arrow(TypeInt,TypeInt),Var("fac"),Func(Arrow(TypeInt,TypeInt),List(Var("n")),Appln(Var("facloop"),List(Var("n"), Num(1)))))),TypeInt,Appln(Var("fac"),List(Num(4)))))

    )
  }
  "test20.simpl" should "parse correctly" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test20.simpl")).mkString
    assert(
      parse(source)== Let(List((Arrow(TypeInt,TypeInt),Var("fac"),Func(Arrow(TypeInt,TypeInt),List(Var("n")),Bin(Add,Var("n"),Num(1))))),TypeInt,Appln(Var("fac"),List(Num(3))))

    )
  }

  "test21.simpl" should "parse correctly" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test21.simpl")).mkString
    assert(
      parse(source)== Let(List((Arrow(TypeInt,TypeInt),Var("f"),RecFunc(Arrow(TypeInt,TypeInt),Var("foo"),List(Var("n")),Cond(Bin(Eq,Var("n"),Num(0)),Num(0),Bin(Add,Num(2),Appln(Var("foo"),List(Bin(Sub,Var("n"),Num(1))))))))),TypeInt,Appln(Var("f"),List(Num(4))))

    )
  }

  "test22.simpl" should "parse correctly" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test22.simpl")).mkString
    assert(
      parse(source)== Let(List((Arrow(TypeInt,TypeInt),Var("f"),RecFunc(Arrow(TypeInt,TypeInt),Var("foo"),List(Var("n")),Cond(Bin(Eq,Var("n"),Num(0)),Num(0),Bin(Add,Appln(Var("foo"),List(Bin(Sub,Var("n"),Num(1)))),Var("n")))))),TypeInt,Appln(Var("f"),List(Num(4))))

    )
  }
  "test23.simpl" should "parse correctly" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test23.simpl")).mkString
    assert(
      parse(source)== Let(List((Arrow(TypeInt,Arrow(TypeInt,TypeInt)),Var("f"),RecFunc(Arrow(TypeInt,Arrow(TypeInt,TypeInt)),Var("foo"),List(Var("n")),Cond(Bin(Eq,Var("n"),Num(0)),Func(Arrow(TypeInt,TypeInt),List(Var("acc")),Var("acc")),Func(Arrow(TypeInt,TypeInt),List(Var("acc")),Appln(Var("foo"),List(Bin(Sub,Var("n"),Num(1)), Bin(Mul,Var("acc"),Var("n"))))))))),TypeInt,Appln(Var("f"),List(Num(30), Num(1))))

    )
  }

  "test24.simpl" should "parse correctly" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test24.simpl")).mkString
    assert(
      parse(source)== Let(List((Arrow(TypeInt,TypeInt),Var("f"),Appln(Func(Arrow(TypeInt,Arrow(TypeInt,TypeInt)),List(Var("x")),Func(Arrow(TypeInt,TypeInt),List(Var("y")),Bin(Add,Var("x"),Var("y")))),List(Num(3))))),TypeInt,Appln(Var("f"),List(Num(4))))

    )
  }

  "test25.simpl" should "parse correctly" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test25.simpl")).mkString
    assert(
      parse(source)== Appln(Func(Arrow(Arrow(TypeInt,TypeInt),TypeInt),List(Var("f")),Appln(Var("f"),List(Num(4)))),List(Appln(Func(Arrow(TypeInt,Arrow(TypeInt,TypeInt)),List(Var("x")),Func(Arrow(TypeInt,TypeInt),List(Var("y")),Bin(Add,Var("x"),Var("y")))),List(Num(3)))))

    )
  }
  "test26.simpl" should "parse correctly" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test26.simpl")).mkString
    assert(
      parse(source)== Let(List((Arrow(TypeInt,TypeInt),Var("f"),Appln(Func(Arrow(TypeInt,Arrow(TypeInt,TypeInt)),List(Var("x")),Func(Arrow(TypeInt,TypeInt),List(Var("y")),Bin(Add,Var("x"),Var("y")))),List(Num(3))))),TypeInt,Appln(Var("f"),List(Num(4))))

    )
  }

  "test27.simpl" should "parse correctly" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test27.simpl")).mkString
    assert(
      parse(source)== Let(List((Arrow(TypeInt,Arrow(TypeInt,TypeInt)),Var("f"),Func(Arrow(TypeInt,Arrow(TypeInt,TypeInt)),List(Var("x")),Func(Arrow(TypeInt,TypeInt),List(Var("y")),Bin(Add,Var("x"),Var("y")))))),Arrow(TypeInt,TypeInt),Appln(Var("f"),List(Num(4))))

    )
  }

  "test28.simpl" should "parse correctly" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test28.simpl")).mkString
    assert(
      parse(source)== Let(List((Arrow(TypeInt,Arrow(TypeInt,TypeInt)),Var("f"),Func(Arrow(TypeInt,Arrow(TypeInt,TypeInt)),List(Var("x")),Func(Arrow(TypeInt,TypeInt),List(Var("y")),Bin(Add,Var("x"),Var("y")))))),Arrow(TypeInt,TypeInt),Func(Arrow(TypeInt,TypeInt),List(Var("pa")),Appln(Var("f"),List(Num(4), Var("pa")))))


    )
  }
  "test29.simpl" should "parse correctly" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test29.simpl")).mkString
    assert(
      parse(source)== Let(List((Arrow(TypeInt,Arrow(TypeInt,TypeInt)),Var("f"),Func(Arrow(TypeInt,Arrow(TypeInt,TypeInt)),List(Var("x")),Func(Arrow(TypeInt,TypeInt),List(Var("y")),Bin(Add,Var("x"),Var("y")))))),TypeInt,Let(List((Arrow(TypeInt,TypeInt),Var("g"),Let(List((TypeInt,Var("y"),Num(3))),Arrow(TypeInt,TypeInt),Appln(Var("f"),List(Num(3)))))),TypeInt,Appln(Var("g"),List(Num(4)))))

    )
  }

  "test30.simpl" should "parse correctly" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test30.simpl")).mkString
    assert(
      parse(source)== Bin(Add,Bin(Mul,Num(2),Num(2)),Num(4))
    )
  }
}
