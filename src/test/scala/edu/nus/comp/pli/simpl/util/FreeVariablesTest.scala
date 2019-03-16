package edu.nus.comp.pli.simpl.util

import org.scalatest.FlatSpec
import FreeVariables._
import edu.nus.comp.pli.simpl.parser.SimplAST._
import edu.nus.comp.pli.simpl.parser.SimplParser._


import scala.io.Source

class FreeVariablesTest  extends FlatSpec {

  "1) single integers" should "have no free vars" in {
    assert(fv(Num(1)) == Set())
  }

  "2) single boolean" should "have no free vars" in {
    assert(fv(Bool(true)) == Set())
  }

  "3) a single variable expression" should "have that variable as free" in {
    assert(fv(Var("x")) == Set(Var("x")))
  }

  "4) an expression with a single operator and two distinct variable" should
    "have those 2 variables as free" in {
    assert(
      fv(Bin(Add, Num(1), Num(2))) ==
        Set()
    )
  }

  "5) an expression with a single operator and two distinct variable" should
     "have those 2 variables as free" in {
    assert(
      fv(Bin(Add, Var("x"), Var("y"))) ==
      Set(Var("x"), Var("y"))
    )
  }

  "6) an expression with a single operator and one variable appearing twice" should
    "have that variable s free" in {
    assert(
      fv(Bin(Add, Var("x"), Var("x"))) ==
        Set(Var("x"))
    )
  }

  "7) an expression with a single operator and one variable appearing twice" should
    "have that variable s free" in {
    assert(
      fv(Ury(Minus,Ury(Minus,Ury(Minus,Num(5))))) ==
        Set()
    )
  }
  "8) an expression with a single operator and one variable appearing twice" should
    "have that variable s free" in {
    assert(
      fv(Ury(Minus,Ury(Minus,Ury(Minus,Var("x"))))) ==
        Set(Var("x"))
    )
  }

  "9) an expression with a single operator and one variable appearing twice" should
    "have that variable s free" in {
    assert(
      fv(Bin(Or,Bin(Eq,Bool(false),Ury(Not,Var("y"))),Bin(And,Bool(false),Bin(Eq,Bin(Add,Num(2),Var("t")),Num(4))))) ==
        Set(Var("y"),Var("t"))
    )
  }

  "10) an expression with a single operator and one variable appearing twice" should
    "have that variable s free" in {
    assert(
      fv(Cond(Bin(Eq,Var("y"),Num(0)),Var("iv"),Appln(Var("op"),List(Var("x"), Appln(Var("recurse"),List(Var("x"), Appln(Var("y"),List(Ury(Minus,Num(1)))), Var("op"), Var("iv"))))))) ==
        Set(Var("x"), Var("y"), Var("recurse"), Var("iv"), Var("op"))
    )
  }


  "11) an expression with a single operator and one variable appearing twice" should
    "have that variable s free" in {
    assert(
      fv(Cond(Bin(Eq,Var("y"),Num(0)),Num(1),Bin(Mul,Var("x"),Appln(Var("mypower"),List(Var("x"), Appln(Var("y"),List(Ury(Minus,Num(1))))))))) ==
        Set(Var("y"), Var("x"), Var("mypower"))
    )
  }

  "12) an expression with a single operator and one variable appearing twice" should
    "have that variable s free" in {
    assert(
      fv(Cond(Bin(Eq,Var("y"),Num(0)),Var("initvalue"),Appln(Var("operation"),List(Var("x"), Appln(Var("recurse"),List(Var("x"), Appln(Var("y"),List(Ury(Minus,Num(1)))), Var("operation"), Var("initvalue"))))))) ==
        Set(Var("x"), Var("initvalue"), Var("y"), Var("recurse"), Var("operation"))
    )
  }

  "13) arguments" should "not appear free in lambdas" in {
    assert(
      fv(
        Func(TypeInt, Seq(Var("x")), Bin(Add, Var("x"), Var("y")))
      ) == Set(Var("y"))
    )
  }

  "14) arguments" should "not appear free in lambdas" in {
    assert(
      fv(
        Func(Arrow(TypeInt,TypeInt),List(Var("x")),Bin(Mul,Num(4),Appln(Var("square"),List(Var("x")))))
      ) == Set(Var("square"))
    )
  }

  "15) arguments" should "not appear free in lambdas" in {
    assert(
      fv(
        Func(Arrow(TypeInt,TypeInt),List(Var("n")),Bin(Add,Var("n"),Num(1)))
      ) == Set()
    )
  }

  "16) arguments" should "not appear free in lambdas" in {
    assert(
      fv(
        RecFunc(Arrow(TypeInt,TypeInt),Var("foo"),List(Var("n")),Cond(Bin(Eq,Var("n"),Num(0)),Num(0),Bin(Add,Num(2),Appln(Var("foo1"),List(Appln(Var("n"),List(Ury(Minus,Num(1)))))))))

      ) == Set(Var("foo1"))
    )
  }

  "17) arguments" should "not appear free in lambdas" in {
    assert(
      fv(
        RecFunc(Arrow(TypeInt,TypeInt),Var("foo"),List(Var("n")),Cond(Bin(Eq,Var("n"),Num(0)),Num(0),Bin(Add,Num(2),Appln(Var("foo"),List(Appln(Var("n"),List(Ury(Minus,Var("m")))))))))

      ) == Set(Var("m"))
    )
  }

  "18) arguments" should "not appear free in lambdas" in {
    assert(
      fv(
        RecFunc(Arrow(TypeInt,TypeInt),Var("foo"),List(Var("n"),Var("m")),Cond(Bin(Eq,Var("n"),Num(0)),Num(0),Bin(Add,Num(2),Appln(Var("foo"),List(Appln(Var("n"),List(Ury(Minus,Var("m")))))))))

      ) == Set()
    )
  }


  "19) a function application" should "produce a flat set" in {
    assert(
      fv(
        Appln(
          Func(
            TypeInt,
            Seq(Var("x"), Var("y")),
            Bin(Add, Var("x"), Bin(Mul, Var("y"), Var("z")))
          ),
          Seq(
            Ury(Not, Var("a")),
            Bin(Div, Var("b"), Var("a"))
          )
        )
      ) == Set(Var("a"), Var("b"), Var("z"))
    )
  }

  "20) a function application" should "produce a flat set" in {
    assert(
      fv(
        Appln(Func(Arrow(TypeInt,Arrow(Arrow(TypeInt,TypeInt),TypeInt)),List(),Bin(Mul,Bin(Mul,Num(4),Var("AboutPi")),Appln(Var("Square"),List(Num(6371))))),List(Num(3), Func(Arrow(TypeInt,TypeInt),List(Var("x")),Bin(Mul,Var("x"),Var("x")))))

      ) == Set(Var("AboutPi"), Var("Square"))
    )
  }

  "21) a function application" should "produce a flat set" in {
    assert(
      fv(
        Appln(Func(Arrow(TypeInt,Arrow(Arrow(TypeInt,TypeInt),TypeInt)),List(Var("AboutPi"), Var("Square")),Bin(Mul,Bin(Mul,Num(4),Var("AboutPi")),Appln(Var("Square"),List(Num(6371))))),List(Num(3), Func(Arrow(TypeInt,TypeInt),List(Var("y")),Bin(Mul,Var("x"),Var("x")))))

      ) == Set(Var("x"))
    )
  }


  "22 ) let" should "work correctly when there are no recursive references" in {
    assert(
      fv(
        Let(
          Seq(
            (TypeInt, Var("x"), Bin(Add, Var("y"), Var("z"))),
            (TypeInt, Var("w"), Num(10))
          ),
          TypeInt,
          Bin(Add, Var("x"), Bin(Sub, Var("y"), Var("w")))
        )
      ) == Set(
        Var("y"), Var("z")
      )
    )
  }

  "23) let" should "work correctly when there are recursive references" in {
    assert(
      fv(
        Let(
          Seq(
            (TypeInt, Var("x"), Bin(Add, Var("y"), Var("z"))),
            (TypeInt, Var("w"), Bin(Div, Num(10), Var("x")))
          ),
          TypeInt,
          Bin(Add, Var("x"), Bin(Sub, Var("y"), Var("w")))
        )
      ) == Set(Var("y"), Var("z"))
    )
  }

  "24) let" should "work correctly when there are recursive references" in {
    assert(
      fv(
        Let(
          Seq(
            (TypeInt, Var("x"), Bin(Add, Var("y"), Var("z"))),
            (TypeInt, Var("w"), Bin(Div, Num(10), Var("x")))
          ),
          TypeInt,
          Cond(Bin(Eq,Var("y"),Num(0)),Num(1),Bin(Mul,Var("x"),Appln(Var("mypower"),List(Var("x"), Appln(Var("y"),List(Ury(Minus,Num(1))))))))
        )
      ) == Set(Var("y"), Var("mypower"), Var("z"))
    )
  }



  "test01.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test01.simpl")).mkString
    assert(
      fv(parse(source))== Set()
    )
  }
  "test02.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test02.simpl")).mkString
    assert(
      fv(parse(source))== Set()
    )
  }
  "test03.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test03.simpl")).mkString
    assert(
      fv(parse(source))== Set()
    )
  }
  "test04.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test04.simpl")).mkString
    assert(
      fv(parse(source))== Set()
    )
  }
  "test05.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test05.simpl")).mkString
    assert(
      fv(parse(source))== Set()
    )
  }
  "test06.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test06.simpl")).mkString
    assert(
      fv(parse(source))== Set()
    )
  }
  "test07.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test07.simpl")).mkString
    assert(
      fv(parse(source))== Set()
    )
  }
  "test08.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test08.simpl")).mkString
    assert(
      fv(parse(source))== Set()
    )
  }
  "test09.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test09.simpl")).mkString
    assert(
      fv(parse(source))== Set()
    )
  }
  "test10.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test10.simpl")).mkString
    assert(
      fv(parse(source))== Set()
    )
  }
  "test11.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test11.simpl")).mkString
    assert(
      fv(parse(source))== Set()
    )
  }
  "test12.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test12.simpl")).mkString
    assert(
      fv(parse(source))== Set()
    )
  }
  "test13.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test13.simpl")).mkString
    assert(
      fv(parse(source))== Set()
    )
  }
  "test14.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test14.simpl")).mkString
    assert(
      fv(parse(source))== Set()
    )
  }
  "test15.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test15.simpl")).mkString
    assert(
      fv(parse(source))== Set()
    )
  }
  "test16.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test16.simpl")).mkString
    assert(
      fv(parse(source))== Set()
    )
  }
  "test17.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test17.simpl")).mkString
    assert(
      fv(parse(source))== Set()
    )
  }
  "test18.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test18.simpl")).mkString
    assert(
      fv(parse(source))== Set()
    )
  }
  "test19.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test19.simpl")).mkString
    assert(
      fv(parse(source))== Set()
    )
  }
  "test20.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test20.simpl")).mkString
    assert(
      fv(parse(source))== Set()
    )
  }
  "test21.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test21.simpl")).mkString
    assert(
      fv(parse(source))== Set()
    )
  }
  "test22.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test22.simpl")).mkString
    assert(
      fv(parse(source))== Set()
    )
  }
  "test23.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test23.simpl")).mkString
    assert(
      fv(parse(source))== Set()
    )
  }
  "test24.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test24.simpl")).mkString
    assert(
      fv(parse(source))== Set()
    )
  }
  "test25.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test25.simpl")).mkString
    assert(
      fv(parse(source))== Set()
    )
  }
  "test26.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test26.simpl")).mkString
    assert(
      fv(parse(source))== Set()
    )
  }
  "test27.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test27.simpl")).mkString
    assert(
      fv(parse(source))== Set()
    )
  }
  "test28.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test28.simpl")).mkString
    assert(
      fv(parse(source))== Set()
    )
  }
  "test29.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test29.simpl")).mkString
    assert(
      fv(parse(source))== Set()
    )
  }
  "test30.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test30.simpl")).mkString
    assert(
      fv(parse(source))== Set()
    )
  }



}
