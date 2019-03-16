package edu.nus.comp.pli.simpl.util

import edu.nus.comp.pli.simpl.util.TypeInfer._
import org.scalatest.FlatSpec
import edu.nus.comp.pli.simpl.parser.SimplAST._
import edu.nus.comp.pli.simpl.parser.SimplParser._


import scala.io.Source

class TypeInferTest extends FlatSpec{

  "test01.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test01.simpl")).mkString
    assert(
      type_infer(Seq(), parse(source))== Some(TypeInt)
    )
  }

  "test02.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test02.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source))== Some(TypeInt)
    )
  }
  "test03.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test03.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source))== Some(Arrow(TypeInt,Arrow(TypeInt,TypeInt)))
    )
  }
  "test04.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test04.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source))== Some(Arrow(TypeInt,Arrow(TypeInt,Arrow(Arrow(TypeInt,Arrow(TypeInt,TypeInt)),Arrow(TypeInt,TypeInt)))))
    )
  }
  "test05.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test05.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)).isEmpty
      // means the type is wrongly matched
    )
  }
  "test06.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test06.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source))== Some(TypeInt)
    )
  }
  "test07.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test07.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source))== Some(TypeInt)
    )
  }
  "test08.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test08.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source))== Some(TypeInt)
    )
  }
  "test09.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test09.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source))== Some(TypeInt)
    )
  }
  "test10.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test10.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source))== Some(TypeInt)
    )
  }
  "test11.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test11.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source))== Some(TypeInt)
    )
  }
  "test12.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test12.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source))== Some(TypeInt)
    )
  }
  "test13.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test13.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source))== Some(TypeInt)
    )
  }
  "test14.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test14.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source))== Some(Arrow(TypeInt,TypeInt))
    )
  }
  "test15.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test15.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source)).isEmpty
    )
  }
  "test16.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test16.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source))== Some(TypeInt)
    )
  }
  "test17.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test17.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source))== Some(TypeInt)
    )
  }
  "test18.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test18.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source))== Some(TypeInt)
    )
  }
  "test19.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test19.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source))== Some(TypeInt)
    )
  }
  "test20.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test20.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source))== Some(TypeInt)
    )
  }
  "test21.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test21.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source))== Some(TypeInt)
    )
  }
  "test22.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test22.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source))== Some(TypeInt)
    )
  }
  "test23.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test23.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source))== Some(TypeInt)
    )
  }
  "test24.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test24.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source))== Some(TypeInt)
    )
  }
  "test25.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test25.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source))== Some(TypeInt)
    )
  }
  "test26.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test26.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source))== Some(TypeInt)
    )
  }
  "test27.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test27.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source))== Some(Arrow(TypeInt,TypeInt))
    )
  }
  "test28.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test28.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source))== Some(Arrow(TypeInt,TypeInt))
    )
  }
  "test29.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test29.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source))== Some(TypeInt)
    )
  }
  "test30.simpl" should "has no free variable" in {
    val source = Source.fromURL(getClass.getResource("/simpl/test30.simpl")).mkString
    assert(
      type_infer(Seq(),parse(source))== Some(TypeInt)
    )
  }
}
