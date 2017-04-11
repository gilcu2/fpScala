/**
  * Created by gilcu2 on 07/04/2017.
  */
package com.gilcu2.fpbook.tests


import org.scalatest._
import com.gilcu2.fpbook.chap4._


class OptionTest extends FlatSpec with Matchers {

  "Option" should "filter true" in {
    val r = Some(1).filter(_ == 1)
    r should be(Some(1))
  }

  "Option" should "filter false" in {
    val r = Some(1).filter(_ != 1)
    r should be(None)
  }

}

class EitherTest extends FlatSpec with Matchers {

  "Either" should "map right value" in {
    val r = Right(1).map(_ + 1)
    r should be(Right(2))
  }

  "Either" should "map2 right value" in {
    val r = Right(1).map2(Right(1))(_ + _)
    r should be(Right(2))
  }


}


class Chap4Test extends FlatSpec with Matchers {

  import ExercisesChap4._

  "Variance" should "compute empty list" in {
    val r = variance(Seq())
    r should be(None)
  }

  "Variance" should "compute non empty list" in {
    val r = variance(Seq(1, 2, 3))
    r shouldBe a[Some[_]]
  }

  "secuence" should "unpack List[Option] without None" in {
    val r = Option.sequence(List(Some(1), Some(2), Some(3)))
    r should be(Some(List(1, 2, 3)))
  }

  "secuence" should "unpack List[Option] with None" in {
    val r = Option.sequence(List(Some(1), None, Some(3)))
    r should be(None)
  }

  "traverse" should "process List without problem" in {
    val r = Option.traverse(List("1", "2", "3"))(x => Option.Try(x.toInt))
    r should be(Some(List(1, 2, 3)))
  }

  "traverse" should "process List with problem" in {
    val r = Option.traverse(List("1", "k", "3"))(x => Option.Try(x.toInt))
    r should be(None)
  }


}