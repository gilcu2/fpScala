/**
  * Created by gilcu2 on 07/04/2017.
  */
package com.gilcu2.fpbook.tests


import org.scalatest._
import com.gilcu2.fpbook._


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

class Chap4Test extends FlatSpec with Matchers {

  import ExercisesChap4._

  "Variance" should "compute empty list" in {
    val r = variance(Seq())
    r should be(None)
  }

  "Variance" should "compute non empty list" in {
    val r = variance(Seq(1, 2, 3))
    r shouldBe a[Some[Double]]
  }

}