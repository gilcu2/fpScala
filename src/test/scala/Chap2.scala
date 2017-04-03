/**
  * Created by gilcu2 on 4/3/17.
  */
package com.gilcu2.fpbook.tests


import org.scalatest._
import com.gilcu2.fpbook.Chap2
import com.gilcu2.fpbook.ExercisesChap2

class Chap2Test extends FlatSpec with Matchers {

  "Chap2" should "implement factorial" in {
    val r = Chap2.factorial(4)
    r should be(24)
  }

  "Chap2" should "implement fibonacci" in {
    val r = ExercisesChap2.fibonacci(5)
    r should be(5)
  }

}