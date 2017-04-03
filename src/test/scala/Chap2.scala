/**
  * Created by gilcu2 on 4/3/17.
  */
package com.gilcu2.fpbook.tests


import org.scalatest._
import com.gilcu2.fpbook.Chap2

class Chap2Test extends FlatSpec with Matchers {

  "Chap2" should "implement factorial" in {
    val r = Chap2.factorial(4)
    r should be(24)
  }
}