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

