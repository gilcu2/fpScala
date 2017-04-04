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

  "findFirst" should "find " in {
    val r = Chap2.findFirst(Array(1, 2, 3), (x: Int) => x == 2)
    r should be(1)
  }

  "findFirst" should "not find " in {
    val r = Chap2.findFirst(Array(1, 2, 3), (x: Int) => x == 4)
    r should be(-1)
  }


}

class ExercisesChap2Test extends FlatSpec with Matchers {

  "Chap2" should "implement fibonacci" in {
    val r = ExercisesChap2.fibonacci(6)
    r should be(8)
  }

  "isSortered" should "return true in 1 element" in {
    val r = ExercisesChap2.isSorted(Array(1), (x: Int, y: Int) => x < y)
    r should be(true)
  }

  "isSortered" should "return true in ordered array " in {
    val r = ExercisesChap2.isSorted(Array(1, 2, 3), (x: Int, y: Int) => x < y)
    r should be(true)
  }

  "isSortered" should "false in disordered array " in {
    val r = ExercisesChap2.isSorted(Array(1, 3, 2), (x: Int, y: Int) => x < y)
    r should be(false)
  }

  "curry" should "construct a function of function " in {
    val f = (a: Int, b: Int) => a + b
    val fcurry = ExercisesChap2.curry(f)
    f(2, 3) should be(fcurry(2)(3))
  }

  "uncurry" should "deconstruct a function of function " in {
    val f = (a: Int) => ((b: Int) => a + b)
    val funcurry = ExercisesChap2.uncurry(f)
    f(2)(3) should be(funcurry(2, 3))
  }

  "compose" should "construct a function from two functions " in {
    val f1 = (a: Int) => a + 1
    val f2 = (a: Int) => 2 * a
    val fcompose = ExercisesChap2.compose(f1, f2)
    f1(f2(2)) should be(fcompose(2))
  }

}