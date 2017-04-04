/**
  * Created by gilcu2 on 04/04/2017.
  */

package com.gilcu2.fpbook.tests


import org.scalatest._
import com.gilcu2.fpbook._
import List._


class Chap3Test extends FlatSpec with Matchers {

  "List" should "match 3 case" in {
    val r = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    r should be(3)
  }

  "List" should "return tail" in {
    val r = tail(List(1, 2, 3, 4, 5))
    r should be(List(2, 3, 4, 5))
  }

  "List" should "set head" in {
    val r = setHead(List(1, 2, 3, 4, 5), -1)
    r should be(List(-1, 2, 3, 4, 5))
  }

  "List" should "dropWhile" in {
    val r = dropWhile(List(1, 2, 3, 4, 5))(_ < 3)
    r should be(List(3, 4, 5))
  }

  "List" should "return init" in {
    val r = init(List(1, 2, 3, 4, 5))
    r should be(List(1, 2, 3, 4))
  }

  "List" should "sum" in {
    val r = sum(List(1, 2, 3, 4, 5))
    r should be(15)
  }

  "foldRight" should "work with Cons" in {
    val r = foldRight(List(1, 2, 3, 4, 5), Nil: List[Int])(Cons(_, _))
    r should be(List(1, 2, 3, 4, 5))
  }

  "List" should "lenght" in {
    val r = List.length(List(1, 2, 3, 4, 5))
    r should be(5)
  }

  "List" should "reverse" in {
    val r = reverse(List(1, 2, 3))
    r should be(List(3, 2, 1))
  }

}
