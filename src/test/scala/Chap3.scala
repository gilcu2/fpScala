/**
  * Created by gilcu2 on 04/04/2017.
  */

package com.gilcu2.fpbook.tests


import org.scalatest._
import com.gilcu2.fpbook._


class ListTest extends FlatSpec with Matchers {

  import list._
  import List._

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

  "List" should "append1" in {
    val r = append1(List(1, 2, 3), List(4, 5, 6))
    r should be(List(1, 2, 3, 4, 5, 6))
  }

  "List" should "be flat" in {
    val r = flat(List(List(1, 2, 3), List(4, 5, 6)))
    r should be(List(1, 2, 3, 4, 5, 6))
  }

  "List" should "map" in {
    val r = map(List(1, 2, 3))(2 * _)
    r should be(List(2, 4, 6))
  }

  "List" should "filter" in {
    val r = filter1(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0)
    r should be(List(2, 4, 6))
  }

  "List" should "flatMap" in {
    val r = flatMap1(List(1, 2, 3))(x => List(x, x))
    r should be(List(1, 1, 2, 2, 3, 3))
  }

  "List" should "zipWith" in {
    val r = zipWith(List(1, 2, 3), List(2, 3, 4))(_ + _)
    r should be(List(3, 5, 7))
  }

  "List" should "test for subsecuence" in {
    val r = hasSubsequence(List(1, 2, 3, 4), List(2, 3))
    r should be(true)
  }

}

class TreeTest extends FlatSpec with Matchers {

  import Tree._

  "Tree" should "return size" in {
    val r = Tree.size(Branch(
      Branch(Leaf(1), Leaf(2)),
      Leaf(3)))
    r should be(5)
  }

  "Tree" should "return dept" in {
    val r = depth(Branch(
      Branch(Leaf(1), Leaf(2)),
      Leaf(3)))
    r should be(2)
  }

  "Tree" should "map" in {
    val r = map(Branch(
      Branch(Leaf(1), Leaf(2)),
      Leaf(3)))(_ * 2)
    r should be(Branch(
      Branch(Leaf(2), Leaf(4)),
      Leaf(6)))
  }

  "Tree" should "return size1" in {
    val r = Tree.size1(Branch(
      Branch(Leaf(1), Leaf(2)),
      Leaf(3)))
    r should be(5)
  }

  "Tree" should "map1" in {
    val r = map1(Branch(
      Branch(Leaf(1), Leaf(2)),
      Leaf(3)))(_ * 2)
    r should be(Branch(
      Branch(Leaf(2), Leaf(4)),
      Leaf(6)))
  }

}