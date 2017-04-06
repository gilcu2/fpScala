/**
  * Created by gilcu2 on 06/04/2017.
  */

package com.gilcu2.fpbook

trait Tree[A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(x, y) => size(x) + size(y)
    }

}
