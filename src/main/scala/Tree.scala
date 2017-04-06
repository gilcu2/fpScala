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
      case Branch(x, y) => 1 + size(x) + size(y)
    }

  def maximun(t: Tree[Int]): Int =
    t match {
      case Leaf(x) => x
      case Branch(x, y) => maximun(x) max maximun(y)
    }

  def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 0
      case Branch(x, y) => 1 + depth(x) max depth(y)
    }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(x) => Leaf(f(x))
      case Branch(x, y) => Branch(map(x)(f), map(y)(f))
    }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B =
    t match {
      case Leaf(x) => f(x)
      case Branch(x, y) => g(fold(x)(f)(g), fold(y)(f)(g))
    }

  def size1[A](t: Tree[A]): Int = fold(t)(x => 1)(1 + _ + _)

  def map1[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(x => Leaf(f(x)): Tree[B])(Branch(_, _))

}
