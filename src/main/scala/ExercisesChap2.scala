/**
  * Created by gilcu2 on 4/3/17.
  */

package com.gilcu2.fpbook

import scala.annotation.tailrec

object ExercisesChap2 {

  def fibonacci(n: Int): Int = {

    @tailrec
    def helper(x: Int, prev: Int, next: Int): Int = x match {
      case 0 => prev
      case 1 => next
      case _ => helper(x - 1, next, (next + prev))
    }

    helper(n, 0, 1)
  }

  def isSorted[A](ar: Array[A], ordered: (A, A) => Boolean): Boolean = {

    @tailrec
    def helper(i: Int): Boolean =
      if (i >= ar.length) true
      else if (!ordered(ar(i - 1), ar(i))) false
      else helper(i + 1)

    helper(1)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => ((b: B) => f(a, b))

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

}
