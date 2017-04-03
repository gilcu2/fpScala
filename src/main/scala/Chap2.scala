/**
  * Created by gilcu2 on 4/3/17.
  */

package com.gilcu2.fpbook

object Chap2 {

  def abs(x: Int): Int = if (x < 1) -x else x

  def factorial(x: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, acc: Int): Int = if (n <= 0) acc else loop(n - 1, n * acc)

    loop(x, 1)
  }

  private def formatResult(x: Int, f: Int => Int, label: String) = {
    label.format(x, f(x))
  }

  def main(args: Array[String]) = {
    println(formatResult(-42, abs, "The absolute value of %d is %d"))
    println(formatResult(7, factorial, "The factorial of %d is %d"))
  }

}
