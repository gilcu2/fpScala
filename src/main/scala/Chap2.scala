/**
  * Created by gilcu2 on 4/3/17.
  */

package com.gilcu2.fpbook

object Chap2 {

  def abs(x: Int): Int = if (x < 1) -x else x

  def factorial(x: Int): Int = {
    def loop(n: Int, acc: Int): Int = if (n <= 0) acc else loop(n - 1, n * acc)

    loop(x, 1)
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def main(args: Array[String]) = {
    println(formatAbs(-42))
  }

}
