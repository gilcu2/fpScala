/**
  * Created by gilcu2 on 4/3/17.
  */

package com.gilcu2.fpbook

import scala.annotation.tailrec

object ExercisesChap2 {

  def fibonacci(n: Int): Int = {
    @tailrec def helper(x: Int, prev: Int, next: Int): Int = x match {
      case 0 => prev
      case 1 => next
      case _ => helper(x - 1, next, (next + prev))
    }

    helper(n, 0, 1)
  }

}
