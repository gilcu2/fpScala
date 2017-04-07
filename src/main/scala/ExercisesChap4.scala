/**
  * Created by gilcu2 on 07/04/2017.
  */

package com.gilcu2.fpbook

object ExercisesChap4 {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.nonEmpty) Some(xs.sum / xs.length) else None


  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))
}
