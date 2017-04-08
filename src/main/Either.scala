/**
  * Created by gilcu2 on 08/04/2017.
  */
package com.gilcu2.fpbook

trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] =
    this match {
      case Right(x) => Right(f(x))
      case Left(x) => Left(x)
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(x) => f(x)
      case Left(x) => Left(x)
    }

  def orElse[EE >: E, B >: A]()


}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

}