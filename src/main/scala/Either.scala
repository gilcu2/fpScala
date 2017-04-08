/**
  * Created by gilcu2 on 07/04/2017.
  */

package com.gilcu2.fpbook

sealed trait Either[+E, +A] {

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

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(x) => Right(x)
      case Left(x) => b
    }

  def map2[EE >: E, B, C](b: => Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    (this, b) match {
      case (Right(x), Right(y)) => Right(f(x, y))
      case (Left(x), _) => Left(x)
      case (_, Left(x)) => Left(x)
    }

  def map2_1[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {a <- this; b1 <- b} yield f(a, b1)


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