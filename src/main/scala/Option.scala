/**
  * Created by gilcu2 on 07/04/2017.
  */

package com.gilcu2.fpbook

trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def orElse[B >: A](default: => Option[B]): Option[B] = map(Some(_)).getOrElse(default)

  def filter(f: A => Boolean): Option[A] = flatMap(x => if (f(x)) Some(x) else None)

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case e: Exception => None
    }

  def sequence[A](a: List[Option[A]]): Option[List[A]]

}