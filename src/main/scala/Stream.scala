/**
  * Created by gilcu2 on 11/04/2017.
  */

package com.gilcu2.fpbook.chap5

import scala.annotation.tailrec

sealed trait Stream[+A] {

  import Stream._

  def head: Option[A] =
    this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

  def toListRecursive: List[A] =
    this match {
      case Empty => List()
      case Cons(h, t) => h() :: t().toListRecursive
    }

  def toList: List[A] = {

    @tailrec
    def loop(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty => acc
      case Cons(h, t) => loop(t(), h() :: acc)
    }

    loop(this, List()).reverse
  }

  // Pure method because mutable object doesn't get out of it

  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]

    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }

    go(this)
  }

  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }

  @tailrec
  final def drop(n: Int): Stream[A] =
    this match {
      case Cons(_, t) if n >= 1 => t().drop(n - 1)
      case s => s
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => empty
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists(f: A => Boolean): Boolean = foldRight(false)((a, b) => f(a) || b)

  def forAll(f: A => Boolean): Boolean = foldRight(true)((a, b) => f(a) && b)

  def takeWhile1(f: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else empty)

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  val fibs = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))

    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =

}

