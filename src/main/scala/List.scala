/**
  * Created by gilcu2 on 04/04/2017.
  */
package com.gilcu2.fpbook.list

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], e: A): List[A] = l match {
    case Nil => Cons(e, Nil)
    case Cons(_, t) => Cons(e, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if (f(h)) => dropWhile(t)(f)
      case _ => l
    }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(h, z))(f)
    }

  def foldRight1[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    val lr = reverse(l)
    foldLeft(lr, z)(f)
  }

  def sum(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def length[A](l: List[A]): Int = foldLeft(l, 0)((x, y) => 1 + y)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((x, y) => Cons(x, y))

  def append1[A](a1: List[A], a2: List[A]): List[A] = {
    val a1r = reverse(a1)
    foldLeft(a1r, a2)((x, y) => Cons(x, y))
  }

  def flat[A](l: List[List[A]]): List[A] = foldRight1(l, List[A]())(append1)

  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight1(as, List[B]())((x, y) => Cons(f(x), y))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    val f1 = (x: A, l: List[A]) => if (f(x)) Cons(x, l) else l
    foldRight1(as, List[A]())(f1)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    val l1 = map(as)(f)
    flat(l1)
  }

  def flatMap1[A, B](as: List[A])(f: A => List[B]): List[B] = foldRight1(as, List[B]())((x, y) => append1(f(x), y))

  def filter1[A](as: List[A])(f: A => Boolean): List[A] = flatMap1(as)(x => if (f(x)) List(x) else List())

  def zipWith[A, B, C](la: List[A], lb: List[B])(f: (A, B) => C): List[C] = {

    @tailrec
    def loop(l1: List[A], l2: List[B], ac: List[C]): List[C] =
      (l1, l2) match {
        case (Nil, _) => ac
        case (_, Nil) => ac
        case (Cons(h1, t1), Cons(h2, t2)) => loop(t1, t2, Cons(f(h1, h2), ac))
      }

    loop(reverse(la), reverse(lb), List[C]())
  }

  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_, t) => hasSubsequence(t, sub)
  }

}

