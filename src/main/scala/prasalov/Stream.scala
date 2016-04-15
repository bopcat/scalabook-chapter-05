package prasalov

import scala.annotation.tailrec

/**
 * Created by kirillp on 13.04.16.
 */
sealed trait Stream[+A]{

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  @tailrec final def fold[B](onCons: (() => A, () => Stream[A], B) => B)(res: B, n: Int): B =
    this match {
      case Empty => res
      case Cons(h, t) =>
        if (n != 0) t().fold(onCons)(onCons(h, t, res), n - 1)
        else res
    }

  @tailrec final def foldWhile[B](onCons: (() => A, () => Stream[A], B) => B)(res: B, p: A => Boolean): B =
    this match {
      case Empty => res
      case Cons(h, t) =>
        if (p(h())) t().foldWhile(onCons)(onCons(h, t, res), p)
        else res
    }

  private def extractHead[A](h: () => A, t: () => Stream[A], res: List[A]) = res :+ h()
  
  def take(n: Int): List[A] = fold(extractHead)(Nil, n)
  def drop(n: Int): Stream[A] = fold((h: () => A, t: () => Stream[A], res: Stream[A]) => t())(this, n)

  // straightforward non-tailrec implementation
  def take1(n: Int): List[A] =
    if (n == 0) Nil
    else this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().take(n - 1)
    }

  // straightforward non-tailrec implementation
  def drop1(n: Int): Stream[A] =
    if (n == 0) this
    else this match {
      case Empty => this
      case Cons(h, t) => t().drop(n - 1)
    }

  def takeWhile(p: A => Boolean): Stream[A] = Stream(foldWhile(extractHead)(Nil, p): _*)

  def foldRight[B](z: => B)(f: (A, =>B) => B): B =
    this match {
      case Empty => z
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
    }

  class CountdownFunc(initial: Int) {
    var count = initial
    def apply = {
      count = count - 1
      count + 1 == 0
    }
  }
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

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
