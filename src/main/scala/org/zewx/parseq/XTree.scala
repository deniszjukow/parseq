package org.zewx.parseq

import cats.kernel.Semigroup
import cats.{Functor, Monad}

case class Path[I](elements: Seq[I])

/**
  * @tparam A - value type
  */
trait XTree[+A] {
  def numerate[I](start: I, step: I)(implicit s: Semigroup[I]): XTree[(Seq[I], A)] =
    XTree.numerate(this, start, step)
}

class XBranch[A](children: Seq[XTree[A]]) extends XTree[A]

case class XPar[A](children: Seq[XTree[A]]) extends XBranch[A](children)

case class XSeq[A](children: Seq[XTree[A]]) extends XBranch[A](children)

case class XLeaf[A](value: A) extends XTree[A]

case object XEmpty extends XTree[Nothing]

object XTree {

  def par[A](as: XTree[A]*): XTree[A] = as.toList match {
    case Nil => XEmpty
    case head :: Nil => head
    case head :: tail => XPar(head :: tail)
  }

  def seq[A](as: XTree[A]*): XTree[A] = as.toList match {
    case Nil => XEmpty
    case head :: Nil => head
    case head :: tail => XSeq(head :: tail)
  }

  def leaf[A](a: A): XTree[A] = XLeaf[A](a)

  def numerate[I, A](fa: XTree[A], start: I, step: I)(implicit s: Semigroup[I]): XTree[(Seq[I], A)] = {
    import cats.syntax.semigroup._

    def iterate(children: Seq[XTree[A]], path: List[I]): Seq[XTree[(Seq[I], A)]] = Stream
      .iterate(start, children.size)(i => i |+| step).toList
      .map(i => i :: path)
      .zip(children)
      .map { case (i, a) => go(i, a) }

    def go(path: List[I], tree: XTree[A]): XTree[(Seq[I], A)] = tree match {
      case XSeq(children) => XSeq(iterate(children, path))
      case XPar(children) => XPar(iterate(children, path))
      case XLeaf(a) => XLeaf(path.reverse, a)
      case XEmpty => XEmpty
    }

    go(List(), fa)
  }

  implicit val xTreeFunctor: Functor[XTree] = new Functor[XTree] {
    override def map[A, B](fa: XTree[A])(f: A => B): XTree[B] = fa match {
      case XSeq(children) => XSeq(children.map(child => map(child)(f)))
      case XPar(children) => XPar(children.map(child => map(child)(f)))
      case XLeaf(value) => XLeaf(f(value))
      case XEmpty => XEmpty
    }
  }

  implicit val xTreeMonad: Monad[XTree] = new Monad[XTree] {

    override def pure[A](x: A): XTree[A] = XLeaf[A](x)

    override def flatMap[A, B](fa: XTree[A])(f: A => XTree[B]): XTree[B] = fa match {
      case XSeq(children) => XSeq(children.map(child => flatMap(child)(f)))
      case XPar(children) => XPar(children.map(child => flatMap(child)(f)))
      case XLeaf(value) => f(value)
      case XEmpty => XEmpty
    }

    override def tailRecM[A, B](a: A)(f: A => XTree[Either[A, B]]): XTree[B] = {
      import cats.syntax.either._
      def go(t: XTree[Either[A, B]]): XTree[B] = t match {
        case XSeq(children) => children.foldLeft[XTree[B]](XSeq(Seq()))((_, x) => go(x))
        case XPar(children) => children.foldLeft[XTree[B]](XPar(Seq()))((_, x) => go(x))
        case XLeaf(value) => value.bimap(tailRecM(_)(f), XLeaf(_)).merge
        case XEmpty => XEmpty
      }

      go(f(a))
    }
  }
}
