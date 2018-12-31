package org.zewx.parseq

import cats.data.NonEmptyList
import cats.kernel.Semigroup
import cats.{Functor, Monad}

//case class Path[I](elements: Seq[I])

/**
  * @tparam A - value type
  */
trait PTree[+A] {
  def numerate[I](start: I, step: I)(implicit s: Semigroup[I]): PTree[(Seq[I], A)] =
    PTree.numerate(this, start, step)

  def prime: NTree[Int, A] =
    PTree.prime[Int, A](this, 0, 1)(cats.instances.int.catsKernelStdGroupForInt)
}

class PBranch[A](children: Seq[PTree[A]]) extends PTree[A]

case class PPar[A](children: Seq[PTree[A]]) extends PBranch[A](children)

case class PSeq[A](children: Seq[PTree[A]]) extends PBranch[A](children)

case class PLeaf[A](value: A) extends PTree[A]

case object PEmpty extends PTree[Nothing]

object PTree {

  def empty[A]: PTree[A] = PEmpty

  def par[A](as: PTree[A]*): PTree[A] = as.toList match {
    case Nil => PEmpty
    case head :: Nil => head
    case head :: tail => PPar(head :: tail)
  }

  def seq[A](as: PTree[A]*): PTree[A] = as.toList match {
    case Nil => PEmpty
    case head :: Nil => head
    case head :: tail => PSeq(head :: tail)
  }

  def leaf[A](a: A): PTree[A] = PLeaf[A](a)

  def numerate[I, A](fa: PTree[A], start: I, step: I)(implicit s: Semigroup[I]): PTree[(Seq[I], A)] = {
    import cats.syntax.semigroup._

    def iterate(children: Seq[PTree[A]], path: List[I]): Seq[PTree[(Seq[I], A)]] = Stream
      .iterate(start, children.size)(i => i |+| step).toList
      .map(i => i :: path)
      .zip(children)
      .map { case (i, a) => go(i, a) }

    def go(path: List[I], tree: PTree[A]): PTree[(Seq[I], A)] = tree match {
      case PSeq(children) => PSeq(iterate(children, path))
      case PPar(children) => PPar(iterate(children, path))
      case PLeaf(a) => PLeaf(path.reverse, a)
      case PEmpty => PEmpty
    }

    go(List(), fa)
  }

  def prime[I, A](fa: PTree[A], start: I, step: I)(implicit s: Semigroup[I]): NTree[I, A] = {
    import cats.syntax.semigroup._

    def iterate(children: Seq[PTree[A]], path: NonEmptyList[I]): Seq[NTree[I, A]] = Stream
      .iterate(start, children.size)(i => i |+| step).toList
      .map(i => i :: path)
      .zip(children)
      .map { case (i, a) => go(i, a) }

    def go(path: NonEmptyList[I], tree: PTree[A]): NTree[I, A] = tree match {
      case PSeq(children) => NBranch(Sequential, path, iterate(children, path))
      case PPar(children) => NBranch(Parallel, path, iterate(children, path))
      case PLeaf(a) => NLeaf(path.reverse, a)
      case PEmpty => NEmpty
    }

    go(NonEmptyList.one(start), fa)
  }

  implicit val xTreeFunctor: Functor[PTree] = new Functor[PTree] {
    override def map[A, B](fa: PTree[A])(f: A => B): PTree[B] = fa match {
      case PSeq(children) => PSeq(children.map(child => map(child)(f)))
      case PPar(children) => PPar(children.map(child => map(child)(f)))
      case PLeaf(value) => PLeaf(f(value))
      case PEmpty => PEmpty
    }
  }

  implicit val xTreeMonad: Monad[PTree] = new Monad[PTree] {

    override def pure[A](x: A): PTree[A] = PLeaf[A](x)

    override def flatMap[A, B](fa: PTree[A])(f: A => PTree[B]): PTree[B] = fa match {
      case PSeq(children) => PSeq(children.map(child => flatMap(child)(f)))
      case PPar(children) => PPar(children.map(child => flatMap(child)(f)))
      case PLeaf(value) => f(value)
      case PEmpty => PEmpty
    }

    override def tailRecM[A, B](a: A)(f: A => PTree[Either[A, B]]): PTree[B] = {
      import cats.syntax.either._
      def go(t: PTree[Either[A, B]]): PTree[B] = t match {
        case PSeq(children) => children.foldLeft[PTree[B]](PSeq(Seq()))((_, x) => go(x))
        case PPar(children) => children.foldLeft[PTree[B]](PPar(Seq()))((_, x) => go(x))
        case PLeaf(value) => value.bimap(tailRecM(_)(f), PLeaf(_)).merge
        case PEmpty => PEmpty
      }

      go(f(a))
    }
  }
}
