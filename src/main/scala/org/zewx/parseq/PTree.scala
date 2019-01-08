package org.zewx.parseq

import cats.data.NonEmptyList
import cats.kernel.Semigroup
import cats.{Functor, Monad, Monoid}


/**
  * @tparam A - value type
  */
trait PTree[+A] {
  def numerate[I](start: I, step: I)(implicit s: Semigroup[I]): PTree[(Seq[I], A)] =
    PTree.numerate(this, start, step)

  def prime: NTree[ParSeq, Int, A] =
    PTree.prime[Int, A](this, 0, 1)(cats.instances.int.catsKernelStdGroupForInt)
}

class PBranch[A](value: A, nodes: Seq[PTree[A]]) extends PTree[A]

case class PPar[A](value: A, nodes: Seq[PTree[A]]) extends PBranch[A](value, nodes)

case class PSeq[A](value: A, nodes: Seq[PTree[A]]) extends PBranch[A](value, nodes)

case class PLeaf[A](value: A) extends PTree[A]

case object PEmpty extends PTree[Nothing]

object PTree {

  def empty[A]: PTree[A] = PEmpty

  def par[A](as: PTree[A]*)(implicit monoidValue: Monoid[A]): PTree[A] = as.toList match {
    case Nil => PEmpty
    case head :: Nil => head
    case head :: tail => PPar(monoidValue.empty, head :: tail)
  }

  def par[A](a: A, as: PTree[A]*): PTree[A] = as.toList match {
    case Nil => PEmpty
    case head :: Nil => head
    case head :: tail => PPar(a, head :: tail)
  }

  def seq[A](as: PTree[A]*)(implicit monoidValue: Monoid[A]): PTree[A] = as.toList match {
    case Nil => PEmpty
    case head :: Nil => head
    case head :: tail => PSeq(monoidValue.empty, head :: tail)
  }

  def seq[A](a: A, as: PTree[A]*): PTree[A] = as.toList match {
    case Nil => PEmpty
    case head :: Nil => head
    case head :: tail => PSeq(a, head :: tail)
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
      case PSeq(value, nodes) => PSeq((path.reverse, value), iterate(nodes, path))
      case PPar(value, nodes) => PPar((path.reverse, value), iterate(nodes, path))
      case PLeaf(value) => PLeaf(path.reverse, value)
      case PEmpty => PEmpty
    }

    go(List(start), fa)
  }

  def prime[I, A](fa: PTree[A], start: I, step: I)(implicit s: Semigroup[I]): NTree[ParSeq, I, A] = {
    import cats.syntax.semigroup._

    def iterate(children: Seq[PTree[A]], path: NonEmptyList[I]): Seq[NTree[ParSeq, I, A]] = Stream
      .iterate(start, children.size)(i => i |+| step).toList
      .map(i => i :: path)
      .zip(children)
      .map { case (i, a) => go(i, a) }

    def go(path: NonEmptyList[I], tree: PTree[A]): NTree[ParSeq, I, A] = tree match {
      case PSeq(value, children) => NBranch(SEQ, path, value, iterate(children, path))
      case PPar(value, children) => NBranch(PAR, path, value, iterate(children, path))
      case PLeaf(value) => NLeaf(path.reverse, value)
      case PEmpty => NEmpty
    }

    go(NonEmptyList.one(start), fa)
  }

  implicit val xTreeFunctor: Functor[PTree] = new Functor[PTree] {
    override def map[A, B](fa: PTree[A])(f: A => B): PTree[B] = PTree.map(fa)(f)
  }

  def map[A, B](fa: PTree[A])(f: A => B): PTree[B] = fa match {
    case PSeq(value, nodes) => PSeq(f(value), nodes.map(child => map(child)(f)))
    case PPar(value, nodes) => PPar(f(value), nodes.map(child => map(child)(f)))
    case PLeaf(value) => PLeaf(f(value))
    case PEmpty => PEmpty
  }

  implicit def xTreeMonad(implicit x: FunctionSplitter[PTree]): Monad[PTree] = new Monad[PTree] {
    override def pure[A](x: A): PTree[A] = PTree.leaf(x)

    override def flatMap[A, B](fa: PTree[A])(f: A => PTree[B]): PTree[B] = x(f) match {
      case (u, v) => map(flatMapA(fa)(u))(v)
    }

    override def tailRecM[A, B](a: A)(f: A => PTree[Either[A, B]]): PTree[B] = ???
  }

  def flatMap[A, B](fa: PTree[A])(f: A => PTree[A])(implicit g: A => B): PTree[B] =
    map(flatMapA(fa)(f))(g)

  def flatMapA[A](fa: PTree[A])(f: A => PTree[A]): PTree[A] = fa match {
    case PSeq(value, nodes) => PSeq(value, nodes.map(flatMapA(_)(f)))
    case PPar(value, nodes) => PPar(value, nodes.map(flatMapA(_)(f)))
    case PLeaf(value) => f(value)
    case PEmpty => PEmpty
  }
}
