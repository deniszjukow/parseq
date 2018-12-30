package org.zewx.parseq

import cats.{Functor, Monoid}
import cats.data.NonEmptyList

sealed trait ParSeq
case object Parallel extends ParSeq
case object Sequential extends ParSeq

sealed trait NTree[+I, +A] {
  def path: NonEmptyList[I]

  def id: I = path.last

  def children: Seq[NTree[I, A]]

  def ids[J >: I]: Set[J] = children.map(c => c.id).toSet[J]

  def apply[J >: I](id: J): Option[NTree[J, A]] = this match {
    case NBranch(_, _, children) => children.find(_.id == id)
    case leaf@NLeaf(_, _) => if (leaf.id == id) Some(leaf) else None
    case NEmpty(_) => None
  }
}

case class NBranch[I, A](path: NonEmptyList[I], kind: ParSeq, children: Seq[NTree[I, A]]) extends NTree[I, A]

case class NLeaf[I, A](path: NonEmptyList[I], value: A) extends NTree[I, A] {
  override def children: Seq[NTree[I, Nothing]] = List.empty
}

case class NEmpty[I](path: NonEmptyList[I]) extends NTree[I, Nothing] {
  override def children: Seq[NTree[I, Nothing]] = List.empty
}

object NTree {

  def empty[I, A](path: NonEmptyList[I]): NTree[I, A] = NEmpty(path)

  def empty[I, A](n1: I): NTree[I, A] = empty(NonEmptyList(n1, Nil))

  def empty[I, A](n1: I, n2: I): NTree[I, A] = empty(NonEmptyList(n1, List(n2)))

  def empty[I, A](n1: I, n2: I, n3: I): NTree[I, A] = empty(NonEmptyList(n1, List(n2, n3)))

  def par[I, A](path: NonEmptyList[I], as: Seq[NTree[I, A]])(implicit m: Monoid[I]): NTree[I, A] = as.toList match {
    case Nil => empty(path)
    case head :: tail => NBranch(path, Parallel, head :: tail)
  }

  def par[I, A](path: I, as: NTree[I, A]*)(implicit m: Monoid[I]): NTree[I, A] = par(NonEmptyList(path, Nil), as)

  def par[I, A](path: (I, I), as: NTree[I, A]*)(implicit m: Monoid[I]): NTree[I, A] = par(NonEmptyList(path._1, List(path._2)), as)

  def par[I, A](path: (I, I, I), as: NTree[I, A]*)(implicit m: Monoid[I]): NTree[I, A] = par(NonEmptyList(path._1, List(path._2, path._3)), as)

  def seq[I, A](path: NonEmptyList[I], as: Seq[NTree[I, A]])(implicit m: Monoid[I]): NTree[I, A] = as.toList match {
    case Nil => empty(path)
    case head :: tail => NBranch(path, Sequential, head :: tail)
  }

  def seq[I, A](path: I, as: NTree[I, A]*)(implicit m: Monoid[I]): NTree[I, A] = seq(NonEmptyList(path, Nil), as)

  def seq[I, A](path: (I, I), as: NTree[I, A]*)(implicit m: Monoid[I]): NTree[I, A] = seq(NonEmptyList(path._1, List(path._2)), as)

  def seq[I, A](path: (I, I, I), as: NTree[I, A]*)(implicit m: Monoid[I]): NTree[I, A] = seq(NonEmptyList(path._1, List(path._2, path._3)), as)

  def leaf[I, A](path: NonEmptyList[I], a: A): NTree[I, A] = NLeaf[I, A](path, a)

  def leaf[I, A](path: I, a: A): NTree[I, A] = leaf[I, A](NonEmptyList(path, Nil), a)

  def leaf[I, A](path: (I, I), a: A): NTree[I, A] = leaf[I, A](NonEmptyList(path._1, List(path._2)), a)

  def leaf[I, A](path: (I, I, I), a: A): NTree[I, A] = leaf[I, A](NonEmptyList(path._1, List(path._2, path._3)), a)

  implicit def nTreeMonoid[I, A](implicit mi: Monoid[I], ordering: Ordering[I], ma: Monoid[A]): Monoid[NTree[I, A]] = new Monoid[NTree[I, A]] {
    override def empty: NTree[I, A] = NTree.empty(NonEmptyList(mi.empty, Nil))

    private def kind(l: ParSeq, r: ParSeq): ParSeq = (l, r) match {
      case (Sequential, Sequential) => Sequential
      case (Sequential, Parallel) => Sequential
      case (Parallel, Sequential) => Sequential
      case (Parallel, Parallel) => Parallel
    }

    private def merge(path: NonEmptyList[I])(left: NTree[I, A], right: NTree[I, A]): NTree[I, A] = (left, right) match {
      case (NBranch(_, lk, _), NBranch(_, rk, _)) => iterate(path)(left, right)((a, b) => NBranch.apply(a, kind(lk, rk), b))
      case (NBranch(_, lk, _), NLeaf(_, _)) => iterate(path)(left, right)((a, b) => NBranch.apply(a, lk, b))
      case (NBranch(_, kind, ls), NEmpty(_)) => NBranch(path, kind, ls)

      case (NLeaf(_, _), NBranch(_, rk, _)) => iterate(path)(left, right)((a, b) => NBranch.apply(a, rk, b))
      case (NLeaf(_, lv), NLeaf(_, rv)) => NTree.leaf(path, ma.combine(lv, rv))
      case (NLeaf(_, _), NEmpty(_)) => left

      case (NEmpty(_), NBranch(_, _, _)) => right
      case (NEmpty(_), NLeaf(_, _)) => right
      case (NEmpty(_), NEmpty(_)) => NTree.empty(path)
    }

    private def join(path: NonEmptyList[I])(left: NTree[I, A], right: NTree[I, A]): NTree[I, A] = (left, right) match {
      case (NBranch(_, lk, _), NBranch(_, rk, _)) => iterate(path)(left, right)((a, b) => NBranch.apply(a, kind(lk, rk), b))
      case (NBranch(pathL, kind, treeL), NLeaf(pathR, valueR)) => NBranch(pathL, kind, treeL ++ Seq[NTree[I, A]](NTree.leaf(pathL ::: pathR, valueR)))
      case (NBranch(_, kind, ls), NEmpty(_)) => NBranch(path, kind, ls)

      case (NLeaf(_, _), NBranch(_, rk, _)) => iterate(path)(left, right)((a, b) => NBranch.apply(a, rk, b))
      case (NLeaf(_, lv), NLeaf(_, rv)) => NTree.leaf(path, ma.combine(lv, rv))
      case (NLeaf(_, _), NEmpty(_)) => left

      case (NEmpty(_), NBranch(_, _, _)) => right
      case (NEmpty(_), NLeaf(_, _)) => right
      case (NEmpty(_), NEmpty(_)) => NTree.empty(path)
    }

    private def iterate(path: NonEmptyList[I])(left: NTree[I, A], right: NTree[I, A])(factory: (NonEmptyList[I], Seq[NTree[I, A]]) => NTree[I, A]): NTree[I, A] = {
      val nodes = (left.ids union right.ids).map { id =>
        merge(path ++ List(id))(
          left(id).getOrElse(NTree.empty(path)),
          right(id).getOrElse(NTree.empty(path))
        )
      }
      factory.apply(path, nodes.toSeq.sortBy(_.id))
    }

    override def combine(left: NTree[I, A], right: NTree[I, A]): NTree[I, A] =
      if (left.path == right.path) merge(left.path)(left, right)
      else join(left.path)(left, right)
  }

  object NTreeFunctor {
    def apply[I]: NTreeFunctor[I] = new NTreeFunctor[I]
  }

  class NTreeFunctor[I] {
    type X[A] = NTree[I, A]
    implicit val nTreeFunctor: Functor[X] = new Functor[X] {
      override def map[A, B](fa: X[A])(f: A => B): X[B] = fa match {
        case NBranch(path, kind, children) => NBranch(path, kind, children.map(child => map(child)(f)))
        case NLeaf(path, value) => NLeaf(path, f(value))
        case NEmpty(path) => NEmpty(path)
      }
    }
  }

}
