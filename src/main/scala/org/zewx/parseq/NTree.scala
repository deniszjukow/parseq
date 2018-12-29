package org.zewx.parseq

import cats.{Functor, Monoid}
import cats.data.NonEmptyList

sealed trait NTree[+I, +A] {
  def path: NonEmptyList[I]

  def id: I = path.last

  def children: Seq[NTree[I, A]]

  def ids[J >: I]: Set[J] = children.map(c => c.id).toSet[J]

  def apply[J >: I](id: J): Option[NTree[J, A]] = this match {
    case NSeq(_, children) => children.find(_.id == id)
    case NPar(_, children) => children.find(_.id == id)
    case leaf@NLeaf(_, _) => if (leaf.id == id) Some(leaf) else None
    case NEmpty(_) => None
  }
}

case class NPar[I, A](path: NonEmptyList[I], children: Seq[NTree[I, A]]) extends NTree[I, A]

case class NSeq[I, A](path: NonEmptyList[I], children: Seq[NTree[I, A]]) extends NTree[I, A]

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
    case head :: tail => NPar(path, head :: tail)
  }

  def par[I, A](path: I, as: NTree[I, A]*)(implicit m: Monoid[I]): NTree[I, A] = par(NonEmptyList(path, Nil), as)

  def par[I, A](path: (I, I), as: NTree[I, A]*)(implicit m: Monoid[I]): NTree[I, A] = par(NonEmptyList(path._1, List(path._2)), as)

  def par[I, A](path: (I, I, I), as: NTree[I, A]*)(implicit m: Monoid[I]): NTree[I, A] = par(NonEmptyList(path._1, List(path._2, path._3)), as)

  def seq[I, A](path: NonEmptyList[I], as: Seq[NTree[I, A]])(implicit m: Monoid[I]): NTree[I, A] = as.toList match {
    case Nil => empty(path)
    case head :: tail => NSeq(path, head :: tail)
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

    private def merge(path: NonEmptyList[I])(left: NTree[I, A], right: NTree[I, A]): NTree[I, A] = (left, right) match {
      case (NPar(_, _), NPar(_, _)) => iterate(path)(left, right)(NPar.apply)
      case (NPar(_, _), NSeq(_, _)) => iterate(path)(left, right)(NSeq.apply)
      case (NPar(_, _), NLeaf(_, _)) => iterate(path)(left, right)(NPar.apply)
      case (NPar(_, ls), NEmpty(_)) => NPar(path, ls)

      case (NSeq(_, _), NPar(_, _)) => iterate(path)(left, right)(NSeq.apply)
      case (NSeq(_, _), NSeq(_, _)) => iterate(path)(left, right)(NSeq.apply)
      case (NSeq(_, _), NLeaf(_, _)) => iterate(path)(left, right)(NSeq.apply)
      case (NSeq(_, ls), NEmpty(_)) => NSeq(path, ls)

      case (NLeaf(_, _), NPar(_, _)) => iterate(path)(left, right)(NPar.apply)
      case (NLeaf(_, _), NSeq(_, _)) => iterate(path)(left, right)(NSeq.apply)
      case (NLeaf(_, lv), NLeaf(_, rv)) => NTree.leaf(path, ma.combine(lv, rv))
      case (NLeaf(_, _), NEmpty(_)) => left

      case (NEmpty(_), NPar(_, _)) => right
      case (NEmpty(_), NSeq(_, _)) => right
      case (NEmpty(_), NLeaf(_, _)) => right
      case (NEmpty(_), NEmpty(_)) => NTree.empty(path)
    }

    private def join(path: NonEmptyList[I])(left: NTree[I, A], right: NTree[I, A]): NTree[I, A] = (left, right) match {
      case (NPar(_, _), NPar(_, _)) => iterate(path)(left, right)(NPar.apply)
      case (NPar(_, _), NSeq(_, _)) => iterate(path)(left, right)(NSeq.apply)
      case (NPar(pathL, treeL), NLeaf(pathR, valueR)) => NPar(pathL, treeL ++ Seq(NTree.leaf(pathL ::: pathR, valueR)))
      case (NPar(_, ls), NEmpty(_)) => NPar(path, ls)

      case (NSeq(_, _), NPar(_, _)) => iterate(path)(left, right)(NSeq.apply)
      case (NSeq(_, _), NSeq(_, _)) => iterate(path)(left, right)(NSeq.apply)
      case (NSeq(pathL, treeL), NLeaf(pathR, valueR)) => NSeq(pathL, treeL ++ Seq(NTree.leaf(pathL ::: pathR, valueR)))
      case (NSeq(_, ls), NEmpty(_)) => NSeq(path, ls)

      case (NLeaf(_, _), NPar(_, _)) => iterate(path)(left, right)(NPar.apply)
      case (NLeaf(_, _), NSeq(_, _)) => iterate(path)(left, right)(NSeq.apply)
      case (NLeaf(_, lv), NLeaf(_, rv)) => NTree.leaf(path, ma.combine(lv, rv))
      case (NLeaf(_, _), NEmpty(_)) => left

      case (NEmpty(_), NPar(_, _)) => right
      case (NEmpty(_), NSeq(_, _)) => right
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
      factory(path, nodes.toSeq.sortBy(_.id))
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
        case NSeq(path, children) => NSeq(path, children.map(child => map(child)(f)))
        case NPar(path, children) => NPar(path, children.map(child => map(child)(f)))
        case NLeaf(path, value) => NLeaf(path, f(value))
        case NEmpty(path) => NEmpty(path)
      }
    }
  }

}
