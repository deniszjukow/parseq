package org.zewx.parseq

import cats.{Functor, Monoid}
import cats.data.NonEmptyList


sealed trait BranchType

case object Parallel extends BranchType

case object Sequential extends BranchType

case object BranchType {
  def apply(l: BranchType, r: BranchType): BranchType = (l, r) match {
    case (Sequential, Sequential) => Sequential
    case (Sequential, Parallel) => Sequential
    case (Parallel, Sequential) => Sequential
    case (Parallel, Parallel) => Parallel
  }
}


sealed trait NTree[+I, +A] {

  def children: Seq[NTree[I, A]]

  def ids[J >: I]: Set[J] = children.flatMap(c => c.optId.toSeq).toSet[J]

  protected def optId: Option[I]

  def apply[J >: I](id: J): Option[NTree[J, A]] = this match {
    case NBranch(_, _, ns) => ns.find(_.optId.contains(id))
    case NLeaf(_, _) => None
    case NEmpty => None
  }

  def updated[J >: I, B >: A](id: J, tree: NTree[J, B])(implicit ord: Ordering[J]): NTree[I, A] = if (tree.isEmpty) this else this match {
    case NBranch(t, p, ns) => NBranch(t, p, (ns.filter(!_.optId.contains(id)) ++ Seq(tree)).sortBy(_.optId)).asInstanceOf[NTree[I, A]]
    case leaf@NLeaf(_, _) => if (leaf.id == id) tree.asInstanceOf[NTree[I, A]] else leaf
    case empty@NEmpty => empty
  }

  def isEmpty: Boolean = this match {
    case NBranch(_, _, ns) => ns.isEmpty
    case NLeaf(_, _) => false
    case NEmpty => true
  }
}

case class NBranch[I, A](branchType: BranchType, path: NonEmptyList[I], children: Seq[NTree[I, A]]) extends NTree[I, A] {
  def id: I = path.last


  override protected def optId: Option[I] = Some(id)
}

case class NLeaf[I, A](path: NonEmptyList[I], value: A) extends NTree[I, A] {
  def id: I = path.last

  override protected def optId: Option[I] = Some(id)

  override def children: Seq[NTree[I, Nothing]] = List.empty
}

case object NEmpty extends NTree[Nothing, Nothing] {
  override def ids[J]: Set[J] = Set.empty

  override protected def optId: Option[Nothing] = None

  override def children: Seq[NTree[Nothing, Nothing]] = List.empty
}

object NTree {

  def empty[I, A]: NTree[I, A] = NEmpty

  def branch[I, A](branchType: BranchType, path: NonEmptyList[I], as: Seq[NTree[I, A]])(implicit m: Monoid[I]): NTree[I, A] = as.toList match {
    case Nil => empty
    case head :: tail => NBranch(branchType, path, head :: tail)
  }

  def par[I, A](path: NonEmptyList[I], as: Seq[NTree[I, A]])(implicit m: Monoid[I]): NTree[I, A] = as.toList match {
    case Nil => empty
    case head :: tail => NBranch(Parallel, path, head :: tail)
  }

  def par[I, A](path: I, as: NTree[I, A]*)(implicit m: Monoid[I]): NTree[I, A] = par(NonEmptyList(path, Nil), as)

  def par[I, A](path: (I, I), as: NTree[I, A]*)(implicit m: Monoid[I]): NTree[I, A] = par(NonEmptyList(path._1, List(path._2)), as)

  def par[I, A](path: (I, I, I), as: NTree[I, A]*)(implicit m: Monoid[I]): NTree[I, A] = par(NonEmptyList(path._1, List(path._2, path._3)), as)

  def seq[I, A](path: NonEmptyList[I], as: Seq[NTree[I, A]])(implicit m: Monoid[I]): NTree[I, A] = as.toList match {
    case Nil => empty
    case head :: tail => NBranch(Sequential, path, head :: tail)
  }

  def seq[I, A](path: I, as: NTree[I, A]*)(implicit m: Monoid[I]): NTree[I, A] = seq(NonEmptyList(path, Nil), as)

  def seq[I, A](path: (I, I), as: NTree[I, A]*)(implicit m: Monoid[I]): NTree[I, A] = seq(NonEmptyList(path._1, List(path._2)), as)

  def seq[I, A](path: (I, I, I), as: NTree[I, A]*)(implicit m: Monoid[I]): NTree[I, A] = seq(NonEmptyList(path._1, List(path._2, path._3)), as)

  def leaf[I, A](path: NonEmptyList[I], a: A): NTree[I, A] = NLeaf[I, A](path, a)

  def leaf[I, A](path: I, a: A): NTree[I, A] = leaf[I, A](NonEmptyList(path, Nil), a)

  def leaf[I, A](path: (I, I), a: A): NTree[I, A] = leaf[I, A](NonEmptyList(path._1, List(path._2)), a)

  def leaf[I, A](path: (I, I, I), a: A): NTree[I, A] = leaf[I, A](NonEmptyList(path._1, List(path._2, path._3)), a)

  implicit def nTreeMonoid[I, A](implicit mi: Monoid[I], ordering: Ordering[I], ma: Monoid[A]): Monoid[NTree[I, A]] = new Monoid[NTree[I, A]] {

    override def empty: NTree[I, A] = NTree.empty

    //    private def

    private def merge(left: NTree[I, A], right: NTree[I, A]): NTree[I, A] = (left, right) match {
      case (NBranch(lType, lPath, _), NBranch(rType, _, _)) => zipAll(left, right)(nodes => branch(BranchType(lType, rType), lPath, nodes))
      case (l@NBranch(_, lPath, _), NLeaf(rPath, rValue)) => l.updated(l.id, merge(l(rPath.last).getOrElse(NTree.empty), leaf(mergeLeftId(lPath, rPath), rValue)))
      case (l@NBranch(_, _, _), NEmpty) => l

      case (NLeaf(lPath, lValue), r@NBranch(_, rPath, _)) => r.updated(r.id, merge(leaf(mergeRightId(lPath, rPath), lValue), r(lPath.last).getOrElse(NTree.empty)))
      case (NLeaf(path, lValue), NLeaf(_, rValue)) => leaf(path, ma.combine(lValue, rValue))
      case (l@NLeaf(_, _), NEmpty) => l

      case (NEmpty, r@NBranch(_, _, _)) => r
      case (NEmpty, r@NLeaf(_, _)) => r
      case (l@NEmpty, NEmpty) => l
    }

    def mergeLeftId(lPath: NonEmptyList[I], rPath: NonEmptyList[I]): NonEmptyList[I] = lPath :+ rPath.last

    def mergeRightId(lPath: NonEmptyList[I], rPath: NonEmptyList[I]): NonEmptyList[I] = rPath :+ lPath.last

    private def join(left: NTree[I, A], right: NTree[I, A]): NTree[I, A] = (left, right) match {
      case (l@NBranch(lType, lPath, _), r@NBranch(rType, _, _)) => zipAll(l, r)(nodes => branch(BranchType(lType, rType), lPath, nodes))
      case (NBranch(lType, lPath, lNodes), NLeaf(rPath, rValue)) => branch(lType, lPath, lNodes :+ leaf(mergeLeftId(lPath, rPath), rValue))
      case (NBranch(lType, path, lNodes), NEmpty) => branch(lType, path, lNodes)

      case (NLeaf(lPath, lValue), NBranch(rType, rPath, rNodes)) => branch(rType, rPath, leaf(mergeRightId(lPath, rPath), lValue) +: rNodes)
      case (NLeaf(path, lv), r@NLeaf(_, rv)) => leaf(path, ma.combine(lv, rv))
      case (l@NLeaf(_, _), NEmpty) => l

      case (NEmpty, r@NBranch(_, _, _)) => r
      case (NEmpty, r@NLeaf(_, _)) => r
      case (l@NEmpty, NEmpty) => l
    }

    private def zipAll(left: NTree[I, A], right: NTree[I, A])(factory: Seq[NTree[I, A]] => NTree[I, A]): NTree[I, A] = {
      val nodes = (left.ids union right.ids).map { id =>
        merge(
          left(id).getOrElse(NTree.empty),
          right(id).getOrElse(NTree.empty)
        )
      }
      factory.apply(nodes.toSeq.sortBy(_.optId))
    }

    override def combine(left: NTree[I, A], right: NTree[I, A]): NTree[I, A] = {
      (left, right) match {
        case (l@NBranch(_, lPath, _), r@NBranch(_, rPath, _)) => if (lPath == rPath) merge(l, r) else join(l, r)
        case (l@NBranch(_, lPath, _), r@NLeaf(rPath, _)) => if (lPath == rPath) merge(l, r) else join(l, r)
        case (l@NBranch(_, _, _), NEmpty) => l

        case (l@NLeaf(lPath, _), r@NBranch(_, rPath, _)) => if (lPath == rPath) merge(l, r) else join(l, r)
        case (l@NLeaf(lPath, _), r@NLeaf(rPath, _)) => if (lPath == rPath) merge(l, r) else join(l, r)
        case (l@NLeaf(_, _), NEmpty) => l

        case (NEmpty, r@NBranch(_, _, _)) => r
        case (NEmpty, r@NLeaf(_, _)) => r
        case (l@NEmpty, NEmpty) => l
      }
    }
  }

  object NTreeFunctor {
    def apply[I]: NTreeFunctor[I] = new NTreeFunctor[I]
  }

  class NTreeFunctor[I] {
    type X[A] = NTree[I, A]
    implicit val nTreeFunctor: Functor[X] = new Functor[X] {
      override def map[A, B](fa: X[A])(f: A => B): X[B] = fa match {
        case NBranch(kind, path, children) => NBranch(kind, path, children.map(child => map(child)(f)))
        case NLeaf(path, value) => NLeaf(path, f(value))
        case empty@NEmpty => empty
      }
    }
  }

}
