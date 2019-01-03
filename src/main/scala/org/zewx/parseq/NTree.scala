package org.zewx.parseq

import cats.{Functor, Monoid}
import cats.data.NonEmptyList


sealed trait BranchType

case object Parallel extends BranchType

case object Sequential extends BranchType

case object BranchType {

  implicit def branchTypeMonoid: Monoid[BranchType] = new Monoid[BranchType] {
    override def empty: BranchType = Sequential

    override def combine(x: BranchType, y: BranchType): BranchType = (x, y) match {
      case (Sequential, Sequential) => Sequential
      case (Sequential, Parallel) => Sequential
      case (Parallel, Sequential) => Sequential
      case (Parallel, Parallel) => Parallel
    }
  }
}


sealed trait NTree[+T, +I, +A] {

  def children: Seq[NTree[T, I, A]]

  def ids[II >: I]: Set[II] = children.flatMap(c => c.optId.toSeq).toSet[II]

  protected def optId: Option[I]

  def apply[II >: I](id: II): Option[NTree[T, II, A]] = this match {
    case NBranch(_, _, ns) => ns.find(_.optId.contains(id))
    case NLeaf(_, _) => None
    case NEmpty => None
  }

  def updated[TT >: T, II >: I, B >: A](id: II, tree: NTree[TT, II, B])(implicit ord: Ordering[II]): NTree[T, I, A] = if (tree.isEmpty) this else this match {
    case NBranch(t, p, ns) => NBranch(t, p, (ns.filter(!_.optId.contains(id)) ++ Seq(tree)).sortBy(_.optId)).asInstanceOf[NTree[T, I, A]]
    case leaf@NLeaf(_, _) => if (leaf.id == id) tree.asInstanceOf[NTree[T, I, A]] else leaf
    case empty@NEmpty => empty
  }

  def isEmpty: Boolean = this match {
    case NBranch(_, _, ns) => ns.isEmpty
    case NLeaf(_, _) => false
    case NEmpty => true
  }

  def mapPaths[II](f: NonEmptyList[I] => NonEmptyList[II]): NTree[T, II, A] = this match {
    case NBranch(t, p, ns) => NBranch(t, f(p), ns.map(n => n.mapPaths(f)))
    case NLeaf(p, v) => NLeaf(f(p), v)
    case NEmpty => NEmpty
  }
}

case class NBranch[T, I, A](branchType: T, path: NonEmptyList[I], children: Seq[NTree[T, I, A]]) extends NTree[T, I, A] {
  def id: I = path.last

  override protected def optId: Option[I] = Some(id)
}

case class NLeaf[T, I, A](path: NonEmptyList[I], value: A) extends NTree[T, I, A] {
  def id: I = path.last

  override protected def optId: Option[I] = Some(id)

  override def children: Seq[NTree[T, I, Nothing]] = List.empty
}

case object NEmpty extends NTree[Nothing, Nothing, Nothing] {
  override def ids[J]: Set[J] = Set.empty

  override protected def optId: Option[Nothing] = None

  override def children: Seq[NTree[Nothing, Nothing, Nothing]] = List.empty
}

object NTree {

  def empty[T, I, A]: NTree[T, I, A] = NEmpty

  def branch[T, I, A](branchType: T, path: NonEmptyList[I], as: Seq[NTree[T, I, A]])(implicit m: Monoid[I]): NTree[T, I, A] = as.toList match {
    case Nil => empty
    case head :: tail => NBranch(branchType, path, head :: tail)
  }

  def par[I, A](path: NonEmptyList[I], as: Seq[NTree[BranchType, I, A]])(implicit m: Monoid[I]): NTree[BranchType, I, A] = as.toList match {
    case Nil => empty
    case head :: tail => NBranch(Parallel, path, head :: tail)
  }

  def par[I, A](path: I, as: NTree[BranchType, I, A]*)(implicit m: Monoid[I]): NTree[BranchType, I, A] = par(NonEmptyList(path, Nil), as)

  def par[I, A](path: (I, I), as: NTree[BranchType, I, A]*)(implicit m: Monoid[I]): NTree[BranchType, I, A] = par(NonEmptyList(path._1, List(path._2)), as)

  def par[I, A](path: (I, I, I), as: NTree[BranchType, I, A]*)(implicit m: Monoid[I]): NTree[BranchType, I, A] = par(NonEmptyList(path._1, List(path._2, path._3)), as)

  def seq[I, A](path: NonEmptyList[I], as: Seq[NTree[BranchType, I, A]])(implicit m: Monoid[I]): NTree[BranchType, I, A] = as.toList match {
    case Nil => empty
    case head :: tail => NBranch(Sequential, path, head :: tail)
  }

  def seq[I, A](path: I, as: NTree[BranchType, I, A]*)(implicit m: Monoid[I]): NTree[BranchType, I, A] = seq(NonEmptyList(path, Nil), as)

  def seq[I, A](path: (I, I), as: NTree[BranchType, I, A]*)(implicit m: Monoid[I]): NTree[BranchType, I, A] = seq(NonEmptyList(path._1, List(path._2)), as)

  def seq[I, A](path: (I, I, I), as: NTree[BranchType, I, A]*)(implicit m: Monoid[I]): NTree[BranchType, I, A] = seq(NonEmptyList(path._1, List(path._2, path._3)), as)

  def leaf[T, I, A](path: NonEmptyList[I], a: A): NTree[T, I, A] = NLeaf[T, I, A](path, a)

  def leaf[T, I, A](path: I, a: A): NTree[T, I, A] = leaf[T, I, A](NonEmptyList(path, Nil), a)

  def leaf[T, I, A](path: (I, I), a: A): NTree[T, I, A] = leaf[T, I, A](NonEmptyList(path._1, List(path._2)), a)

  def leaf[T, I, A](path: (I, I, I), a: A): NTree[T, I, A] = leaf[T, I, A](NonEmptyList(path._1, List(path._2, path._3)), a)

  implicit def nTreeMonoid[T, I, A](implicit typeMonoid: Monoid[T], idMonoid: Monoid[I], idOrdering: Ordering[I], valueMonoid: Monoid[A]): Monoid[NTree[T, I, A]] = new Monoid[NTree[T, I, A]] {

    override def empty: NTree[T, I, A] = NTree.empty

    private def merge(left: NTree[T, I, A], right: NTree[T, I, A]): NTree[T, I, A] = (left, right) match {
      case (NBranch(lType, lPath, _), NBranch(rType, _, _)) => zipAll(left, right)(nodes => branch(typeMonoid.combine(lType, rType), lPath, nodes))
      case (l@NBranch(_, lPath, _), NLeaf(rPath, rValue)) => l.updated(l.id, merge(l(rPath.last).getOrElse(NTree.empty), leaf(mergeLeftId(lPath, rPath), rValue)))
      case (l@NBranch(_, _, _), NEmpty) => l

      case (NLeaf(lPath, lValue), r@NBranch(_, rPath, _)) => r.updated(r.id, merge(leaf(mergeRightId(lPath, rPath), lValue), r(lPath.last).getOrElse(NTree.empty)))
      case (NLeaf(path, lValue), NLeaf(_, rValue)) => leaf(path, valueMonoid.combine(lValue, rValue))
      case (l@NLeaf(_, _), NEmpty) => l

      case (NEmpty, r@NBranch(_, _, _)) => r
      case (NEmpty, r@NLeaf(_, _)) => r
      case (l@NEmpty, NEmpty) => l
    }

    def mergeLeftId(lPath: NonEmptyList[I], rPath: NonEmptyList[I]): NonEmptyList[I] = lPath :+ rPath.last

    def mergeRightId(lPath: NonEmptyList[I], rPath: NonEmptyList[I]): NonEmptyList[I] = rPath :+ lPath.last

    import NonEmptyList.one

    private def join(left: NTree[T, I, A], right: NTree[T, I, A]): NTree[T, I, A] = (left, right) match {
      case (l@NBranch(lType, _, _), r@NBranch(rType, _, _)) => branch(typeMonoid.combine(lType, rType), one(idMonoid.empty), Seq(
        l.mapPaths(p => idMonoid.empty :: p),
        r.mapPaths(p => idMonoid.empty :: p)
      ))
      case (l@NBranch(lType, _, _), r@NLeaf(_, _)) => branch(lType, one(idMonoid.empty), Seq(
        l.mapPaths(p => idMonoid.empty :: p),
        r.mapPaths(p => idMonoid.empty :: p)
      ))
      case (NBranch(lType, path, lNodes), NEmpty) => branch(lType, path, lNodes)

      case (l@NLeaf(_, _), r@NBranch(rType, _, _)) => branch(rType, one(idMonoid.empty), Seq(
        l.mapPaths(p => idMonoid.empty :: p),
        r.mapPaths(p => idMonoid.empty :: p)
      ))
      case (l@NLeaf(_, _), r@NLeaf(_, _)) => branch(typeMonoid.empty, one(idMonoid.empty), Seq(
        l.mapPaths(p => idMonoid.empty :: p),
        r.mapPaths(p => idMonoid.empty :: p)))
      case (l@NLeaf(_, _), NEmpty) => l

      case (NEmpty, r@NBranch(_, _, _)) => r
      case (NEmpty, r@NLeaf(_, _)) => r
      case (l@NEmpty, NEmpty) => l
    }

    private def zipAll(left: NTree[T, I, A], right: NTree[T, I, A])(factory: Seq[NTree[T, I, A]] => NTree[T, I, A]): NTree[T, I, A] = {
      val nodes = (left.ids union right.ids).map { id =>
        merge(
          left(id).getOrElse(NTree.empty),
          right(id).getOrElse(NTree.empty)
        )
      }
      factory.apply(nodes.toSeq.sortBy(_.optId))
    }

    override def combine(left: NTree[T, I, A], right: NTree[T, I, A]): NTree[T, I, A] = {
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
    def apply[T, I]: NTreeFunctor[T, I] = new NTreeFunctor[T, I]
  }

  class NTreeFunctor[T, I] {
    type X[A] = NTree[T, I, A]
    implicit val nTreeFunctor: Functor[X] = new Functor[X] {
      override def map[A, B](fa: X[A])(f: A => B): X[B] = fa match {
        case NBranch(kind, path, children) => NBranch(kind, path, children.map(child => map(child)(f)))
        case NLeaf(path, value) => NLeaf(path, f(value))
        case empty@NEmpty => empty
      }
    }
  }

}
