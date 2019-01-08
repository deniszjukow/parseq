package org.zewx.parseq

import cats.data.NonEmptyList
import org.zewx.parseq.ParSeq.ParSeqTree
import cats.instances.map._
import cats.instances.string._
import cats.syntax.semigroup._
import cats.instances.tuple._
import cats.instances.int._
import cats.kernel.Monoid
import org.zewx.parseq.ParSeq.branchTypeMonoid
import cats.data.NonEmptyList.{of => path}

object Task {

  // case object InitialReq extends Req

  sealed trait Res {
    def isEmpty: Boolean = false

    def orElse[A](f: => Res): Res = Res.orElse(this)(f)
  }

  case object EmptyRes extends Res {
    override def isEmpty: Boolean = true
  }

  case class RequiredPropertyRes(path: NonEmptyList[Id], property: String) extends Res

  case class RequiredNodeRes(kind: ParSeq, path: NonEmptyList[Id], ctrlName: CtrlName) extends Res

  object Res {

    val empty: Res = EmptyRes

    def requiredProperty(path: NonEmptyList[Id], property: String) =
      RequiredPropertyRes(path, property)

    def requiredNode(kind: ParSeq, path: NonEmptyList[Id], ctrlName: CtrlName) =
      RequiredNodeRes(kind, path, ctrlName)

    def orElse[A](res: Res)(f: => Res): Res = if (res.isEmpty) f else res
  }

  type Id = Int
  type CtrlName = String
  type Data = Map[String, String]
  type Elem = (CtrlName, Data)
  type Tree = ParSeqTree[Id, Elem]
  type Chain = List[(Id, Data)]
  type Factory = CtrlName => Ctrl

  trait Ctrl {
    def selfCheck(ctx: Context): Res

    def postCheck(ctx: Context): Res
  }

  object Ctrl {
    val empty: Ctrl = new Ctrl {
      override def selfCheck(ctx: Context): Res = Res.empty

      override def postCheck(ctx: Context): Res = Res.empty
    }
  }

  case class Context(path: NonEmptyList[Id], ctrlName: CtrlName, data: Data, children: Seq[Tree])

  def run(factory: Factory, tree: Tree): Res = {
    def go(t: NTree[ParSeq, Id, Context]): Res = t match {
      case NBranch(_, _, ctx, nodes) =>
        val ctrl = factory(ctx.ctrlName)
        ctrl.selfCheck(ctx)
          .orElse(nodes.foldLeft(Res.empty) { case (acc, node) =>
            if (!acc.isEmpty) acc else go(node)
          })
          .orElse(ctrl.postCheck(ctx))
      case NLeaf(_, ctx) =>
        val ctrl = factory(ctx.ctrlName)
        ctrl.selfCheck(ctx).orElse(ctrl.postCheck(ctx))
      case NEmpty =>
        Res.empty
    }

    go(enrich(tree))
  }

  def enrich(tree: Tree): NTree[ParSeq, Id, Context] = {
    NTree.map(tree.zipWithChildren.zipWithPath) {
      case (path, (children, (name, data))) => Context(path, name, data, children)
    }
  }

  case class SumCtrl(step: Id)(implicit m: Monoid[Id]) extends Ctrl {
    override def selfCheck(ctx: Context): Res =
      if (!ctx.data.contains("n"))
        Res.requiredProperty(ctx.path, "n")
      else
        Res.empty

    override def postCheck(ctx: Context): Res =
      if (ctx.children.size < ctx.data("n").toInt) {
        val ch = ctx.children
        val id =
          if (ch.isEmpty) m.empty
          else if (ch.last.ids.isEmpty) m.empty
          else m.combine(ch.last.ids.last, step)

        Res.requiredNode(PAR, ctx.path ++ List(id), "value")
      } else Res.empty
  }

  val valueCtrl: Ctrl = new Ctrl {
    override def selfCheck(ctx: Context): Res =
      if (!ctx.data.contains("x"))
        Res.requiredProperty(ctx.path, "x")
      else
        Res.empty

    override def postCheck(ctx: Context): Res = Res.empty
  }

  def main(args: Array[String]): Unit = {

    val factory: Factory = name => Map[CtrlName, Ctrl](
      "sum" -> SumCtrl(1),
      "value" -> valueCtrl
    ).getOrElse(name, Ctrl.empty)

    org.zewx.parseq.NTree.nTreeMonoid[ParSeq, Id, Elem]
    import ParSeq._

    // 0
    val t0 = leaf[Int, Elem](path(0), ("sum", Map.empty[String, String]))

    // 1
    val r1 = run(factory, t0)
    val t1 = t0 |+| fromPath(path(0), ("", Map("n" -> "2")))

    // 2
    val r2 = run(factory, t1)
    val t2 = t1 |+| branch[Id, Elem](PAR, path(0), ("", Map()), Seq(leaf(path(0, 0), ("value", Map.empty[String, String]))))

    // 3
    val r3 = run(factory, t2)
    val t3 = t2 |+| fromPath(path(0, 0), ("", Map("x" -> "10")))

    // 4
    val r4 = run(factory, t3)
    val t4 = t3 |+| branch[Id, Elem](PAR, path(0), ("", Map()), Seq(leaf(path(0, 1), ("value", Map.empty[String, String]))))

    // 5
    val r5 = run(factory, t4)
    val t5 = t4 |+| fromPath(path(0, 1), ("", Map("x" -> "20")))

    // 6
    val r6 = run(factory, t5)

    println(r6)
  }
}