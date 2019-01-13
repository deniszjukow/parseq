package io.parseq

import cats.instances.map._
import cats.instances.string._
import cats.instances.tuple._
import cats.instances.int._
import cats.syntax.semigroup._
import cats.data.NonEmptyList.{of => path}
import cats.kernel.Monoid
import io.parseq.ParSeq._

import scala.annotation.tailrec
import scala.io.StdIn

object SumExample {

  case class SumCtrl(step: Id)(implicit m: Monoid[Id]) extends Controller {
    override def selfCheck(ctx: Context): Response =
      if (!ctx.data.contains("n"))
        Response.requiredProperty(ctx.path, "n")
      else
        Response.empty

    override def postCheck(ctx: Context): Response =
      if (ctx.children.size < ctx.data("n").toInt) {
        val ch = ctx.children
        val id = if (ch.isEmpty) m.empty else
          ch.last.optId.fold(m.empty)(m.combine(_, step))

        Response.requiredNode(PAR, ctx.path ++ List(id), "value")
      } else Response.empty
  }

  case object ValueCtrl extends Controller {
    override def selfCheck(ctx: Context): Response =
      if (!ctx.data.contains("x"))
        Response.requiredProperty(ctx.path, "x")
      else
        Response.empty

    override def postCheck(ctx: Context): Response = Response.empty
  }


  val factory: Factory = name => Map[CtrlName, Controller](
    "sum" -> SumCtrl(1),
    "value" -> ValueCtrl
  ).getOrElse(name, Controller.empty)

  val validator = TreeValidator(factory)
  val emptyElem: Elem = ("", Map.empty[String, String])

  @tailrec
  def run(tree: Tree[Elem]): Tree[Elem] = {
    println(tree)
    validator(tree) match {
      case RequiredPropertyRes(path, property) =>
        // TODO: add validation here...
        val value = StdIn.readLine("enter [%s]: ", property)
        val increment = fromPath(path, ("", Map(property -> value)))
        run(tree |+| increment)
      case RequiredNodeRes(_, path, ctrlName) =>
        val increment = fromPath(path, (ctrlName, Map.empty[String, String]))
        run(tree |+| increment)
      case EmptyRes => tree
    }
  }

  def test(): Unit = {
    val t0 = leaf[Int, Elem](path(0), ("sum", Map()))

    val r1 = validator(t0)
    val t1 = t0 |+| fromPath(path(0), ("", Map("n" -> "2")))

    val r2 = validator(t1)
    val t2 = t1 |+| branch[Id, Elem](PAR, path(0), emptyElem, Seq(leaf(path(0, 0), ("value", Map()))))

    val r3 = validator(t2)
    val t3 = t2 |+| fromPath(path(0, 0), ("", Map("x" -> "10")))

    val r4 = validator(t3)
    val t4 = t3 |+| branch[Id, Elem](PAR, path(0), emptyElem, Seq(leaf(path(0, 1), ("value", Map()))))

    val r5 = validator(t4)
    val t5 = t4 |+| fromPath(path(0, 1), ("", Map("x" -> "20")))

    val r6 = validator(t5)

    println(r6)
  }

  def main(args: Array[String]): Unit = {
    println("done: " + run(leaf[Int, Elem](path(0), ("sum", Map()))))
  }
}